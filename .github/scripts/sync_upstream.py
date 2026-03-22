#!/usr/bin/env python3
"""
Sync IWFM upstream releases from CNRA's CKAN data portal.

Subcommands:
    check       - Query CKAN API, compare against git tags, output new versions
    import      - Download ZIP, extract source, commit to main, create branch + tag
    release     - Create GitHub Release with executables as assets
    create-issue - Create a review GitHub Issue

The upstream ZIP is a nested archive:
    outer.zip
    ├── IWFM-{ver}_SourceCode.zip   → IWFM/IWFM-{ver}/SourceCode/...
    ├── IWFM-{ver}_Executables.zip  → *.exe files
    ├── IWFM-{ver}_DLL.zip          → *.dll files
    ├── IWFM-{ver}_SampleModel.zip
    ├── IWFM-{ver}_Templates.zip
    ├── *.pdf (docs)
    └── ...

Environment variables:
    VERSION_FILTER  - Specific version to import (optional, for workflow_dispatch)
    DRY_RUN         - "true" to check without importing
    VERSION_JSON    - JSON object with version metadata (for import/release)
    GITHUB_TOKEN    - GitHub token for creating releases
    GITHUB_OUTPUT   - File path for GitHub Actions outputs
    ASSET_DIR       - Directory to store extracted executables (default: auto tmpdir)
    NO_PUSH         - "true" to skip git push (for local testing)
"""

import io
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.request
import urllib.error
import zipfile
from pathlib import Path

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CKAN_API_URL = (
    "https://data.cnra.ca.gov/api/3/action/package_show"
    "?id=iwfm-integrated-water-flow-model"
)

VERSION_REGEX = re.compile(r"IWFM-(\d+\.\d+\.\d+)")

TAG_PREFIX = "iwfm-"

# Paths (relative to repo root) that must never be overwritten by upstream.
# Directories must end with '/'.
PROTECTED_PATHS = [
    "CMakeLists.txt",
    "BUILD.md",
    "CLAUDE.md",
    "BENCHMARK_SUMMARY.md",
    ".gitignore",
    ".github/",
    ".claude/",
    "cmake/",
    "docker/",
    "Bin/.gitkeep",
    "build_windows.bat",
    "Build-IWFM.ps1",
    "Build-IWFM-VS.ps1",
    "Build-IWFM-VS-CppSolver.ps1",
    "build-all-release.ps1",
    "build_cpp_solver.bat",
    "diagnose-build.ps1",
    "package-release.ps1",
    "SourceCode/CMakeLists.txt",
    "SourceCode/IWFM-kernel/CMakeLists.txt",
    "SourceCode/IWFM-kernel/cpp/",
    "SourceCode/ResultsExtract/",
    "SourceCode/tests/",
]

MAX_RETRIES = 3
RETRY_BACKOFF = [30, 60, 120]  # seconds


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def log(msg: str) -> None:
    print(f"[sync_upstream] {msg}", flush=True)


def run(cmd: list[str], **kwargs) -> subprocess.CompletedProcess:
    """Run a subprocess, printing the command for transparency."""
    log(f"  $ {' '.join(cmd)}")
    return subprocess.run(cmd, check=True, text=True, capture_output=True, **kwargs)


def set_github_output(name: str, value: str) -> None:
    """Write a GitHub Actions output variable."""
    output_file = os.environ.get("GITHUB_OUTPUT")
    if output_file:
        with open(output_file, "a") as f:
            if "\n" in value:
                delimiter = "EOF_DELIM"
                f.write(f"{name}<<{delimiter}\n{value}\n{delimiter}\n")
            else:
                f.write(f"{name}={value}\n")
    else:
        log(f"  OUTPUT {name}={value[:200]}")


def get_existing_tags() -> set[str]:
    """Return the set of existing IWFM version strings from git tags."""
    result = run(["git", "tag", "-l", f"{TAG_PREFIX}*"])
    tags = set()
    for line in result.stdout.strip().splitlines():
        line = line.strip()
        if line.startswith(TAG_PREFIX):
            tags.add(line[len(TAG_PREFIX):])
    return tags


def fetch_ckan_resources() -> list[dict]:
    """Query the CKAN API and return ZIP resources with parsed version info."""
    log(f"Fetching CKAN metadata from {CKAN_API_URL}")

    for attempt in range(MAX_RETRIES):
        try:
            req = urllib.request.Request(CKAN_API_URL)
            req.add_header("User-Agent", "IWFM-Mirror-Bot/1.0")
            with urllib.request.urlopen(req, timeout=30) as resp:
                data = json.loads(resp.read().decode())
            break
        except (urllib.error.URLError, TimeoutError) as e:
            if attempt < MAX_RETRIES - 1:
                wait = RETRY_BACKOFF[attempt]
                log(f"  CKAN API request failed ({e}), retrying in {wait}s...")
                time.sleep(wait)
            else:
                raise RuntimeError(f"CKAN API unavailable after {MAX_RETRIES} attempts: {e}")

    if not data.get("success"):
        raise RuntimeError(f"CKAN API returned error: {data}")

    resources = []
    for res in data["result"]["resources"]:
        if res.get("format", "").upper() != "ZIP":
            continue
        match = VERSION_REGEX.search(res.get("name", ""))
        if not match:
            log(f"  Skipping resource (no version match): {res.get('name')}")
            continue
        version = match.group(1)
        resources.append({
            "version": version,
            "name": res["name"],
            "url": res["url"],
            "resource_id": res["id"],
            "created": res.get("created", ""),
            "last_modified": res.get("last_modified", ""),
        })

    log(f"Found {len(resources)} ZIP resources on CKAN portal")
    return resources


def download_file(url: str, dest: Path) -> None:
    """Download a file using curl (urllib gets 403 on CKAN redirects)."""
    log(f"Downloading {url}")
    dest.parent.mkdir(parents=True, exist_ok=True)

    for attempt in range(MAX_RETRIES):
        try:
            result = subprocess.run(
                ["curl", "-L", "-f", "-o", str(dest), "--progress-bar", url],
                check=True, capture_output=True, text=True,
                timeout=600,
            )
            # Verify it's a valid ZIP
            if not zipfile.is_zipfile(dest):
                raise RuntimeError(f"Downloaded file is not a valid ZIP: {dest}")
            size_mb = dest.stat().st_size // (1024 * 1024)
            log(f"  Download complete: {size_mb} MB")
            return
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired, RuntimeError) as e:
            if attempt < MAX_RETRIES - 1:
                wait = RETRY_BACKOFF[attempt]
                log(f"  Download failed ({e}), retrying in {wait}s...")
                dest.unlink(missing_ok=True)
                time.sleep(wait)
            else:
                dest.unlink(missing_ok=True)
                raise RuntimeError(f"Download failed after {MAX_RETRIES} attempts: {e}")


def find_inner_zip(outer: zipfile.ZipFile, pattern: str) -> str | None:
    """Find an inner ZIP entry matching a pattern (case-insensitive)."""
    pattern_lower = pattern.lower()
    for name in outer.namelist():
        if name.lower().endswith(".zip") and pattern_lower in name.lower():
            return name
    return None


def detect_content_root(zf: zipfile.ZipFile) -> str:
    """Detect the path prefix to strip from an inner ZIP.

    The source code ZIP typically has structure like:
        IWFM/IWFM-2024.2.1594/SourceCode/...
        IWFM/IWFM-2024.2.1594/Budget/...
    We want to strip 'IWFM/IWFM-2024.2.1594/' so we get SourceCode/, Budget/, etc.
    """
    names = zf.namelist()
    if not names:
        raise RuntimeError("ZIP file is empty")

    # Find the SourceCode/ directory and work backwards
    for name in names:
        parts = name.split("/")
        for i, part in enumerate(parts):
            if part == "SourceCode":
                return "/".join(parts[:i]) + "/" if i > 0 else ""

    # Fallback: use common prefix of all entries
    first_part = names[0].split("/")[0]
    if all(n.startswith(first_part + "/") or n == first_part for n in names):
        second_parts = set()
        for n in names:
            rest = n[len(first_part) + 1:]
            if "/" in rest:
                second_parts.add(rest.split("/")[0])
        if len(second_parts) == 1:
            return first_part + "/" + second_parts.pop() + "/"
        return first_part + "/"
    return ""


def is_protected(path: str) -> bool:
    """Check if a relative path is in the protected paths list."""
    for p in PROTECTED_PATHS:
        if p.endswith("/"):
            if path == p.rstrip("/") or path.startswith(p):
                return True
        else:
            if path == p:
                return True
    return False


def is_executable_asset(name: str) -> bool:
    """Check if a filename is an executable/DLL that should be a release asset."""
    lower = name.lower()
    return lower.endswith((".exe", ".dll"))


# ---------------------------------------------------------------------------
# Subcommands
# ---------------------------------------------------------------------------


def cmd_check() -> None:
    """Check for new upstream releases not yet tagged in this repo."""
    resources = fetch_ckan_resources()
    existing = get_existing_tags()

    log(f"Existing tags: {sorted(existing)}")

    version_filter = os.environ.get("VERSION_FILTER", "").strip()
    dry_run = os.environ.get("DRY_RUN", "").lower() == "true"

    # Determine the latest existing tag's CKAN created date for forward-only filtering
    latest_tag_created = None
    if existing:
        ckan_dates = {r["version"]: r.get("created", "") for r in resources}
        for v in existing:
            d = ckan_dates.get(v, "")
            if d and (latest_tag_created is None or d > latest_tag_created):
                latest_tag_created = d
        if latest_tag_created:
            log(f"Latest imported version CKAN date: {latest_tag_created}")

    # Filter to new versions only
    new_resources = []
    for res in resources:
        if res["version"] in existing:
            log(f"  Already imported: {res['version']}")
            continue
        if version_filter and version_filter != res["version"]:
            continue
        # Forward-only: skip older versions (unless version_filter is set)
        if not version_filter and latest_tag_created and res.get("created", "") <= latest_tag_created:
            log(f"  Skipping older version: {res['version']} (created {res.get('created', 'unknown')})")
            continue
        new_resources.append(res)

    new_resources.sort(key=lambda r: r.get("created", ""))

    if new_resources:
        log(f"New versions to import: {[r['version'] for r in new_resources]}")
    else:
        log("No new versions found")

    has_new = len(new_resources) > 0 and not dry_run
    set_github_output("has_new", str(has_new).lower())
    set_github_output("new_versions", json.dumps(new_resources))

    if dry_run and new_resources:
        log("DRY RUN: would import the above versions")

    for r in new_resources:
        log(f"  {r['version']}: {r['name']} ({r['url']})")


def cmd_import() -> None:
    """Download and import a single upstream release."""
    version_json = os.environ.get("VERSION_JSON", "")
    if not version_json:
        raise RuntimeError("VERSION_JSON environment variable is required")

    info = json.loads(version_json)
    version = info["version"]
    url = info["url"]
    tag_name = f"{TAG_PREFIX}{version}"
    no_push = os.environ.get("NO_PUSH", "").lower() == "true"

    log(f"=== Importing IWFM {version} ===")

    # Idempotency check
    existing = get_existing_tags()
    if version in existing:
        log(f"Tag {tag_name} already exists, skipping")
        return

    repo_root = Path(run(["git", "rev-parse", "--show-toplevel"]).stdout.strip())
    asset_dir = Path(os.environ.get("ASSET_DIR", tempfile.mkdtemp(prefix="iwfm-assets-")))
    asset_dir.mkdir(parents=True, exist_ok=True)

    # 1. Download the outer ZIP
    zip_path = Path(tempfile.mkdtemp()) / f"iwfm-{version}.zip"
    download_file(url, zip_path)

    with zipfile.ZipFile(zip_path, "r") as outer:
        log(f"Outer ZIP contains {len(outer.namelist())} entries:")
        for entry in outer.namelist():
            info_entry = outer.getinfo(entry)
            log(f"  {entry} ({info_entry.file_size // (1024*1024)} MB)")

        # 2. Extract executables from inner Executables ZIP
        exe_zip_name = find_inner_zip(outer, "executables")
        if exe_zip_name:
            log(f"Extracting executables from {exe_zip_name}...")
            exe_data = outer.read(exe_zip_name)
            with zipfile.ZipFile(io.BytesIO(exe_data)) as exe_zip:
                for entry in exe_zip.namelist():
                    basename = os.path.basename(entry)
                    if basename and is_executable_asset(basename):
                        target = asset_dir / basename
                        with exe_zip.open(entry) as src, open(target, "wb") as dst:
                            shutil.copyfileobj(src, dst)
                        log(f"  Saved: {basename}")

        # 3. Extract DLLs from inner DLL ZIP
        dll_zip_name = find_inner_zip(outer, "dll")
        if dll_zip_name:
            log(f"Extracting DLLs from {dll_zip_name}...")
            dll_data = outer.read(dll_zip_name)
            with zipfile.ZipFile(io.BytesIO(dll_data)) as dll_zip:
                for entry in dll_zip.namelist():
                    basename = os.path.basename(entry)
                    if basename and is_executable_asset(basename):
                        target = asset_dir / basename
                        with dll_zip.open(entry) as src, open(target, "wb") as dst:
                            shutil.copyfileobj(src, dst)
                        log(f"  Saved: {basename}")

        asset_count = len([f for f in asset_dir.iterdir() if is_executable_asset(f.name)]) if asset_dir.exists() else 0
        log(f"Total executable assets: {asset_count}")

        # 4. Extract source code from inner SourceCode ZIP
        src_zip_name = find_inner_zip(outer, "sourcecode")
        if not src_zip_name:
            raise RuntimeError("No SourceCode ZIP found inside the outer archive")

        log(f"Processing source from {src_zip_name}...")
        src_data = outer.read(src_zip_name)

    with zipfile.ZipFile(io.BytesIO(src_data)) as src_zip:
        content_root = detect_content_root(src_zip)
        log(f"Source ZIP content root: '{content_root}' ({len(src_zip.namelist())} entries)")

        # 5. Back up protected paths
        backup_dir = Path(tempfile.mkdtemp(prefix="iwfm-protected-"))
        log("Backing up protected paths...")
        for p in PROTECTED_PATHS:
            src_path = repo_root / p
            dst_path = backup_dir / p
            if src_path.is_dir() and src_path.exists():
                shutil.copytree(src_path, dst_path, dirs_exist_ok=True)
                log(f"  Backed up directory: {p}")
            elif src_path.is_file():
                dst_path.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(src_path, dst_path)
                log(f"  Backed up file: {p}")

        # 6. Clear existing source and upstream top-level dirs
        source_dir = repo_root / "SourceCode"
        if source_dir.exists():
            log("Clearing non-protected SourceCode/ content...")
            for item in list(source_dir.iterdir()):
                rel = f"SourceCode/{item.name}"
                if item.is_dir():
                    rel += "/"
                if not is_protected(rel):
                    if item.is_dir():
                        shutil.rmtree(item)
                    else:
                        item.unlink()

        # Clear legacy VS project directories
        upstream_toplevel_dirs = [
            "Budget", "BuildAll", "IWFM_DLL", "PreProcessor",
            "Simulation", "Simulation_MM", "Simulation_Parallel", "ZBudget",
        ]
        for d in upstream_toplevel_dirs:
            p = repo_root / d
            if p.exists():
                shutil.rmtree(p)

        # 7. Extract upstream source
        log("Extracting upstream source...")
        extracted_count = 0
        for entry in src_zip.namelist():
            if entry.endswith("/"):
                continue

            # Strip the content root prefix (e.g., "IWFM/IWFM-2024.2.1594/")
            rel_path = entry
            if content_root and rel_path.startswith(content_root):
                rel_path = rel_path[len(content_root):]

            if not rel_path:
                continue

            # Skip executables
            basename = os.path.basename(rel_path)
            if is_executable_asset(basename):
                continue

            # Skip protected paths
            if is_protected(rel_path):
                continue

            # Extract to repo
            target = repo_root / rel_path
            target.parent.mkdir(parents=True, exist_ok=True)
            with src_zip.open(entry) as src_file, open(target, "wb") as dst_file:
                shutil.copyfileobj(src_file, dst_file)
            extracted_count += 1

        log(f"Extracted {extracted_count} source files")

    # 8. Restore protected files
    log("Restoring protected paths...")
    for p in PROTECTED_PATHS:
        src_path = backup_dir / p
        dst_path = repo_root / p
        if src_path.is_dir() and src_path.exists():
            shutil.copytree(src_path, dst_path, dirs_exist_ok=True)
        elif src_path.is_file():
            dst_path.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src_path, dst_path)

    # 9. Update CMakeLists.txt version if it exists
    version_str = None
    version_fi = repo_root / "SourceCode" / "IWFM_Version" / "IWFMVersion.fi"
    if version_fi.exists():
        content = version_fi.read_text()
        match = re.search(r"f_cTag\s*=\s*'([^']+)'", content)
        if match:
            version_str = match.group(1).strip()
    else:
        revision_fi = repo_root / "SourceCode" / "IWFM_Core_Version" / "Revision.fi"
        if revision_fi.exists():
            version_str = version

    if version_str:
        cmakelists = repo_root / "CMakeLists.txt"
        if cmakelists.exists():
            cmake_content = cmakelists.read_text()
            cmake_content = re.sub(
                r"(PROJECT_VERSION\s+)\S+",
                rf"\g<1>{version_str}",
                cmake_content,
                count=1,
            )
            cmakelists.write_text(cmake_content)
            log(f"Updated CMakeLists.txt version to {version_str}")

    # 10. Detect locally-modified files that were overwritten
    log("Checking for overwritten locally-modified files...")
    try:
        diff_result = run(["git", "diff", "--name-only"])
        changed_files = [
            f for f in diff_result.stdout.strip().splitlines()
            if f.strip() and not is_protected(f.strip())
        ]
        if changed_files:
            log(f"  {len(changed_files)} upstream files changed:")
            for f in changed_files[:50]:
                log(f"    - {f}")
            changes_file = asset_dir / "changed_files.txt"
            changes_file.write_text("\n".join(changed_files))
    except subprocess.CalledProcessError:
        changed_files = []

    # Also check for new untracked files
    try:
        status_result = run(["git", "status", "--porcelain", "-u"])
        new_files = []
        deleted_files = []
        for line in status_result.stdout.strip().splitlines():
            if line.startswith("??"):
                new_files.append(line[3:].strip())
            elif line.startswith(" D") or line.startswith("D "):
                deleted_files.append(line[3:].strip())
        if new_files:
            log(f"  {len(new_files)} new files added by upstream")
        if deleted_files:
            log(f"  {len(deleted_files)} files removed vs upstream")
    except subprocess.CalledProcessError:
        pass

    # 11. Commit
    run(["git", "add", "-A"])

    # Check if there are changes to commit
    try:
        run(["git", "diff", "--cached", "--quiet"])
        log("No changes detected — upstream source is identical to current")
        return
    except subprocess.CalledProcessError:
        pass  # There are changes, proceed

    commit_msg = f"Import upstream IWFM {version}"
    run(["git", "commit", "-m", commit_msg])
    log(f"Committed: {commit_msg}")

    # 12. Create version branch and tag
    try:
        run(["git", "rev-parse", "--verify", version])
        log(f"Branch {version} already exists, skipping creation")
    except subprocess.CalledProcessError:
        run(["git", "branch", version])
        log(f"Created branch: {version}")

    run(["git", "tag", tag_name])
    log(f"Created tag: {tag_name}")

    # 13. Push (unless NO_PUSH is set)
    if no_push:
        log("NO_PUSH set — skipping push")
    else:
        run(["git", "push", "origin", "main"])
        run(["git", "push", "origin", version])
        run(["git", "push", "origin", tag_name])
        log(f"Pushed main, {version}, and {tag_name} to origin")

    set_github_output("asset_dir", str(asset_dir))
    set_github_output("version", version)
    set_github_output("tag_name", tag_name)

    log(f"=== Import of IWFM {version} complete ===")


def cmd_release() -> None:
    """Create a GitHub Release with executables as assets."""
    version_json = os.environ.get("VERSION_JSON", "")
    if not version_json:
        raise RuntimeError("VERSION_JSON environment variable is required")

    info = json.loads(version_json)
    version = info["version"]
    tag_name = f"{TAG_PREFIX}{version}"

    asset_dir = Path(os.environ.get("ASSET_DIR", f"/tmp/iwfm-assets-{version}"))

    log(f"=== Creating GitHub Release for IWFM {version} ===")

    # Check if release already exists
    try:
        run(["gh", "release", "view", tag_name])
        log(f"Release {tag_name} already exists, skipping")
        return
    except subprocess.CalledProcessError:
        pass

    # Collect assets
    assets = []
    if asset_dir.exists():
        for f in sorted(asset_dir.iterdir()):
            if is_executable_asset(f.name):
                assets.append(str(f))
                log(f"  Asset: {f.name} ({f.stat().st_size // (1024*1024)} MB)")

    # Build release notes
    notes_lines = [
        f"Automated import of IWFM {version} from the "
        f"[CNRA data portal](https://data.cnra.ca.gov/dataset/iwfm-integrated-water-flow-model).",
        "",
    ]

    changes_file = asset_dir / "changed_files.txt" if asset_dir.exists() else None
    if changes_file and changes_file.exists():
        changed = changes_file.read_text().strip().splitlines()
        if changed:
            notes_lines.append("### Files changed from previous version")
            notes_lines.append("")
            for f in changed[:30]:
                notes_lines.append(f"- `{f}`")
            if len(changed) > 30:
                notes_lines.append(f"- ... and {len(changed) - 30} more")
            notes_lines.append("")

    if assets:
        notes_lines.append(f"### Included executables ({len(assets)} files)")
        notes_lines.append("")
        for a in assets:
            notes_lines.append(f"- `{os.path.basename(a)}`")
    else:
        notes_lines.append("*No pre-built executables included in this release.*")

    notes = "\n".join(notes_lines)

    # Write notes to a temp file (avoids shell escaping issues with long notes)
    notes_file = Path(tempfile.mktemp(suffix=".md", prefix="release-notes-"))
    notes_file.write_text(notes, encoding="utf-8")

    cmd = [
        "gh", "release", "create", tag_name,
        "--title", f"IWFM {version}",
        "--notes-file", str(notes_file),
    ]
    cmd.extend(assets)

    try:
        run(cmd)
        log(f"Created GitHub Release: {tag_name}")
    finally:
        notes_file.unlink(missing_ok=True)


def cmd_create_issue() -> None:
    """Create a GitHub Issue for post-import review."""
    version_json = os.environ.get("VERSION_JSON", "")
    if not version_json:
        raise RuntimeError("VERSION_JSON environment variable is required")

    info = json.loads(version_json)
    version = info["version"]
    asset_dir = Path(os.environ.get("ASSET_DIR", f"/tmp/iwfm-assets-{version}"))

    log(f"=== Creating review Issue for IWFM {version} ===")

    body_lines = [
        f"IWFM **{version}** has been automatically imported from the "
        f"[CNRA data portal](https://data.cnra.ca.gov/dataset/iwfm-integrated-water-flow-model).",
        "",
        "## Review checklist",
        "",
        "- [ ] Review upstream changes to locally-modified files (listed below)",
        "- [ ] Update `SourceCode/CMakeLists.txt` source file lists if new `.f90` files were added or removed",
        "- [ ] Test build (`./build-iwfm.ps1`)",
        f"- [ ] Create `{version}.rev` branch for development",
        "- [ ] Re-apply any local patches to upstream files if needed",
        "",
    ]

    changes_file = asset_dir / "changed_files.txt" if asset_dir.exists() else None
    if changes_file and changes_file.exists():
        changed = changes_file.read_text().strip().splitlines()
        if changed:
            body_lines.append("## Upstream files that changed")
            body_lines.append("")
            body_lines.append("These files were overwritten with the upstream version. "
                              "Check if any had local patches that need to be re-applied:")
            body_lines.append("")
            for f in changed:
                body_lines.append(f"- `{f}`")
            body_lines.append("")
    else:
        body_lines.append("*No locally-modified files were detected.*")
        body_lines.append("")

    body = "\n".join(body_lines)

    try:
        run([
            "gh", "issue", "create",
            "--title", f"IWFM {version} imported — review needed",
            "--body", body,
            "--label", "upstream-import",
        ])
        log("Review issue created")
    except subprocess.CalledProcessError:
        run([
            "gh", "issue", "create",
            "--title", f"IWFM {version} imported — review needed",
            "--body", body,
        ])
        log("Review issue created (without label)")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <check|import|release|create-issue>")
        sys.exit(1)

    command = sys.argv[1]

    commands = {
        "check": cmd_check,
        "import": cmd_import,
        "release": cmd_release,
        "create-issue": cmd_create_issue,
    }

    if command not in commands:
        print(f"Unknown command: {command}")
        print(f"Available: {', '.join(commands)}")
        sys.exit(1)

    try:
        commands[command]()
    except Exception as e:
        log(f"ERROR: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
