# IWFM Docker Containers

This directory contains Docker configurations for building and running IWFM.

## Intel oneAPI Version

Both Dockerfiles use the same Intel oneAPI HPC Toolkit version for consistency:

```
intel/oneapi-hpckit:2025.3.0-0-devel-ubuntu22.04
```

This version includes:
- Intel Fortran Compiler (ifx) 2025.3
- Intel C/C++ Compiler (icx) 2025.3
- Intel MPI Library
- Intel MKL

**To update the version:**
1. Check available tags at: https://hub.docker.com/r/intel/oneapi-hpckit/tags
2. Update the `FROM` line in both `Dockerfile` and `Dockerfile.runtime`
3. Rebuild the containers

**Note:** The Docker container version should match your local development environment for binary compatibility. Both Dockerfiles use the official Intel base image rather than manual apt installation for simplicity and consistency.

## Container Types

| Container | Purpose | Size | Use Case |
|-----------|---------|------|----------|
| `iwfm-build` | Full build environment | ~5 GB | Development, compiling IWFM |
| `iwfm-runtime` | Minimal runtime | ~310 MB | Running model simulations |

## Quick Start

### Build the Runtime Container

```bash
# From the src/ directory
cd docker
docker compose build iwfm-runtime

# Or using docker directly
docker build -t iwfm-runtime:2025.0 -f docker/Dockerfile.runtime .
```

### Run a Simulation

```bash
# Using docker compose with environment variables
MODEL_DIR=/path/to/your/model docker compose run --rm iwfm-simulation

# Or using docker directly
docker run --rm -v /path/to/your/model:/data iwfm-runtime:2025.0 \
    iwfm-simulation Simulation_MAIN.IN
```

## Available Commands

The runtime container includes these executables:

| Command | Alias | Description |
|---------|-------|-------------|
| `iwfm-preprocessor` | `PreProcessor` | Pre-process model input files |
| `iwfm-simulation` | `Simulation` | Run main simulation |
| `iwfm-budget` | `Budget` | Run budget post-processor |
| `iwfm-zbudget` | `ZBudget` | Run zone budget post-processor |

## Usage Examples

### 1. Show Version Information

```bash
docker run --rm iwfm-runtime:2025.0
```

### 2. Run PreProcessor

```bash
docker run --rm \
    -v /path/to/model/PreProcessor:/data \
    iwfm-runtime:2025.0 \
    iwfm-preprocessor PreProcessor_MAIN.IN
```

### 3. Run Simulation

```bash
docker run --rm \
    -v /path/to/model/Simulation:/data \
    iwfm-runtime:2025.0 \
    iwfm-simulation Simulation_MAIN.IN
```

### 4. Run Budget Post-Processor

```bash
docker run --rm \
    -v /path/to/model/Budget:/data \
    iwfm-runtime:2025.0 \
    iwfm-budget Budget.in
```

### 5. Run ZBudget Post-Processor

```bash
docker run --rm \
    -v /path/to/model/ZBudget:/data \
    iwfm-runtime:2025.0 \
    iwfm-zbudget ZBudget.in
```

### 6. Run Complete Pipeline

```bash
# Using docker compose
MODEL_DIR=/path/to/complete/model docker compose run --rm iwfm-pipeline

# Or manually
docker run --rm \
    -v /path/to/model:/model \
    iwfm-runtime:2025.0 \
    /bin/sh -c "
        cd /model/PreProcessor && iwfm-preprocessor PreProcessor_MAIN.IN &&
        cd /model/Simulation && iwfm-simulation Simulation_MAIN.IN &&
        cd /model/Budget && iwfm-budget Budget.in &&
        cd /model/ZBudget && iwfm-zbudget ZBudget.in
    "
```

### 7. Interactive Shell in Runtime Container

```bash
docker run --rm -it \
    -v /path/to/model:/data \
    iwfm-runtime:2025.0 \
    /bin/sh
```

## Docker Compose Services

### Using docker-compose.yml

```bash
cd src/docker

# Build all containers
docker compose build

# Run specific services
docker compose run --rm iwfm-preprocessor
docker compose run --rm iwfm-simulation
docker compose run --rm iwfm-budget
docker compose run --rm iwfm-zbudget

# Run complete pipeline
docker compose run --rm iwfm-pipeline

# Interactive development shell
docker compose run --rm iwfm-shell
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MODEL_DIR` | `.` | Path to model directory to mount |
| `INPUT_FILE` | (varies) | Input file name for each executable |

Example:
```bash
MODEL_DIR=/home/user/my_model INPUT_FILE=my_simulation.in \
    docker compose run --rm iwfm-simulation
```

## Building the Runtime Container

### Standard Build

```bash
docker build -t iwfm-runtime:2025.0 -f docker/Dockerfile.runtime .
```

### Build with Custom Tag

```bash
docker build -t myorg/iwfm:latest -f docker/Dockerfile.runtime .
```

### Build Arguments

The Dockerfile uses a multi-stage build:
1. **Stage 1 (builder)**: Uses Intel oneAPI to compile IWFM
2. **Stage 2 (runtime)**: Copies only executables to minimal Ubuntu image

### Pushing to Registry

```bash
# Tag for Docker Hub
docker tag iwfm-runtime:2025.0 username/iwfm-runtime:2025.0

# Push
docker push username/iwfm-runtime:2025.0
```

## Development Build Environment

For building IWFM from source with Intel compilers:

```bash
# Build the development image
docker compose build iwfm-build

# Run build
docker compose run --rm iwfm-build

# Or interactive shell
docker compose run --rm iwfm-shell
```

The development environment includes:
- Intel ifx (Fortran) and icx (C) compilers
- CMake and Ninja
- Full oneAPI toolkit

## Model Directory Structure

IWFM expects a specific directory structure. When mounting your model:

```
model/
├── PreProcessor/
│   ├── PreProcessor_MAIN.IN
│   └── ... (input files)
├── Simulation/
│   ├── Simulation_MAIN.IN
│   └── ... (input files)
├── Budget/
│   ├── Budget.in
│   └── ... (input files)
└── ZBudget/
    ├── ZBudget.in
    └── ... (input files)
```

## Troubleshooting

### Permission Denied

If you get permission errors, the container runs as user `iwfm` (non-root). Ensure your model directory is readable:

```bash
chmod -R a+rX /path/to/model
```

Or run as root (not recommended for production):
```bash
docker run --rm -u root -v /path/to/model:/data iwfm-runtime:2025.0 ...
```

### Missing Input Files

Ensure the input file path is correct relative to the mounted directory:

```bash
# Check what's mounted
docker run --rm -v /path/to/model:/data iwfm-runtime:2025.0 ls -la /data
```

### Memory Issues

For large models, increase Docker memory limits:

```bash
docker run --rm -m 8g -v /path/to/model:/data iwfm-runtime:2025.0 \
    iwfm-simulation Simulation_MAIN.IN
```

### Viewing Output

Output files are written to the mounted directory. They persist after the container exits:

```bash
# Run simulation
docker run --rm -v /path/to/model:/data iwfm-runtime:2025.0 \
    iwfm-simulation Simulation_MAIN.IN

# Check output files
ls -la /path/to/model/
```

## Container Security

The runtime container:
- Runs as non-root user `iwfm`
- Contains only necessary executables (no compilers, no shell utilities except `/bin/sh`)
- Uses minimal base image (Ubuntu 22.04)
- Has health check for monitoring

## Image Size Optimization

The multi-stage build produces a minimal image:

| Stage | Contents | Size |
|-------|----------|------|
| Builder | Intel oneAPI + build tools + source | ~5 GB |
| Runtime | Ubuntu + IWFM executables + Intel runtime libs | ~310 MB |

Note: The runtime image includes Intel runtime libraries (libirng, libimf, libsvml, libintlc) required by Intel-compiled executables.

To verify image size:
```bash
docker images iwfm-runtime:2025.0
```

## Container Labels

The containers use OCI (Open Container Initiative) standard labels for metadata:

| Label | Value | Description |
|-------|-------|-------------|
| `maintainer` | IWFMtechsupport@water.ca.gov | Technical support contact |
| `org.opencontainers.image.title` | IWFM Runtime / IWFM Build Environment | Container name |
| `org.opencontainers.image.description` | (varies) | Container purpose |
| `org.opencontainers.image.version` | 2025.0.1747 | IWFM version |
| `org.opencontainers.image.vendor` | California Department of Water Resources | Organization |
| `org.opencontainers.image.licenses` | GPL-2.0 | Software license |

These labels follow the [OCI Image Format Specification](https://github.com/opencontainers/image-spec/blob/main/annotations.md) and can be inspected with:

```bash
docker inspect iwfm-runtime:2025.0 --format='{{json .Config.Labels}}' | jq
```

## Support

For technical support: IWFMtechsupport@water.ca.gov
