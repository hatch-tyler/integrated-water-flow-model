#!/bin/bash
# Build all IWFM Linux targets in both Release and Debug
# Runs inside the Intel oneAPI Docker container
#
# FC selects the Fortran compiler (default: the image's ifx). Release builds
# must use the compiler DWR used for the official release, or results differ
# at round-off level: for 2026.0.1848, ifx 2026.1.1, e.g.
#   FC=/opt/ifx2611/bin/ifx bash docker/build-linux-all.sh
set -e
FC=${FC:-ifx}

SRCDIR=/src
BINDIR=/src/Bin
STAGINGDIR=/src/release-staging

COMMON_ARGS=(
    -G Ninja
    -DCMAKE_Fortran_COMPILER=$FC
    -DCMAKE_C_COMPILER=icx
    -DIWFM_MONOLITHIC_BUILD=ON
    -DIWFM_BUILD_IWFM2OBS=ON
    -DIWFM_BUILD_CALCTYPHYD=ON
)

build_config() {
    local BUILD_TYPE=$1
    echo ""
    echo "========================================"
    echo "  Building $BUILD_TYPE"
    echo "========================================"

    local BUILD_DIR=/src/build-linux-$(echo "$BUILD_TYPE" | tr '[:upper:]' '[:lower:]')
    rm -rf "$BUILD_DIR"
    mkdir -p "$BUILD_DIR"
    cd "$BUILD_DIR"

    echo "Configuring..."
    cmake "$SRCDIR" "${COMMON_ARGS[@]}" -DCMAKE_BUILD_TYPE="$BUILD_TYPE"

    # Build HDF5 first with limited parallelism
    echo "Building HDF5 from source..."
    cmake --build . --target HDF5_External --parallel 4

    # Build everything else at full speed
    echo "Building all IWFM targets..."
    cmake --build . --parallel "$(nproc)"

    echo "$BUILD_TYPE build complete."
}

echo "=== Intel Compiler Versions ==="
$FC --version
icx --version

# Build Release
build_config Release

# Package Release
echo "Packaging Release..."
mkdir -p "$STAGINGDIR"
RELEASE_TAR="$STAGINGDIR/IWFM-2026.0.1848-Linux-x64-Release.tar.gz"
cd "$BINDIR"
tar czf "$RELEASE_TAR" \
    Simulation PreProcessor Simulation_PLL \
    Budget ZBudget IWFM2OBS CalcTypeHyd \
    libiwfm_c.so 2>/dev/null || true
echo "  $(du -h "$RELEASE_TAR" | cut -f1) $RELEASE_TAR"

# Build Debug
build_config Debug

# Package Debug
echo "Packaging Debug..."
DEBUG_TAR="$STAGINGDIR/IWFM-2026.0.1848-Linux-x64-Debug.tar.gz"
cd "$BINDIR"
tar czf "$DEBUG_TAR" \
    Simulation_d PreProcessor_d Simulation_PLL_d \
    Budget_d ZBudget_d IWFM2OBS_d CalcTypeHyd_d \
    libiwfm_c_d.so 2>/dev/null || true
echo "  $(du -h "$DEBUG_TAR" | cut -f1) $DEBUG_TAR"

echo ""
echo "========================================"
echo "  All Linux builds complete!"
echo "========================================"
ls -lh "$STAGINGDIR"/*.tar.gz
