#!/bin/bash
# Build all IWFM Linux targets in both Release and Debug
# Runs inside the Intel oneAPI Docker container
set -e

SRCDIR=/src
BINDIR=/src/Bin
STAGINGDIR=/src/release-staging

COMMON_ARGS=(
    -G Ninja
    -DCMAKE_Fortran_COMPILER=ifx
    -DCMAKE_C_COMPILER=icx
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
ifx --version
icx --version

# Build Release
build_config Release

# Package Release
echo "Packaging Release..."
mkdir -p "$STAGINGDIR"
RELEASE_TAR="$STAGINGDIR/IWFM-2025.0.1747-Linux-x64-Release.tar.gz"
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
DEBUG_TAR="$STAGINGDIR/IWFM-2025.0.1747-Linux-x64-Debug.tar.gz"
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
