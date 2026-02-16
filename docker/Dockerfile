#***********************************************************************
#  IWFM Linux Build Environment with Intel oneAPI (ifx/icx)
#
#  Uses Intel ifx for Fortran and icx for C.
#  Builds HDF5 and dependencies from source via CMake FetchContent/ExternalProject.
#
#  Build: docker build -t iwfm-build -f docker/Dockerfile .
#  Run:   docker run --rm -v ${PWD}:/src iwfm-build
#
#  Note: Use the same Intel oneAPI version as Dockerfile.runtime for consistency.
#  Check available tags at: https://hub.docker.com/r/intel/oneapi-hpckit/tags
#***********************************************************************

FROM intel/oneapi-hpckit:2025.3.0-0-devel-ubuntu22.04

# Labels
LABEL maintainer="IWFMtechsupport@water.ca.gov" \
      org.opencontainers.image.title="IWFM Build Environment" \
      org.opencontainers.image.description="Intel oneAPI build environment for IWFM" \
      org.opencontainers.image.version="2025.0.1747" \
      org.opencontainers.image.vendor="California Department of Water Resources"

# Prevent interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install build tools (Intel compilers are already in the base image)
RUN apt-get update && apt-get install -y --no-install-recommends \
    ninja-build \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create working directory
WORKDIR /src

# Default command: configure, build, and test with Intel ifx/icx
# Note: Intel base image already has oneAPI environment configured, no need for setvars.sh
CMD ["/bin/bash", "-c", "\
    echo '=== Intel Compiler Versions ===' && \
    ifx --version && \
    icx --version && \
    echo '=== Configuring IWFM ===' && \
    rm -rf /src/build-linux && \
    mkdir -p /src/build-linux && \
    cd /src/build-linux && \
    cmake .. -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_Fortran_COMPILER=ifx \
        -DCMAKE_C_COMPILER=icx && \
    echo '=== Building IWFM ===' && \
    cmake --build . --parallel $(nproc) && \
    echo '=== Build Complete ===' && \
    ls -la /src/Bin/ \
"]
