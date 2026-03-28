#***********************************************************************
#  IWFM CMake - Compiler Flags Module
#
#  Platform and compiler-specific flags for IWFM
#  Supports: Intel ifx/ifort, GNU gfortran
#***********************************************************************

include(CheckFortranCompilerFlag)

# =============================================================================
# Detect Compiler Type
# =============================================================================
set(IWFM_USING_INTEL FALSE)
set(IWFM_USING_IFX FALSE)
set(IWFM_USING_IFORT FALSE)
set(IWFM_USING_GFORTRAN FALSE)

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(IWFM_USING_INTEL TRUE)
    if(CMAKE_Fortran_COMPILER MATCHES "ifx")
        set(IWFM_USING_IFX TRUE)
        message(STATUS "Detected Intel ifx compiler")
    else()
        set(IWFM_USING_IFORT TRUE)
        message(STATUS "Detected Intel ifort compiler")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(IWFM_USING_GFORTRAN TRUE)
    message(STATUS "Detected GNU gfortran compiler")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
    message(STATUS "Detected Flang compiler (experimental support)")
else()
    message(WARNING "Untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# =============================================================================
# Create Interface Library for Compiler Flags
# =============================================================================
add_library(iwfm_compiler_flags INTERFACE)
add_library(IWFM::CompilerFlags ALIAS iwfm_compiler_flags)


# =============================================================================
# Profile-Guided Optimization (PGO) Options
# =============================================================================
option(IWFM_PGO_GENERATE "Build with PGO instrumentation (Phase 1: generate profiles)" OFF)
option(IWFM_PGO_USE "Build with PGO optimization (Phase 2: use profiles)" OFF)
set(IWFM_PGO_DIR "${CMAKE_SOURCE_DIR}/pgo_data" CACHE PATH "Directory for PGO profile data (outside build dir to survive cleans)")

# =============================================================================
# Intel Fortran Flags (ifx / ifort)
# =============================================================================
if(IWFM_USING_INTEL)
    # Override CMake's default Release flags for Intel compilers.
    # CMake's platform module sets CMAKE_Fortran_FLAGS to "/nologo /fpp" (Windows)
    # or "-fpp" (Linux). We keep these defaults — /fpp is needed for optimal code
    # generation. The Ninja two-step preprocessing issue is solved by setting
    # CMAKE_Fortran_PREPROCESS OFF in root CMakeLists.txt (not by removing /fpp).
    #
    # Blank CMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_OFF to prevent CMake from
    # injecting -nofpp (which would override /fpp in the compile flags).
    set(CMAKE_Fortran_COMPILE_OPTIONS_PREPROCESS_OFF "" CACHE STRING "" FORCE)
    if(WIN32)
        set(CMAKE_Fortran_FLAGS_RELEASE "/O2" CACHE STRING
            "Fortran Release flags" FORCE)
        set(CMAKE_C_FLAGS_RELEASE "/O2 /DNDEBUG" CACHE STRING
            "C Release flags" FORCE)
    else()
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2" CACHE STRING
            "Fortran Release flags" FORCE)
        set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG" CACHE STRING
            "C Release flags" FORCE)
    endif()

    if(WIN32)
        # Windows Intel flags
        # Match the .vfproj: /fp:consistent for reproducible floating-point results.
        # Previous /fp:fast=1 testing showed it was faster, but the comparison was
        # confounded by unconditional /Qopenmp on non-parallel targets. With that
        # fixed, /fp:consistent should match .vfproj performance.
        target_compile_options(iwfm_compiler_flags INTERFACE
            /fp:consistent
            /assume:norealloc_lhs
            /assume:noieee_compares
            /Qdiag-disable:10448
            /standard-semantics
            /fpp
        )

        # Static linking - use static runtime libraries
        # /libs:static - link Intel Fortran runtime statically
        # /MT - link C runtime statically (Release)
        # /MTd - link C runtime statically (Debug)
        target_compile_options(iwfm_compiler_flags INTERFACE
            /libs:static
            /threads
            $<$<CONFIG:Release>:/MT>
            $<$<CONFIG:Debug>:/MTd>
        )

        # Disable specific diagnostics
        target_compile_options(iwfm_compiler_flags INTERFACE
            /Qdiag-disable:5462
        )

        # Debug-specific flags
        target_compile_options(iwfm_compiler_flags INTERFACE
            $<$<CONFIG:Debug>:/debug:full>
            $<$<CONFIG:Debug>:/Od>
            $<$<CONFIG:Debug>:/traceback>
            $<$<CONFIG:Debug>:/check:bounds>
            $<$<CONFIG:Debug>:/warn:unused>
            $<$<CONFIG:Debug>:/warn:interfaces>
            $<$<CONFIG:Debug>:/warn:uncalled>
        )

        # Note: /heap-arrays:900 was removed from global flags — it causes a 3x
        # performance penalty by forcing all arrays >900 bytes onto the heap.
        # The VS solution only applies it to the DLL project (see iwfm_add_dll_flags).
        # The 256MB stack (/STACK:256000000) is sufficient for all EXE targets.

        # Profile-Guided Optimization (PGO)
        # ifx (LLVM-based) uses LLVM-style PGO flags, not classic Intel /Qprof-gen
        if(IWFM_PGO_GENERATE)
            target_compile_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-generate>
            )
            target_link_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-generate>
            )
            message(STATUS "PGO instrumentation enabled (LLVM generate phase)")
        elseif(IWFM_PGO_USE)
            target_compile_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-use=${IWFM_PGO_DIR}/default.profdata>
            )
            target_link_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-use=${IWFM_PGO_DIR}/default.profdata>
            )
            message(STATUS "PGO optimization enabled (LLVM use phase)")
        endif()

        # Linker: 256MB stack
        # Must use LINKER: prefix so CMake passes the flag through to link.exe
        # rather than to the ifx compiler driver (which ignores it with warning #10006)
        target_link_options(iwfm_compiler_flags INTERFACE
            "LINKER:/STACK:256000000"
        )

        # Note: IWFM executables should link IWFM libraries with WHOLE_ARCHIVE
        # to force inclusion of all object files (matching VS .vfproj behavior).
        # This is done per-target using $<LINK_LIBRARY:WHOLE_ARCHIVE,...> in
        # SourceCode/CMakeLists.txt, not globally here (global /WHOLEARCHIVE
        # would also apply to system libraries causing duplicate symbol errors).

    else()
        # Linux/macOS Intel flags
        # Note: Multiple -assume options must be comma-separated or use separate -assume flags
        target_compile_options(iwfm_compiler_flags INTERFACE
            -fp-model=fast=1
            "-assume" "norealloc_lhs,noieee_compares"
            -diag-disable=10448
            -standard-semantics
            -fpp
            -fPIC  # Required for shared library (IWFM_C_DLL) support
        )

        # Static linking - use static runtime libraries
        target_compile_options(iwfm_compiler_flags INTERFACE
            -static-intel
            -qopenmp-link=static
        )
        target_link_options(iwfm_compiler_flags INTERFACE
            -static-intel
        )


        # Disable specific diagnostics
        target_compile_options(iwfm_compiler_flags INTERFACE
            -diag-disable=5462
        )

        # Debug-specific flags
        # Note: Multi-word options like "-check bounds" must use SHELL: prefix
        # to prevent CMake from splitting them into separate arguments
        target_compile_options(iwfm_compiler_flags INTERFACE
            $<$<CONFIG:Debug>:-g>
            $<$<CONFIG:Debug>:-O0>
            $<$<CONFIG:Debug>:-traceback>
            "$<$<CONFIG:Debug>:SHELL:-check bounds>"
            "$<$<CONFIG:Debug>:SHELL:-warn unused>"
            "$<$<CONFIG:Debug>:SHELL:-warn interfaces>"
            "$<$<CONFIG:Debug>:SHELL:-warn uncalled>"
        )

        # Release-specific flags (matches DWR Visual Studio project)
        target_compile_options(iwfm_compiler_flags INTERFACE
            $<$<CONFIG:Release>:-O2>
            $<$<CONFIG:Release>:-ipo>
        )

        # Note: -heap-arrays 900 was removed from global flags — it causes a 3x
        # performance penalty. Only applied to DLL target (see iwfm_add_dll_flags).

        # Profile-Guided Optimization (PGO)
        if(IWFM_PGO_GENERATE)
            target_compile_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-generate>
            )
            target_link_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-generate>
            )
            message(STATUS "PGO instrumentation enabled (LLVM generate phase)")
        elseif(IWFM_PGO_USE)
            target_compile_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-use=${IWFM_PGO_DIR}/default.profdata>
            )
            target_link_options(iwfm_compiler_flags INTERFACE
                $<$<CONFIG:Release>:-fprofile-instr-use=${IWFM_PGO_DIR}/default.profdata>
            )
            message(STATUS "PGO optimization enabled (LLVM use phase)")
        endif()

        # IPO link: -ipo must also be passed at link time
        target_link_options(iwfm_compiler_flags INTERFACE
            $<$<CONFIG:Release>:-ipo>
        )

        # Linker: 256MB stack (same as Windows)
        # Uses LINKER: prefix so CMake passes the flag through to ld
        target_link_options(iwfm_compiler_flags INTERFACE
            "LINKER:-z,stacksize=256000000"
        )
    endif()

# =============================================================================
# GNU Fortran Flags (gfortran)
# =============================================================================
elseif(IWFM_USING_GFORTRAN)
    # Note: Use -std=legacy to allow obsolete features (PAUSE, ASSIGN, assigned GOTO)
    # that exist in some IWFM source files (e.g., pgmres.f90, Ludcmp.f90)
    target_compile_options(iwfm_compiler_flags INTERFACE
        -std=legacy
        -ffree-line-length-none
        -cpp
        -fno-realloc-lhs
        -fall-intrinsics
    )

    # Static linking for gfortran
    target_link_options(iwfm_compiler_flags INTERFACE
        -static-libgfortran
        -static-libgcc
    )

    # Linker: 256MB stack for large models (C2VSimFG has 30,000+ nodes)
    target_link_options(iwfm_compiler_flags INTERFACE
        "LINKER:-z,stacksize=256000000"
    )

    # Debug-specific flags
    target_compile_options(iwfm_compiler_flags INTERFACE
        $<$<CONFIG:Debug>:-g>
        $<$<CONFIG:Debug>:-O0>
        $<$<CONFIG:Debug>:-fbacktrace>
        $<$<CONFIG:Debug>:-fcheck=bounds>
        $<$<CONFIG:Debug>:-Wall>
        $<$<CONFIG:Debug>:-Wextra>
        $<$<CONFIG:Debug>:-Wno-unused-dummy-argument>
    )

    # Release-specific flags
    target_compile_options(iwfm_compiler_flags INTERFACE
        $<$<CONFIG:Release>:-O2>
        $<$<CONFIG:Release>:-march=native>
    )
endif()

# =============================================================================
# Function: Add OpenMP Flags to a Target
# =============================================================================
function(iwfm_add_openmp_flags target)
    if(IWFM_USING_INTEL)
        if(WIN32)
            target_compile_options(${target} PRIVATE /Qopenmp)
            target_link_options(${target} PRIVATE /Qopenmp)
        else()
            target_compile_options(${target} PRIVATE -qopenmp)
            target_link_options(${target} PRIVATE -qopenmp)
        endif()
    elseif(IWFM_USING_GFORTRAN)
        find_package(OpenMP REQUIRED)
        target_link_libraries(${target} PRIVATE OpenMP::OpenMP_Fortran)
    endif()
    message(STATUS "OpenMP enabled for target: ${target}")
endfunction()

# =============================================================================
# Function: Add Coarray Flags to a Target (Intel only)
# =============================================================================
function(iwfm_add_coarray_flags target)
    if(IWFM_USING_INTEL)
        if(WIN32)
            target_compile_options(${target} PRIVATE /Qcoarray=shared)
            target_link_options(${target} PRIVATE /Qcoarray=shared)
        else()
            target_compile_options(${target} PRIVATE -coarray=shared)
            target_link_options(${target} PRIVATE -coarray=shared)
        endif()
        message(STATUS "Coarray (Intel shared) enabled for target: ${target}")
    elseif(IWFM_USING_GFORTRAN)
        # gfortran requires OpenCoarrays library
        find_package(OpenCoarrays QUIET)
        if(OpenCoarrays_FOUND)
            target_link_libraries(${target} PRIVATE OpenCoarrays::caf_mpi)
            message(STATUS "Coarray (OpenCoarrays) enabled for target: ${target}")
        else()
            message(WARNING "OpenCoarrays not found. Coarray support disabled for ${target}")
            message(WARNING "Install OpenCoarrays or use Intel compiler for coarray support")
        endif()
    else()
        message(WARNING "Coarray Fortran not supported for compiler: ${CMAKE_Fortran_COMPILER_ID}")
    endif()
endfunction()

# =============================================================================
# Function: Add DLL-specific Flags to a Target
# =============================================================================
function(iwfm_add_dll_flags target)
    # DLL targets need heap-arrays to avoid stack overflow when called from
    # external processes with limited stack size. EXE targets use the 256MB
    # stack set via /STACK linker flag instead (3x faster than heap-arrays).
    if(IWFM_USING_INTEL)
        if(WIN32)
            target_compile_options(${target} PRIVATE /heap-arrays:900)
        else()
            target_compile_options(${target} PRIVATE "SHELL:-heap-arrays 900")
        endif()
    endif()
    if(IWFM_USING_GFORTRAN)
        target_compile_options(${target} PRIVATE -fPIC)
    endif()
endfunction()

# =============================================================================
# Function: Set Debug/Release Output Names
# =============================================================================
function(iwfm_set_output_name target base_name)
    if(WIN32)
        set_target_properties(${target} PROPERTIES
            OUTPUT_NAME "${base_name}_x64$<$<CONFIG:Debug>:_D>"
        )
    else()
        set_target_properties(${target} PROPERTIES
            OUTPUT_NAME "${base_name}$<$<CONFIG:Debug>:_d>"
        )
    endif()
endfunction()

