# Integrated Water Flow Model (IWFM)
# C++ Sparse Solver Library
#
# This builds a static library (iwfm_solver) that provides high-performance
# replacements for the Fortran SPARSKIT routines. All routines use 1-based
# indexing (Fortran convention) and a C-linkage API for seamless interop.

cmake_minimum_required(VERSION 3.20)

add_library(iwfm_solver STATIC
    blas.cpp
    spmv.cpp
    lusol.cpp
    ilut.cpp
    gmres.cpp
    pgmres_solve.cpp
)

target_include_directories(iwfm_solver PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Require C++17
target_compile_features(iwfm_solver PUBLIC cxx_std_17)

# Compiler-specific flags for numerical reproducibility and performance
if(CMAKE_CXX_COMPILER_ID MATCHES "Intel|IntelLLVM")
    # Intel oneAPI (icx/icpx)
    if(WIN32)
        target_compile_options(iwfm_solver PRIVATE
            /O3
            /fp:consistent       # Match Fortran /fp:consistent
            /Qopt-prefetch
        )
    else()
        target_compile_options(iwfm_solver PRIVATE
            -O3
            -fp-model=consistent
            -ffp-contract=off
            -qopt-prefetch
        )
    endif()

elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    target_compile_options(iwfm_solver PRIVATE
        -O3
        -ffp-contract=off
        -march=native
    )

elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(iwfm_solver PRIVATE
        -O3
        -ffp-contract=off
        -march=native
    )

elseif(MSVC)
    target_compile_options(iwfm_solver PRIVATE
        /O2
        /fp:precise
    )
endif()

# OpenMP support for AMUX (SpMV)
find_package(OpenMP COMPONENTS CXX)
if(OpenMP_CXX_FOUND)
    target_link_libraries(iwfm_solver PUBLIC OpenMP::OpenMP_CXX)
    message(STATUS "IWFM C++ solver: OpenMP enabled")
else()
    message(STATUS "IWFM C++ solver: OpenMP not found, SpMV will be sequential")
endif()

# Static CRT on Windows (match Fortran build: /MT for Release, /MTd for Debug)
if(WIN32)
    if(CMAKE_MSVC_RUNTIME_LIBRARY)
        set(_CRT ${CMAKE_MSVC_RUNTIME_LIBRARY})
    else()
        set(_CRT "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    endif()
    set_property(TARGET iwfm_solver PROPERTY MSVC_RUNTIME_LIBRARY ${_CRT})
endif()

# Unit test and benchmark
if(IWFM_BUILD_TESTS)
    # Output directory: use project Bin/ if available, else local Bin/
    if(EXISTS "${CMAKE_SOURCE_DIR}/SourceCode")
        set(_SOLVER_BIN_DIR "${CMAKE_SOURCE_DIR}/Bin")
    else()
        set(_SOLVER_BIN_DIR "${CMAKE_CURRENT_SOURCE_DIR}/Bin")
    endif()

    add_executable(test_iwfm_solver test_solver.cpp)
    target_link_libraries(test_iwfm_solver PRIVATE iwfm_solver)
    set_target_properties(test_iwfm_solver PROPERTIES
        OUTPUT_NAME "test_iwfm_solver"
        RUNTIME_OUTPUT_DIRECTORY "${_SOLVER_BIN_DIR}"
    )

    add_executable(bench_iwfm_solver bench_solver.cpp)
    target_link_libraries(bench_iwfm_solver PRIVATE iwfm_solver)
    set_target_properties(bench_iwfm_solver PROPERTIES
        OUTPUT_NAME "bench_iwfm_solver"
        RUNTIME_OUTPUT_DIRECTORY "${_SOLVER_BIN_DIR}"
    )

    if(WIN32)
        if(CMAKE_MSVC_RUNTIME_LIBRARY)
            set(_TEST_CRT ${CMAKE_MSVC_RUNTIME_LIBRARY})
        else()
            set(_TEST_CRT "MultiThreaded$<$<CONFIG:Debug>:Debug>")
        endif()
        set_property(TARGET test_iwfm_solver PROPERTY MSVC_RUNTIME_LIBRARY ${_TEST_CRT})
        set_property(TARGET bench_iwfm_solver PROPERTY MSVC_RUNTIME_LIBRARY ${_TEST_CRT})
    endif()

    add_test(NAME IWFM_CppSolver_UnitTest COMMAND test_iwfm_solver)
    set_tests_properties(IWFM_CppSolver_UnitTest PROPERTIES
        TIMEOUT 30
        LABELS "unit;solver"
    )
endif()
