# Builds the HEC-DSS 7 C library (heclib) as a shared library for conda-forge.
#
# Upstream's top-level CMakeLists.txt is not used because it downloads zlib and
# the DSS test-data repository at configure time and links the C runtime
# statically, none of which conda-forge allows. This file compiles the same
# sources (heclib/heclib_c/src, plus the hecdss C API) against the
# environment's zlib.
#
# Usage: cmake -S <this dir> -B build -DHECDSS_SOURCE_DIR=<hec-dss source tree>
cmake_minimum_required(VERSION 3.20)
project(heclib LANGUAGES C)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(HECDSS_SOURCE_DIR "" CACHE PATH "Root of the hec-dss source tree")
if(NOT EXISTS "${HECDSS_SOURCE_DIR}/heclib/heclib_c/src")
    message(FATAL_ERROR "HECDSS_SOURCE_DIR must point to a hec-dss source tree")
endif()
set(HECLIB_SO_VERSION "7" CACHE STRING "SONAME version of libheclib")

find_package(ZLIB REQUIRED)

set(SRC "${HECDSS_SOURCE_DIR}/heclib/heclib_c/src")
# Same 357 files as upstream heclib/heclib_c/CMakeLists.txt lists explicitly.
file(GLOB_RECURSE HECLIB_SOURCES CONFIGURE_DEPENDS "${SRC}/*.c")
list(APPEND HECLIB_SOURCES "${HECDSS_SOURCE_DIR}/heclib/hecdss/hecdss.c")

add_library(heclib SHARED ${HECLIB_SOURCES})
add_library(heclib::heclib ALIAS heclib)
set_target_properties(heclib PROPERTIES
    SOVERSION ${HECLIB_SO_VERSION}
    C_VISIBILITY_PRESET default)

target_include_directories(heclib PUBLIC
    "$<BUILD_INTERFACE:${SRC}/headers>"
    "$<BUILD_INTERFACE:${HECDSS_SOURCE_DIR}/heclib/hecdss>"
    "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/heclib>")
target_link_libraries(heclib PRIVATE ZLIB::ZLIB)

if(WIN32)
    # heclib has no export annotations (only the hecdss API does); export
    # every function so Fortran and C consumers can bind to zopen, ztsStore...
    set_target_properties(heclib PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
    target_compile_definitions(heclib PRIVATE
        HECDSS_EXPORTS _CRT_SECURE_NO_WARNINGS _CRT_NONSTDC_NO_WARNINGS)
    target_link_libraries(heclib PRIVATE ws2_32 shlwapi)
else()
    # Upstream compiles with gnu99; newer GCC defaults reject its K&R code.
    target_compile_options(heclib PRIVATE -std=gnu99 -w)
    target_compile_definitions(heclib PRIVATE _GNU_SOURCE)
    find_package(Threads REQUIRED)
    target_link_libraries(heclib PRIVATE m Threads::Threads ${CMAKE_DL_LIBS})
endif()

install(TARGETS heclib EXPORT heclibTargets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
file(GLOB HECLIB_HEADERS "${SRC}/headers/*.h")
install(FILES ${HECLIB_HEADERS} "${HECDSS_SOURCE_DIR}/heclib/hecdss/hecdss.h"
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/heclib)

# find_package(heclib CONFIG) -> heclib::heclib
install(EXPORT heclibTargets NAMESPACE heclib::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/heclib)
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/heclibConfig.cmake"
"include(CMakeFindDependencyMacro)
include(\"\${CMAKE_CURRENT_LIST_DIR}/heclibTargets.cmake\")
")
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/heclibConfig.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/heclib)
