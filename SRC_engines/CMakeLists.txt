cmake_minimum_required(VERSION 3.9)

if("${CMAKE_BINARY_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
    message(FATAL_ERROR "In-source builds are disabled.")
endif()

project(
    "EPANET"
    VERSION 2.2.0
    DESCRIPTION "The EPANET Program for Water Distribution System Analysis"
)

# Append local dir to module search path
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# Sets the position independent code property for all targets
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Sets default install prefix when cmakecache is initialized for first time
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install CACHE PATH "..." FORCE)
endif()

# Sets the output directory for executables and libraries.
set(TOOL_DIST "bin")
set(INCLUDE_DIST "include")
set(LIBRARY_DIST "lib")
set(CONFIG_DIST  "cmake")

# option(BUILD_TESTS "Build component tests (requires Boost)" OFF)
option(BUILD_DEF   "Builds library with def file interface" OFF)

# Added option to statically link libraries to address GitHub
option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

add_subdirectory(src/solver)
add_subdirectory(src/run)

# Create install rules for vcruntime.dll, msvcp.dll, vcomp.dll etc.
include(InstallRequiredSystemLibraries)

install(
    FILES
        "CONTRIBUTORS"
    DESTINATION
        "."
)

# Configure CPack driven installer package
set(CPACK_GENERATOR "ZIP;TGZ")
set(CPACK_PACKAGE_VENDOR "US_EPA")
set(CPACK_ARCHIVE_FILE_NAME "epanet-${PROJECT_VERSION}-${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

include(CPack)