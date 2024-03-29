#
# CMakeLists.txt - CMake configuration file for EPANET Runner
#
# Created: Nov 13, 2023
# Modified: Nov 13, 2023
#
# Author: Caleb Buahin
#         US EPA ORD/CESER
#

if(APPLE)
    set(RPATH_ROOT "@loader_path")
else()
    set(RPATH_ROOT "$ORIGIN")
endif()

set(PACKAGE_RPATH "${RPATH_ROOT}/../lib;${RPATH_ROOT}/../extern")


# Creates the EPANET command line executable
add_executable(runepanet
    main.c
)

target_include_directories(runepanet
    PUBLIC
        ${PROJECT_SOURCE_DIR}/include
)

target_link_libraries(runepanet
    LINK_PUBLIC
        epanet2
)

# Set up rpath for runswmm inside install package
set_target_properties(runepanet
    PROPERTIES
        MACOSX_RPATH TRUE
        SKIP_BUILD_RPATH FALSE
        BUILD_WITH_INSTALL_RPATH FALSE
        INSTALL_RPATH "${PACKAGE_RPATH}"
        INSTALL_RPATH_USE_LINK_PATH TRUE
)

install(TARGETS runepanet
    DESTINATION "${TOOL_DIST}"
)


