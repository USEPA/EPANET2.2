#
# CMakeLists.txt - CMake configuration file for EPANET solver
#
# Created: Nov 13, 2023
# Modified: Nov 13, 2023
#
# Author: Caleb Buahin
#         US EPA ORD/CESER
#


include(../../extern/version.cmake)

set(EPANET_PUBLIC_HEADERS
    ${PROJECT_SOURCE_DIR}/include/epanet2.h
    ${PROJECT_SOURCE_DIR}/include/epanet2_2.h
    ${PROJECT_SOURCE_DIR}/include/epanet2_enums.h
)

file(GLOB
    EPANET_SOURCES
    RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.c *.h *.dat
)

if(BUILD_DEF)
    # Builds library with def file interface for backward compatibility
    set_source_files_properties(${PROJECT_SOURCE_DIR}/include/epanet2.def
        PROPERTIES_HEADER_FILE_ONLY TRUE
    )

    add_library(epanet2
            ${EPANET_SOURCES}
            ${PROJECT_SOURCE_DIR}/include/epanet2.def
    )

else()
    # Performs standard library build
    add_library(epanet2
            ${EPANET_SOURCES}
    )

endif()

# Sets MSVC compiler flags
target_compile_options(epanet2
    PUBLIC
        "$<$<C_COMPILER_ID:MSVC>:"
            "$<$<CONFIG:Release>:/GL>"
            "$<$<CONFIG:Release>:/fp:fast>"
            "$<$<CONFIG:Release>:/Zi>"
        ">"
        $<$<C_COMPILER_ID:AppleClang>:
            $<$<STREQUAL:"${CMAKE_GENERATOR}","Ninja">:-O3>
        >
)

target_link_options(epanet2
    PUBLIC
        "$<$<C_COMPILER_ID:MSVC>:"
            "$<$<CONFIG:Release>:/LTCG:incremental>"
        ">"
)

target_link_libraries(epanet2
    PUBLIC
        $<$<NOT:$<BOOL:$<C_COMPILER_ID:MSVC>>>:m>
)


target_include_directories(epanet2
    PUBLIC
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:${INCLUDE_DIST}>
)

include(GenerateExportHeader)
generate_export_header(epanet2
    BASE_NAME epanet2
    EXPORT_MACRO_NAME DLLEXPORT
    EXPORT_FILE_NAME epanet2_export.h
    STATIC_DEFINE SHARED_EXPORTS_BUILT_AS_STATIC
)

file(COPY ${CMAKE_CURRENT_BINARY_DIR}/epanet2_export.h
    DESTINATION ${PROJECT_SOURCE_DIR}/include
)


if(APPLE)
    set(LIB_ROOT "@loader_path")
else()
    set(LIB_ROOT "$ORIGIN")
endif()

set_target_properties(epanet2
    PROPERTIES
        MACOSX_RPATH TRUE
        SKIP_BUILD_RPATH FALSE
        BUILD_WITH_INSTALL_RPATH FALSE
        INSTALL_RPATH "${LIB_ROOT};${PACKAGE_RPATH};"
        INSTALL_RPATH_USE_LINK_PATH TRUE
)


install(TARGETS epanet2 EXPORT epanetTargets
    RUNTIME    DESTINATION "${TOOL_DIST}"
    LIBRARY    DESTINATION "${LIBRARY_DIST}"
    ARCHIVE    DESTINATION "${LIBRARY_DIST}"
    FRAMEWORK  DESTINATION "${TOOL_DIST}"
)

# Create target import scripts so other cmake projects can use swmm libraries
install(
    EXPORT
        epanetTargets
    DESTINATION
        "${CONFIG_DIST}"
    FILE
        epanet-config.cmake
)

install(
    FILES 
        ${EPANET_PUBLIC_HEADERS} 
    DESTINATION 
        "${INCLUDE_DIST}"
)
