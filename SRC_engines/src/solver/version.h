/*
 *  version.h - EPANET version header file
 *
 *  Created on: Sep 2, 2023
 *  
 *  Author:     see CONTRIBUTORS
 *
 *  Note: 
 *    The cmake build process automatically generates this file. Do not edit.
 */


#ifndef VERSION_H_
#define VERSION_H_



#define PROJECT             "EPANET"
#define ORGANIZATION        "US EPA ORD"

#define VERSION             "2.2.0"
#define VERSION_MAJOR       2
#define VERSION_MINOR       2
#define VERSION_PATCH       0
#define GIT_HASH            "c2224b3c1a85ec9f35cca1f6ddfec3d4bdf6465c"

#define PLATFORM            "Windows"
#define COMPILER            "MSVC"
#define COMPILER_VERSION    "19.37.32825.0"
#define BUILD_ID            "2023-11-15T13:41:19Z"


static inline int get_version_legacy() { \
    return VERSION_MAJOR * 10000 + VERSION_MINOR * 1000 + VERSION_PATCH; \
}



#endif /* VERSION_H_ */
