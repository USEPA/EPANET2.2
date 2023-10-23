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
#define GIT_HASH            "31eb962b1f7c65ac739899105d27617c2a94b7f3"

#define PLATFORM            "Windows"
#define COMPILER            "MSVC"
#define COMPILER_VERSION    "19.37.32824.0"
#define BUILD_ID            "2023-10-23T18:30:13Z"


static inline int get_version_legacy() { \
    return VERSION_MAJOR * 10000 + VERSION_MINOR * 1000 + VERSION_PATCH; \
}



#endif /* VERSION_H_ */
