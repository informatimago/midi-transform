//
//  options.h
//  transform
//
//  Created by Pascal Bourguignon on 07/05/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

#ifndef options_h
#define options_h
#include <stdbool.h>

typedef struct {
    bool help;
    bool list_devices;
    const char* controller_name;
    int controller_channel;
    const char* device_name;
    int device_channel;
} options;

options* parse_options(int argc,const char** argv);
void usage(const char* pname);
const char* program_name(const char* argv0);

#endif /* options_h */
