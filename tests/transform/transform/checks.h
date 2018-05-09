//
//  checks.h
//  transform
//
//  Created by Pascal Bourguignon on 07/05/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

#ifndef checks_h
#define checks_h

#include <stdbool.h>
#include <CoreMIDI/CoreMIDI.h>


void* check_memory(void* pointer);
bool check_status(OSStatus status,const char* function);

#endif /* checks_h */
