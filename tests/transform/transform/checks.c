//
//  checks.c
//  transform
//
//  Created by Pascal Bourguignon on 07/05/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>

#include "checks.h"

void* check_memory(void* pointer){
    if(pointer){return pointer;}
    fprintf(stderr,"\nOut of memory\n");
    exit(EX_SOFTWARE);
}

bool check_status(OSStatus status,const char* function){
    if(status!=0){
        fprintf(stderr,"\n%s returned status %d\n",function,status);
        return false;
    }
    return true;
}

