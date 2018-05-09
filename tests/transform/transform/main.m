//
//  main.m
//  transform
//
//  Created by Pascal Bourguignon on 07/05/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

#import <Foundation/Foundation.h>
#include "options.h"
#include "list_devices.h"
#include "convert.h"

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        options* options=parse_options(argc,argv);
        const char* pname=program_name(argv[0]);
        if(options->help){
            usage(pname);
        }else if(options->list_devices){
            list_devices();
        }else{
            convert(options);
        }
    }
    return 0;
}
