//
//  options.c
//  transform
//
//  Created by Pascal Bourguignon on 07/05/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

#include <stdlib.h>
#include <sysexits.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include <libgen.h>

#include "options.h"
#include "checks.h"

#define max(a,b) (((a)<(b))?(b):(a))
#define min(a,b) (((a)>(b))?(b):(a))

options* options_alloc(){
    options* options=check_memory(malloc(sizeof(*options)));
    options->help=false;
    options->list_devices=false;
    options->controller_name=0;
    options->controller_channel=0;
    options->device_name=0;
    options->device_channel=0;
    return options;
}

void usage(const char* pname){
    char blank[80];
    unsigned long len=min((unsigned long)sizeof(blank)-1,(unsigned long)strlen(pname));
    memset(blank,' ',len);
    blank[len]=0;

    printf("\n%s usage:\n",pname);
    printf("\n");
    printf("\t%s [-h|--help|-l|--list-devices]\n",pname);
    printf("\t%s -cd|--controller-device-name $NAME -cc|--controller-channel $CHANNEL \\\n",pname);
    printf("\t%s -dd|--dw-8000-device-name|-ed|--ex-8000-device-name $NAME \\\n",blank);
    printf("\t%s -dc|--dw-8000-channel|-ec|--ex-8000-channel $CHANNEL\n",blank);
    printf("\n");
}

static void expect_argument(const char* pname,int argc,int i,const char* option,const char* name){
    if(i>=argc-1){
        fprintf(stderr,"Missing %s after %s\n",name,option);
        usage(pname);
        exit(EX_USAGE);
    }
}

static inline int inbounds(int min,int value,int max){
    return (min<=value)&&(value<=max);
}

static int raw_parse_integer(const char* string,bool* errorp){
    (*errorp)=false;
    int n=0;
    int sign=1;
    while(*string==' '){ string++; }
    if(*string=='+'){
        string++;
    }else if(*string=='-'){
        string++;
        sign=-1;
    }
    while(isdigit(*string)){
        int digit=(*string)-'0';
        string++;
        // Note; we don't reach INT_MIN, only -INT_MAX.
        if((n<INT_MAX/10)
           ||((n==INT_MAX/10)&&(digit<=INT_MAX-((INT_MAX/10)*10)))){
            n*=10;
            n+=digit;
        }else{
            (*errorp)=true;
        }
    }
    n*=sign;
    if(*string){
        (*errorp)=true;
    }
    return n;
}

static int parse_integer(const char* pname,const char* string,const char* name,int min,int max){
    bool errorp;
    int n=raw_parse_integer(string,&errorp);
    if(errorp || !inbounds(min,n,max)){
        fprintf(stderr,"%s error: invalid %s '%s', it should be an integer between %d and %d.\n",
                pname,name,string,min,max);
        usage(pname);
        exit(EX_USAGE);
    }
    return n;
}

const char* program_name(const char* argv0){
    static char* lpname=0;
    const char* pname=0;
    if(!pname){
        lpname=check_memory(strdup(argv0));
        pname=basename(lpname);
    }
    return pname;
}

void check_missing_argument(const char* pname,unsigned long argument,const char* name){
    if(!argument){
        fprintf(stderr,"%s error: Missing %s\n",pname,name);
        usage(pname);
        exit(EX_USAGE);
    }
}

options* parse_options(int argc,const char** argv){
    options* options=options_alloc();
    const char* pname=program_name(argv[0]);
    for(int i=1;i<argc;i++){
        if((0==strcmp(argv[i],"-h"))||(0==strcmp(argv[i],"--help"))){
            options->help=true;
            break;
        }
        if((0==strcmp(argv[i],"-l"))||(0==strcmp(argv[i],"--list-devices"))){
            options->list_devices=true;
            break;
        }
        if((0==strcmp(argv[i],"-cd"))||(0==strcmp(argv[i],"--controller-device-name"))){
            expect_argument(pname,argc,i,argv[i],"controller device name");
            i++;
            options->controller_name=argv[i];
        }
        if((0==strcmp(argv[i],"-dd"))||(0==strcmp(argv[i],"--dw-8000-device-name"))
            ||(0==strcmp(argv[i],"-ed"))||(0==strcmp(argv[i],"--ex-8000-device-name"))){
            expect_argument(pname,argc,i,argv[i],"dw-8000/ex-8000 device name");
            i++;
            options->device_name=argv[i];
        }
        if((0==strcmp(argv[i],"-cc"))||(0==strcmp(argv[i],"--controller-channel"))){
            expect_argument(pname,argc,i,argv[i],"controller MIDI channel");
            i++;
            options->controller_channel=parse_integer(pname,argv[i],"controller MIDI channel",1,16);
        }
        if((0==strcmp(argv[i],"-dc"))||(0==strcmp(argv[i],"--dw-8000-channel"))
           ||(0==strcmp(argv[i],"-ec"))||(0==strcmp(argv[i],"--ex-8000-channel"))){
            expect_argument(pname,argc,i,argv[i],"dw-8000/ex-8000 MIDI channel");
            i++;
            options->device_channel=parse_integer(pname,argv[i],"device MIDI channel",1,16);
        }
    }
    if(!(options->help||options->list_devices)){
        check_missing_argument(pname,0!=options->controller_name,"controller name");
                check_missing_argument(pname,options->controller_channel,"controller MIDI channel");
                check_missing_argument(pname,0!=options->device_name,"device name");
                check_missing_argument(pname,options->device_channel,"device MIDI channel");
    }
    return options;
}

