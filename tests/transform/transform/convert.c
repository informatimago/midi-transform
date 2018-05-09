//
//  convert.c
//  transform
//
//  Created by Pascal Bourguignon on 07/05/2018.
//  Copyright © 2018 Pascal Bourguignon. All rights reserved.
//

#include <stdio.h>

#include <CoreMIDI/CoreMIDI.h>

#include "checks.h"
#include "convert.h"



MIDIObjectType object_type(MIDIObjectRef object){
    MIDIUniqueID id;
    MIDIObjectType type;
    if(check_status(MIDIObjectGetIntegerProperty(object,kMIDIPropertyUniqueID,&id),
                    "MIDIObjectGetIntegerProperty kMIDIPropertyUniqueID")
       && check_status(MIDIObjectFindByUniqueID(id,&object,&type),
                       "MIDIObjectFindByUniqueID")){
        return type;
    }
    return kMIDIObjectType_Other;
}

char* cstring(CFStringRef string) {
    if(!string) {
        return "(null)";
    }
    UInt32 encoding;
    //encoding=kCFStringEncodingISOLatin1;
    encoding=kCFStringEncodingASCII;
    const char* cstring=CFStringGetCStringPtr(string,encoding);
    if(cstring) {
        return strdup(cstring);
    }
    CFIndex length=CFStringGetLength(string);
    char* buffer=check_memory(malloc(1+length));

    if(!CFStringGetCString(string,buffer,1+length,encoding)) {
        fprintf(stderr,"Error in conversion of CFString.\n");
    }
    return buffer;
}

#define string_property(name,property)\
char* name(MIDIObjectRef object){\
    CFStringRef string;\
    if(check_status(MIDIObjectGetStringProperty(object,property,&string),\
                    "MIDIObjectGetStringProperty " #property)){\
        return cstring(string);\
    }else{\
        return strdup("(null)");\
    }\
}

string_property(name,kMIDIPropertyName)
string_property(display_name,kMIDIPropertyDisplayName)

MIDIObjectRef endpoint_entity(MIDIObjectRef endpoint){
    MIDIObjectRef entity;
    OSStatus result=MIDIEndpointGetEntity(endpoint,&entity);
    if(result==kMIDIObjectNotFound){
        return 0;
    }
    if(check_status(result,"MIDIEndpointGetEntity")){
        return entity;
    }
    return 0;
}

MIDIDeviceRef entity_device(MIDIObjectRef entity){
    MIDIDeviceRef device;
    OSStatus result=MIDIEntityGetDevice(entity,&device);
    if(result==kMIDIObjectNotFound){
        return 0;
    }
    if(check_status(result,"MIDIEntityGetDevice")){
        return entity;
    }
    return 0;
}

MIDIObjectRef entity(MIDIObjectRef object){
    switch(object_type(object)){
        case kMIDIObjectType_Device: // fall thru
        case kMIDIObjectType_ExternalDevice:
            return 0;
        case kMIDIObjectType_Entity: // fall thru
        case kMIDIObjectType_ExternalEntity:
            return object;
        case kMIDIObjectType_Source: //fall thru
        case kMIDIObjectType_Destination: //fall thru
        case kMIDIObjectType_ExternalSource: //fall thru
        case kMIDIObjectType_ExternalDestination:
            return endpoint_entity(object);
    }
    return 0;
}

MIDIDeviceRef device(MIDIObjectRef object){
    switch(object_type(object)){
        case kMIDIObjectType_Device: // fall thru
        case kMIDIObjectType_ExternalDevice:
            return 0;
        case kMIDIObjectType_Entity: // fall thru
        case kMIDIObjectType_ExternalEntity:
            return entity_device(object);
        case kMIDIObjectType_Source: //fall thru
        case kMIDIObjectType_Destination: //fall thru
        case kMIDIObjectType_ExternalSource: //fall thru
        case kMIDIObjectType_ExternalDestination:
        {
            MIDIObjectRef ent=entity(object);
            return ent?entity_device(ent):0;
        }
    }
    return 0;
}





typedef struct{
    ItemCount count;
    MIDIObjectRef* objects;
} MIDIObjects;

MIDIObjects* objects_alloc(ItemCount count){
    MIDIObjects* objects=check_memory(malloc(sizeof(*objects)));
    size_t size=(sizeof(objects->objects[0])*count);
    objects->count=count;
    objects->objects=check_memory(malloc(size));
    memset(objects->objects,0,size);
    return objects;
}

void objects_free(MIDIObjects* objects){
    free(objects->objects);
    free(objects);
}



typedef struct {
    ItemCount count;
    MIDIObjects** objects;
} objects_objects;

objects_objects* objects_objects_alloc(ItemCount count){
    objects_objects* objects=check_memory(malloc(sizeof(*objects)));
    size_t size=(sizeof(objects->objects[0])*count);
    objects->count=count;
    objects->objects=check_memory(malloc(size));
    memset(objects->objects,0,size);
    return objects;
}

void objects_objects_free(objects_objects* objects){
    for(ItemCount i=0;i<objects->count;i++){
        objects_free(objects->objects[i]);
    }
    free(objects->objects);
    free(objects);
}


bool objects_member(MIDIObjectRef object,MIDIObjects* objects){
    for(ItemCount i=0;i<objects->count;i++){
        if(object==objects->objects[i]){
            return true;
        }
    }
    return false;
}

MIDIObjects* objects_append(MIDIObjects* a,MIDIObjects* b){
    MIDIObjects* objects=objects_alloc(a->count+b->count);
    ItemCount dst=0;
    for(ItemCount src=0;src<a->count;src++){
        objects->objects[dst++]=a->objects[src];
    }
    for(ItemCount src=0;src<b->count;src++){
        objects->objects[dst++]=b->objects[src];
    }
    return objects;
}

MIDIObjects* objects_remove_duplicates(MIDIObjects* objects){
    MIDIObjects* results=objects_alloc(objects->count);
    ItemCount dst=0;
    // dumb O(n^2) algorithm, until we have hash-tables.
    for(ItemCount src=0;src<objects->count;src++){
        bool duplicate=false;
        for(ItemCount other=src+1;other<objects->count;other++){
            if(objects->objects[src]==objects->objects[other]){
                duplicate=true;
                break;
            }
        }
        if(!duplicate){
            results->objects[dst++]=objects->objects[src];
        }
    }
    results->count=dst;
    return results;
}

typedef MIDIObjectRef (*object_mapcar_pr)(MIDIObjectRef object);
typedef MIDIObjects* (*object_mapcan_pr)(MIDIObjectRef object);

MIDIObjects* objects_mapcar(object_mapcar_pr fun,MIDIObjects* objects){
    MIDIObjects* results=objects_alloc(objects->count);
    for(ItemCount i=0;i<objects->count;i++){
        results->objects[i]=fun(objects->objects[i]);
    }
    return results;
}

MIDIObjects* objects_objects_concatenate_all(objects_objects* lol){
    ItemCount total=0;
    for(ItemCount i=0;i<lol->count;i++){
        MIDIObjects* sources=lol->objects[i];
        total+=sources->count;
    }
    MIDIObjects* objects=objects_alloc(total);
    ItemCount dst=0;
    for(ItemCount i=0;i<lol->count;i++){
        MIDIObjects* sources=lol->objects[i];
        for(ItemCount src=0;src<sources->count;src++){
            objects->objects[dst++]=sources->objects[src];
        }
    }
    return objects;
}

MIDIObjects* objects_mapcan(object_mapcan_pr fun,MIDIObjects* objects){
    objects_objects* lol=objects_objects_alloc(objects->count);
    for(ItemCount i=0;i<objects->count;i++){
        lol->objects[i]=fun(objects->objects[i]);
    }
    MIDIObjects* results=objects_objects_concatenate_all(lol);
    objects_objects_free(lol);
    return results;
}

#define getObjects(countFunction,getFunction) \
    do{\
        MIDIObjects* objects=check_memory(malloc(sizeof(*objects)));\
        objects->count=countFunction();\
        objects->objects=check_memory(malloc(sizeof(objects->objects[0])*objects->count));\
        for(ItemCount i=0;i<objects->count;i++){\
            objects->objects[i]=getFunction(i);\
        }\
        return objects;\
    }while(0)

#define getObjectsWithParameters(countFunction,getFunction,...) \
    do{\
        MIDIObjects* objects=check_memory(malloc(sizeof(*objects)));\
        objects->count=countFunction(__VA_ARGS__);\
        objects->objects=check_memory(malloc(sizeof(objects->objects[0])*objects->count));\
        for(ItemCount i=0;i<objects->count;i++){\
            objects->objects[i]=getFunction(__VA_ARGS__,i);\
        }\
        return objects;\
    }while(0)

MIDIObjects* devices(void){
    getObjects(MIDIGetNumberOfDevices,MIDIGetDevice);
}

MIDIObjects* external_devices(void){
    getObjects(MIDIGetNumberOfExternalDevices,MIDIGetExternalDevice);
}

MIDIObjects* sources(void){
    getObjects(MIDIGetNumberOfSources,MIDIGetSource);
}

MIDIObjects* destinations(void){
    getObjects(MIDIGetNumberOfDestinations,MIDIGetDestination);
}

MIDIObjects* entity_sources(MIDIObjectRef entity){
    getObjectsWithParameters(MIDIEntityGetNumberOfSources,MIDIEntityGetSource,entity);
}

MIDIObjects* entity_destinations(MIDIObjectRef entity){
    getObjectsWithParameters(MIDIEntityGetNumberOfDestinations,MIDIEntityGetDestination,entity);
}

MIDIObjects* device_entities(MIDIDeviceRef device){
    getObjectsWithParameters(MIDIDeviceGetNumberOfEntities,MIDIDeviceGetEntity,device);
}

MIDIDeviceRef find_device_named_in_devices(const char* name,MIDIObjects* objects){
    CFStringRef cfname=CFStringCreateWithCString(0,name,kCFStringEncodingASCII);
    for(ItemCount i=0;i<objects->count;i++){
        CFStringRef deviceName=0;
        if(check_status(MIDIObjectGetStringProperty(objects->objects[i],kMIDIPropertyName,&deviceName),
                        "MIDIObjectGetStringProperty")){
            if(kCFCompareEqualTo==CFStringCompare(cfname,deviceName,0)){
                return objects->objects[i];
            }
        }
    }
    return 0;
}

MIDIDeviceRef find_device_named(const char* name){
    MIDIObjects* objects=devices();
    MIDIDeviceRef result=find_device_named_in_devices(name,objects);
    objects_free(objects);
    return result;
}

MIDIDeviceRef find_external_device_named(const char* name){
    MIDIObjects* objects=external_devices();
    MIDIDeviceRef result=find_device_named_in_devices(name,objects);
    objects_free(objects);
    return result;
}


typedef enum { direction_source, direction_destination} direction;
typedef MIDIObjects*(*entity_endpoints_pr)(MIDIObjectRef entity);

MIDIObjectRef entity_with_both_source_and_destination(MIDIDeviceRef device){
    MIDIObjects* entities=device_entities(device);
    for(ItemCount i=0;i<entities->count;i++){
        MIDIObjects* sources=entity_sources(entities->objects[i]);
        MIDIObjects* destinations=entity_destinations(entities->objects[i]);
        if(sources && destinations && sources->count && destinations->count){
            objects_free(sources);
            objects_free(destinations);
            MIDIObjectRef result=entities->objects[i];
            objects_free(entities);
            return result;
        }
        objects_free(sources);
        objects_free(destinations);
    }
    objects_free(entities);
    return 0;
}

MIDIObjectRef find_endpoint_for_device(MIDIDeviceRef device,direction direction){
    bool input=(direction==direction_source);
    entity_endpoints_pr entity_endpoints=input?&entity_sources:&entity_destinations;
    MIDIObjectRef entity=entity_with_both_source_and_destination(device);
    MIDIObjects* endpoints=entity?entity_endpoints(entity):(MIDIObjects*)0;
    if(endpoints && endpoints->count){
        MIDIObjectRef result=endpoints->objects[0];
        objects_free(endpoints);
        return result;
    }
    objects_free(endpoints);
    return 0;
}

MIDIObjects* filter_object_with_device(MIDIObjectRef object){
    MIDIDeviceRef dev=device(object);
    if(dev){
        MIDIObjects* devices=objects_alloc(1);
        devices->objects[0]=dev;
        return devices;
    }
    return 0;
}


CFDataRef connection_unique_ids(MIDIObjectRef object){
    CFDataRef ids;
    OSStatus status=MIDIObjectGetDataProperty(object,kMIDIPropertyConnectionUniqueID,&ids);
    if(status==kMIDIUnknownProperty){
        return 0;
    }
    if(check_status(status,"MIDIObjectGetIntegerProperty kMIDIPropertyConnectionUniqueID")){
        return ids;
    }
    return 0;
}

MIDIObjectRef find_object_by_unique_id(MIDIUniqueID id){
    MIDIObjectRef object;
    MIDIObjectType type;
    if(check_status(MIDIObjectFindByUniqueID(id,&object,&type),
                    "MIDIObjectFindByUniqueID")){
        return object;
    }
    return 0;
}

MIDIObjects* connected_objects_from_unique_ids(CFDataRef ids){
    CFIndex size=CFDataGetLength(ids);
    const UInt8 * data=CFDataGetBytePtr(ids);
    MIDIObjects* objects=objects_alloc(size/sizeof(MIDIUniqueID));
    ItemCount dst=0;
    for(CFIndex i=0;i<size;i+=sizeof(MIDIUniqueID)){
        MIDIUniqueID id=data[3]|(data[2]<<8)|(data[1]<<16)|(data[0]<<24);
        objects->objects[dst++]=find_object_by_unique_id(id);
    }
    return objects;
}

MIDIObjects* connected_objects(MIDIObjectRef object){
    CFDataRef ids=connection_unique_ids(object);
    if(ids){
        return connected_objects_from_unique_ids(ids);
    }
    return objects_alloc(0);
}

MIDIObjects* connected_devices(MIDIObjectRef object){
    MIDIObjects* objects=connected_objects(object);
    MIDIObjects* results=objects_mapcan(filter_object_with_device,objects);
    objects_free(objects);
    return results;
}

MIDIObjectRef find_endpoint_for_external_device(MIDIDeviceRef external_device,direction direction){
    bool input=(direction==direction_source);
    entity_endpoints_pr entity_endpoints=input?&entity_sources:&entity_destinations;
    MIDIObjects* entities=device_entities(external_device);
    MIDIObjectRef result=0;
    for(ItemCount i=0;i<entities->count;i++){
        MIDIObjects* endpoints=objects_append(entity_sources(entities->objects[i]),
                                              entity_destinations(entities->objects[i]));
        MIDIObjects* devices=objects_mapcan(connected_devices,endpoints);
        MIDIObjects* unique_devices=objects_remove_duplicates(devices);
        for(ItemCount i=0;i<unique_devices->count;i++){
            MIDIObjects* entities=device_entities(unique_devices->objects[i]);
            for(ItemCount j=0;j<entities->count;j++){
                MIDIObjects* endpoints=entity_endpoints(entities->objects[j]);
                for(ItemCount k=0;k<endpoints->count;k++){
                    MIDIObjects* cdevices=connected_devices(endpoints->objects[k]);
                    if(objects_member(external_device,cdevices)){
                        result=endpoints->objects[k];
                        objects_free(cdevices);
                        objects_free(endpoints);
                        objects_free(entities);
                        objects_free(unique_devices);
                        objects_free(devices);
                        objects_free(endpoints);
                        goto done;
                    }
                    objects_free(cdevices);
                }
                objects_free(endpoints);
            }
            objects_free(entities);
        }
        objects_free(unique_devices);
        objects_free(devices);
        objects_free(endpoints);
    }
done:
    objects_free(entities);
    return result;
}


MIDIEndpointRef find_endpoint_for_device_named(const char* name,direction direction){
    MIDIDeviceRef device;
    if((device=find_external_device_named(name))){
        return find_endpoint_for_external_device(device,direction);
    }
    if((device=find_device_named(name))){
        return find_endpoint_for_device(device,direction);
    }
        return 0;
}

MIDIEndpointRef find_source_endpoint_for_device_named(const char* name){
    return find_endpoint_for_device_named(name,direction_source);
}

MIDIEndpointRef find_destination_endpoint_for_device_named(const char* name){
    return find_endpoint_for_device_named(name,direction_destination);
}


void print_endpoints(const char* arrow,MIDIObjects* endpoints){
    if(endpoints && endpoints->count){
        for(ItemCount s=0;s<endpoints->count;s++){
            MIDIObjectRef endpoint=endpoints->objects[s];
            char* ename=name(endpoint);
            printf("               %s %s: ",arrow,ename);
            free(ename);
            MIDIObjects* cdevs=connected_devices(endpoint);
            for(ItemCount c=0;c<cdevs->count;c++){
                char* cname=name(cdevs->objects[c]);
                printf("%s ",cname);
                free(cname);
            }
            objects_free(cdevs);
            printf("\n");
        }
    }
}

void list_devices(void){
    MIDIObjects* devs=devices();
    MIDIObjects* edevs=external_devices();
    MIDIObjects* all_devices=objects_append(devs,edevs);
    for(ItemCount d=0;d<all_devices->count;d++){
        MIDIDeviceRef device=all_devices->objects[d];
        MIDIObjects* entities=device_entities(device);
        printf("%-30s \n",name(device));
        for(ItemCount e=0;e<entities->count;e++){
            MIDIObjectRef entity=entities->objects[e];
            printf("        - %s \n",name(entity));
            MIDIObjects* sources=entity_sources(entity);
            MIDIObjects* destinations=entity_destinations(entity);
            print_endpoints("<-",sources);
            print_endpoints("->",destinations);
            objects_free(destinations);
            objects_free(sources);
        }
        objects_free(entities);
    }


    objects_free(devs);
    objects_free(edevs);
    objects_free(all_devices);
}



typedef struct {
    MIDIClientRef client;
    MIDIPortRef output;
    MIDIPortRef input;
    MIDIEndpointRef controller_source;
    MIDIEndpointRef controller_destination;
    MIDIEndpointRef device_source;
    MIDIEndpointRef device_destination;
} midi;

void midiNotify(const MIDINotification *message, void *refCon){
    midi* midi=refCon;
    printf("midiNotify %p %p\n",message,midi);
}

void midiReadProc(const MIDIPacketList *pktlist, void *readProcRefCon, void *srcConnRefCon){
    midi* midi=readProcRefCon;
    printf("midiReadProc %p %p\n",pktlist,midi);
}

midi* initialize(const options* options){
    midi* midi=check_memory(malloc(sizeof(*midi)));
    midi->client=0;
    midi->output=0;
    midi->input=0;
    if(check_status(MIDIClientCreate(CFSTR("convert"),&midiNotify,midi,&(midi->client)),"MIDIClientCreate")
       && check_status(MIDIOutputPortCreate(midi->client,CFSTR("convert output"),&(midi->output)),"MIDIOutputPortCreate")
       && check_status(MIDIInputPortCreate(midi->client,CFSTR("convert input"),&midiReadProc,midi,&(midi->output)),"MIDInputPortCreate")){

        midi->controller_source=find_source_endpoint_for_device_named(options->controller_name);
        midi->controller_destination=find_destination_endpoint_for_device_named(options->controller_name);
        midi->controller_source=find_source_endpoint_for_device_named(options->device_name);
        midi->controller_destination=find_destination_endpoint_for_device_named(options->device_name);

        printf("controller source       = %16u (%s)\n",(unsigned int)midi->controller_source,options->controller_name);
        printf("controller destination  = %16u (%s)\n",(unsigned int)midi->controller_destination,options->controller_name);
        printf("device     source       = %16u (%s)\n",(unsigned int)midi->device_source,options->device_name);
        printf("device     destination  = %16u (%s)\n",(unsigned int)midi->device_destination,options->device_name);

        return midi;
    }
    fprintf(stderr,"Error while initializing MIDI stuff\n");        
    free(midi);
    return 0;
}



void convert(const options* options){
    printf("Hi! Converting MIDI\n");
    midi* midi=initialize(options);
    if(!midi){
        return;
    }
    while(true){
        CFRunLoopRunInMode(kCFRunLoopDefaultMode,1.0,false);
    }
}
