#include <stdio.h>
#include <stdlib.h>
// #include <dlfcn.h>
#include <CoreFoundation/CoreFoundation.h>
#include <CoreMIDI/CoreMIDI.h>
#include "print-constants.h"

void* checkMemory(void* memory) {
    if(memory) {
        return memory;
    }
    fprintf(stderr,"Out of memory\n");
    exit(1);
}

const char* cstring(CFStringRef string) {
    if(!string) {
        return "(null)";
    }
    UInt32 encoding;
    //encoding=kCFStringEncodingISOLatin1;
    encoding=kCFStringEncodingASCII;
    const char* cstring=CFStringGetCStringPtr(string,encoding);
    if(cstring) {
        return cstring;
    }
    CFIndex length=CFStringGetLength(string);
    char* buffer=checkMemory(malloc(1+length));
    
    if(!CFStringGetCString(string,buffer,1+length,encoding)) {
        fprintf(stderr,"Error in conversion of CFString.\n");
    }
    return buffer;
}

void print_constants() {
    MIDIRestart();
    printf("%-40s = \"%s\"\n", "kMIDIPropertyName",                     cstring(kMIDIPropertyName));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyManufacturer",             cstring(kMIDIPropertyManufacturer));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyModel",                    cstring(kMIDIPropertyModel));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyUniqueID",                 cstring(kMIDIPropertyUniqueID));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyDeviceID",                 cstring(kMIDIPropertyDeviceID));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceiveChannels",          cstring(kMIDIPropertyReceiveChannels));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitChannels",         cstring(kMIDIPropertyTransmitChannels));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyMaxSysExSpeed",            cstring(kMIDIPropertyMaxSysExSpeed));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyAdvanceScheduleTimeMuSec", cstring(kMIDIPropertyAdvanceScheduleTimeMuSec));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyIsEmbeddedEntity",         cstring(kMIDIPropertyIsEmbeddedEntity));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyIsBroadcast",              cstring(kMIDIPropertyIsBroadcast));
    printf("%-40s = \"%s\"\n", "kMIDIPropertySingleRealtimeEntity",     cstring(kMIDIPropertySingleRealtimeEntity));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyConnectionUniqueID",       cstring(kMIDIPropertyConnectionUniqueID));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyOffline",                  cstring(kMIDIPropertyOffline));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyPrivate",                  cstring(kMIDIPropertyPrivate));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyDriverOwner",              cstring(kMIDIPropertyDriverOwner));
    // printf("%-40s = \"%s\"\n", "kMIDIPropertyFactoryPatchNameFile",     cstring(kMIDIPropertyFactoryPatchNameFile));
    // printf("%-40s = \"%s\"\n", "kMIDIPropertyUserPatchNameFile",        cstring(kMIDIPropertyUserPatchNameFile));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyNameConfiguration",        cstring(kMIDIPropertyNameConfiguration));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyImage",                    cstring(kMIDIPropertyImage));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyDriverVersion",            cstring(kMIDIPropertyDriverVersion));
    printf("%-40s = \"%s\"\n", "kMIDIPropertySupportsGeneralMIDI",      cstring(kMIDIPropertySupportsGeneralMIDI));
    printf("%-40s = \"%s\"\n", "kMIDIPropertySupportsMMC",              cstring(kMIDIPropertySupportsMMC));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyCanRoute",                 cstring(kMIDIPropertyCanRoute));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceivesClock",            cstring(kMIDIPropertyReceivesClock));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceivesMTC",              cstring(kMIDIPropertyReceivesMTC));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceivesNotes",            cstring(kMIDIPropertyReceivesNotes));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceivesProgramChanges",   cstring(kMIDIPropertyReceivesProgramChanges));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceivesBankSelectMSB",    cstring(kMIDIPropertyReceivesBankSelectMSB));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyReceivesBankSelectLSB",    cstring(kMIDIPropertyReceivesBankSelectLSB));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitsClock",           cstring(kMIDIPropertyTransmitsClock));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitsMTC",             cstring(kMIDIPropertyTransmitsMTC));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitsNotes",           cstring(kMIDIPropertyTransmitsNotes));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitsProgramChanges",  cstring(kMIDIPropertyTransmitsProgramChanges));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitsBankSelectMSB",   cstring(kMIDIPropertyTransmitsBankSelectMSB));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyTransmitsBankSelectLSB",   cstring(kMIDIPropertyTransmitsBankSelectLSB));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyPanDisruptsStereo",        cstring(kMIDIPropertyPanDisruptsStereo));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyIsSampler",                cstring(kMIDIPropertyIsSampler));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyIsDrumMachine",            cstring(kMIDIPropertyIsDrumMachine));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyIsMixer",                  cstring(kMIDIPropertyIsMixer));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyIsEffectUnit",             cstring(kMIDIPropertyIsEffectUnit));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyMaxReceiveChannels",       cstring(kMIDIPropertyMaxReceiveChannels));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyMaxTransmitChannels",      cstring(kMIDIPropertyMaxTransmitChannels));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyDriverDeviceEditorApp",    cstring(kMIDIPropertyDriverDeviceEditorApp));
    printf("%-40s = \"%s\"\n", "kMIDIPropertySupportsShowControl",      cstring(kMIDIPropertySupportsShowControl));
    printf("%-40s = \"%s\"\n", "kMIDIPropertyDisplayName",              cstring(kMIDIPropertyDisplayName));          
}

