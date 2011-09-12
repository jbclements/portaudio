#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib/portaudio.h"

typedef struct rackaudioClosure{
  void *dataHandle;
  long curPosition;
  long stopPosition;
  int stopNow;
} rackaudioClosure;

#define CHANNELS 2

#define MYMIN(a,b) ((a)<(b) ? (a) : (b))

unsigned long oldFetchFirstWord(unsigned long *data){
  return (unsigned long)data;
}

unsigned long fetchFirstWord(void *handle){
  return *((unsigned long*)handle);
}
