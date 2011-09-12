#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib/portaudio.h"

typedef struct rackaudioClosure{
  short *nextData;
  short *stop;
  int stopNow;
} rackaudioClosure;

#define CHANNELS 2

#define MYMIN(a,b) ((a)<(b) ? (a) : (b))

// simplest possible feeder; copy bytes until you run out.
// assumes 16-bit ints, 2 channels.
// no external control
int copyingCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData ) {

  rackaudioClosure *ri = userData;

  if (ri->stopNow) {
    return(paAbort);
  }

  short *copyBegin = ri->nextData;
  short *copyMaybeEnd = copyBegin + (frameCount * CHANNELS);

  if (ri->stop <= copyMaybeEnd) {
    // this is the last chunk.
    size_t bytesToCopy = ((char *)ri->stop) - ((char *)copyBegin);
    memcpy(output,(void *)copyBegin,bytesToCopy);
    // zero out the rest of the buffer:
    char *zeroRegionBegin = (char *)output + bytesToCopy;
    size_t bytesToZero = (frameCount * CHANNELS * sizeof(short)) - bytesToCopy;
    memset(zeroRegionBegin,0,bytesToZero);
    ri->nextData = ri->stop;
    return(paComplete);
  } else {
    // this is not the last chunk. 
    size_t bytesToCopy = sizeof(short) * (copyMaybeEnd - copyBegin);
    memcpy(output,(void *)copyBegin,bytesToCopy);
    ri->nextData = copyMaybeEnd;
    return(paContinue);
  }
}

  
