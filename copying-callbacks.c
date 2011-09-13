#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <scheme.h>
#include "lib/portaudio.h"

typedef struct rackaudioClosure{
  // this sound is assumed to be malloc'ed, and gets freed when finished.
  short *sound;
  unsigned long curSample;
  unsigned long numSamples;
  int stopNow;
  Scheme_Object *stopSema;
} rackaudioClosure;

#define CHANNELS 2

#define MYMIN(a,b) ((a)<(b) ? (a) : (b))

// copySound: just copy the whole darn sound into a freshly malloc'ed chunk.
// not great, but solves *all* of the problems interacting with GC
rackaudioClosure *createClosure(short *data,
                                unsigned long samples,
                                Scheme_Object *stopSema) {

  size_t numSoundBytes = (sizeof(short) * samples);
  short *copiedSound = malloc(numSoundBytes);

  if (copiedSound == NULL) {
    return(NULL);
  } else {
    memcpy((void *)copiedSound,(void *)data,numSoundBytes);

    rackaudioClosure *result = malloc(sizeof(rackaudioClosure));

    if (result == NULL) {
      free(copiedSound);
      return(NULL);
    } else {
      result->sound = copiedSound;
      result->curSample = 0;
      result->numSamples = samples;
      result->stopNow = 0;
      result->stopSema = stopSema;
      return(result);
    }
  }
}

  
// simplest possible feeder; copy bytes until you run out.
// assumes 16-bit ints, 2 channels.
// CALLS FREE ON THE SOUND AND THE RECORD WHEN FINISHED
int copyingCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData ) {

  rackaudioClosure *ri = userData;

  if (ri->stopNow) {
    freeClosure(ri);
    return(paAbort);
  }

  short *copyBegin = ri->sound + ri->curSample;
  unsigned long samplesToCopy = frameCount * CHANNELS;
  unsigned long nextCurSample = ri->curSample + samplesToCopy;
  //short *copyMaybeEnd = copyBegin + (frameCount * CHANNELS);

  if (ri->numSamples <= nextCurSample) {
    // this is the last chunk.
    size_t bytesToCopy = sizeof(short) * (ri->numSamples - ri->curSample);
    memcpy(output,(void *)copyBegin,bytesToCopy);
    // zero out the rest of the buffer:
    char *zeroRegionBegin = (char *)output + bytesToCopy;
    size_t bytesToZero = (frameCount * CHANNELS * sizeof(short)) - bytesToCopy;
    memset(zeroRegionBegin,0,bytesToZero);
    ri->curSample = ri->numSamples;

    freeClosure(ri);
    return(paComplete);
    
  } else {
    // this is not the last chunk. 
    size_t bytesToCopy = sizeof(short) * samplesToCopy;
    memcpy(output,(void *)copyBegin,bytesToCopy);
    ri->curSample = nextCurSample;
    return(paContinue);
  }
}

// clean up when done: post to the finished
// semaphore, free the sound data and the
// closure data
void freeClosure(rackaudioClosure *ri){
  scheme_post_sema(ri->stopSema);
  free(ri->sound);
  free(ri);
}
  
