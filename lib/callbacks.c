#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <scheme.h>
#include "portaudio.h"

typedef struct soundCopyingInfo{
  // this sound is assumed to be malloc'ed, and gets freed when finished.
  short *sound;
  unsigned long curSample;
  unsigned long numSamples;
  int stopNow;
  Scheme_Object **stoppedPtr;
} soundCopyingInfo;

#define STREAMBUFS 4

typedef struct soundStreamInfo{
  int   bufferFrames;
  short *buffers[STREAMBUFS];
  // mutated by racket only:
  int   bufNumbers[STREAMBUFS];
  // mutated by callback only:
  int   lastUsed;
  int   faultCount;
  struct mzrt_sema *bufferNeeded;
} soundStreamInfo;

#define CHANNELS 2
#define SAMPLEBYTES 2

#define MYMIN(a,b) ((a)<(b) ? (a) : (b))


void freeClosure(soundCopyingInfo *ri);
void freeStreamingInfo(soundStreamInfo *ssi);



// simplest possible feeder; copy bytes until you run out.
// assumes 16-bit ints, 2 channels.
// CALLS FREE ON THE SOUND AND THE RECORD WHEN FINISHED
int copyingCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData ) {

  soundCopyingInfo *ri = (soundCopyingInfo *)userData;
  short *copyBegin = ri->sound + ri->curSample;
  unsigned long samplesToCopy = frameCount * CHANNELS;
  unsigned long nextCurSample = ri->curSample + samplesToCopy;
  // !@#$ windows makes me declare them at the top of the function:
  size_t bytesToCopy;
  char *zeroRegionBegin;
  size_t bytesToZero;

  if (ri->stopNow) {
    freeClosure(ri);
    return(paAbort);
  }

  if (ri->numSamples <= nextCurSample) {
    // this is the last chunk.
    bytesToCopy = SAMPLEBYTES * (ri->numSamples - ri->curSample);
    memcpy(output,(void *)copyBegin,bytesToCopy);
    // zero out the rest of the buffer:
    zeroRegionBegin = (char *)output + bytesToCopy;
    bytesToZero = (frameCount * CHANNELS * SAMPLEBYTES) - bytesToCopy;
    memset(zeroRegionBegin,0,bytesToZero);
    ri->curSample = ri->numSamples;

    return(paComplete);

  } else {
    // this is not the last chunk. 
    bytesToCopy = SAMPLEBYTES * samplesToCopy;
    memcpy(output,(void *)copyBegin,bytesToCopy);
    ri->curSample = nextCurSample;
    return(paContinue);
  }
}

// copy buffers to the output buffer (if they're available)
int streamingCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData ) {

  soundStreamInfo *ssi = (soundStreamInfo *)userData;
  int bufferBytes = frameCount * CHANNELS * SAMPLEBYTES;
  int nextBufNum = ssi->lastUsed + 1;
  int modCounter = nextBufNum % STREAMBUFS;

  // right number of frames requested?
  if (ssi->bufferFrames != frameCount) {
    fprintf(stderr,"audio unit requested %ld frames, instead of expected %d.\n",
            frameCount,
            ssi->bufferFrames);
    return(paAbort);
  }
  
  // has the desired buffer been written?
  if (ssi->bufNumbers[modCounter] == nextBufNum) {
    // yes, do the copy:
    memcpy(output,(void *)ssi->buffers[modCounter],bufferBytes);
  } else {
    // no, just use silence:
    memset(output,0,bufferBytes);
    // increment the fault count:
    ssi->faultCount += 1;
  }
  ssi->lastUsed = nextBufNum;
  mzrt_sema_post(ssi->bufferNeeded);
  return(paContinue);

}

// clean up when done:  free the sound data and the
// closure data
void freeClosure(soundCopyingInfo *ri){
  *(ri->stoppedPtr) = scheme_true;
  free(ri->sound);
  free(ri);
}

// clean up a streamingInfo record when done.
void freeStreamingInfo(soundStreamInfo *ssi){
  int i;

  for (i = 0; i < STREAMBUFS; i++) {
    free(ssi->buffers[i]);
  }
  // Racket code must destroy the listener before
  // this happens.
  mzrt_sema_destroy(ssi->bufferNeeded);
  free(ssi);
}
