#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "portaudio.h"

// This file provides callbacks suitable for passing to
// portaudio that can respond to portaudio requests for
// data. There are three callbacks, the copyingCallback,
// the only-partially-implemented copyingCallbackRec,
// and the streamingCallback.  The first one is for
// playing sounds that are completely pre-rendered
// in a buffer, and the third is for playing sounds
// that are being generated on the fly. The difference
// between the two is that the streaming one has mutable fields
// in its struct that allow two-way communication between
// C and Racket, and that it knows how to loop around to
// the beginning of the buffer again after it reaches the end.

// Implementation note: Portaudio is very specific that these
// callbacks definitely can't block; this is why we need
// a double-callback architecture in the case of streaming
// output; the low-level callback never blocks, and the higher-level
// callback is written in Racket (and might block).

typedef struct soundCopyingInfo{
  // this sound is assumed to be malloc'ed, and gets freed when finished.
  short *sound;
  unsigned long curSample;
  unsigned long numSamples;
} soundCopyingInfo;

typedef struct soundStreamInfo{
  unsigned int   bufferFrames;
  char *buffer;

  // only mutated by C (er... I believe?)
  unsigned int lastFrameRead;
  unsigned int lastOffsetRead;

  // only mutated by Racket
  unsigned int lastFrameWritten;
  unsigned int lastOffsetWritten;

  int   faultCount;
  int   *all_done;
} soundStreamInfo;

#define CHANNELS 2
#define SAMPLEBYTES 2

#define MYMIN(a,b) ((a)<(b) ? (a) : (b))
#define MYMAX(a,b) ((a)>(b) ? (a) : (b))
#define FRAMES_TO_BYTES(a) ((a)*CHANNELS*SAMPLEBYTES)


void freeCopyingInfo(soundCopyingInfo *ri);
void freeStreamingInfo(soundStreamInfo *ssi);

// this is a callback that plays sound from a fixed buffer.
// note that this callback's interface is fixed by portaudio.
// assumes 16-bit ints, 2 channels.

// NB: the only effect of this callback is to copy bytes from
// one buffer to another. No allocation or freeing takes place.
int copyingCallback(
    const void *input, // pointer to input sounds : unused here
    void *output, // the buffer to copy into
    unsigned long frameCount, // the number of frames to copy
    const PaStreamCallbackTimeInfo* timeInfo, // info on time
    PaStreamCallbackFlags statusFlags, // info on status
    void *userData ) // the userdata from racket (containing
  // the actual sound to be played)
{

  soundCopyingInfo *ri = (soundCopyingInfo *)userData;
  short *copyBegin = ri->sound + ri->curSample;
  unsigned long samplesToCopy = frameCount * CHANNELS;
  unsigned long nextCurSample = ri->curSample + samplesToCopy;
  // !@#$ windows makes me declare them at the top of the function:
  size_t bytesToCopy;
  char *zeroRegionBegin;
  size_t bytesToZero;

  if (ri->numSamples <= nextCurSample) {
    // request is for more samples than the rest of the sound.
    // Therefore, this is the last chunk.
    bytesToCopy = SAMPLEBYTES * (ri->numSamples - ri->curSample);
    memcpy(output,(void *)copyBegin,bytesToCopy);
    // zero out the rest of the buffer:
    zeroRegionBegin = (char *)output + bytesToCopy;
    bytesToZero = FRAMES_TO_BYTES(frameCount) - bytesToCopy;
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

// this is a recording callback. I believe it works for some
// sets of inputs, but I don't believe it works in general.
// for one thing, it records a fixed duration sound.

// assumes 16-bit ints, 2 channels.

// NB: the only effect of this callback is to copy bytes from
// one buffer to another. No allocation or freeing takes place.
int copyingCallbackRec(
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

  if (ri->numSamples <= nextCurSample) {
    // this is the last chunk.
    bytesToCopy = SAMPLEBYTES * (ri->numSamples - ri->curSample);
    memcpy((void *)copyBegin,input,bytesToCopy);
    ri->curSample = ri->numSamples;
    return(paComplete);

  } else {
    // this is not the last chunk.
    bytesToCopy = SAMPLEBYTES * samplesToCopy;
    memcpy((void *)copyBegin,input,bytesToCopy);
    ri->curSample = nextCurSample;
    return(paContinue);
  }
}

// this is a streaming callback, to be used with sounds
// that are being generated as they're being played back.

// the interface here is fixed by portaudio. See comments above about
// meanings of input arguments.

// NB: the only effect of this callback is to copy bytes from
// one buffer to another. No allocation or freeing takes place.
int streamingCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData ) {

  soundStreamInfo *ssi = (soundStreamInfo *)userData;

  unsigned int lastFrameRequested = ssi->lastFrameRead + frameCount;
  unsigned int lastFrameToCopy = MYMAX(ssi->lastFrameRead,
                                       MYMIN(lastFrameRequested,
                                             ssi->lastFrameWritten));
  unsigned int framesToCopy = lastFrameToCopy - ssi->lastFrameRead;
  unsigned int bytesToCopy = FRAMES_TO_BYTES(framesToCopy);
  unsigned int lastOffsetToCopy = ssi->lastOffsetRead + bytesToCopy;
  unsigned int bufferBytes = FRAMES_TO_BYTES(ssi->bufferFrames);
  // stupid windows. I bet there's some way to get around this restriction.
  unsigned int bytesInEnd;
  unsigned int bytesAtBeginning;
  
  if (lastOffsetToCopy > bufferBytes) {
    // break it into two pieces:
    bytesInEnd = bufferBytes - ssi->lastOffsetRead;
    memcpy(output,(void *)((ssi->buffer)+(ssi->lastOffsetRead)),bytesInEnd);
    bytesAtBeginning = bytesToCopy - bytesInEnd;
    memcpy((void *)((char *)output+bytesInEnd),(void *)ssi->buffer,bytesAtBeginning);
  } else {
    // otherwise just copy it all at once:
    memcpy(output,(void *)((ssi->buffer)+(ssi->lastOffsetRead)),bytesToCopy);
  }
  // fill the rest with zeros, if any:
  if (lastFrameToCopy < lastFrameRequested) {
    memset((void *)((char *)output+bytesToCopy),0,FRAMES_TO_BYTES(lastFrameRequested - lastFrameToCopy));
    ssi->faultCount += 1;
  }
  // update record. Advance to the desired point, even
  // if it wasn't available.
  ssi->lastFrameRead = lastFrameRequested;
  ssi->lastOffsetRead = (ssi->lastOffsetRead + FRAMES_TO_BYTES(frameCount)) % bufferBytes;

  return(paContinue);

}

// clean up when done:  free the sound data and the
// closure data
void freeCopyingInfo(soundCopyingInfo *ri){
  free(ri->sound);
  free(ri);
}

// clean up a streamingInfo record when done, sets a cell used to indicate
// the stream can be freed
void freeStreamingInfo(soundStreamInfo *ssi){
  // when all_done is 1, this triggers racket to call PaClose on the stream.
  // note that we're not mutating the structure here,
  // but rather a cell that it points to, so it will
  // survive the free(ssi).
  *(ssi->all_done) = 1;
  free(ssi->buffer);
  free(ssi);
}

// this is just a stub to call malloc.
// it's necessary on windows, to ensure
// that the free & malloc used on the
// sound info blocks are associated
// with the same library.
void *dll_malloc(size_t bytes){
  return malloc(bytes);
}
