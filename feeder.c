#include <portaudio.h>

typedef struct rackaudioClosure{
  short *nextData;
  int taken;
} rackaudioClosure;
  
typedef int simpleFeeder(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData ) {

  racketInfo *ri = userData;

  
