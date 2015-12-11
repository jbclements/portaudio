#lang scribble/doc

@(require scribble/manual)

@title{Portaudio: Bindings for the Portaudio portable sound library}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket))

@defmodule[portaudio]{This collection provides 
 bindings to the cross-platform ``Portaudio'' library, capable of playing 
 sound on Windows, OS X, and Linux. 
 
 This package includes the portaudio dynamic libraries for Windows and Mac, 
 where I believe that users will have trouble compiling and installing 
 such a package.  On Linux, users must install it themselves.
 
 In addition, there is a small C library that provides interface code.
 This is provided in compiled form for all platforms.
 
 These C libraries, like all other higher-level parts of this 
 package, assume that all samples are represented as 16-bit
 signed integers, and that there are exactly two channels of
 interleaved audio.  Doing something else would require recompiling 
 the callback code, or writing your own callbacks and calling the
 portaudio functions directly.
 
 The functions of the portaudio package are provided directly. These
 are not documented; instead, read @filepath{portaudio.rkt} to see
 the header file for portaudio, which turns out to be essentially
 the best existing documentation for the package. Furthermore,
 these functions are not at all safe, and it's definitely possible
 to crash racket using these functions.
 
 There are also higher-level functions, @racket[s16vec-play],
 and @racket[stream-play], and @racket[stream-play/unsafe], 
 for playing sounds and streams. The first two are safe.
 These are documented below.
 
 Platforms differ in their support for simultaneous streams. The
 Mac OS X Coreaudio API seems able to support hundreds of simultaneous
 streams. The Windows API's that I've tested don't reliably support
 more than one. On platforms that don't support more than one stream,
 then, playing multiple sounds at once requires adding them all to 
 one stream. This solution also has the advantage of substantially 
 lower latency. The RSound library provides support for this, in
 the form of its @racket[pstream] abstraction.
 
 My ability to test on different platforms is limited; I'm always 
 eager to hear about successes and failures that people experience
 with different OS / Hardware combinations.

 Cheers!}

@section{Using Windows, Choosing Host APIs}

Using Portaudio on Windows raises a few extra challenges.  In 
particular, Windows machines generally support a number of different
"Host API"s that Portaudio can use to interact with the machine.
In addition, these Host APIs may also target multiple different
devices. 

The default Host API for windows is MME. My observations suggest
that this API is limited; it can open only a small number of 
simultaneous streams, and the latency for playing sounds is extremely
high. 

The WASAPI API (if that's not redundant) has its own issues; in 
particular, it seems to be necessary to manually set the playback device
to the right sample rate (for rsound, typically 44100Hz) before 
starting DrRacket.  Failing to do so simply results in an "invalid
device" error from Portaudio.

To address these issues, Portaudio 
includes a number of functions used to control the selection
of the host API.

@defproc[(pa-maybe-initialize) void?]{
 Initialize portaudio if it's not already initialized.
 Not necessary for the vast majority of the higher-level
 calls documented below.}

@defproc[(pa-initialized?) boolean]{
 Is portaudio initialized?}

@defproc[(pa-terminate-completely) void?]{
 Call pa-terminate repeatedly until @racket[(pa-initialized?)] 
 returns @racket[false].}

@defproc[(display-device-table) void?]{
 Prints out salient information about the devices (currently)
 available to portaudio.}

@defproc[(default-host-api) symbol?]{
 Returns the default API for the platform.}

@defproc[(all-host-apis) (listof symbol?)]{
 Returns a list of the APIs supported by the platform.}

@defparam[host-api api (or/c false? symbol?)]{
 Controls the choice of API made when opening a stream (including calls
 to s16vec-play and stream-play).}

@defproc[(find-output-device [desired-latency number?]) exact-nonnegative-integer?]{
   Given a latency, finds a device number that uses the current API and has the
   desired latency and two output channels.}

@defproc[(device-low-output-latency [device-number exact-nonnegative-integer?])
          number?]{
 Given a device number, return the "low output latency" associated with that device.}

@section{Playing Sounds}

The first high-level interface involves copying the entire sound
into a malloc'ed buffer, and then playing it.  This is relatively
low-latency. On the other hand, copying the sound involves doubling
the memory required for the sound itself, so it's a bad idea
to call this for sounds that are really big (> 100MB?).

@defproc[(s16vec-play [s16vec s16vector?] 
                      [start-frame nat?]
                      [end-frame nat?]
                      [sample-rate nonnegative-real?])
         (-> void?)]{
 Given an s16vector containing interleaved 16-bit signed integer
 samples, plays the given sound, starting at the given frame
 and ending at the given frame. Returns a thunk that can be used
 to halt the sound, if desired. Play is asynchronous: control
 returns as soon as the sound has started playing.
 
 This function signals an error if start and end frames are
 not ordered and legal.
                     
 Here's an example of a short program that plays a sine wave
 at 426 Hz for 2 seconds:
 
@codeblock|{
#lang racket

(require portaudio
         ffi/vector)

(define pitch 426)

(define sample-rate 44100.0)
(define tpisr (* 2 pi (/ 1.0 sample-rate)))
(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))

(define vec (make-s16vector (* 88200 2)))
(for ([t (in-range 88200)])
  (define sample (real->s16 (* 0.2 (sin (* tpisr t pitch)))))
  (s16vector-set! vec (* 2 t) sample)
  (s16vector-set! vec (add1 (* 2 t)) sample))

(s16vec-play vec 0 88200 sample-rate)
}|}

@section{Playing Streams}

@defproc[(stream-play [buffer-filler (-> buffer-setter? nat? nat? void?)] 
                      [buffer-time nonnegative-real?] 
                      [sample-rate nonnegative-real?])
         (list/c (-> real?) (-> (list-of (list/c symbol? number?)))(-> void?))]{
 Given a buffer-filling callback and a buffer time (in seconds) and a sample
 rate, starts playing a stream that uses the given callback to supply data.
 The buffer-filler receives two arguments: a procedure that can be used
 to mutate the buffer, and the length of the buffer in frames.

 Note that the buffer length may be longer than the specified length, if the
 provided length is too short for the chosen device.
 
 The function returns a list containing three functions: one that queries the
 stream for a time in seconds, one that returns statistics about the stream, 
 and a third that stops the stream.
 
 This function is believed safe; it should not be possible to crash DrRacket
 by using this function badly (unless you exhaust memory by choosing an 
 enormous buffer size).
 
 Here's an example of a program that uses @racket[stream-play] to play a 
 constant pitch of 426 Hz forever:
 
@codeblock|{
#lang racket

(require portaudio)

(define pitch 426)
(define base-frames 0)
(define sample-rate 44100.0)
(define tpisr (* 2 pi (/ 1.0 sample-rate)))
(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))

(define (buffer-filler setter frames) 
  (for ([i (in-range frames)]
        [f (in-range base-frames (+ base-frames frames))])
    (define sample 
      (real->s16 (* 0.2 (sin (* tpisr f pitch)))))
    (setter (* i 2) sample)
    (setter (+ 1 (* i 2)) sample))
  (set! base-frames (+ base-frames frames)))

(match-define (list timer stats stopper)
              (stream-play buffer-filler 0.2 sample-rate))
}|

Note that this example uses a long buffer of 0.2 seconds (= 200 milliseconds) 
so that most GC pauses won't 
interrupt it. 

However, this a latency of 200ms is be pretty
terrible for an interactive system. I usually use 50ms, and just
put up with the occasional miss in return for lower latency.

 }

@defproc[(stream-play/unsafe [buffer-filler (-> cpointer? int? void?)]
                      [buffer-time nonnegative-real?] 
                      [sample-rate nonnegative-real?])
         (list/c (-> real?) (-> void?))]{
 Given a callback and a buffer time (in seconds) and a sample rate,
 starts playing a stream using the given callback to supply data.
 
 The difference is that this function's callback is called with a cpointer,
 rather than a set!-proxy.  This saves the overhead of a function call
 and several checks, but perhaps more importantly allows the use of 
 functions like memcpy and vector-add that can operate at much higher 
 speeds (currently ~5x) than the current vector operations.
 
 }

@section{Recording Sounds}

This library also provides a high-level interface for recording sounds
of a fixed length.

@defproc[(s16vec-record [frame frame?] [frame-rate integer?] [num-channels channels?]) s16vector?]{
 Record a sound of the given number of frames, at the specified frame rate, of the specified
 number of channels. Returns an s16vector containing interleaved samples. Signals an error
 if the default input device does not allow that many channels.}

@section{A Note on Memory, Synchronization, and Concurrency}

@emph{Note: the following is not organized to the high standards of a technical paper.
The Management would like to apologize in advance, and humbly requests
your forgiveness.}

Interacting with sound libraries is tricky. The basic framework for this 
library is what's called a "pull" architecture; the OS makes a call to 
a callback every 5-50ms[*], asking for new data to be shoveled into 
a given buffer. This callback runs on a separate OS thread, which means
that Racket must somehow synchronize with this thread to provide data
when needed.

One difficulty here is that Racket is garbage-collected, with GC pauses
that typically run from 50ms to 100ms. This means that when a program 
is generating garbage, there are simply bound to be hiccoughs in a stream-based
program. In general,
these don't seem to be too awful, and it's often possible to write programs
that generate very little garbage.

After trying several architectures, the model that seems to work the 
best is a shared-memory design, where the callback is written entirely
in C, and takes its data from a buffer shared with Racket. If Racket
has written the data into the buffer, then this routine copies it into
the OS's buffer. If not, then it just zeros out the buffer to play silence.

@subsection{Copying Vs. Streaming}

This package supports two different play interfaces: a "copying" interface
and a "streaming" interface.

The copying interface is simple: Racket stuffs an entire sound into a buffer,
then opens a new stream, providing a callback that pulls samples out of the buffer until it's
done. This means that the sound is not affected by GC pauses or Racket's 
speed. On the other hand, it means duplicating the entire sound (expensive,
for large sounds), and it requires a platform that can support multiple streams
simultaneously. (OS X, yes. Windows, usually no.) Also, it tends to have higher
startup latency (especially on windows), because there's time required to start
a new stream. Finally, it requires pre-rendering of the entire sound, meaning
that interactivity is out.

The streaming interface solves these problems, but exposes more of the grotty
stuff to the programmer.  Rather than providing sound data, the user provides 
a racket callback that can generate sound data on demand. If the given callback
can't keep up with the demand, the stream starts to hiccough.

More specifically, this package uses a ring buffer, whose length can be
specified independently of the underlying machine latency. The Portaudio
engine calls the user's racket callback quite frequently--on the order of 
every 1-5ms--to top up this ring buffer.  When GC pauses occur, the C
callback will drink up everything left in the ring buffer, and then just 
play silence.

Choosing the length of this ring buffer is therefore difficult: too short, and
you'll hear frequent hiccoughs as the C callback runs out of data. Too long, and 
you get high-latency, sluggish response. Times on the order of 50ms seem to be 
an acceptable compromise.

@subsection{Memory}

Shared memory management is a big pain. Racket is garbage-collected, but it's
interacting with an audio library that is not. It's nearly impossible to 
avoid all possible race conditions related to the free-ing of memory.

The first and largest issue is the block of memory shared between the Racket
engine and the C callback. The current setup is that the memory is freed by
a close-stream callback associated with the stream on the Portaudio side. 
The sequence is therefore this: Racket calls CloseStream. Portaudio then stops
calling the callback, and closes the stream. Then, it calls the provided
"all-done" callback, which frees the memory. One note here is that Racket
should probably wrap the pointer in a mutable object so that it can be severed
on the Racket side when the stream is closed. Actually, that's true of the 
stream, as well.


[*] Different platforms are different; currently, this package insists on
a latency of at most 50ms, or it just refuses to run. It appears that all
modern platform can provide this, though it's sometimes a bit tricky to 
decide which output device to use.