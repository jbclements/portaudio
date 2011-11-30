#lang scribble/doc

@(require scribble/manual
          planet/scribble)

@title{@bold{Portaudio}: Bindings for the Portaudio portable sound library}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket))

@defmodule/this-package[main]{This collection provides 
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

 Cheers!}

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

(require (planet clements/portaudio)
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
         (list/c (-> real?) (-> void?))]{
 Given a buffer-filling callback and a buffer time (in seconds) and a sample
 rate, starts playing a stream that uses the given callback to supply data.
 The buffer-filler receives three arguments: a procedure that can be used
 to mutate the buffer, the length of the buffer in frames, and the frame-number
 to start at.
 
 The function returns a list contaning two functions: one that queries the
 stream for a time in seconds, and the other that stops the stream.
 
 This function is believed safe; it should not be possible to crash DrRacket
 by using this function badly (unless you exhaust memory by choosing an 
 enormous buffer size).
 
 Here's an example of a program that uses @racket[stream-play] to play a 
 constant pitch of 426 Hz forever:
 
@codeblock|{
#lang racket

(require (planet clements/portaudio))

(define (buffer-filler setter frames base-frames)
  (define pitch 426)
  (for ([i (in-range frames)]
        [f (in-range base-frames (+ base-frames frames))])
    (define sample 
      (real->s16 (* 0.2 (sin (* tpisr f pitch)))))
    (setter (* i 2) sample)
    (setter (+ 1 (* i 2)) sample)))

(define sample-rate 44100.0)
(define tpisr (* 2 pi (/ 1.0 sample-rate)))
(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))


(match-define (list timer stopper)
              (stream-play buffer-filler 0.1 44100.0))
}|

Note that this example uses a long buffer of 0.1 seconds (= 100 milliseconds) 
so that most GC pauses won't 
interrupt it. 

However, this a latency of 100ms is be pretty
terrible for an interactive system. I usually use 50ms, and just
put up with the occasional miss in return for lower latency.

 }

@defproc[(stream-play/unsafe [buffer-filler (-> cpointer? int? int? void?)]
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

