#lang racket

(require ffi/vector
         ffi/unsafe
         (rename-in racket/contract [-> c->])
         racket/runtime-path)

(define-runtime-path lib "lib/")

(define (frames? n)
  (and (exact-integer? n)
       (<= 0 n)))



(provide/contract 
 [make-sndplay-record (c-> s16vector? cpointer?)]
 [copying-callback cpointer?]
 [stop-sound (c-> cpointer? void?)]
 [make-streamplay-record (c-> procedure? cpointer?)]
 #;[streaming-callback cpointer?])

;; all of these functions assume 2-channel-interleaved 16-bit input:
(define channels 2)
(define s16max 32767)
(define s16-bytes 2)

;; COPYING CALLBACK STRUCT
(define-cstruct _copied-sound-info
  ([sound         _pointer]
   [curSample     _ulong]
   [numSamples    _ulong]
   [stop-now      _bool]
   [stop-sema-ptr _pointer]))

;; STREAMING CALLBACK STRUCT
(define streambufs 4)
(define-cstruct _sound-stream-info
  ([buffer-frames _int]
   [buffers (_array _pointer streambufs)]
   [buf-numbers (_array _int streambufs)]
   [last-used _int]
   [stop-now _int]
   [already-stopped? _pointer]))

#|typedef struct soundStreamInfo{
  int   bufferFrames;
  short *buffers[STREAMBUFS];
  // mutated by racket only:
  int   bufNumbers[STREAMBUFS];
  // mutated by callback only:
  int   lastUsed;
  int   stopNow;
  Scheme_Object **stoppedPtr;
} soundStreamInfo;
|#

;; create a fresh copied-sound-info structure, including a full
;; malloc'ed copy of the sound data
(define (make-sndplay-record s16vec)
  ;; will never get freed.... but 4 bytes lost should be okay.
  (define already-freed? (malloc-immobile-cell #f))
  (define sndplay-record
    (create-closure/raw (s16vector->cpointer s16vec) 
                        (s16vector-length s16vec)
                        already-freed?))
  (unless sndplay-record
    (error 'make-sndplay-record
           "failed to allocate space for ~s samples."
           (s16vector-length s16vec)))
  (hash-set! sound-stopping-table sndplay-record 
             (list already-freed? (make-semaphore 1)
                   (lambda ()
                     (set-copied-sound-info-stop-now! sndplay-record #t))))
  sndplay-record)

;; create a fresh streaming-sound-info structure, including
;; four buffers to be used in rendering the sound.
(define (make-streamplay-record buffer-frames)
  ;; will never get freed.... but 4 bytes lost should be okay.
  (define already-freed? (malloc-immobile-cell #f))
  (define streamplay-record
    (create-streamplay-record/raw 
     buffer-frames
     already-freed?))
  (unless streamplay-record
    (error 'make-streamplay-record
           "failed to allocate space for streamplay buffer, ~sx~s frames."
           streambufs
           buffer-frames))
  (hash-set! sound-stopping-table streamplay-record
             (list already-freed? (make-semaphore 1)
                   (lambda ()
                     (set-sound-stream-info-stop-now! 
                      streamplay-record
                      #t))))
  streamplay-record)

;; stop a sound
;; EFFECT: stops the sound *and frees the sndplay-record*, unless
;; it's already done.
(define (stop-sound sndplay-record)
  (match (hash-ref sound-stopping-table sndplay-record #f)
    [#f (error 'stop-sound "record had no entry in the stopping table")]
    [(list already-freed? sema stop-thunk)
     (match (semaphore-try-wait? sema)
       ;; sound has already been stopped
       [#f (void)]
       [#t 
        ;; for debugging only:
        #;(printf "already freed: ~s\n"
                  (ptr-ref already-freed? _scheme))        
        (unless (ptr-ref already-freed? _scheme)
          (stop-thunk))])]))


;; FFI OBJECTS FROM THE C CALLBACK LIBRARY


;; the library containing the C copying callbacks
(define copying-callbacks-lib (ffi-lib (build-path lib
                                                   (system-library-subpath)
                                                   "copying-callbacks")))

(define create-closure/raw
  (get-ffi-obj "createClosure" copying-callbacks-lib
               (_fun _pointer _ulong _pointer -> _copied-sound-info-pointer)))

(define create-streamplay-record/raw
  (get-ffi-obj "createSoundStreamInfo" copying-callbacks-lib
               (_fun _int _pointer -> _sound-stream-info-pointer)))

;; in order to get a raw pointer to pass back to C, we declare 
;; the function pointers as being simple structs:
(define-cstruct _bogus-struct
  ([datum _uint16]))

(define copying-callback
  (get-ffi-obj "copyingCallback"   copying-callbacks-lib _bogus-struct))

(define streaming-callback
  (get-ffi-obj "streamingCallback" copying-callbacks-lib _bogus-struct))

;; DON'T IMPORT THE FREE FUNCTIONS... they should only be called by C.



(define sound-stopping-table (make-weak-hash))


#|
;; create a callback that creates frames by calling a signal repeatedly.
;; note that we don't bother checking to see whether the buffer is successfully
;; filled.
(define (make-generating-callback signal buffer-frames response-channel abort-box)
  (define signal-exn-box (box #f))
  (define-values (filled-buffer semaphore)
    (start-filler-thread signal buffer-frames signal-exn-box abort-box))
  (define buffer-samples (* buffer-frames channels))
  ;; allow the first buffer-fill to happen:
  (semaphore-post semaphore)
  (define callback-holding-box (box #f))
  (define (the-callback input output frame-count time-info status-flags user-data)
    ;; the following code is believed not to be able to raise any errors.
    ;; if this is wrong, racket will die with abort().
    
    ;; don't run if abort is set:
    (cond [(unbox abort-box)
           (define response
             (or (unbox signal-exn-box) 'finished))
           (channel-put/async response-channel response)
           (set-box! callback-holding-box #f)
           'pa-abort]
          ;; don't run if we get the wrong number of frames requested:
          [(not (= frame-count buffer-frames))
           (channel-put/async
            response-channel
            (exn:fail
             (format
              "make-generating-callback: callback wanted ~s frames instead of expected ~s." 
              frame-count
              buffer-frames)
             (current-continuation-marks)))
           (set-box! callback-holding-box #f)
           'pa-abort]
          ;; otherwise, copy and release the semaphore to generate again.
          [else
           (memcpy output
                   0
                   filled-buffer
                   0
                   buffer-samples
                   _sint16)
           (semaphore-post semaphore)
           'pa-continue]))
  (set-box! callback-holding-box the-callback)
  the-callback)


;; this thread is not run as a callback. That way, if the user's signal
;; misbehaves, it won't destroy the audio engine. It fills 
;; the output buffer once for each post to the semaphore.
(define (start-filler-thread signal buffer-frames signal-exn-box abort-box)
  (define signal-semaphore (make-semaphore))
  (define buffer (make-s16vector (* channels buffer-frames)))
  (define cpointer (s16vector->cpointer buffer))
  (define frame-offset 0)
  (thread
   (lambda ()
     (let loop ()
       (semaphore-wait signal-semaphore)
       (with-handlers ([(lambda (exn) #t)
                        (lambda (exn)
                          (set-box! signal-exn-box exn)
                          (set-box! abort-box #t)
                          ;; fall off the end of the thread:
                          (void))])
         (for ([t (in-range frame-offset (+ frame-offset buffer-frames))]
               [i (in-range 0 (* 2 buffer-frames) 2)])
           (define sample 
             (inexact->exact 
              (round (* s16max (min 1.0 (max -1.0 (signal t)))))))
           (ptr-set! cpointer _sint16 i sample)
           (ptr-set! cpointer _sint16 (+ i 1) sample))
         (set! frame-offset (+ frame-offset buffer-frames))
         (loop)))))
  (values cpointer signal-semaphore))

;; create a callback that creates frames by passing a cblock to a function
(define (make-block-generating-callback signal/block/s16 response-channel abort-box)
  (define frame-offset 0)
  (define callback-holding-box (box #f))
  (define (the-callback input output frame-count time-info status-flags user-data)
    (cond [(unbox abort-box) 'pa-abort]
          ;; MUST NOT ALLOW AN EXCEPTION TO ESCAPE.
          [else (with-handlers ([(lambda (exn) #t)
                                 (lambda (exn)
                                   (channel-put/async response-channel exn)
                                   'pa-abort)])
                  (define keep-running? 
                    (signal/block/s16 output frame-offset frame-count))
                  (set! frame-offset (+ frame-offset frame-count))
                  (cond [keep-running? 'pa-continue]
                        [else (channel-put/async response-channel 'finished)
                              (set-box! callback-holding-box #f)
                              'pa-complete]))]))
  (set-box! callback-holding-box the-callback)
  the-callback)

;; we need to hold on to the callbacks, so they don't get collected while
;; the streams are still being processed.
(define callback-boxes (list))


|#