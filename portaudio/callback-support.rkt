#lang racket/base

(require ffi/vector
         ffi/unsafe
         (rename-in racket/contract [-> c->])
         racket/runtime-path
         "portaudio.rkt"
         (only-in racket/match match-define match))

(define-runtime-path lib "lib/")

;; this module provides an intermediate layer between 
;; the raw C primitives of portaudio and the higher-level
;; functions.  In particular, it's the one that "knows about"
;; the racket-specific C callback code, and provides functions
;; to interact with and manage those callbacks.

;; one of the tricky things is managing the resources that are shared
;; between C and Racket.

;; there are two sets of functions here; one for use in playing
;; a single sound from an existing buffer, and one for use in 
;; playing streaming audio. The nice thing about single sounds
;; is that they keep playing even through GC pauses. The bad thing
;; about single sounds is that you have to have them in memory 
;; before playing them, and you can't synchronize them accurately
;; (e.g., play one after the other so they sound seamless).

;; all of these functions except s16vec-record assume 
;; 2-channel-interleaved 16-bit input:
(define CHANNELS 2)
(define SAMPLE-BITS 16)
(define s16max (sub1 (expt 2 (sub1 SAMPLE-BITS))))
(define SAMPLE-BYTES (/ SAMPLE-BITS 8))


(provide
 (contract-out
  ;; make a sndplay record for playing a precomputed sound.
  [make-copying-info (c-> s16vector? frame? (or/c false? frame?) cpointer?)]
  ;; the raw pointer to the copying callback, for use with
  ;; a sndplay record:
  [copying-callback cpointer?]
  ;; the free function for a copying callback
  [copying-info-free cpointer?]
  ;; the free function callable from racket
  
  ;; make a sndplay record for recording a precomputed sound.
  [make-copying-info/rec (c-> frame? nat? cpointer?)]
  ;; the raw pointer to the copying callback, for use with
  ;; a sndplay record:
  [copying-callback/rec cpointer?]
  ;; produce an s16vector from the given copying-info
  [extract-recorded-sound (c-> cpointer? s16vector?)]
  
  ;; make a streamplay record for playing a stream.
  [make-streaming-info (c-> frame? (list/c cpointer? cpointer?))]
  ;; is the stream all done?
  [all-done? (c-> cpointer? boolean?)]
  ;; call the given procedure with the buffers to be filled:
  [call-buffer-filler (c-> cpointer? procedure? any)]
  ;; the raw pointer to the streaming callback, for use with a
  ;; streamplay record:
  [streaming-callback cpointer?]
  ;; how many times has a given stream failed (i.e. not had a 
  ;; buffer provided in time by racket)?
  [stream-fails (c-> cpointer? integer?)]
  ;; the free function for a streaming callback
  [streaming-info-free cpointer?]
  
  ;; is this a legitimate argument for a 'frames' ?
  [frame? (c-> any/c boolean?)]
  
  [CHANNELS nat?]))

;; check the correspondence between C types and racket types.
(define correspondences
  (list (list _uint 'int)
        (list _sint16 'short)
        (list _ulong 'long)))

;; check that they match. If they don't, then the Racket code
;; and the C code won't agree about the sizes of fields that
;; they're sharing. Core dumps ensue?
(for ([c correspondences])
  (unless (= (ctype-sizeof (car c))
             (compiler-sizeof (cadr c)))
    (error 'type-correspondence 
           "ctype ~v and C type ~v don't have the same size"
           (car c)
           (cadr c))))

(unless (= 2 (compiler-sizeof 'short))
  (error 'type-correspondence "compiler's 'short' is not 16 bits."))


;; thinking about overflow: we're interacting with C code here, and
;; bad things will happen if our indexes overflow. Let's set some
;; "plausibility" limits.
;; MAXIMUM PLAUSIBLE # OF CHANNELS: 8.
(define MINIMUM-PLAUSIBLE-CHANNELS 1)
(define MAXIMUM-PLAUSIBLE-CHANNELS 8)
;; MAXIMUM SOUND BUFFER: 1 Gigabyte (or, more precisely, 1 Gibibyte).
(define MAXIMUM-BUFSIZE (expt 2 30))
(define MINIMUM-FRAMESIZE (* MINIMUM-PLAUSIBLE-CHANNELS
                                       SAMPLE-BYTES))
(define MAXIMUM-FRAMES (/ MAXIMUM-BUFSIZE
                                    MINIMUM-FRAMESIZE))

;; to represent frame counts and buffer offsets, we're going
;; to use unsigned longs:
(define MAXIMUM-ULONG (expt 2 (* 8 (ctype-sizeof _ulong))))

;; make sure that a _ulong can represent a number of frames:
(unless (<= MAXIMUM-FRAMES MAXIMUM-ULONG)
  (error "ulong is not large enough to handle the longest possible sound"))
;; make sure that a _ulong can represent a sample offset:
(unless (<= MAXIMUM-BUFSIZE MAXIMUM-ULONG)
  (error "ulong is not large enough to handle the longest possible sound"))

;; we should be able to handle sounds at least this long:
(define MAXIMUM-SOUND-LENGTH-SECONDS 3600)
(define MAXIMUM-PLAUSIBLE-SAMPLE-RATE 96000)

(unless (<= (* MAXIMUM-SOUND-LENGTH-SECONDS MAXIMUM-PLAUSIBLE-SAMPLE-RATE)
            MAXIMUM-FRAMES)
  (error "can't accommodate long enough sounds"))

;; looks like we're okay. If we want to bump the sample size up to 4 or 
;; 8, we'll need to 


(unless (integer? SAMPLE-BYTES)
  (error 'portaudio "SAMPLE-BITS must be divisible by 8"))

(define (frame? n)
  (and (exact-nonnegative-integer? n)
       (<= n MAXIMUM-FRAMES)))

(define nat? exact-nonnegative-integer?)
(define false? not)

;; providing these for test cases only:
(provide stream-rec-buffer
         stream-rec-buffer-frames
         stream-rec-last-frame-read
         set-stream-rec-last-frame-written!
         set-stream-rec-last-offset-written!
         )


(define (frames->bytes f channels) (* channels (samples->bytes f)))
;; this should never be a non-integer. Typed racket would help here.
(define (bytes->frames b channels) (/ b (* channels SAMPLE-BYTES)))
(define (samples->bytes f) (* SAMPLE-BYTES f))

;; COPYING CALLBACK STRUCT ... we can use this for recording, too.
(define-cstruct _copying
  ([sound         _pointer]
   [cur-sample    _ulong]
   [num-samples   _ulong]
   ;; not yet used...
   [channels      _uint]))

;; create a fresh copying structure, including a full
;; malloc'ed copy of the sound data. No sanity checking of start
;; & stop is done.
(define (make-copying-info s16vec start-frame maybe-stop-frame)
  (define stop-frame (or maybe-stop-frame
                         (/ (s16vector-length s16vec) CHANNELS)))
  (define frames-to-copy (- stop-frame start-frame))
  ;; do this allocation first: it's much bigger, and more likely to fail:
  (define copied-sound (dll-malloc (frames->bytes frames-to-copy
                                                  CHANNELS)))
  (define src-ptr (ptr-add (s16vector->cpointer s16vec)
                           (frames->bytes start-frame
                                          CHANNELS)))
  (memcpy copied-sound src-ptr (frames->bytes frames-to-copy
                                              CHANNELS))
  (define copying (cast (dll-malloc (ctype-sizeof _copying))
                             _pointer
                             _copying-pointer))
  (set-copying-sound! copying copied-sound)
  (set-copying-cur-sample! copying 0)
  (set-copying-num-samples! copying (* frames-to-copy CHANNELS))
  (set-copying-channels! copying CHANNELS)
  copying)

(define (make-copying-info/rec frames channels)
  ;; this restriction is only here because extract-recorded-sound
  ;; will (currently) fail for more than 2 channels.
  (unless (or (= channels 1)
              (= channels 2))
    (raise-argument-error 'make-copying-info/rec
                          "number in {1,2}"
                          1 frames channels))
  ;; do this allocation first: it's much bigger, and more likely to fail:
  (define record-buffer (dll-malloc (frames->bytes frames channels)))
  (define copying (cast (dll-malloc (ctype-sizeof _copying))
                             _pointer
                             _copying-pointer))
  (set-copying-sound! copying record-buffer)
  (set-copying-cur-sample! copying 0)
  (set-copying-num-samples! copying (* frames channels))
  (set-copying-channels! copying channels)
  copying)

;; pull the recorded sound out of a copying structure.  This function
;; does not guarantee that the sound has been completely recorded yet.
(define (extract-recorded-sound copying)
  (define num-in-samples (copying-num-samples copying))
  (unless (= CHANNELS 2)
    (error 'extract-recorded-sound 
           "internal error---time to edit this code 201410030924"))
  (define recorded-channels (copying-channels copying))
  ;; it would save memory in the case of monaural recording to 
  ;; use unsafe operations to dupliate the bits as they're being
  ;; copied, but I think the added chance of core dumps is
  ;; not worth the memory savings.
  (define num-out-samples num-in-samples)
  (define s16vec (make-s16vector num-in-samples))
  (define src-ptr (copying-sound copying))
  (define dst-ptr (s16vector->cpointer s16vec))
  (memcpy dst-ptr src-ptr (samples->bytes num-out-samples))
  (match recorded-channels
    [2 s16vec]
    [1 (duplicate-channels s16vec)]))

;; ... how to make sure that it doesn't get freed before it's copied out?


;; turn mono into stereo by duplicating every sample, e.g.
;; 0.4 0.2 0.8 -0.2 -> 0.4 0.4 0.2 0.2 0.8 0.8 -0.2 -0.2
(define (duplicate-channels s16vec)
  (define result (make-s16vector (* 2 (s16vector-length s16vec))))
  (for ([i (s16vector-length s16vec)])
    (define sample (s16vector-ref s16vec i))
    (s16vector-set! result (* 2 i)        sample)
    (s16vector-set! result (add1 (* 2 i)) sample))
  result)



;; STREAMING CALLBACK STRUCT

;; NOTE: CHANGES HERE MUST BE ECHOED IN CALLBACKS.C
;; *AND* IN TEST-STREAMING-CALLBACK.
(define-cstruct _stream-rec
  (;; the number of frames in the circular buffer
   [buffer-frames _ulong]
   ;; the circular buffer
   [buffer _pointer]
   
   ;; mutated only by the C side:
   ;; the last frame read by the callback
   [last-frame-read _ulong]
   ;; the offset of the last byte read by the callback.
   [last-offset-read _ulong]
   ;; number of faults:
   [fault-count _uint]
   ;; a pointer to a 4-byte cell; when it's nonzero,
   ;; the supplying procedure should shut down, and
   ;; free this cell. If it doesn't get freed, well,
   ;; that's four bytes wasted until the next store-prompt.
   [all-done _pointer]
   
   ;; mutated only by Racket:
   ;; the last frame written by Racket
   [last-frame-written _ulong]
   ;; the offset of the last byte written by Racket.
   [last-offset-written _ulong]
))


;; how many fails have occurred on the stream?
(define (stream-fails stream-rec)
  (stream-rec-fault-count stream-rec))

;; create a fresh streaming-sound-info structure, including
;; a ring buffer to be used in rendering the sound.
(define (make-streaming-info buffer-frames)
  ;; we must use the malloc defined in the dll here, to
  ;; keep windows happy.
  (define info (cast (dll-malloc (ctype-sizeof _stream-rec))
                     _pointer
                     _stream-rec-pointer))
  (set-stream-rec-buffer-frames! info buffer-frames)
  (set-stream-rec-buffer! info (dll-malloc (frames->bytes buffer-frames
                                                          CHANNELS)))
  (set-stream-rec-last-frame-read! info 0)
  (set-stream-rec-last-offset-read! info 0)
  (set-stream-rec-last-frame-written! info 0)
  (set-stream-rec-last-offset-written! info 0)
  (set-stream-rec-fault-count! info 0)
  (define all-done-cell (malloc 'raw 4))
  (ptr-set! all-done-cell _uint32 0)
  (set-stream-rec-all-done! info all-done-cell)
  (list info all-done-cell))

;; given an all-done? cell, check whether it's nonzero.
;; be careful to call this with an all-done? cell, and not
;; just the stream-rec pointer that points to it, or you'll
;; get an immediate true.
(define (all-done? all-done-ptr)
  (not (= (ptr-ref all-done-ptr _uint32) 0)))

;; given a stream-rec and a buffer-filler, call the 
;; buffer filler twice: once to fill to the end of the buffer, and once 
;; to fill the beginning of the buffer up to the last-read point.
;; I'm ignoring the race conditions here; I believe the worst-case
;; is audible glitches, and we'll see how common they are.
(define (call-buffer-filler stream-info filler)
  (define buffer (stream-rec-buffer stream-info))
  (define buffer-frames (stream-rec-buffer-frames stream-info))
  (define buffer-bytes (frames->bytes buffer-frames
                                      CHANNELS))

  ;; the potential race condition here has no "major" bad effects, I believe:
  (define last-frame-read (stream-rec-last-frame-read stream-info))
  (define last-offset-read (stream-rec-last-offset-read stream-info))
  ;; safe to write ahead up to wraparound of last point read:
  (define last-frame-to-write (+ last-frame-read buffer-frames))
  (define last-offset-to-write last-offset-read)
  
  ;; start at last-written or last-read, whichever is later.
  (define last-frame-written (stream-rec-last-frame-written stream-info))
  (define last-offset-written (stream-rec-last-offset-written stream-info))
  (define underflow? (< last-frame-written last-frame-read))
  (define first-frame-to-write (cond [underflow? last-frame-read]
                                     [else       last-frame-written]))
  (define first-offset-to-write (cond [underflow? last-offset-read]
                                      [else       last-offset-written]))

  (unless (= first-frame-to-write last-frame-to-write)
    ;; do we have to wrap around?
    (cond [(<= last-offset-to-write first-offset-to-write)
           (define frames-to-end 
             (bytes->frames (- buffer-bytes first-offset-to-write)
                            CHANNELS))
           (filler (ptr-add buffer first-offset-to-write)
                   frames-to-end)
           (filler buffer
                   (bytes->frames last-offset-to-write
                                  CHANNELS))]
          [else
           (filler (ptr-add buffer first-offset-to-write)
                   (- last-frame-to-write first-frame-to-write))])
    ;; update the stream-rec
    (set-stream-rec-last-frame-written! stream-info last-frame-to-write)
    (set-stream-rec-last-offset-written! stream-info last-offset-to-write)))

;; FFI OBJECTS FROM THE C CALLBACK LIBRARY


;; the library containing the C copying callbacks
(define callbacks-lib (ffi-lib (build-path lib
                                           (system-library-subpath)
                                           "callbacks")))

;; in order to get a raw pointer to pass back to C, we declare 
;; the function pointers as being simple structs:
(define-cstruct _bogus-struct
  ([datum _uint16]))

(define copying-callback
  (cast
   (get-ffi-obj "copyingCallback" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-callback))

;; the callback for recording sounds
(define copying-callback/rec
  (cast
   (get-ffi-obj "copyingCallbackRec" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-callback))

(define streaming-callback
  (cast
   (get-ffi-obj "streamingCallback" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-callback))

(define copying-info-free-fn
  (get-ffi-obj "freeCopyingInfo" callbacks-lib 
               (_fun _pointer -> _void)))

;; the copying-free function pointer as a cpointer
(define copying-info-free
  (cast
   (get-ffi-obj "freeCopyingInfo" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-finished-callback))

(define streaming-info-free
  (cast
   (get-ffi-obj "freeStreamingInfo" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-finished-callback))

(define dll-malloc
  (get-ffi-obj "dll_malloc" callbacks-lib (_fun _uint -> _pointer)))




(module+ test
  (require rackunit)
  
  (define (random-s16)
    (- (random #xffff) #x8000))

  (define copying-callback
    (get-ffi-obj "copyingCallback" callbacks-lib 
                 (_fun _pointer _pointer _ulong
                       _pointer _pa-stream-callback-flags
                       _copying-pointer
                       -> _int)))
  (define copying-callback-rec
    (get-ffi-obj "copyingCallbackRec" callbacks-lib 
                 (_fun _pointer _pointer _ulong
                       _pointer _pa-stream-callback-flags
                       _copying-pointer
                       -> _int)))
  
  (provide the-test-suite)
  (define the-test-suite
    (test-suite 
     "copying callbacks"
     (let ()
       
       ;; create a bogus source vector with noise:
       (define src-vec (make-s16vector 2048))
       (for ([i (in-range 2048)])
         (s16vector-set! src-vec i (random-s16)))
       
       (define offset-frame 436)
       (define offset-sample (* 2 offset-frame))
       (define remaining-samples (- 2048 offset-sample))
       
       ;; create a copying info, make sure it's correct:
       (define copying (make-copying-info src-vec offset-frame #f))
       (check-equal? (copying-cur-sample copying) 0)
       (check-equal? (copying-num-samples copying) (- 2048 offset-sample))
       (define copied-buf-ptr (copying-sound copying))
       (for ([i (in-range remaining-samples)])
         (check-equal? (s16vector-ref src-vec (+ i offset-sample))
                       (ptr-ref copied-buf-ptr _sint16 i)))
       
       
       ;; use the copying-callback to put it into another buffer.
       
       ;; target buffer:
       (define dst-ptr (malloc _sint16 1024))
       
       
       
       (copying-callback #f dst-ptr 512 #f '() copying)
       (for ([i (in-range 1024)])
         (check-equal? (s16vector-ref src-vec (+ i offset-sample))
                       (ptr-ref dst-ptr _sint16 i)))
       (check-equal? (copying-cur-sample copying) 1024)
       
       (copying-callback #f dst-ptr 512 #f '() copying)
       (for ([i (in-range (* 2 (- 512 offset-frame)))])
         (check-equal? (s16vector-ref src-vec (+ i 1024 offset-sample))
                       (ptr-ref dst-ptr _sint16 i)))
       (for ([i (in-range offset-frame)])
         (check-equal? (ptr-ref dst-ptr _sint16 (+ i (- 1024 (* 2 offset-frame))))
                       0))
       (check-equal? (copying-cur-sample copying) (- 2048 (* 2 offset-frame)))
       )
     
     (let ()
       ;; try again with recording callback:
       
       ;; create a bogus source vector with noise:
       (define src-vec (make-s16vector 2048))
       (for ([i (in-range 2048)])
         (s16vector-set! src-vec i (random-s16)))
       (check-equal? (ptr-ref (cast src-vec _s16vector _pointer) _sint16 17)
                     (s16vector-ref src-vec 17))
       
       (define offset-frame 436)
       (define offset-sample (* 2 offset-frame))
       (define remaining-samples (- 2048 offset-sample))
       
       ;; create a copying info, make sure it's correct:
       (define copying (make-copying-info/rec (/ remaining-samples 2) 2))
       (check-equal? (copying-cur-sample copying) 0)
       (check-equal? (copying-num-samples copying) (- 2048 offset-sample))
       (check-equal? (copying-channels copying) 2)
       
       ;; use the copying-callback to put it into another buffer.
       
       
       (copying-callback-rec (cast src-vec
                                   _s16vector
                                   _pointer)
                             #f 512 #f '() copying)
       (check-equal? (copying-cur-sample copying) 1024)
       
       (copying-callback-rec (ptr-add
                              (cast src-vec
                                    _s16vector
                                    _pointer)
                              2048)
                             #f 512 #f '() copying)
       (check-equal? (copying-cur-sample copying) (- 2048 offset-sample))
       
       (define dst-ptr (copying-sound copying))
       (for ([i (in-range (* 2 (- 1024 offset-frame)))])
         (check-equal? (s16vector-ref src-vec i)
                       (ptr-ref dst-ptr _sint16 i)))
       
       (define result (extract-recorded-sound copying))
       (for ([i (in-range (* 2 (- 1024 offset-frame)))])
         (check-equal? (s16vector-ref src-vec i)
                       (s16vector-ref result i)))
       
       (define testvec (list->s16vector '(3 28 1 2 9)))
       (check-equal? (s16vector->list (duplicate-channels testvec))
                     '(3 3 28 28 1 1 2 2 9 9))
       
       ))))
