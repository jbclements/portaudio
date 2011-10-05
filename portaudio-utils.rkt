#lang racket

(require ffi/vector
         ffi/unsafe
         (rename-in racket/contract [-> c->])
         racket/runtime-path
         "mzrt-sema.rkt"
         "signalling.rkt"
         "portaudio.rkt")

(define-runtime-path lib "lib/")

;; this module provides an intermediate layer between 
;; the raw C primitives of portaudio and the higher-level
;; functions.  In particular, it's the one that "knows about"
;; the racket-specific C callback code, and provides functions
;; to interact with and manage those callbacks.

;; the tricky thing is managing the resources that are shared
;; between C and Racket.

;; currently, there are two bits that racket doesn't manage,
;; the streaming record and the mzrt_sema. These are both 
;; freed by the finished-callback that's attached to the 
;; stream.  This means that the place that's blocked on 
;; that semaphore had better be destroyed *before* the stream
;; is closed.

(define (frames? n)
  (and (exact-integer? n)
       (<= 0 n)))

(provide/contract 
 ;; make a sndplay record for playing a precomputed sound.
 [make-sndplay-record (c-> s16vector? cpointer?)]
 ;; the raw pointer to the copying callback, for use with
 ;; a sndplay record:
 [copying-callback cpointer?]
 ;; stop a sound, using the thunk stored in the hash table
 [stop-sound (c-> cpointer? void?)]
 ;; make a streamplay record for playing a stream.
 [make-streamplay-record (c-> integer? (list/c cpointer? place?))]
 ;; if a streaming sound needs a buffer, returns the necessary
 ;; info
 [buffer-if-waiting (c-> cpointer? (or/c false? (list/c cpointer?
                                                        integer?
                                                        integer?
                                                        procedure?)))]
 ;; the raw pointer to the streaming callback, for use with a
 ;; streamplay record:
 [streaming-callback cpointer?]
 ;; how many times has a given stream failed (i.e. not had a 
 ;; buffer provided in time by racket)?
 [stream-fails (c-> cpointer? integer?)]
 ;; the free function for a copying callback
 [copying-info-free cpointer?]
 ;; the free function for a streaming callback
 [streaming-info-free cpointer?])

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
(define-cstruct _stream-rec
  ([buffer-frames _int]
   ;; the buffers used by the filling process:
   [buffers (_array _pointer streambufs)]
   ;; the index of the buffer last placed
   ;; in this slot by the filling process:
   [buf-numbers (_array _int streambufs)]
   ;; the index of the buffer last copied
   ;; out by the callback:
   [last-used _int]
   ;; number of faults:
   [fault-count _int]
   ;; a pointer to an immobile cell containing 
   ;; an mzrt-sema, used to signal when data is
   ;; needed
   [buffer-needed-sema _pointer]))


;; how many fails have occurred on the stream?
(define (stream-fails stream-rec)
  (stream-rec-fault-count stream-rec))

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
  sndplay-record)

;; create a fresh streaming-sound-info structure, including
;; four buffers to be used in rendering the sound.
(define (make-streamplay-record buffer-frames)
  ;; will never get freed.... but a few bytes lost should be okay....
  (define mzrt-sema (mzrt-sema-create 0))
  (define info (cast (malloc _stream-rec 'raw)
                     _pointer
                     _stream-rec-pointer))
  (set-stream-rec-buffer-frames! info buffer-frames)
  (for ([i (in-range streambufs)])
    (array-set! (stream-rec-buffers info) 
                i
                (malloc _sint16 (* buffer-frames channels) 'raw))
    (array-set! (stream-rec-buf-numbers info) i -1))
  (set-stream-rec-last-used! info -1)
  (set-stream-rec-fault-count! info 0)
  (set-stream-rec-buffer-needed-sema! info mzrt-sema)
  (define listening-place (mzrt-sema-listener mzrt-sema))
  (hash-set! sound-stopping-table info
             (list (make-semaphore 1)
                   (lambda ()
                     (place-kill listening-place))))
  (list info listening-place))


;; if a buffer needs to be filled, return the info needed to fill it
(define (buffer-if-waiting stream-info)
  (define next-to-be-used (add1 (stream-rec-last-used stream-info)))
  (define buf-numbers (stream-rec-buf-numbers stream-info))
  (define buffer-index (modulo next-to-be-used streambufs))
  (cond [(= (array-ref buf-numbers buffer-index)
            next-to-be-used)
         ;; already present:
         #f]
        [else (list 
               ;; the pointer to the next buffer:
               (array-ref (stream-rec-buffers stream-info)
                          buffer-index)
               ;; the length of the buffer:
               (stream-rec-buffer-frames stream-info)
               ;; the index of the next buffer:
               next-to-be-used
               ;; a thunk to use to indicate it's ready:
               (lambda ()
                 (array-set! buf-numbers
                             buffer-index
                             next-to-be-used)))]))

;; stop a sound
;; EFFECT: stops the stream, unless it's already done.
(define (stop-sound streamplay-info)
  (match (hash-ref sound-stopping-table streamplay-info #f)
    [#f (error 'stop-sound "record had no entry in the stopping table")]
    [(list sema stop-thunk)
     #;(printf "stopping!\n")
     (match (semaphore-try-wait? sema)
       ;; sound has already been stopped
       [#f (void)]
       [#t (stop-thunk)])]))

;; the same pointer may be allocated again, so use hasheq:
(define sound-stopping-table (make-hasheq))

;; FFI OBJECTS FROM THE C CALLBACK LIBRARY


;; the library containing the C copying callbacks
(define callbacks-lib (ffi-lib (build-path lib
                                           (system-library-subpath)
                                           "callbacks")))

(define create-closure/raw
  (get-ffi-obj "createClosure" callbacks-lib
               (_fun _pointer _ulong _pointer -> _copied-sound-info-pointer)))

;; in order to get a raw pointer to pass back to C, we declare 
;; the function pointers as being simple structs:
(define-cstruct _bogus-struct
  ([datum _uint16]))

(define copying-callback
  (cast
   (get-ffi-obj "copyingCallback" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-callback))

(define streaming-callback
  (cast
   (get-ffi-obj "streamingCallback" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-callback))

(define copying-info-free
  (cast
   (get-ffi-obj "freeClosure" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-finished-callback))

(define streaming-info-free
  (cast
   (get-ffi-obj "freeStreamingInfo" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-finished-callback))


