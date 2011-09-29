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
 [make-streamplay-record (c-> integer? cpointer?)]
 [buffer-if-waiting (c-> cpointer? (or/c false? (list/c cpointer?
                                                        integer?
                                                        integer?
                                                        procedure?)))]
 [streaming-callback cpointer?])

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
   ;; the buffers used by the filling process:
   [buffers (_array _pointer streambufs)]
   ;; the index of the buffer last placed
   ;; in this slot by the filling process:
   [buf-numbers (_array _int streambufs)]
   ;; the index of the buffer last copied
   ;; out by the callback:
   [last-used _int]
   ;; set this int to 1 to halt playback:
   [stop-now _int]
   ;; pointer to an immutable cell, the callback
   ;; sets this to #t when the record is free'd.
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

;; if a buffer needs to be filled, return the info needed to fill it
(define (buffer-if-waiting stream-info)
  (define next-to-be-used (add1 (sound-stream-info-last-used stream-info)))
  (define buf-numbers (sound-stream-info-buf-numbers stream-info))
  (define buffer-index (modulo next-to-be-used streambufs))
  (cond [(= (array-ref buf-numbers buffer-index)
            next-to-be-used)
         ;; already present:
         #f]
        [else (list 
               ;; the pointer to the next buffer:
               (array-ref (sound-stream-info-buffers stream-info)
                          buffer-index)
               ;; the length of the buffer:
               (sound-stream-info-buffer-frames stream-info)
               ;; the index of the next buffer:
               next-to-be-used
               ;; a thunk to use to indicate it's ready:
               (lambda ()
                 (array-set! buf-numbers
                             buffer-index
                             next-to-be-used)))]))

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

