#lang racket/base

(require ffi/vector
         ffi/unsafe
         (rename-in racket/contract [-> c->])
         racket/runtime-path
         racket/place
         "mzrt-sema.rkt"
         "signalling.rkt"
         "portaudio.rkt"
         (only-in racket/match match-define))

(define-runtime-path lib "lib/")

;; this module provides an intermediate layer between 
;; the raw C primitives of portaudio and the higher-level
;; functions.  In particular, it's the one that "knows about"
;; the racket-specific C callback code, and provides functions
;; to interact with and manage those callbacks.

;; the tricky thing is managing the resources that are shared
;; between C and Racket.

(define (frames? n)
  (and (exact-integer? n)
       (<= 0 n)))

(define nat? exact-nonnegative-integer?)
(define false? not)

(provide/contract 
 ;; make a sndplay record for playing a precomputed sound.
 [make-copying-info (c-> s16vector? nat? (or/c false? nat?) cpointer?)]
 ;; the raw pointer to the copying callback, for use with
 ;; a sndplay record:
 [copying-callback cpointer?]
 ;; the free function for a copying callback
 [copying-info-free cpointer?]
 ;; make a streamplay record for playing a stream.
 [make-streaming-info (c-> integer? cpointer?)]
 ;; if a streaming sound needs a buffer, returns the necessary
 #;[buffer-if-waiting (c-> cpointer? (or/c false? (list/c cpointer?
                                                        integer?
                                                        integer?
                                                        procedure?)))]
 ;; the raw pointer to the streaming callback, for use with a
 ;; streamplay record:
 [streaming-callback cpointer?]
 ;; how many times has a given stream failed (i.e. not had a 
 ;; buffer provided in time by racket)?
 [stream-fails (c-> cpointer? integer?)]
 ;; the free function for a streaming callback
 [streaming-info-free cpointer?])

;; providing these for test cases only:
(provide stream-rec-buffer
         stream-rec-last-frame-read
         set-stream-rec-last-frame-written!
         set-stream-rec-last-offset-written!
         )

;; all of these functions assume 2-channel-interleaved 16-bit input:
(define channels 2)
(define s16max 32767)
(define s16-bytes 2)

;; COPYING CALLBACK STRUCT
(define-cstruct _copying-rec
  ([sound         _pointer]
   [cur-sample    _ulong]
   [num-samples   _ulong]))

;; create a fresh copying-rec structure, including a full
;; malloc'ed copy of the sound data. No sanity checking of start
;; & stop is done.
(define (make-copying-info s16vec start-frame maybe-stop-frame)
  (define stop-frame (or maybe-stop-frame
                         (/ (s16vector-length s16vec) channels)))
  (define frames-to-copy (- stop-frame start-frame))
  ;; do this allocation first: it's much bigger, and more likely to fail:
  (define copied-sound (dll-malloc (* (ctype-sizeof _sint16) (* channels frames-to-copy))))
  (define src-ptr (ptr-add (s16vector->cpointer s16vec)
                           (* channels start-frame)
                           _sint16))
  (memcpy copied-sound src-ptr (* channels frames-to-copy) _sint16)
  (define copying-info (cast (dll-malloc (ctype-sizeof _copying-rec))
                             _pointer
                             _copying-rec-pointer))
  (set-copying-rec-sound! copying-info copied-sound)
  (set-copying-rec-cur-sample! copying-info 0)
  (set-copying-rec-num-samples! copying-info (* frames-to-copy channels))
  copying-info)

;; STREAMING CALLBACK STRUCT

(define-cstruct _stream-rec
  (;; the number of frames in the circular buffer
   [buffer-frames _int]
   ;; the circular buffer
   [buffer _pointer]
   ;; the last frame read by the callback
   [last-frame-read _uint]
   ;; the offset of the last byte read by the callback.
   [last-offset-read _uint]
   ;; the last frame written by Racket
   [last-frame-written _uint]
   ;; the offset of the last byte written by Racket.
   [last-offset-written _uint]
   ;; number of faults:
   [fault-count _int]
   ;; a pointer to a 4-byte cell; when it's nonzero,
   ;; the supplying procedure should shut down, and
   ;; free this cell. If it doesn't get freed, well,
   ;; that's four bytes wasted forever.
   [all-done _pointer]))


;; how many fails have occurred on the stream?
(define (stream-fails stream-rec)
  (stream-rec-fault-count stream-rec))

;; create a fresh streaming-sound-info structure, including
;; four buffers to be used in rendering the sound.
(define (make-streaming-info buffer-frames)
  ;; we must use the malloc defined in the dll here, to
  ;; avoid hideous windows unpleasantness.
  (define info (cast (dll-malloc (ctype-sizeof _stream-rec))
                     _pointer
                     _stream-rec-pointer))
  (set-stream-rec-buffer-frames! info buffer-frames)
  (set-stream-rec-buffer! info (dll-malloc (* (ctype-sizeof _sint16)
                                              channels
                                              buffer-frames)))
  (set-stream-rec-last-frame-read! info 0)
  (set-stream-rec-last-offset-read! info 0)
  (set-stream-rec-last-frame-written! info 0)
  (set-stream-rec-last-offset-written! info 0)
  (set-stream-rec-fault-count! info 0)
  (define all-done-cell (malloc 'raw 4))
  (ptr-set! all-done-cell _uint32 0)
  (set-stream-rec-all-done! info all-done-cell)
  info)


;; if a buffer needs to be filled, return the info needed to fill it
#;(define (buffer-if-waiting stream-info)
  (define next-to-be-used (add1 (stream-rec-last-used stream-info)))
  (define buf-numbers (stream-rec-buf-numbers stream-info))
  (define buffer-index (modulo next-to-be-used streambufs))
  (cond [(= (hack-array-ref-2 buf-numbers buffer-index)
            next-to-be-used)
         ;; already present:
         #f]
        [else (list 
               ;; the pointer to the next buffer:
               (hack-array-ref (stream-rec-buffers stream-info)
                               buffer-index)
               ;; the length of the buffer:
               (stream-rec-buffer-frames stream-info)
               ;; the index of the next buffer:
               next-to-be-used
               ;; a thunk to use to indicate it's ready:
               (lambda ()
                 ;; CAN'T DO THIS IN 5.1.3, USE HACK:
                 #;(array-set! buf-numbers
                             buffer-index
                             next-to-be-used)
                 ;; HERE'S THE HACK:
                 (define updater!
                   (case buffer-index
                     [(0) set-array-hack-2-a!]
                     [(1) set-array-hack-2-b!]
                     [(2) set-array-hack-2-c!]
                     [(3) set-array-hack-2-d!]))
                 (updater! buf-numbers next-to-be-used)))]))

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

(define streaming-callback
  (cast
   (get-ffi-obj "streamingCallback" callbacks-lib _bogus-struct)
   _bogus-struct-pointer
   _pa-stream-callback))

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


