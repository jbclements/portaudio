#lang racket

(require ffi/unsafe
         ffi/vector
         rackunit
         rackunit/text-ui
         racket/runtime-path
         "../portaudio.rkt"
         "../portaudio-utils.rkt"
         "helpers.rkt")

(define-runtime-path libs "../lib")

(define feeder-lib (ffi-lib (build-path libs "copying-callbacks.dylib")))

feeder-lib

(define src-buf (make-s16vector 800 03))
;; fill with rands between 0 & 99:
(for ([i (in-range 800)])
  (s16vector-set! src-buf i (random 100)))

(define tgt-buf (make-s16vector 500 0))

(define _my-pa-stream-callback
  (_fun #:atomic? #t
        #:keep #t
        #:async-apply (lambda (t) (t))
        _pointer
        _pointer
        _ulong
        _pa-stream-callback-time-info-pointer/null
        _pa-stream-callback-flags
        _pointer
        -> _pa-stream-callback-result))


(define-cstruct _rack-audio-closure
  ([sound         _pointer]
   [curSample     _ulong]
   [numSamples    _ulong]
   [stop-now      _bool]
   [stop-sema-ptr _pointer]))

(define feeder 
  (get-ffi-obj "copyingCallback" feeder-lib _my-pa-stream-callback))

(run-tests
(test-suite "call to C audio feeder"
(let ()
  
  (define closure-info (make-sndplay-record src-buf))

(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info)
 'pa-continue)

(check-equal? (for/and ([i (in-range 200)])
                (= (s16vector-ref tgt-buf i) 
                   (s16vector-ref src-buf i)))
              #t)
(check-equal? (for/and ([i (in-range 200 500)])
                (= (s16vector-ref tgt-buf i)
                   0))
              #t)
(check-equal? (rest (rack-audio-closure->list closure-info))
              (list 200 800 #f (rack-audio-closure-stop-sema-ptr closure-info)))

(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info)
 'pa-continue)
(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info)
 'pa-continue)
(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info)
 'pa-complete)
(check-equal? (for/and ([i (in-range 200)])
                (= (s16vector-ref src-buf (+ 600 i)) 
                   (s16vector-ref tgt-buf i)))
              #t)
(check-equal? (for/and ([i (in-range 200 500)])
                (= (s16vector-ref tgt-buf i)
                   0))
              #t)
(check-equal? (rest (rack-audio-closure->list closure-info))
              (list 800 800 #f (rack-audio-closure-stop-sema-ptr closure-info)))

;; how about when things don't come out even?

  (define uneven-len-vec (make-s16vector 350 0))
  (for ([i (in-range 350)])
    (s16vector-set! uneven-len-vec i (random 100)))
  
(define closure-info-uneven (make-sndplay-record uneven-len-vec))
  
(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info-uneven)
 'pa-continue)
  
(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info-uneven)
 'pa-complete)
  
(check-equal? (for/and ([i (in-range 150)])
                (= (s16vector-ref uneven-len-vec (+ 200 i)) 
                   (s16vector-ref tgt-buf i)))
              #t)
(check-equal? (for/and ([i (in-range 150 500)])
                (= (s16vector-ref tgt-buf i)
                   0))
              #t))))




;; now to try playing a sound using it.

(define tone-buf-470 (make-tone-buf 470 (* 1 sr)))
(define tone-buf-cpointer (s16vector->cpointer tone-buf-470))
(define closure-info-470 
  (make-sndplay-record tone-buf-470))

;; reload as a raw pointer:

(define-cstruct _bogus-struct
  ([datum _uint64]))

(define feeder/ptr
  (get-ffi-obj "copyingCallback" feeder-lib _bogus-struct))

(pa-maybe-initialize)
(define my-stream 
  (pa-open-default-stream
   0             ;; input channels
   2             ;; output channels
   'paInt16      ;; sample format
   44100.0       ;; sample rate
   1000          ;;frames-per-buffer  ;; frames per buffer
   feeder/ptr    ;; callback (NULL means just wait for data)
   closure-info-470))

(printf "playing 1 sec @ 470 Hz.\n")
(sleep 1.0)
(printf "starting now...\n")
(pa-start-stream my-stream)
(sleep 1.0)
(printf "... all done.\n")
(sleep 1.0)

;; now using the abort flag


(let ()
  (define closure-info-470
    (make-sndplay-record tone-buf-470))
  
  (define my-stream
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     1000          ;;frames-per-buffer  ;; frames per buffer
     feeder/ptr    ;; callback (NULL means just wait for data)
     closure-info-470))
  
  (printf "playing 1/2 sec @ 470 Hz.\n")
  (sleep 2.0)
  (printf "starting now...\n")
  (pa-start-stream my-stream)
  (sleep 0.5)
  (set-rack-audio-closure-stop-now! closure-info-470 #t)
  (printf "... all done.\n")
  (sleep 1.0))

