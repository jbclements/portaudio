#lang racket

(require ffi/unsafe
         rackunit
         rackunit/text-ui
         racket/runtime-path
         ffi/vector
         (only-in "../callback-support.rkt" make-copying-info))

;; test the copying callbacks

(define-runtime-path libs "../lib")

(define _pa-time _double)

(define-cstruct _pa-stream-callback-time-info
  ([input-buffer-adc-time  _pa-time]
   [current-time           _pa-time]
   [output-buffer-dac-time _pa-time]))


(define _pa-stream-callback-flags
  (_bitmask
   '(pa-input-underflow  = #x00000001
     pa-input-overflow   = #x00000002
     pa-output-underflow = #x00000004
     pa-output-overflow  = #x00000008
     pa-priming-output   = #x00000010)
   _ulong))

(define _pa-stream-callback-result
  (_enum
   '(pa-continue = 0
     pa-complete = 1
     pa-abort    = 2)))

(define _pa-stream-callback
  (_fun _pointer
        _pointer
        _ulong
        _pa-stream-callback-time-info-pointer
        _pa-stream-callback-flags
        _pointer
        -> _pa-stream-callback-result))

(define-cstruct _copying-rec
  ([sound         _pointer]
   [cur-sample    _ulong]
   [num-samples   _ulong]))


(define (random-s16)
  (- (random #xffff) #x8000))

(run-tests
(test-suite "portaudio"
(let ()
 
  
  (define callback-lib
    (ffi-lib (build-path libs (system-library-subpath) "callbacks")))
  
  (define copying-callback
    (get-ffi-obj "copyingCallback"
                 callback-lib
                 _pa-stream-callback))
  
  
  ;; create a bogus source vector with noise:
  (define src-vec (make-s16vector 2048))
  (for ([i (in-range 2048)])
    (s16vector-set! src-vec i (random-s16)))
  
  (define offset-frame 436)
  (define offset-sample (* 2 offset-frame))
  (define remaining-samples (- 2048 offset-sample))
  
  ;; create a copying info, make sure it's correct:
  (define copying-info (make-copying-info src-vec offset-frame #f))
  (check-equal? (copying-rec-cur-sample copying-info) 0)
  (check-equal? (copying-rec-num-samples copying-info) (- 2048 offset-sample))
  (define copied-buf-ptr (copying-rec-sound copying-info))
  (for ([i (in-range remaining-samples)])
    (check-equal? (s16vector-ref src-vec (+ i offset-sample))
                  (ptr-ref copied-buf-ptr _sint16 i)))
  
  
  ;; use the copying-callback to put it into another buffer.
  
  ;; target buffer:
  (define dst-ptr (malloc _sint16 1024))
  
  (printf "this test was not completely implemented...\n")
  #;(copying-callback )

  
  13)))