#lang racket

(require ffi/unsafe
         ffi/vector
         rackunit
         rackunit/text-ui
         racket/runtime-path
         "../portaudio.rkt"
         "helpers.rkt")

(define-runtime-path libs "../")

(define feeder-lib (ffi-lib (build-path libs "feeder.dylib")))

(define src-buf (make-s16vector 1000 03))
;; fill with rands between 0 & 99:
(for ([i (in-range 1000)])
  (s16vector-set! src-buf i (random 100)))

(define tgt-buf (make-s16vector 500 0))

(define-cstruct _rack-audio-closure
  ([next-data _pointer]
   [stop      _pointer]
   [stop-now  _bool]))

(define _my-pa-stream-callback
  (_fun #:atomic? #t
        #:keep #t
        #:async-apply (lambda (t) (t))
        _pointer
        _pointer
        _ulong
        _pa-stream-callback-time-info-pointer/null
        _pa-stream-callback-flags
        _rack-audio-closure-pointer
        -> _pa-stream-callback-result))

(define feeder 
  (get-ffi-obj "copyingCallback" feeder-lib _my-pa-stream-callback))



(define src-cpointer (s16vector->cpointer src-buf))
(define src-end (ptr-add src-cpointer 800 _sint16))

(define closure-info
  (make-rack-audio-closure src-cpointer
                            src-end
                            #f))

(run-tests
(test-suite "call to C audio feeder"
(let ()
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
(check-equal? (rack-audio-closure->list closure-info)
              (list (ptr-add src-cpointer 200 _sint16) src-end #f))

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
(check-equal? (rack-audio-closure->list closure-info)
              (list (ptr-add src-cpointer 800 _sint16) src-end #f))

;; how about when things don't come out even?

(define closure-info-uneven
  (make-rack-audio-closure src-cpointer
                           (ptr-add src-cpointer 350 _sint16)
                           #f))
(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info-uneven)
 'pa-continue)
(check-equal?
 (feeder #f (s16vector->cpointer tgt-buf) 100 #f '() closure-info-uneven)
 'pa-complete)
(check-equal? (for/and ([i (in-range 150)])
                (= (s16vector-ref src-buf (+ 200 i)) 
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
  (make-rack-audio-closure tone-buf-cpointer
                           (ptr-add tone-buf-cpointer
                                    (s16vector-length tone-buf-470)
                                    _sint16)
                           #f))

;; reload as a raw pointer:

(define feeder/raw 
  (get-ffi-obj "copyingCallback" feeder-lib (_array _uint64 1)))

(define feeder/ptr
  (array-ptr feeder/raw))

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
    (make-rack-audio-closure tone-buf-cpointer
                             (ptr-add tone-buf-cpointer (s16vector-length tone-buf-470) _sint16)
                             #f))
  
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

