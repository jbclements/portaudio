#lang racket

(require "../portaudio.rkt"
         "../portaudio-utils.rkt"
         "helpers.rkt"
         ffi/vector
         ffi/unsafe
         rackunit
         rackunit/text-ui)

(define twopi (* 2 pi))

(run-tests
(test-suite "portaudio"
(let ()
  
  (pa-maybe-initialize)
  
  (define (open-test-stream callback streaming-info-ptr)
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     1024          ;;frames-per-buffer  ;; frames per buffer
     callback      ;; callback (NULL means just wait for data)
     streaming-info-ptr))
  
  (define (test-start) 
    (sleep 2)
    (printf "starting now... "))
  
  (define (test-end)
    (printf "... ending now.\n")
    (sleep 1))

  (define srinv (exact->inexact (/ 1 44100)))
  (define (fill-buf ptr index)
    (define base-frames (* index 1024))
    (define base-t (exact->inexact (* base-frames srinv)))
    (for ([i (in-range 1024)])
      (define t (+ base-t (* i srinv)))
      (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* twopi t 403)))))))
      (define sample-idx (* channels i))
      (ptr-set! ptr _sint16 sample-idx sample)
      (ptr-set! ptr _sint16 (add1 sample-idx) sample)))
  
  (let ()
    (define t1 (make-s16vector 2048 0))
    (fill-buf (s16vector->cpointer t1) 3)
    (for ([i (in-range 2 #;2048)])
      (define t (floor (/ i 2)))
      (check-= (asin (* 5.0 (/ (s16vector-ref t1 i) 32767.0)))
               (* twopi (/ 1 44100) (+ (* 3 1024) t))
               1e-4)))
  
  
  ;; first test with the vector interface:
  #;(let ()
    (define abort-box (box #f))
    (define callback-info (make-sndplay-record tone-buf-330))
    (define stream (open-test-stream copying-callback callback-info))
    (printf "1/2 second @ 330 Hz\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 0.5)
    (test-end))
  

  )))


