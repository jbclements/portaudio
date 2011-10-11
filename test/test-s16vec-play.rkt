#lang racket

(require "../s16vec-play.rkt"
         ffi/vector
         rackunit
         rackunit/text-ui)

(define channels 2)

(run-tests
(test-suite "s16vec-play"
(let ()
  
  (define v (make-s16vector (* channels 22055)))
  (for ([i (in-range 22055)])
    (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* (/ 1 44100) pi 2 i 302)))))))
    (s16vector-set! v (* i channels) sample)
    (s16vector-set! v (add1 (* i channels)) sample))
  (sleep 2)
  (printf "start...\n")
  (s16vec-play v 0 #f 44100)
  (sleep 0.5)
  (printf "...stop.\n")
  

  )))

