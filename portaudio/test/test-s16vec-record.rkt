#lang racket

(require "../s16vec-record.rkt"
         ffi/vector
         rackunit
         rackunit/text-ui
         plot)

(define channels 2)

(run-tests
(test-suite "s16vec-record"
(let ()
  
  (define s (s16vec-record 1000 44100))
  
  (check-equal? (* 1000 2) (s16vector-length s))
  
  (define data (for/list ([p (s16vector->list s)]
                          [i (in-naturals)])
                 (vector i p)))
  (display (plot (points data)))
  )))


