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
  
  (define s (s16vec-record 22050 44100.0))
  
  (check-equal? (* 22050 2) (s16vector-length s))
  
  ;;(rsound s 0 22050 22050.0)
  
  

  )))


