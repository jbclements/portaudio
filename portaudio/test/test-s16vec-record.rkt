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
  
  (define s1 (s16vec-record 1000 44100 2))
  
  (check-equal? (s16vector-length s1) (* 1000 2))
  
  (define s2 (s16vec-record 1000 44100 1))
  
  (check-equal? (s16vector-length s2) (* 1000 2))
  
  (check-exn (lambda (exn)
               (regexp-match (regexp-quote
                              "expected: channels?")
                             (exn-message exn)))
             (lambda ()
               (s16vec-record 1000 44100 10)))
  
  #;(define data (for/list ([p (s16vector->list s)]
                          [i (in-naturals)])
                 (vector i p)))
  #;(display (plot (points data)))
  )))


