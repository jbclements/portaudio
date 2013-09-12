#lang racket

(require ffi/vector)

;; common code for test cases:

(provide (all-defined-out))

(define sr 44100)
(define sr-inv 1/44100)

(define channels 2)
(define s16max 32767)
  
  
;; make a buffer filled with a tone at a given pitch 
(define (make-tone-buf pitch frames)
  (define result (make-s16vector (* frames channels)))
  (for ([i (in-range 0 frames)])
    (define sample 
      (inexact->exact 
       (round (* s16max 0.1 (sin 
                             (* i sr-inv pitch 2 pi))))))
    (s16vector-set! result (* i 2) sample)
    (s16vector-set! result (add1 (* i 2)) sample))
  result)


  