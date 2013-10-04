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
  (for ([i (in-range 10000)])
    (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* (/ 1 44100) pi 2 i 302)))))))
    (s16vector-set! v (* i channels) sample)
    (s16vector-set! v (add1 (* i channels)) sample))
  (for ([i (in-range 10000 22055)])
    (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* (/ 1 44100) pi 2 i 500)))))))
    (s16vector-set! v (* i channels) sample)
    (s16vector-set! v (add1 (* i channels)) sample))
  (printf "1/2-second tone at 302/500 Hz\n")
  (sleep 2)
  (printf "start...\n")
  (s16vec-play v 0 #f 44100)
  (check-not-exn (lambda () (s16vec-play v 0 #f 44100)))
  (sleep 0.5)
  (printf "...stop.\n")
  (sleep 1)
  (printf "1/2-second tone at 302/500 Hz using explicit boundaries\n")
  (sleep 2)
  (printf "start...\n")
  (check-not-exn (lambda () (s16vec-play v 0 22055 44100)))
  (sleep 0.5)
  (printf "...stop.\n")
  (sleep 1)
  (printf "1/4-second tone at 302/500 Hz using offsets\n")
  (sleep 2)
  (printf "start...\n")
  (check-not-exn (lambda () (s16vec-play v 7500 20000 44100)))
  (sleep 0.5)
  (printf "...stop.\n")
  (sleep 1)
  (check-exn exn:fail?
             (lambda ()
               (s16vec-play v -3 20000 44100)))
  (check-exn exn:fail?
             (lambda ()
               (s16vec-play v 0 40000 44100)))
  (check-exn exn:fail?
             (lambda ()
               (s16vec-play v 10000 0 44100)))
  ;; simultaneous play:
  (printf "check whether closes are happening:\n")
  (for ([i (in-range 100)])
    (check-not-exn (lambda () (s16vec-play v 0 4410 44100)))
    (sleep 0.1))
  
  (printf "10 near-simultaneous copies of the sound")
  (sleep 2)
  (printf "start...\n")
  (for ([i (in-range 10)])
    (check-not-exn (lambda () (s16vec-play v 0 #f 44100)))
    (sleep 0.03))
  (sleep 0.5)
  (printf "...stop.\n")
  

  )))


