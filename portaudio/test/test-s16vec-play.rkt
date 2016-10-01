#lang racket

(require "../s16vec-play.rkt"
         ffi/vector
         rackunit
         rackunit/text-ui)

(define channels 2)

(define (print-and-flush str)
  (printf "~a" str)
  (flush-output))

(run-tests
(test-suite "s16vec-play"
(let ()
  
  ;; given a sample rate, make a 302/500 Hz test vector
  (define (make-test-vec SR)
    (define half-sec (floor (/ SR 2)))
    (define qtr-sec (floor (/ SR 4)))
    (define TONE1 302) ;; in Hz
    (define TONE2 500) ;; in Hz
    (define (samp pitch frame)
      (inexact->exact 
       (round (* 32767 (* 0.2 (sin (* (/ 1 SR) pi 2 frame pitch)))))))
    (define v (make-s16vector (* channels half-sec)))
    (for ([i (in-range qtr-sec)])
      (define sample (samp TONE1 i))
      (s16vector-set! v (* i channels) sample)
      (s16vector-set! v (add1 (* i channels)) sample))
    (for ([i (in-range qtr-sec half-sec)])
      (define sample (samp TONE2 i))
      (s16vector-set! v (* i channels) sample)
      (s16vector-set! v (add1 (* i channels)) sample))
    v)

  (define v (make-test-vec 44100))
  (define v2 (make-test-vec 48000))
  
  ;; using the stopper while the sound is playing, no error on the cleanup thread please...
  (print-and-flush "using the stopper while the sound is playing, no error on the cleanup thread please...\n")
  (print-and-flush "start...\n")
  (define stopper (s16vec-play v 0 #f 44100))
  (stopper)
  (sleep 3)
  (print-and-flush "...stop.\n")
  (print-and-flush "1/2-second tone at 302/500 Hz\n")
  (sleep 2)
  (print-and-flush "start...\n")
  (check-not-exn (lambda () (s16vec-play v 0 #f 44100)))
  (sleep 0.5)
  (print-and-flush "...stop.\n")
  (sleep 1)
  (print-and-flush "1/2-second tone at 302/500 Hz using 48K\n")
  (sleep 2)
  (print-and-flush "start...\n")
  (check-not-exn (lambda () (s16vec-play v2 0 #f 48000)))
  (sleep 0.5)
  (print-and-flush "...stop.\n")
  (sleep 1)
  (print-and-flush "1/2-second tone at 302/500 Hz using explicit boundaries\n")
  (sleep 2)
  (print-and-flush "start...\n")
  (check-not-exn (lambda () (s16vec-play v 0 22050 44100)))
  (sleep 0.5)
  (print-and-flush "...stop.\n")
  (sleep 1)
  (print-and-flush "1/4-second tone at 302/500 Hz using offsets\n")
  (sleep 2)
  (print-and-flush "start...\n")
  (check-not-exn (lambda () (s16vec-play v 7500 20000 44100)))
  (sleep 0.5)
  (print-and-flush "...stop.\n")
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
  (print-and-flush "check whether closes are happening:\n")
  (for ([i (in-range 100)])
    (check-not-exn (lambda () (s16vec-play v 0 4410 44100)))
    (sleep 0.1))
  
  (print-and-flush "10 near-simultaneous copies of the sound\n")
  (sleep 2)
  (print-and-flush "start...\n")
  (for ([i (in-range 10)])
    (check-not-exn (lambda () (s16vec-play v 0 #f 44100)))
    (sleep 0.03))
  (sleep 0.5)
  (print-and-flush "...stop.\n")
  

  )))


