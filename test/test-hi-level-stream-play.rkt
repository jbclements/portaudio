#lang racket

(require "../stream-play.rkt"
         ffi/unsafe
         (planet williams/science/statistics))

(define srinv (/ 1.0 44100))
(define twopi (* 2 pi))
(define channels 2)

(define log1 empty)
(define log2 empty)

(define time-checker #f)

;; sine wave at 403 Hz
(define (buffer-filler sample-setter idx)
  (define pre-time (and time-checker (time-checker)))
  (define base-frames (* idx 1024))
  (define base-t (exact->inexact (* base-frames srinv)))
  (for ([i (in-range 1024)])
    (define t (+ base-t (* i srinv)))
    (define sample
      (inexact->exact (round (* 32767 (* 0.2 (sin (* twopi t 403)))))))
    (define sample-idx (* channels i))
    (sample-setter sample-idx sample)
    (sample-setter (add1 sample-idx) sample))
  (define post-time (and time-checker (time-checker)))
  (if (and pre-time post-time)
      (set! log1 (cons (- post-time pre-time) log1))
      (set! log1 (cons #f log1))))

(match-define (list checker stopper)
  (stream-play buffer-filler 1024 44100))
(set! time-checker checker)
(sleep 3.0)
(stopper)

;; fails
(length (filter not log1))
(mean-and-variance (filter (lambda (x) x) log1))


(define (buffer-filler/unsafe ptr len idx)
  (define pre-time (and time-checker (time-checker)))
  (define base-frames (* idx 1024))
  (define base-t (exact->inexact (* base-frames srinv)))
  (for ([i (in-range 1024)])
    (define t (+ base-t (* i srinv)))
    (define sample
      (inexact->exact (round (* 32767 (* 0.2 (sin (* twopi t 403)))))))
    (define sample-idx (* channels i))
    (ptr-set! ptr _sint16 sample-idx sample)
    (ptr-set! ptr _sint16 (add1 sample-idx) sample))
  (define post-time (and time-checker (time-checker)))
  (if (and pre-time post-time)
      (set! log2 (cons (- post-time pre-time) log2))
      (set! log2 (cons #f log2))))

(match-define (list checker2 stopper2)
  (stream-play/unsafe buffer-filler/unsafe 1024 44100))
(set! time-checker checker2)
(sleep 3.0)
(stopper2)

;; fails
(length (filter not log2))
(mean-and-variance (filter (lambda (x) x) log2))



