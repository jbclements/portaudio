#lang racket

(require "../stream-play.rkt"
         ffi/unsafe
         math/statistics)

(define srinv (/ 1.0 44100))
(define twopi (* 2 pi))
(define channels 2)

(define log2 empty)

(define (my-stats log len-log)
  ;; fails
  (printf "fails: ~s\n" (length (filter not log)))
  (define times-taken (filter (lambda (x) x) log))
  (printf "mean: ~s\n" (mean times-taken))
  (printf "std deviation: ~s\n" (stddev times-taken))
  (printf "mean len: ~s\n" (exact->inexact (mean len-log)))
  (printf "std deviation len: ~s\n" (stddev len-log)))


;; takes a buffer-filler that expects to be told
;; the current frame and produce one that keeps
;; track of it internally.
(define (three-arg->two-arg fun init)
  (define frames init)
  (lambda (a num-frames)
    (begin0 (fun a num-frames frames)
            (set! frames (+ frames num-frames)))))


(define (t1)

  (define log1 empty)

  (define len-log empty)
(define time-checker #f)

;; sine wave at 403 Hz
(define (buffer-filler sample-setter len base-frames)
  (set! len-log (cons len len-log))
  (define pre-time (and time-checker (time-checker)))
  (define base-t (exact->inexact (* base-frames srinv)))
  (for ([i (in-range len)])
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

(printf "3 seconds of 403 Hz, 50ms buffer length, safe interface\n")
(collect-garbage)
(collect-garbage)
(collect-garbage)
(match-define (list checker stats stopper)
  (stream-play (three-arg->two-arg buffer-filler 0) 0.05 44100))
(set! time-checker checker)


(sleep 3.0)
(stats)
(stopper)

(my-stats log1 len-log)
  (sleep 1)
)

(t1)
(t1)

(define (t2)
(define time-checker-2 #f)

(define len-log empty)

(define (buffer-filler/unsafe ptr len base-frames)
  (set! len-log (cons len len-log))
  (define pre-time (and time-checker-2 (time-checker-2)))
  (define base-t (exact->inexact (* base-frames srinv)))
  (for ([i (in-range len)])
    (define t (+ base-t (* i srinv)))
    (define sample
      (inexact->exact (round (* 32767 (* 0.2 (sin (* twopi t 403)))))))
    (define sample-idx (* channels i))
    (ptr-set! ptr _sint16 sample-idx sample)
    (ptr-set! ptr _sint16 (add1 sample-idx) sample))
  (define post-time (and time-checker-2 (time-checker-2)))
  (if (and pre-time post-time)
      (set! log2 (cons (- post-time pre-time) log2))
      (set! log2 (cons #f log2))))

(printf "3 seconds at 403 Hz, 50ms buffer length, unsafe interface.\n")

(collect-garbage)
(collect-garbage)
(collect-garbage)
(match-define (list checker2 stats2 stopper2)
  (stream-play/unsafe (three-arg->two-arg buffer-filler/unsafe 0) 0.05 44100))
(set! time-checker-2 checker2)
(sleep 3.0)
(stats2)
(stopper2)

;; fails

(my-stats log2 len-log))

(t2)

