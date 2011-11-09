#lang racket

(require "../signalling.rkt"
         "../mzrt-sema.rkt")

(define s (mzrt-sema-create 0))
(match-define (list place kill-thunk) (mzrt-sema-listener s))

(define pre-times '())
(define post-times '())

(thread 
 (lambda ()
   (let loop ()
     (place-channel-get place)
     (set! post-times (cons (current-inexact-milliseconds)
                            post-times))
     (loop))))

(define trials 40)

(for ([i (in-range trials)])
  (sleep 1.0)
  (when (= 0 (modulo i 5))
    (printf "~a\n" (format "posting(~s/~s)..." i trials)))
  (define pre (current-inexact-milliseconds))
  (mzrt-sema-post s)
  (set! pre-times (cons pre pre-times)))

(define intervals
  (for/list ([pre (in-list (reverse pre-times))]
             [post (in-list (reverse post-times))])
    (- post pre)))


(define (mean l) (/ (apply + l) (length l)))
(define (stdevp l) 
  (define m (mean l))
  (sqrt
   (/ (apply
       +
       (for/list ([i (in-list l)])
         (* (- i m) (- i m))))
      (length l))))


(printf "intervals (in ms): ~s\nmean: ~s ms\nstandard deviation: ~s ms\n"
        intervals
        (mean intervals)
        (stdevp intervals))
