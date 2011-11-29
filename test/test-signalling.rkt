#lang racket

(require "../signalling.rkt"
         "../mzrt-sema.rkt"
         ffi/unsafe)

(define s (mzrt-sema-create 0))

(define all-done-cell (malloc 'raw 4))
(ptr-set! all-done-cell _uint32 0)
(define place (mzrt-sema-listener s all-done-cell))

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
  (sleep 0.25)
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

;; test killing a place ... you have to have log-debug on
;; to see this test succeed.
(printf "when -W debug is on, you should see a \"received kill signal\" message here\n")
(ptr-set! all-done-cell _uint32 23)
(mzrt-sema-post s)
(sleep 1.0)

