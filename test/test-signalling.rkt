#lang racket

(require "../signalling.rkt"
         "../mzrt-sema.rkt")

(define s (mzrt-sema-create 0))
(define place (mzrt-sema-listener s))

(thread 
 (lambda ()
   (let loop ()
     (place-channel-get place)
     (printf "post: ~s\n" (current-inexact-milliseconds))
     (loop))))

;; when run, a "got one!"
;; should appear immediately after
;; each "posting..."
(for ([i (in-range 5)])
  (sleep 1.0)
  (printf "posting...\n")
  (define pre (current-inexact-milliseconds))
  (mzrt-sema-post s)
  (printf "pre: ~s\n" pre))
