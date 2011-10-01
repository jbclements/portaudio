#lang racket

(require "../signalling.rkt"
         "../mzrt-sema.rkt")

(define s (mzrt-sema-create 0))
(define place (mzrt-sema-listener s))

(thread 
 (lambda ()
   (let loop ()
     (place-channel-get place)
     (printf "got one!\n")
     (loop))))

;; when run, a "got one!"
;; should appear immediately after
;; each "posting..."
(for ([i (in-range 5)])
  (sleep 1.0)
  (printf "posting...\n")
  (mzrt-sema-post s))
