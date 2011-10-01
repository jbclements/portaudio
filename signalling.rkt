#lang racket

(require "mzrt-sema.rkt")

(provide mzrt-sema-listener)

;; this function is in its own file to lower the overhead 
;; of creating the place.

;; given a mzrt-semaphore, create a new
;; place that waits on that semaphore and 
;; sends a message on its place-channel
;; whenever there's a post to the semaphore.
;; returns the place-descriptor. This comment
;; is probably now longer and less clear
;; than the function itself. Oh well.
(define (mzrt-sema-listener mzrt-sema)
  (define p 
    (place 
     ch
     (define mzrt-sema (place-channel-get ch))
     (let loop ()
       (mzrt-sema-wait mzrt-sema)
       (place-channel-put ch #t)
       (loop))))
  (place-channel-put p mzrt-sema)
  p)