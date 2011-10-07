#lang racket/base

(require "mzrt-sema.rkt"
         racket/place)


(provide mzrt-sema-listener)

;; this function is in its own file to lower the overhead 
;; of creating the place.

;; given a mzrt-semaphore, create a new
;; place that waits on that semaphore and 
;; sends a message on its place-channel
;; whenever there's a post to the semaphore.
;; returns the place-descriptor.
(define (mzrt-sema-listener mzrt-sema)
  (define pre (current-inexact-milliseconds))
  (define p 
    (place 
     ch
     (define mzrt-sema (place-channel-get ch))
     (place-channel-put ch 'ready)
     (let loop ()
       (mzrt-sema-wait mzrt-sema)
       (place-channel-put ch 'signal)
       (loop))))
  (place-channel-put p mzrt-sema)
  ;; wait for the place to come up:
  (define up (place-channel-get p))
  (unless (eq? up 'ready)
    (error 'mzrt-sema-listener "failed to initialize place correctly"))
  (define post (current-inexact-milliseconds))
  ;; for debugging, if desired:
  #;(printf "startup time: ~s\n" (- post pre))
  p)