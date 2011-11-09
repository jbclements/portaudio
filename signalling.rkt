#lang racket/base

(require "mzrt-sema.rkt"
         "horrible-pointer-hack.rkt"
         ffi/unsafe ;; only required for the horrible hack....
         racket/place)


(provide mzrt-sema-listener)

;; this function is in its own file to lower the overhead 
;; of creating the place.

;; given a mzrt-semaphore, create a new
;; place that waits on that semaphore and 
;; sends a message on its place-channel
;; whenever there's a post to the semaphore.
;; returns a list containing the place-descriptor 
;; and the kill-box.
;; if the "kill" box is set to #t, then 
;; posting to the mzrt-sema will cause
;; the listener to die.
(define (mzrt-sema-listener mzrt-sema all-done-cell)
  (define pre (current-inexact-milliseconds))
  (define p 
    (place 
     ch
     ;; in 5.1.3 and below, need to cheat 
     ;; to get the pointer through the channel:
     (define mzrt-sema 
       (cast (num->pointer (place-channel-get ch))
             _pointer
             _mzrt-semaphore))
     (define all-done-cell
       (num->pointer (place-channel-get ch)))
     (place-channel-put ch 'ready)
     (let loop ()
       (mzrt-sema-wait mzrt-sema)
       (cond [(not (= 0 (ptr-ref all-done-cell _uint32)))
              ;; free things. First make sure this works....
              (log-debug "received kill signal")
              'done]
             [else
              (place-channel-put ch 'signal)
              (loop)]))))
  ;; in 5.1.3 and below, need to cheat
  ;; to get the pointer through the channel
  (place-channel-put p (pointer->num mzrt-sema))
  (place-channel-put p (pointer->num all-done-cell))
  ;; wait for the place to come up:
  (define up (place-channel-get p))
  (unless (eq? up 'ready)
    (error 'mzrt-sema-listener "failed to initialize place correctly"))
  (define post (current-inexact-milliseconds))
  ;; for debugging, if desired:
  #;(printf "startup time: ~s\n" (- post pre))
  p)