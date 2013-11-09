#lang racket/base

(require ffi/vector
         ffi/unsafe
         (rename-in racket/contract [-> c->])
         "portaudio.rkt"
         "callback-support.rkt")

;; CODE IN PROGRESS! NOT CURRENTLY WORKING!

;; this module provides a function that records a sound.

(define nat? exact-nonnegative-integer?)

(provide/contract [s16vec-record (c-> nat? integer? s16vector?)])

(define channels 2)

;; given a number of frames and a sample rate, record the sound
;; and return it. Blocks!
(define (s16vec-record frames sample-rate)
  (pa-maybe-initialize)
  (define copying-info (make-copying-info/rec frames))
  (define sr/i (exact->inexact sample-rate))
  (define stream
    (pa-open-default-stream
     2             ;; input channels
     0             ;; output channels
     'paInt16      ;; sample format
     sr/i          ;; sample rate
     0             ;;frames-per-buffer
     copying-callback/rec ;; callback (NULL means just wait for data)
     copying-info))
  ;;(pa-set-stream-finished-callback stream copying-info-free)
  (pa-start-stream stream)
  ;; need to figure out the "right" way to do this. Start with something crude:
  ;; some way to signal this directly? ... AH! use stream-finished-callback?
  (sleep (* frames (/ 1 sample-rate)))
  (let loop ()
    (when (pa-stream-active? stream)
      (sleep 0.5)
      (loop)))
  (extract-recorded-sound copying-info))


