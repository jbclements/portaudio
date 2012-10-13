#lang racket

(require ffi/vector
         ffi/unsafe
         (rename-in racket/contract [-> c->])
         "portaudio.rkt"
         "callback-support.rkt"
         "devices.rkt")

;; this module provides a function that plays a sound.

(define nat? exact-nonnegative-integer?)

(provide/contract [s16vec-play (c-> s16vector? nat? (or/c false? nat?) integer?
                                    (c-> void?))])

;; it would use less memory to use stream-play, but
;; there's an unacceptable 1/2-second lag in starting
;; a new place.

(define channels 2)
(define reasonable-latency 0.1)

;; given an s16vec, a starting frame, a stopping frame or 
;; false, and a sample rate, play the sound.
(define (s16vec-play s16vec start-frame pre-stop-frame sample-rate)
  (define total-frames (/ (s16vector-length s16vec) channels))
  (define stop-frame (or pre-stop-frame
                        total-frames))
  (check-args s16vec total-frames start-frame stop-frame)
  (define sound-frames (- stop-frame start-frame))  
  (pa-maybe-initialize)
  (define copying-info (make-copying-info s16vec start-frame stop-frame))
  (define sr/i (exact->inexact sample-rate))
  (define device-number (find-output-device reasonable-latency))
  (define device-latency (device-low-output-latency device-number))
  (define output-stream-parameters
    (make-pa-stream-parameters
     device-number ;; device
     2             ;; channels
     '(paInt16)    ;; sample format
     device-latency ;; latency
     #f))            ;; host-specific info
  (define stream
    (with-handlers ([(lambda (exn) 
                       (string=? (exn-message exn)
                                 "pa-open-stream: invalid device"))
                     (lambda (exn)
                       (error "open-stream failed with error message: ~s. See documentation for possible fixes."))])
      (pa-open-stream
       #f            ;; input parameters
       output-stream-parameters
       sr/i
       0             ;; frames-per-buffer
       '()           ;; stream-flags
       copying-callback
       copying-info)))
  (pa-set-stream-finished-callback stream copying-info-free)
  (pa-start-stream stream)
  (define (stopper)
    (pa-maybe-stop-stream stream))
  stopper)

(define (check-args vec total-frames start-frame stop-frame)
  (unless (integer? total-frames)
    (raise-type-error 's16vec-play "vector of length divisible by 2" 0 vec start-frame stop-frame))
  (when (<= total-frames start-frame)
    (raise-type-error 's16vec-play "start frame < total number of frames" 1 vec start-frame stop-frame))
  (when (< total-frames stop-frame)
    (raise-type-error 's16vec-play "end frame < total number of frames" 2 vec start-frame stop-frame))
  (when (< stop-frame start-frame)
    (raise-type-error 's16vec-play "start frame <= end frame" 1 vec start-frame stop-frame)))
