#lang racket/base

(require racket/match
         racket/place
         ffi/unsafe
         "portaudio.rkt"
         "callback-support.rkt"
         (rename-in racket/contract [-> c->]))

(define nat? exact-nonnegative-integer?)

(define sample-setter/c (c-> nat? nat? void?))
(define buffer-filler/c (c-> procedure? nat? nat? void?))
(define buffer-filler/unsafe/c (c-> cpointer? nat? nat? void?))
(define time-checker/c (c-> number?))
(define sound-killer/c (c-> void?))

(provide/contract [stream-play
                   (c-> buffer-filler/c real? real? 
                        (list/c time-checker/c
                                sound-killer/c))]
                  [stream-play/unsafe 
                   (c-> procedure? real? real? 
                        (list/c time-checker/c
                                sound-killer/c))])

(define channels 2)

(define sleep-time 0.01)

;; given a buffer-filler and a frame length and a sample rate,
;; starts a stream, using the buffer-filler to provide data as
;; needed.
(define (stream-play/unsafe buffer-filler buffer-time sample-rate)
  (define buffer-frames (buffer-time->frames buffer-time sample-rate))
  (pa-maybe-initialize)
  (match-define (list stream-info all-done-ptr)
    (make-streaming-info buffer-frames))
  (define sr/i (exact->inexact sample-rate))
  (define stream
    (stream-choose stream-info sample-rate))
  (pa-set-stream-finished-callback stream
                                   streaming-info-free)
  ;; pre-fill of first buffer:
  (call-buffer-filler stream-info buffer-filler)
  ;; 5ms should definitely be fast enough. 
  ;; really, it's kind of outrageously fast, but 
  ;; the price seems low.
  (define sleep-interval 0.005)
  (define filling-thread
    (thread
     (lambda ()
       (let loop ()
         (cond [(all-done? all-done-ptr)
                (free all-done-ptr)]
               [else
                (define start-time (pa-get-stream-time stream))
                (call-buffer-filler stream-info buffer-filler)
                (define time-used (- (pa-get-stream-time stream) start-time))
                (sleep (max 0.0 (- sleep-interval time-used)))
                (loop)])))))
  (pa-start-stream stream)
  (define (stream-time)
    (pa-get-stream-time stream))
  (define (stopper)
    (pa-maybe-stop-stream stream))
  (list stream-time stopper))

;; the safe version checks the index of each sample before it's 
;; used in a ptr-set!
(define (stream-play safe-buffer-filler buffer-time sample-rate)
  (define buffer-frames (buffer-time->frames buffer-time sample-rate))
  (define buffer-samples (* channels buffer-frames))
  (define (check-sample-idx sample-idx)
    (unless (<= 0 sample-idx (sub1 buffer-samples))
      (error 'check-sample-idx 
             (format "must have 0<=sample-index<~s, given ~s"
                     buffer-samples sample-idx))))
  (define (call-safe-buffer-filler ptr frames idx)
    (safe-buffer-filler (lambda (sample-idx sample)
                          (check-sample-idx sample-idx)
                          ;; this should check that sample is legal....
                          (ptr-set! ptr _sint16 sample-idx sample))
                        frames
                        idx))
  (stream-play/unsafe call-safe-buffer-filler buffer-time sample-rate))

;; compute the number of frames in the buffer from the given time
(define (buffer-time->frames buffer-time sample-rate)
  (unless (< 0.01 buffer-time 1.0)
    (error 'stream-play "expected buffer-time between 10ms and 1 second, given ~s seconds"
           buffer-time))
  (inexact->exact 
   (ceiling (* buffer-time sample-rate))))


;; stream-choose : stream-info number -> stream
;; given a stream-info and a sample-rate, search for an 
;; output device that can provide output channels with 
;; a reasonable latency, and open that device.
(define (stream-choose stream-info sample-rate)
  (define sr/i (exact->inexact sample-rate))
  (define reasonable-devices (reasonable-latency-output-devices))
  (when (null? reasonable-devices)
    (error 'stream-choose "no devices available with 50ms latency or less."))
  (define default-device (pa-get-default-output-device))
  (define selected-device 
    (cond [(member default-device reasonable-devices) default-device]
          [else (car reasonable-devices)]))
  (define output-stream-parameters
    (make-pa-stream-parameters
     selected-device
     2
     'paInt16
     suggested-latency
     #f))
  (pa-open-stream
   #f ;; input parameters
   output-stream-parameters
   sr/i
   0 ;; frames-per-buffer
   '() ;; stream-flags
   streaming-callback
   stream-info))


;; reasonable-latency-output-devices : -> (list-of natural?)
;; output devices with less than 50ms latencies
(define (reasonable-latency-output-devices)
  (for/list ([i (in-range (pa-get-device-count))]
        #:when (has-outputs? i)
        #:when (reasonable-latency? i))
    i))

;; has-outputs? : natural -> boolean
;; return true if the device has at least
;; two output channels
(define (has-outputs? i)
  (<= 2 (pa-device-info-max-output-channels (pa-get-device-info i))))

;; reasonable-latency? : natural -> boolean
;; return true when the device has low latency
;; no greater than 50ms
(define (reasonable-latency? i)
  (<= (pa-device-info-default-output-latency (pa-get-device-info i))
      reasonable-latency))


(define suggested-latency 0.05)
(define reasonable-latency 0.05)