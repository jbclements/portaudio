#lang racket/base

(require racket/match
         racket/place
         ffi/unsafe
         "portaudio.rkt"
         "callback-support.rkt"
         "devices.rkt"
         (rename-in racket/contract [-> c->]))


;; this file contains the code required to play streams

(define nat? exact-nonnegative-integer?)

;; using this contract would kill runtimes:
(define sample-setter/c (c-> nat? nat? void?))
(define buffer-filler/c (c-> procedure? ;; could be sample-setter/c
                             nat? void?))
;; leaving this one out, too, to save runtime. might not be worth it:
(define buffer-filler/unsafe/c (c-> cpointer? nat? nat? void?))
(define time-checker/c (c-> number?))
(define sound-killer/c (c-> void?))
(define stats/c (c-> (listof (list/c symbol? number?))))

(provide/contract [stream-play
                   (c-> buffer-filler/c real? real? 
                        (list/c time-checker/c
                                stats/c
                                sound-killer/c))]
                  [stream-play/unsafe 
                   (c-> procedure? ;; could be buffer-filler/unsafe/c
                        real? real? 
                        (list/c time-checker/c
                                stats/c
                                sound-killer/c))])

(define channels 2)

;; we insist on an engine with latency at least this low:
(define reasonable-latency 0.05)
;; the wake interval for the buffer-filler:
(define sleep-interval 0.01)

;; given a buffer-filler (unsafe) and a frame length and a sample rate,
;; starts a stream, using the buffer-filler to provide data as
;; needed.
(define (stream-play/unsafe buffer-filler buffer-time sample-rate)
  (pa-maybe-initialize)
  (define chosen-device (find-output-device reasonable-latency))
  (log-debug (format "Portaudio: chosen number/name: ~s,~s"
                     chosen-device
                     (device-name chosen-device)))
  (define promised-latency (device-low-output-latency chosen-device))
  ;; totally heuristic here:
  (define min-buffer-time (+ promised-latency (* 2 sleep-interval)))
  (when (< buffer-time min-buffer-time)
    (fprintf (current-error-port) "WARNING: using buffer of ~sms to satisfy API requirements.\n"
             (* 1000 min-buffer-time)))
  (log-debug (format "Portaudio: chosen device requested latency: ~sms" (round-to-hundredth (* 1000 promised-latency))))
  (define buffer-frames (buffer-time->frames (max min-buffer-time buffer-time) sample-rate))
  (match-define (list stream-info all-done-ptr)
    (make-streaming-info buffer-frames))
  (define stream (stream-open stream-info chosen-device promised-latency sample-rate))
  (pa-set-stream-finished-callback stream
                                   streaming-info-free)
  ;; pre-fill of first buffer:
  (call-buffer-filler stream-info buffer-filler)
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
  (define (stats)
    (stream-stats stream))
  (define (stopper)
    (pa-maybe-close-stream stream))
  (list stream-time stats stopper))

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
  (define (call-safe-buffer-filler ptr frames)
    (safe-buffer-filler (lambda (sample-idx sample)
                          (check-sample-idx sample-idx)
                          ;; this should check that sample is legal....
                          (ptr-set! ptr _sint16 sample-idx sample))
                        frames))
  (stream-play/unsafe call-safe-buffer-filler buffer-time sample-rate))

;; compute the number of frames in the buffer from the given time
(define (buffer-time->frames buffer-time sample-rate)
  (unless (< 0.01 buffer-time 1.0)
    (error 'stream-play "expected buffer-time between 10ms and 1 second, given ~s seconds"
           buffer-time))
  (inexact->exact 
   (ceiling (* buffer-time sample-rate))))

;; stream-open : stream-info natural? real? real? -> stream
;; open the given device using the given stream-info, latency, and sample-rate.
(define (stream-open stream-info device-number latency sample-rate)
  (define sr/i (exact->inexact sample-rate))
  (define output-stream-parameters
    (make-pa-stream-parameters
     device-number ;; device
     2             ;; channels
     '(paInt16)    ;; sample format
     latency       ;; latency
     #f))            ;; host-specific info
  (with-handlers ([(lambda (exn) 
                     (string=? (exn-message exn)
                               "pa-open-stream: invalid device"))
                   (lambda (exn)
                     (error "open-stream failed with error message: ~s. See documentation for possible fixes."))])
    (pa-open-stream
     #f ;; input parameters
     output-stream-parameters
     sr/i
     0 ;; frames-per-buffer
     '() ;; stream-flags
     streaming-callback
     stream-info)))

(define (round-to-hundredth x)
  (/ (round (* x 100)) 100))
