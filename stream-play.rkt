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
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     sr/i          ;; sample rate
     0             ;;frames-per-buffer -- let the system decide
     streaming-callback ;; callback (NULL means just wait for data)
     stream-info))
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
