#lang racket

(require ffi/unsafe
         "portaudio.rkt"
         "portaudio-utils.rkt"
         (rename-in racket/contract [-> c->]))

(define nat? exact-nonnegative-integer?)

(define sample-setter/c (c-> nat? nat? void?))
(define buffer-filler/c (c-> procedure? nat? nat? void?))
(define buffer-filler/unsafe/c (c-> cpointer? nat? nat? void?))
(define time-checker/c (c-> number?))
(define sound-killer/c (c-> void?))

(provide/contract [stream-play
                   (c-> buffer-filler/c nat? integer? 
                        (list/c time-checker/c
                                sound-killer/c))]
                  [stream-play/unsafe 
                   (c-> procedure? nat? integer? 
                        (list/c time-checker/c
                                sound-killer/c))])

(define channels 2)

;; given a buffer-filler and a frame length and a sample rate,
;; starts a stream, using the buffer-filler to provide data as
;; needed.
(define (stream-play/unsafe buffer-filler buffer-frames sample-rate)
  (pa-maybe-initialize)
  (match-define (list stream-info signal-channel)
    (make-streamplay-record buffer-frames))
  (define sr/i (exact->inexact sample-rate))
  (define stream
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     sr/i          ;; sample rate
     buffer-frames ;;frames-per-buffer  ;; frames per buffer
     streaming-callback ;; callback (NULL means just wait for data)
     stream-info))
  (define filling-thread
    (thread
     (lambda ()
       ;; pre-fill of first buffer:
       (call-fill-buf stream-info buffer-filler)
       (let loop ()
         (place-channel-get signal-channel)
         (call-fill-buf stream-info buffer-filler)
         (loop)))))
  (pa-start-stream stream)
  (define (stream-time)
    (pa-get-stream-time stream))
  (define (stopper)
    (kill-thread filling-thread)
    (stop-sound stream-info))
  (list stream-time stopper))

;; the safe version checks the index of each sample before it's 
;; used in a ptr-set!
(define (stream-play safe-buffer-filler buffer-frames sample-rate)
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
  (stream-play/unsafe call-safe-buffer-filler buffer-frames sample-rate))

;; find info on the current buffer-to-be-played,
;; call filler 
(define (call-fill-buf streaming-info-ptr buffer-filler)
  (match (buffer-if-waiting streaming-info-ptr)
    [#f ;; oops, probably stacked up signals. go wait again.
     #f]
    [(list ptr frames idx finished-thunk)
     (buffer-filler ptr frames idx)
     (finished-thunk)]))