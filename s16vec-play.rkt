#lang racket

(require ffi/vector
         ffi/unsafe
         "stream-play.rkt"
         (rename-in racket/contract [-> c->]))

;; builds on stream-play to play s16vecs.

(define channels 2)

(define nat? exact-nonnegative-integer?)

(provide/contract [s16vec-play (c-> s16vector? nat? nat? integer?
                                    void?)])

(define (s16vec-play s16vec start-frame stop-frame sample-rate)
  (define sound-frames (- stop-frame start-frame))
  (define stopper-box (box #f))
  (define (feeder buf buffer-frames idx)
    (define t (* buffer-frames idx))
    (define offset (+ start-frame t))
    (define frames-remaining (- sound-frames t))
    (define frames-to-copy (max 0 
                                (min buffer-frames
                                     frames-remaining)))
    (memcpy buf (ptr-add (s16vector->cpointer s16vec)
                         (* channels offset) 
                         _sint16)
            (* channels frames-to-copy)
            _sint16)
    (when (< frames-to-copy buffer-frames)
      (memset (ptr-add buf (* channels frames-to-copy) _sint16)
              0
              (* channels (- buffer-frames frames-to-copy))
              _sint16)
      (thread 
       (lambda ()
         (and (unbox stopper-box)
              ((unbox stopper-box)))))))
  (match-define (list timer stopper)
    (stream-play/unsafe feeder default-buffer-frames sample-rate))
  (set! stopper-box stopper))

;; at 44100, this is close to 100ms, which should cover most
;; GC pauses.
(define default-buffer-frames 4096)