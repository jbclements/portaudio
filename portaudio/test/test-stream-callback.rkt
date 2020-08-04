#lang racket

(require "helpers.rkt"
         "../callback-support.rkt"
         "../callbacks-lib.rkt"
         ffi/unsafe
         ffi/vector
         rackunit
         rackunit/text-ui
         racket/runtime-path)

(define twopi (* 2 pi))

(define-runtime-path libs "../lib")

(define pa-abort 2)
(define pa-continue 0)

(run-tests
(test-suite "portaudio"
(let ()
  
  (define streaming-callback
    (get-ffi-obj "streamingCallback"
                 callbacks-lib
                 (_fun 
                  (_pointer = #f)
                  _pointer
                  _ulong
                  (_pointer = #f)
                  (_ulong = 0)
                  _stream-rec-pointer
                  -> _int)))
  
  ;; changing these will mess up the "around-the-corner"-ness
  ;; of the tests.
  (define buffer-frames 2048)
  (define output-buffer-frames 224)
  
  (match-define
      (list stream-info all-done-ptr)
    (make-streaming-info buffer-frames))
  
  (check-equal? (stream-rec-buffer-frames stream-info)
                buffer-frames)
  (ptr-set! (stream-rec-buffer stream-info) _sint16 39 -47)
  (check-equal? (ptr-ref (stream-rec-buffer stream-info) _sint16 39) -47)
  (check-equal? (stream-rec-last-frame-read stream-info) 0)
  (check-equal? (stream-rec-last-offset-read stream-info) 0)
  (check-equal? (stream-rec-last-frame-written stream-info) 0)
  (check-equal? (stream-rec-last-offset-written stream-info) 0)
  (check-equal? (stream-rec-fault-count stream-info) 0)
  (check-equal? (all-done? all-done-ptr) #f)
  
  ;; randomize the buffers
  (for ([j (in-range (* channels buffer-frames))])
    (ptr-set! (stream-rec-buffer stream-info) _sint16 j (- (random 1000) 500)))
  (define tgt (make-s16vector (* channels output-buffer-frames) 1))
  
  (define buffer-bytes (* 2 channels buffer-frames))
  
  ;; buffer-not ready yet (frames written still zero):
  (set-stream-rec-last-frame-read! stream-info 7000)
  (set-stream-rec-last-offset-read! stream-info (modulo (* 4 7000) buffer-bytes))
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 output-buffer-frames
                 stream-info)
                pa-continue)
  (check-equal? (stream-rec-last-frame-read stream-info) 7224)
  (check-equal? (stream-rec-last-offset-read stream-info) 
                (modulo (* 4 7224) buffer-bytes))
  (check-equal? (stream-rec-fault-count stream-info) 1)
  (for ([i (in-range (* channels output-buffer-frames))])
    (check-equal? (s16vector-ref tgt i) 0))
  
  ;; buffer ready:
  (set-stream-rec-last-frame-written! stream-info 8000)
  (set-stream-rec-last-offset-written! stream-info (modulo (* 4 8000) buffer-bytes))
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 output-buffer-frames
                 stream-info)
                pa-continue)
  (check-equal? (stream-rec-last-frame-read stream-info) 7448)
  (check-equal? (stream-rec-last-offset-read stream-info) 
                (modulo (* 4 7448) buffer-bytes))
  (for ([i (in-range (* 2 output-buffer-frames))]
        [j (in-range (modulo (* 2 7224)
                             (* 2 2048))
                     (+ (modulo (* 2 7224)
                                (* 2 2048))
                        (* 2 output-buffer-frames)))])
    (check-equal? (s16vector-ref tgt i)
                  (ptr-ref (stream-rec-buffer stream-info) _sint16 j)))
  
  ;; try an "around-the-corner" with a data failure too
  
  (set-stream-rec-last-frame-written! stream-info 8200)
  (set-stream-rec-last-offset-written! stream-info (modulo (* 4 8200) buffer-bytes))
  (set-stream-rec-last-frame-read! stream-info 8000)
  (set-stream-rec-last-offset-read! stream-info (modulo (* 4 8000) buffer-bytes))
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 output-buffer-frames
                 stream-info)
                pa-continue)
  (check-equal? (stream-rec-last-frame-read stream-info) 8224)
  (check-equal? (stream-rec-last-offset-read stream-info) (modulo (* 4 8224)
                                                                  buffer-bytes))
  ;; end of buffer:
  (for ([i (in-range (* 2 192))]
        [j (in-range (modulo (* 2 8000)
                             (* 2 buffer-frames))
                     (+ (* 2 192)
                        (modulo (* 2 8000)
                                (* 2 buffer-frames))))])
    (check-equal? 
     (s16vector-ref tgt i)
     (ptr-ref (stream-rec-buffer stream-info) _sint16 j)))
  ;; around the corner:
  (for ([i (in-range (* 2 192) (* 2 200))]
        [j (in-range (* 2 8))])
    (check-equal? 
     (s16vector-ref tgt i)
     (ptr-ref (stream-rec-buffer stream-info) _sint16 j)))
  (for ([i (in-range (* 2 200) (* 2 224))])
    (check-equal? (s16vector-ref tgt i) 0))
  
  ;; tests for call-buffer-filler
  (let () (define ptr-log empty)
    (define ftw-log empty)
    (define (bogus-buffer-filler cpointer frames-to-write)
      (set! ptr-log (cons cpointer ptr-log))
      (set! ftw-log (cons frames-to-write ftw-log)))
    (set-stream-rec-last-frame-read! stream-info 1000)
    (set-stream-rec-last-offset-read! stream-info (modulo (* 4 1000) buffer-bytes))
    (set-stream-rec-last-frame-written! stream-info 1500)
    (set-stream-rec-last-offset-written! stream-info 
                                         (modulo (* 4 1500) buffer-bytes))
    (call-buffer-filler stream-info bogus-buffer-filler)
    (check-equal? ptr-log (list (stream-rec-buffer stream-info)
                                (ptr-add (stream-rec-buffer stream-info)
                                         (* 4 1500))))
    (check-equal? ftw-log (list 1000
                                (- 2048 1500))))
  
  ;; check on 2nd iteration:
  (let () (define ptr-log empty)
    (define ftw-log empty)
    (define (bogus-buffer-filler cpointer frames-to-write)
      (set! ptr-log (cons cpointer ptr-log))
      (set! ftw-log (cons frames-to-write ftw-log)))
    (set-stream-rec-last-frame-read! stream-info 3048)
    (set-stream-rec-last-offset-read! stream-info (modulo (* 4 3048) buffer-bytes))
    (set-stream-rec-last-frame-written! stream-info 3548)
    (set-stream-rec-last-offset-written! stream-info 
                                         (modulo (* 4 3548) buffer-bytes))
    (call-buffer-filler stream-info bogus-buffer-filler)
    (check-equal? ptr-log (list (stream-rec-buffer stream-info)
                                (ptr-add (stream-rec-buffer stream-info)
                                         (* 4 1500))))
    (check-equal? ftw-log (list 1000
                                (- 2048 1500))))
  
  ;; check for reader got ahead of writer:
  (let () (define ptr-log empty)
    (define ftw-log empty)
    (define (bogus-buffer-filler cpointer frames-to-write)
      (set! ptr-log (cons cpointer ptr-log))
      (set! ftw-log (cons frames-to-write ftw-log)))
    ;; 1K frames after the beginning of the tenth go-round:
    (define read-frame (+ 1000 (* 10 buffer-frames)))
    (set-stream-rec-last-frame-read! stream-info read-frame)
    (set-stream-rec-last-offset-read! stream-info
                                      (modulo (* 4 read-frame) buffer-bytes))
    ;; writer fell way way behind:
    (set-stream-rec-last-frame-written! stream-info 14)
    (set-stream-rec-last-offset-written! stream-info 
                                         (modulo (* 4 14) buffer-bytes))
    (call-buffer-filler stream-info bogus-buffer-filler)
    (check-equal? ptr-log (list (stream-rec-buffer stream-info)
                                (ptr-add (stream-rec-buffer stream-info)
                                         (* 4 1000))))
    (check-equal? ftw-log (list 1000
                                (- buffer-frames 1000))))
  
  )))


