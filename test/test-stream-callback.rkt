#lang racket

(require "helpers.rkt"
         "../portaudio-utils.rkt"
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
  
  (define callback-lib
    (ffi-lib (build-path libs (system-library-subpath) "callbacks")))
  
  (define streambufs 4)
  (define-cstruct _stream-rec
    ([buffer-frames _int]
     [buffers (_array _pointer streambufs)]
     [buf-numbers (_array _int streambufs)]
     [last-used _int]
     [fault-count _int]
     [mzrt-sema _pointer]))
  
  (define streaming-callback
    (get-ffi-obj "streamingCallback"
                 callback-lib
                 (_fun 
                  (_pointer = #f)
                  _pointer
                  _ulong
                  (_pointer = #f)
                  (_ulong = 0)
                  _stream-rec-pointer
                  -> _int)))
  
  
  (match-define
    (list stream-info place-channel) 
    (make-streamplay-record 1024))
  
  (check-equal? (stream-rec-buffer-frames stream-info)
                1024)
  (check-equal? (stream-rec-last-used stream-info) -1)
  (check-equal? (for/list ([i (in-range streambufs)])
                  (array-ref (stream-rec-buf-numbers
                              stream-info)
                             i))
                '(-1 -1 -1 -1))
  (check-equal? (stream-rec-fault-count stream-info) 0)
  
  ;; randomize all the buffers
  (for ([i (in-range streambufs)])
    (define buf (array-ref (stream-rec-buffers stream-info) i))
    (for ([j (in-range (* channels 1024))])
      (ptr-set! buf _sint16 j (random 100))))
  (define tgt (make-s16vector (* channels 1024) 1))
  
  ;; wrong number of frames:
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 14
                 stream-info)
                pa-abort)
  ;; it's now been freed....
  (set! stream-info #f)
  
  (match-define (list stream-info-2 place-channel-2)
    (make-streamplay-record 1024))
  
  ;; buffer-not ready yet:
  (set-stream-rec-last-used! stream-info-2 1025)
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 1024
                 stream-info-2)
                pa-continue)
  (check-equal? (stream-rec-last-used stream-info-2) 1026)
  (check-equal? (stream-rec-fault-count stream-info-2) 1)
  (for ([i (in-range (* channels 1024))])
    (check-equal? (s16vector-ref tgt i) 0))
  
  ;; buffer ready:
  (set-stream-rec-last-used! stream-info-2 1025)
  (array-set! (stream-rec-buf-numbers stream-info-2) 2 1026)
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 1024
                 stream-info-2)
                pa-continue)
  (check-equal? (stream-rec-last-used stream-info-2) 1026)
  (let ()
    (define buf (array-ref (stream-rec-buffers stream-info-2) 2))
    (for ([i (in-range (* channels 1024))])
      (check-equal? (s16vector-ref tgt i)
                    (ptr-ref buf _sint16 i))))
  

  )))


