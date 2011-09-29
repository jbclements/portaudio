#lang racket

(require "helpers.rkt"
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
    (ffi-lib (build-path libs (system-library-subpath) "copying-callbacks")))
  
#|  typedef struct soundStreamInfo{
  int   bufferFrames;
  short *buffers[STREAMBUFS];
  // mutated by racket only:
  int   bufNumbers[STREAMBUFS];
  // mutated by callback only:
  int   lastUsed;
  int   stopNow;
} soundStreamInfo;|#

  (define streambufs 4)
  (define-cstruct _sound-stream-info
    ([buffer-frames _int]
     [buffers (_array _pointer streambufs)]
     [buf-numbers (_array _int streambufs)]
     [last-used _int]
     [stop-now _int]
     [already-stopped? _pointer]
     [fault-count _int]))
  
  (define streaming-callback
    (get-ffi-obj "streamingCallback"
                 callback-lib
                 (_fun 
                  (_pointer = #f)
                  _pointer
                  _ulong
                  (_pointer = #f)
                  (_ulong = 0)
                  _sound-stream-info-pointer
                  -> _int)))
  
  (define create-stream-info
    (get-ffi-obj "createSoundStreamInfo"
                 callback-lib
                 (_fun _int _pointer -> _sound-stream-info-pointer)))
  
  (define stream-info (create-stream-info 
                       1024
                       (malloc-immobile-cell #f)))
  
  (check-equal? (sound-stream-info-buffer-frames stream-info)
                1024)
  (check-equal? (sound-stream-info-last-used stream-info) -1)
  (check-equal? (sound-stream-info-stop-now stream-info) 0)
  (check-equal? (for/list ([i (in-range streambufs)])
                  (array-ref (sound-stream-info-buf-numbers
                              stream-info)
                             i))
                '(-1 -1 -1 -1))
  (check-equal? (ptr-ref (sound-stream-info-already-stopped? stream-info)
                         _scheme)
                #f)
  (check-equal? (sound-stream-info-fault-count stream-info) 0)
  
  ;; randomize all the buffers
  (for ([i (in-range streambufs)])
    (define buf (array-ref (sound-stream-info-buffers stream-info) i))
    (for ([j (in-range (* channels 1024))])
      (ptr-set! buf _sint16 j (random 100))))
  (define tgt (make-s16vector (* channels 1024) 1))
  
  ;; wrong number of frames:
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 14
                 stream-info)
                pa-abort)
  (check-equal? (ptr-ref (sound-stream-info-already-stopped? stream-info)
                         _scheme) 
                #t)
  ;; it's now been freed....
  (set! stream-info #f)
  
  (define stream-info-2 (create-stream-info 1024 
                                          (malloc-immobile-cell #f)))
  
  ;; buffer-not ready yet:
  (set-sound-stream-info-last-used! stream-info-2 1025)
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 1024
                 stream-info-2)
                pa-continue)
  (check-equal? (sound-stream-info-last-used stream-info-2) 1026)
  (check-equal? (sound-stream-info-fault-count stream-info-2) 1)
  (for ([i (in-range (* channels 1024))])
    (check-equal? (s16vector-ref tgt i) 0))
  
  ;; buffer ready:
  (set-sound-stream-info-last-used! stream-info-2 1025)
  (array-set! (sound-stream-info-buf-numbers stream-info-2) 2 1026)
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 1024
                 stream-info-2)
                pa-continue)
  (check-equal? (sound-stream-info-last-used stream-info-2) 1026)
  (let ()
    (define buf (array-ref (sound-stream-info-buffers stream-info-2) 2))
    (for ([i (in-range (* channels 1024))])
      (check-equal? (s16vector-ref tgt i)
                    (ptr-ref buf _sint16 i))))
  
  ;; stop-now
  (set-sound-stream-info-stop-now! stream-info-2 1)
  (check-equal? (streaming-callback
                 (s16vector->cpointer tgt)
                 1024
                 stream-info-2)
                pa-abort)
  (check-equal? (ptr-ref (sound-stream-info-already-stopped? stream-info-2)
                         _scheme) 
                #t)


  )))


