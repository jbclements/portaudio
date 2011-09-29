#lang racket

(require "../portaudio.rkt"
         "../portaudio-utils.rkt"
         "helpers.rkt"
         ffi/vector
         ffi/unsafe
         rackunit
         rackunit/text-ui)

(define twopi (* 2 pi))

(run-tests
(test-suite "portaudio"
(let ()
  
  (pa-maybe-initialize)
  
  (define (open-test-stream callback streaming-info-ptr buffer-frames)
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     buffer-frames ;;frames-per-buffer  ;; frames per buffer
     callback      ;; callback (NULL means just wait for data)
     streaming-info-ptr))
  
  (define (test-start) 
    (sleep 2)
    (printf "starting now... "))
  
  (define (test-end)
    (printf "... ending now.\n")
    (sleep 1))

  (define log-counter 0)
  (define log empty)
  (define log2 empty)
  
  (define srinv (exact->inexact (/ 1 44100)))
  (define (fill-buf ptr frames index)
    (unless (= frames 1024)
      (error 'fill-buf "expected 1024 frames, got ~s\n"
             frames))
    (define base-frames (* index 1024))
    (define base-t (exact->inexact (* base-frames srinv)))
    (for ([i (in-range 1024)])
      (define t (+ base-t (* i srinv)))
      (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* twopi t 403)))))))
      (define sample-idx (* channels i))
      (ptr-set! ptr _sint16 sample-idx sample)
      (ptr-set! ptr _sint16 (add1 sample-idx) sample))
    (sleep 0.015))
  
  ;; return #t if a buffer got written
  (define (maybe-fill-buf streaming-info-ptr)
    (match (buffer-if-waiting streaming-info-ptr)
      [#f (set! log (cons '_ log))
          #f]
      [(list ptr frames idx finished-thunk)
       (set! log (cons 1 log))
       (set! log-counter (+ log-counter 1))
       (fill-buf ptr frames idx)
       (finished-thunk)
       #t]))
  
  (let ()
    ;; map 0<rads<2pi to -pi/2<rads<pi/2
    (define (left-half rads)
      (cond [(<= rads (* 1/2 pi)) rads]
            [(<= rads (* 3/2 pi)) (- pi rads)]
            [else (- rads (* 2 pi))]))
    (define (frac n)
      (- n (floor n)))
    (define t1 (make-s16vector 2048 0))
    (time (fill-buf (s16vector->cpointer t1) 1024 3))
    (for ([i (in-range 2048)])
      (define t (floor (/ i 2)))
      (check-= (asin (* 5.0 (/ (s16vector-ref t1 i) 32767.0)))
               (left-half (* twopi (frac (* (/ 1 44100) (* (+ (* 3 1024) t) 403)))))
               1e-2)))
  
  
  ;; first just play silence; there's no process feeding the buffer
  (let ()
    (define buffer-frames 1024)
    (define stream-info (make-streamplay-record buffer-frames))
    (check-not-false (buffer-if-waiting stream-info))
    (define stream (open-test-stream streaming-callback
                                     stream-info
                                     buffer-frames))
    (printf "total silence\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 1.0)
    (pa-stop-stream stream)
    (test-end))
  
  (let ()
    (define buffer-frames 1024)
    (define stream-info (make-streamplay-record buffer-frames))
    (define stream (open-test-stream streaming-callback
                                     stream-info
                                     buffer-frames))
    (printf "tone at 403 Hz\n")
    (define wake-delay (* 1/4 (/ 1 (/ 44100 1024))))
    (define filling-thread
      (thread
       (lambda ()
         (maybe-fill-buf stream-info)
         (for ([i (in-range 1000)])
           (define pre-time (pa-get-stream-time stream))
           (maybe-fill-buf stream-info)
           (define post-time (pa-get-stream-time stream))
           (define time-taken (- post-time pre-time))
           (set! log2 (cons (max 0.001 (- wake-delay time-taken)) log2))
           #;(sleep wake-delay)
           (when (< 0 (- wake-delay time-taken))
               (sleep (- wake-delay time-taken)))
           #;(sleep (max 0.000 (- wake-delay time-taken)))))))
    (sleep 0.5)      
    (test-start)
    (pa-start-stream stream)
    (sleep 1.0)
    (pa-stop-stream stream)
    (test-end)
    (kill-thread filling-thread)
    (printf "log2: \n~s\n" log2)
    (printf "log: \n~s\n" log)
    (printf "fails: ~s\n" (stream-fails stream-info))
    (check-equal? log-counter 44))
  
  
  ;; first test with the vector interface:
  #;(let ()
    (define abort-box (box #f))
    (define callback-info (make-sndplay-record tone-buf-330))
    (define stream (open-test-stream copying-callback callback-info))
    (printf "1/2 second @ 330 Hz\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 0.5)
    (test-end))
  

  )))


