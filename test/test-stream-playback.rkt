#lang racket

(require "../portaudio.rkt"
         "../callback-support.rkt"
         "helpers.rkt"
         ffi/vector
         ffi/unsafe
         rackunit
         rackunit/text-ui)

(define twopi (* 2 pi))

(run-tests
(test-suite "low-level stream playbackt"
(let ()
  
  (pa-maybe-initialize)
  
  (define (open-test-stream callback streaming-info-ptr)
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     0             ;;frames-per-buffer -- let the system decide
     callback      ;; callback (NULL means just wait for data)
     streaming-info-ptr))
  
  (define (test-start) 
    #;(sleep 2)
    (printf "starting now... "))
  
  (define (test-end)
    (printf "... ending now.\n")
    (sleep 1))

  (define log-counter 0)
  (define log empty)
  (define log2 empty)
  (define log3 empty)
  
  (define srinv (exact->inexact (/ 1 44100)))
  (define (fill-buf ptr frames start-frame)
    (define base-t (exact->inexact (* start-frame srinv)))
    (for ([i (in-range frames)])
      (define t (+ base-t (* i srinv)))
      (define sample (inexact->exact (round (* 32767 (* 0.2 (sin (* twopi t 403)))))))
      (define sample-idx (* channels i))
      (ptr-set! ptr _sint16 sample-idx sample)
      (ptr-set! ptr _sint16 (add1 sample-idx) sample)))
  
  #;(define (call-fill-buf streaming-info-ptr)
    (match (buffer-if-waiting streaming-info-ptr)
      [#f ;; oops, probably stacked up signals. go wait again.
       #f]
      [(list ptr frames idx finished-thunk)
       (set! log-counter (+ log-counter 1))
       (fill-buf ptr frames idx)
       (finished-thunk)]))
  
  (let ()
    ;; map 0<rads<2pi to -pi/2<rads<pi/2
    (define (left-half rads)
      (cond [(<= rads (* 1/2 pi)) rads]
            [(<= rads (* 3/2 pi)) (- pi rads)]
            [else (- rads (* 2 pi))]))
    (define (frac n)
      (- n (floor n)))
    (define t1 (make-s16vector 2048 0))
    (time (fill-buf (s16vector->cpointer t1) 1024 2789))
    (for ([i (in-range 2048)])
      (define t (floor (/ i 2)))
      (check-= (asin (* 5.0 (/ (s16vector-ref t1 i) 32767.0)))
               (left-half (* twopi (frac (* (/ 1 44100) (* (+ 2789 t) 403)))))
               1e-2)))
  
  
  ;; first just play silence; there's no process feeding the buffer
  #;(let ()
    (define buffer-frames 2048)
    (match-define stream-info (make-streaming-info buffer-frames))
    (define stream (open-test-stream streaming-callback
                                     stream-info))
    (pa-set-stream-finished-callback stream streaming-info-free)
    (printf "total silence\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 1.0)
    ;; check 3x stop:
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (test-end)
    (printf "faults: ~s\n" (stream-fails stream-info)))
  
  ;; test how a 20ms wakeup performs
  (let ()
    (define buffer-frames 2048)
    (match-define stream-info (make-streaming-info buffer-frames))
    (define stream (open-test-stream streaming-callback
                                     stream-info))
    (pa-set-stream-finished-callback stream streaming-info-free)
    (printf "total silence\n")
    (test-start)
    (pa-start-stream stream)
    (define log empty)
    (thread 
     (lambda ()
       (for ([i (in-range 200)])
       (set! log (cons (stream-rec-last-frame-read stream-info) log))
       (sleep 0.02))))
    (sleep 4.0)
    ;; check 3x stop:
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (test-end)
    (define log2 (reverse log))
    (printf "faults: ~s\n" (stream-fails stream-info))
    (define diffs (for/list ([j (in-list (rest log2))]
                             [i (in-list log2)])
                    (- j i)))
    (define mean t(/ (apply + diffs) (length diffs)))
    (printf "diffs: ~s\n" diffs)
    (printf "mean delay: ~s\n" (exact->inexact mean)))
  
  ;; try playing a looped buffer without dynamic filling:
  (let ()
    (define buffer-frames 22050)
    (match-define stream-info (make-streaming-info buffer-frames))
    (define stream (open-test-stream streaming-callback stream-info))
    (pa-set-stream-finished-callback stream streaming-info-free)
    (printf "tone, hiccup every half-second \n")
    ;; manipulate the stream-info to pretend that a lot of info is there:
    (fill-buf (stream-rec-buffer stream-info) 22050 0)
    (set-stream-rec-last-frame-written! stream-info (* 44100 2))
    (set-stream-rec-last-offset-written! stream-info 0)
    (test-start)
    (pa-start-stream stream)
    (sleep 5.0)
    ;; check 3x stop:
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (test-end)
    (printf "faults: ~s\n" (stream-fails stream-info)))
  
  ;; try playing a tone at 403 Hz:
  #;(let ()
    (define buffer-frames 1024)
    (match-define (list stream-info signal-channel)
      (make-streaming-info buffer-frames))
    (define stream (open-test-stream streaming-callback
                                     stream-info
                                     buffer-frames))
    (pa-set-stream-finished-callback stream streaming-info-free)
    (printf "tone at 403 Hz\n")
    (define filling-thread
      (thread
       (lambda ()
         (call-fill-buf stream-info)
         (for ([i (in-range 1000)])
           (place-channel-get signal-channel)
           (set! log2 (cons (pa-get-stream-time stream) log2))
           (call-fill-buf stream-info)))))
    (test-start)
    (pa-start-stream stream)
    (sleep 1.0)
    (place-kill signal-channel)
    ;; check 3x stop:
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (pa-maybe-stop-stream stream)
    (test-end)
    (kill-thread filling-thread)
    
    (define diffs (for/list ([j (in-list (rest log2))]
                             [i (in-list log2)])
                    (- i j)))
    (printf "log2 diffs: \n~s\n" diffs)
    (printf "log: \n~s\n" log)

    (printf "fails: ~s\n" (stream-fails stream-info))
    (check-equal? log-counter 44))
  
  
  

  )))


