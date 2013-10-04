#lang racket

(require "../portaudio.rkt"
         "../callback-support.rkt"
         "helpers.rkt"
         rackunit
         rackunit/text-ui
         ffi/unsafe)

(define twopi (* 2 pi))

(run-tests
(test-suite "portaudio"
(let ()
  
  (check-not-exn (lambda () (pa-get-version)))
  (check-not-exn (lambda () (pa-get-version-text)))
  
  ;; this can change, as long as it's something sensible:
  (check-equal? (pa-get-error-text 'paBufferTooBig) "Buffer too big")
  
  (check-equal? (pa-terminate-completely) #t)
  (check-exn (lambda (exn) (string=? (exn-message exn) "PortAudio not initialized"))
             (lambda () (pa-get-host-api-count)))
  (check-exn (lambda (exn) (string=? (exn-message exn) "PortAudio not initialized"))
             (lambda () (pa-get-default-host-api)))

  (check-equal? (pa-initialized?) #f)
  (check-not-exn (lambda () (pa-initialize)))
  (check-equal? (pa-initialized?) #t)
  (check-not-exn (lambda () (pa-terminate)))
  (check-equal? (pa-initialized?) #f)
  (check-not-exn (lambda () (pa-initialize)))
  (check-not-exn (lambda () (pa-initialize)))
  (check-not-exn (lambda () (pa-initialize)))
  (check-not-exn (lambda () (pa-initialize)))
  (check-equal? (pa-initialized?) #t) 
  (check-not-exn (lambda () (pa-terminate)))
  (check-equal? (pa-initialized?) #t)
  (check-equal? (pa-terminate-completely) #t)
  (check-not-exn (lambda () (pa-initialize)))
  
  ;; on different platforms, the results will be different....
  (check < 0 (pa-get-host-api-count))
  (pa-get-host-api-count)
  (check <= 0 (pa-get-default-host-api))
  (printf "default-host-api: ~v\n" (pa-get-default-host-api))
  (check-not-exn (lambda () (pa-get-host-api-info 0)))
  (check-exn exn:fail? 
             (lambda () (pa-get-host-api-info (+ 14 (pa-get-host-api-count)))))
  
  (printf "pa-version-text: ~s" (pa-get-version-text))
  (printf "devices available: ~s\n" (pa-get-device-count))
  (printf "info on available devices: ~s\n" (available-devices-info))
  (printf "default device: ~s\n" (default-device-info))
  

  ;; the remainder of these require 2-channel output
  
  (pa-maybe-initialize)
  
  (define (open-test-stream callback closure-info-ptr)
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     1000          ;;frames-per-buffer  ;; frames per buffer
     callback      ;; callback (NULL means just wait for data)
     closure-info-ptr))
  
  (define (test-start) 
    (sleep 2)
    (printf "starting now... "))
  
  (define (test-end)
    (printf "... ending now.\n")
    (sleep 1))
  
    
  (define tone-buf-330 (make-tone-buf 330 22050))
  
  
  ;; test how many open streams a platform supports....
  #;(let loop ([i 0])
    (printf "iter: ~v\n" i)
    (define abort-box (box #f))
    (define tone-buf-330-10 (make-tone-buf 330 (* 10 44100)))
    (define callback-info (make-copying-info tone-buf-330-10 0 #f))
    (define stream (open-test-stream copying-callback callback-info))
    (pa-set-stream-finished-callback stream copying-info-free)
    (printf "10 seconds @ 330 Hz\n")
    (pa-start-stream stream)
    (sleep 0.25)
    (loop (add1 i)))
  
  ;; first, test the copying-callback.
  (let ()
    (define abort-box (box #f))
    (define callback-info (make-copying-info tone-buf-330 0 #f))
    (define stream (open-test-stream copying-callback callback-info))
    (pa-set-stream-finished-callback stream copying-info-free)
    (check-equal? (pa-stream-stopped? stream) #t)
    (check-equal? (pa-stream-active? stream) #f)
    (printf "1/2 second @ 330 Hz\n")
    (test-start)
    (pa-start-stream stream)
    (check-equal? (pa-stream-stopped? stream) #f)
    (check-equal? (pa-stream-active? stream) #t)
    (sleep 0.5)
    (check-not-exn (lambda () (pa-get-stream-time stream)))
    (sleep 0.5)
    (test-end)
    (check-equal? (pa-stream-active? stream) #f))
  
  (define tone-buf-380 (make-tone-buf 380 22050))
  (define longlongtone (make-tone-buf 380 (* 30 44100)))
  
  ;; two simultaneous streams, 330 & 380 Hz
  (let ()
    (define stream-1 (open-test-stream 
                      copying-callback
                      (make-copying-info tone-buf-330 0 #f)))
    (pa-set-stream-finished-callback stream-1 copying-info-free)
    (define stream-2 (open-test-stream
                      copying-callback
                      (make-copying-info tone-buf-380 0 #f)))
    (pa-set-stream-finished-callback stream-2 copying-info-free)
    (printf "1/2 second @ 330 & 380 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (pa-start-stream stream-2)
    (sleep 0.5)
    (test-end))

  ;; a 10-second tone  
  (define longer-tone-buf (make-tone-buf 440 441000))
  
  ;; ending a stream with stop-stream
  (let ()
    (define info (make-copying-info longer-tone-buf 0 #f))
    (define stream-1 (open-test-stream 
                      copying-callback
                      info))
    (pa-set-stream-finished-callback stream-1 copying-info-free)
    (printf "1/2 second @ 440 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (sleep 0.5)
    (pa-stop-stream stream-1)
    (sleep 0.1)
    (check-equal? (pa-stream-active? stream-1) #f)
    (pa-close-stream stream-1)
    (test-end))
  
  ;; try closing a stream a sound 3 times:
  (let ()
    (define info (make-copying-info longer-tone-buf 0 #f))
    (define stream-1 (open-test-stream 
                      copying-callback
                      info))
    (pa-set-stream-finished-callback stream-1 copying-info-free)
    (printf "1/2 second @ 440 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (sleep 0.5)
    (pa-close-stream stream-1)
    (pa-close-stream stream-1)
    (pa-close-stream stream-1)
    (test-end))
  
  ;; try stopping a sound that's already over:
  (let ()
    (define info (make-copying-info tone-buf-380 0 #f))
    (define stream-1 (open-test-stream 
                      copying-callback
                      info))
    (pa-set-stream-finished-callback stream-1 copying-info-free)
    (printf "1/2 second @ 380 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (sleep 1.0)
    (pa-stop-stream stream-1)
    (test-end))
  
  
  #;(let ()
    (define info (make-copying-info longlongtone 0 #f))
    (define stream-1 (open-test-stream 
                      copying-callback
                      info))
    (pa-set-stream-finished-callback stream-1 copying-info-free)
    (printf "30 seconds @ 380 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (test-end))
  
  ;; tests for stream-play and s16vec-play....
  ;; check for wrong size buffer
  
  #;(let ()
    (define abort-box (box #f))
    (define (signal t)
      (* 0.1 (sin (* t 1/44100 2 pi 410))))
    (define callback
      ;; wrong buffer length:
      (make-generating-callback signal 2000 response-channel abort-box))
    (define stream (open-test-stream callback))
    (pa-start-stream stream)
    (sleep 0.25)
    ;; should auto-abort:
    (check-pred (lambda (exn) (and (exn:fail? exn)
                                   (regexp-match #px"callback wanted 1000 frames"
                                                 (exn-message exn))))
                (channel-try-get response-channel))
    (check-equal? (channel-try-get response-channel) #f))
  
  ;; check for signal that fails
  #;(let ()
    (define abort-box (box #f))
    (define (signal t)
      (error 'signal "I'm a bad signal."))
    (define callback
      (make-generating-callback signal 1000 response-channel abort-box))
    (define stream (open-test-stream callback))
    (pa-start-stream stream)
    (sleep 0.25)
    ;; should auto-abort:
    (check-pred (lambda (exn) 
                  (and (exn:fail? exn)
                       (regexp-match #px"I'm a bad signal"
                                     (exn-message exn))))
                (channel-try-get response-channel))
    (check-equal? (channel-try-get response-channel) #f))
  
  ;; check for signal that stalls (should play brief burst of static)
  #;(let ()
    (define abort-box (box #f))
    (define (signal t)
      ;; sleep essentially forever:
      (sleep 10))
    (define callback
      (make-generating-callback signal 1000 response-channel abort-box))
    (define stream (open-test-stream callback))
    (printf "short burst of static\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 0.075)
    (set-box! abort-box #t)
    (test-end))

  

  )))
