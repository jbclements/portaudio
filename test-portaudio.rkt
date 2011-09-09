#lang racket

(require "portaudio.rkt"
         "portaudio-utils.rkt"
         ffi/vector
         rackunit
         rackunit/text-ui)

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
  (check <= 0 (pa-get-default-host-api))
  (check-not-exn (lambda () (pa-get-host-api-info 0)))
  (check-exn exn:fail? 
             (lambda () (pa-get-host-api-info (+ 14 (pa-get-host-api-count)))))
  

  
  ;; the remainder of these require 2-channel output
  
  (pa-maybe-initialize)
  
  (define (open-test-stream callback)
    (pa-open-default-stream
     0             ;; input channels
     2             ;; output channels
     'paInt16      ;; sample format
     44100.0       ;; sample rate
     1000          ;;frames-per-buffer  ;; frames per buffer
     callback ;; callback (NULL means just wait for data)
     #f))
  
  (define (make-tone-buf pitch frames)
    (define result (make-s16vector (* frames channels)))
    (for ([i (in-range 0 frames)])
      (define sample 
        (inexact->exact 
         (round (* s16max 0.1 (sin 
                               (* i 1/44100 pitch 2 pi))))))
      (s16vector-set! result (* i 2) sample)
      (s16vector-set! result (add1 (* i 2)) sample))
    result)
  
  (define channels 2)
  (define s16max 32767)
  
  ;; first, test the copying-callback.

  (define response-channel (make-channel))
  
  (define (check-for-finished)
    (check-for-finished/n 1))
  
  ;; check that there are 'n' 'finished' symbols waiting in 
  ;; the response channel.
  (define (check-for-finished/n n)
    (for ([i (in-range n)])
      (check-equal? (channel-try-get response-channel) 'finished))
    (check-equal? (channel-try-get response-channel) #f))
  
  (define (test-start) 
    (sleep 2)
    (check-equal? (channel-try-get response-channel) #f)
    (printf "starting now... "))
  
  (define (test-end)
    (printf "... ending now.\n")
    (sleep 1))
  
    
  (define tone-buf-330 (make-tone-buf 330 22050))
  
  ;; first test with the vector interface:
  (let ()
    (define abort-box (box #f))
    (define callback
      (make-copying-callback tone-buf-330
                             response-channel
                             abort-box))
    (define stream (open-test-stream callback))
    (printf "1/2 second @ 330 Hz\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 0.5)
    (test-end)
    (check-for-finished))

  ;; second test with the cpointer interface:
  (let ()
    (define abort-box (box #f))
    (define callback
      (make-copying-callback/cpointer
       (s16vector->cpointer tone-buf-330)
       22050
       response-channel
       abort-box))
    (define stream (open-test-stream callback))
    (printf "1/2 second @ 330 Hz\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 0.5)
    (test-end)
    (check-for-finished))
  
  (define tone-buf-380 (make-tone-buf 380 22050))
  
  ;; two simultaneous streams, 330 & 380 Hz
  (let ()
    (define stream-1 (open-test-stream 
                      (make-copying-callback
                       tone-buf-330
                       response-channel 
                       (box #f))))
    (define stream-2 (open-test-stream
                      (make-copying-callback 
                       tone-buf-380
                       response-channel 
                       (box #f))))
    (printf "1/2 second @ 330 & 380 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (pa-start-stream stream-2)
    (sleep 0.5)
    (test-end)
    (check-for-finished/n 2))
  
  ;; ending a stream with the abort-box
  (let ()
    ;; a 10-second tone
    (define abort-box (box #f))
    (define longer-tone-buf (make-tone-buf 440 441000))
    (define stream-1 (open-test-stream 
                      (make-copying-callback
                       longer-tone-buf
                       response-channel 
                       abort-box)))
    (printf "1/2 second @ 440 Hz\n")
    (test-start)
    (pa-start-stream stream-1)
    (sleep 0.5)
    (set-box! abort-box #t)
    (test-end)
    (check-for-finished))
  
  ;; GENERATING CALLBACKS
  (let ()
    (define abort-box (box #f))
    (define (signal t)
      (* 0.1 (sin (* t 1/44100 2 pi 410))))
    (define callback
      (make-generating-callback signal 1000 response-channel abort-box))
    (define stream (open-test-stream callback))
    (printf "1/2 second @ 410 Hz\n")
    (test-start)
    (pa-start-stream stream)
    (sleep 0.5)
    (set-box! abort-box #t)
    (test-end)
    (check-for-finished))
  
  ;; check for wrong size buffer
  
  (let ()
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
  (let ()
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
  
  ;; check for signal that stalls
  3
  )))

#|;; multiple streams? 

(define (tone-1 t)
  (* 0.1 (sin (* t 1/44100 440 twopi))))

(define tone-1-callback ((make-generating-callback tone-1) (make-channel)))

(define stream-1
  (pa-open-default-stream
                0             ;; input channels
                2             ;; output channels
                'paInt16      ;; sample format
                44100.0       ;; sample rate
                1000          ;;frames-per-buffer  ;; frames per buffer
                tone-1-callback ;; callback (NULL means just wait for data)
                #f))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(pa-start-stream stream-1)

(sleep 3)

(define (tone-2 t)
  (* 0.1 (sin (* t 1/44100 550 twopi))))

(define tone-2-callback ((make-generating-callback tone-2) (make-channel)))


(define stream-2
  (pa-open-default-stream
                0             ;; input channels
                2             ;; output channels
                'paInt16      ;; sample format
                44100.0       ;; sample rate
                1000          ;;frames-per-buffer  ;; frames per buffer
                tone-2-callback ;; callback (NULL means just wait for data)
                #f))

(pa-start-stream stream-2)

(sleep 3)
|#

