#lang racket


(require "portaudio.rkt"
         "portaudio-utils.rkt"
         ffi/vector
         rackunit
         rackunit/text-ui)

(printf "abc\n")
(pa-maybe-initialize)
(printf "def\n")

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

(define response-channel (make-channel))

 (define (test-start) 
    (sleep 2)
    (check-equal? (channel-try-get response-channel) #f)
    (printf "starting now... \n"))
  
  (define (test-end)
    (printf " ... ending now.\n")
    (sleep 1))

    
  (define tone-buf-330 (make-tone-buf 330 (* 2 44100)))

 (let ()
    (define abort-box (box #f))
    (printf "2 seconds @ 330 Hz\n")
   (test-start)
   (pa-start-stream (open-test-stream (make-copying-callback tone-buf-330
                             response-channel
                             abort-box)))
   (sleep 1.0)
   (collect-garbage)
   (collect-garbage)
   (collect-garbage)
   (collect-garbage)
   (sleep 1.0)
   (test-end))
 
 
 
 
