#lang racket

(require ffi/vector
         (except-in ffi/unsafe ->))


(define (frames? n)
  (and (exact-integer? n)
       (<= 0 n)))

(provide/contract 
 [make-copying-callback (-> s16vector? channel? box? any)]
 ;; UNSAFE:
 [make-copying-callback/cpointer (-> cpointer? frames? channel? box? any)]
 [make-generating-callback (-> procedure? frames? channel? box? any)]
 [make-block-generating-callback (-> procedure? channel? box? any)])

;; all of these functions assume 2-channel-interleaved 16-bit input:
(define channels 2)
(define s16max 32767)
(define s16-bytes 2)


(define-cstruct _rack-audio-closure
  ([next-data _pointer]
   [stop      _pointer]
   [condition _int]))

(define feeder-lib (ffi-lib (build-path libs "feeder.dylib")))

;; in order to get a raw pointer to pass back to C, we declare 
;; the function pointer as being a simple array of data:
(define c-copying-callback-ptr
  (array-ptr (get-ffi-obj "copyingCallback" feeder-lib (_array _uint64 1))))


;; UTILITIES:

;; spawn a new thread to put a value on a synchronous channel
(define (channel-put/async channel val)
  (thread (lambda () 
            (channel-put channel val))))

;; create a callback that supplies frames from an s16vector.
(define (make-copying-callback s16vec response-channel abort-box)
  (define s16vec-len (s16vector-length s16vec))
  (unless (= (modulo s16vec-len 2) 0)
    (raise-type-error 'make-copying-callback
                      "vector of length divisible by two"
                      0
                      s16vec
                      response-channel
                      abort-box))
  (make-copying-callback/cpointer (s16vector->cpointer s16vec) 
                                  (/ s16vec-len 2)
                                  response-channel
                                  abort-box))

;; create a callback that supplies frames from a buffer, using memcpy.
;; UNSAFE! : give a very large value for total-frames and watch it play memory
;; then crash.
(define (make-copying-callback/cpointer master-buffer total-frames)
  (define sample-offset 0) ;; mutable, to track through the buffer
  (define total-samples (* total-frames channels))
  (define end-pointer (ptr-add master-buffer total-samples _sint16))
  (define closure-info (make-rack-audio-closure master-buffer end-pointer #f))
  closure-info)

;; create a callback that creates frames by calling a signal repeatedly.
;; note that we don't bother checking to see whether the buffer is successfully
;; filled.
(define (make-generating-callback signal buffer-frames response-channel abort-box)
  (define signal-exn-box (box #f))
  (define-values (filled-buffer semaphore)
    (start-filler-thread signal buffer-frames signal-exn-box abort-box))
  (define buffer-samples (* buffer-frames channels))
  ;; allow the first buffer-fill to happen:
  (semaphore-post semaphore)
  (define callback-holding-box (box #f))
  (define (the-callback input output frame-count time-info status-flags user-data)
    ;; the following code is believed not to be able to raise any errors.
    ;; if this is wrong, racket will die with abort().
    
    ;; don't run if abort is set:
    (cond [(unbox abort-box)
           (define response
             (or (unbox signal-exn-box) 'finished))
           (channel-put/async response-channel response)
           (set-box! callback-holding-box #f)
           'pa-abort]
          ;; don't run if we get the wrong number of frames requested:
          [(not (= frame-count buffer-frames))
           (channel-put/async
            response-channel
            (exn:fail
             (format
              "make-generating-callback: callback wanted ~s frames instead of expected ~s." 
              frame-count
              buffer-frames)
             (current-continuation-marks)))
           (set-box! callback-holding-box #f)
           'pa-abort]
          ;; otherwise, copy and release the semaphore to generate again.
          [else
           (memcpy output
                   0
                   filled-buffer
                   0
                   buffer-samples
                   _sint16)
           (semaphore-post semaphore)
           'pa-continue]))
  (set-box! callback-holding-box the-callback)
  the-callback)


;; this thread is not run as a callback. That way, if the user's signal
;; misbehaves, it won't destroy the audio engine. It fills 
;; the output buffer once for each post to the semaphore.
(define (start-filler-thread signal buffer-frames signal-exn-box abort-box)
  (define signal-semaphore (make-semaphore))
  (define buffer (make-s16vector (* channels buffer-frames)))
  (define cpointer (s16vector->cpointer buffer))
  (define frame-offset 0)
  (thread
   (lambda ()
     (let loop ()
       (semaphore-wait signal-semaphore)
       (with-handlers ([(lambda (exn) #t)
                        (lambda (exn)
                          (set-box! signal-exn-box exn)
                          (set-box! abort-box #t)
                          ;; fall off the end of the thread:
                          (void))])
         (for ([t (in-range frame-offset (+ frame-offset buffer-frames))]
               [i (in-range 0 (* 2 buffer-frames) 2)])
           (define sample 
             (inexact->exact 
              (round (* s16max (min 1.0 (max -1.0 (signal t)))))))
           (ptr-set! cpointer _sint16 i sample)
           (ptr-set! cpointer _sint16 (+ i 1) sample))
         (set! frame-offset (+ frame-offset buffer-frames))
         (loop)))))
  (values cpointer signal-semaphore))

;; create a callback that creates frames by passing a cblock to a function
(define (make-block-generating-callback signal/block/s16 response-channel abort-box)
  (define frame-offset 0)
  (define callback-holding-box (box #f))
  (define (the-callback input output frame-count time-info status-flags user-data)
    (cond [(unbox abort-box) 'pa-abort]
          ;; MUST NOT ALLOW AN EXCEPTION TO ESCAPE.
          [else (with-handlers ([(lambda (exn) #t)
                                 (lambda (exn)
                                   (channel-put/async response-channel exn)
                                   'pa-abort)])
                  (define keep-running? 
                    (signal/block/s16 output frame-offset frame-count))
                  (set! frame-offset (+ frame-offset frame-count))
                  (cond [keep-running? 'pa-continue]
                        [else (channel-put/async response-channel 'finished)
                              (set-box! callback-holding-box #f)
                              'pa-complete]))]))
  (set-box! callback-holding-box the-callback)
  the-callback)

;; we need to hold on to the callbacks, so they don't get collected while
;; the streams are still being processed.
(define callback-boxes (list))


