#lang racket/base

;; manage devices and the selection thereof across platforms

(require "portaudio.rkt"
         racket/match
         racket/bool
         racket/contract)

(provide (contract-out 
          [default-host-api (->  symbol?)]
          [all-host-apis (-> (listof symbol?))]
          [host-api (parameter/c (or/c false? symbol?))]
          [output-device (parameter/c (or/c false? nat?))]
          [find-output-device (-> number? nat?)]
          [device-low-output-latency (-> nat? number?)]
          [default-input-device-channels (-> nat?)]))

;; can't put contract on it, or can't use in teaching languages:
(provide set-host-api!
         set-output-device!
         display-device-table)


(define nat? exact-nonnegative-integer?)

;; default-host-api : 
;; return the symbol associated with the host API
(define (default-host-api)
  (define host-api-index (pa-get-default-host-api))
  (pa-host-api-info-type (pa-get-host-api-info host-api-index)))

;; all-host-apis : return the symbols associated with supported host APIs
;; enumerate the symbols associated with the supported APIs
;; this list is in order of the API indexes, so, for instance, if element
;; 3 of the list is 'foo, then API 'foo has index 3.
(define (all-host-apis)
  (for/list ([i (in-range (pa-get-host-api-count))])
    (pa-host-api-info-type (pa-get-host-api-info i))))

;; host-api : a parameter used when opening streams, to determine which
;; api to use; false indicates that no value has been set, and the default
;; will be used (we can't set it beforehand, because you can't check
;; the default before initializing portaudio).
(define host-api 
  (make-parameter 
   #f
   (lambda (new-val)
     (cond [(not (or (false? new-val)
                     (member new-val (all-host-apis))))
            (raise-argument-error 'host-api "false or host api symbol"
                                  0 new-val)]
           [else new-val]))))

;; set-host-api! : a version of the parameter setter that you can use
;; in a beginner language
(define (set-host-api! host-api-str)
  (cond [(false? host-api-str) (host-api host-api-str)]
        [else (host-api (string->symbol host-api-str))])
  host-api-str)

;; output-device : a parameter used when opening streams, to determine which
;; device to use for an output stream; false indicates that no value has been set, and the default
;; will be used (we can't set it beforehand, because you can't check
;; the default before initializing portaudio). Note that this supersedes
;; the host-api parameter
(define output-device
  (make-parameter
   #f
   (lambda (new-val)
     (define device-info-count (pa-get-device-count))
     (cond [(not (or (false? new-val)
                     ;; might have to disable this if you can't check
                     ;; the # of devices before pa-initialize...
                     (and (exact-nonnegative-integer? new-val)
                          (< new-val device-info-count))))
            (raise-argument-error 'host-api (format "false or device index in [0,~a)"
                                                device-info-count)
                                  0 new-val)]
           [else new-val]))))

;; set-output-device! : a setter for the output-device parameter that can be 
;; used in a teaching language
(define (set-output-device! index)
  (output-device index)
  index)

;; find-output-device : number -> number
;; return a device from the current host api with the given latency, using
;; the default, if possible
(define (find-output-device latency)
  (cond [(output-device) (output-device)]
        [else
         (define selected-host-api (or (host-api) (default-host-api)))
         (define reasonable-devices 
           (low-latency-output-devices latency selected-host-api))
         (when (null? reasonable-devices)
           (error 'stream-choose "no devices available in current API ~s with ~sms latency or less."
                  selected-host-api
                  (* 1000 latency)))
         (define default-output-device (host-api-default-output-device 
                                        selected-host-api))
         ;; choose the default if it matches the spec:
         (cond [(member default-output-device reasonable-devices) 
                default-output-device]
               [else 
                ;; arbitrarily choose the first...
                (log-warning 
                 (format
                  "default output device doesn't support low-latency (~sms) output, using device ~s instead"
                  (* 1000 latency)
                  (device-name (car reasonable-devices))))
                (car reasonable-devices)])]))

;; determine the host API index associated with a host API symbol
(define (host-api-id->index id)
  (define apis (all-host-apis))
  (match (for/or ([i (in-naturals)] 
                  [this-host-api (in-list apis)]
                  #:when (symbol=? id this-host-api))
           i)
    [#f (error 'host-api-id->index 
               "couldn't find id ~s in api list ~s"
               id
               apis)]
    [result result]))

;; return the default output device associated with a host API
;; need to do a reverse lookup...
;; symbol -> number
(define (host-api-default-output-device host-api)
  (define api-index (host-api-id->index host-api))
  (pa-host-api-info-default-output-device
   (pa-get-host-api-info api-index)))

;; reasonable-latency-output-devices : real -> (list-of natural?)
;; output devices with reasonable latency
(define (low-latency-output-devices latency host-api)
  (for/list ([i (in-range (pa-get-device-count))]
             #:when (belongs-to-selected-api? host-api i)
             #:when (has-outputs? i)
             #:when (matches-latency? latency i))
    i))

;; does this device belong to the current host api?
;; nat -> boolean
(define (belongs-to-selected-api? selected-host-api i)  
  (define device-host-api-index 
    (pa-device-info-host-api (pa-get-device-info i)))
  (define host-api (pa-host-api-info-type (pa-get-host-api-info 
                                           device-host-api-index)))
  (symbol=? selected-host-api host-api))

;; has-outputs? : natural -> boolean
;; return true if the device has at least
;; two output channels
(define (has-outputs? i)
  (<= 2 (pa-device-info-max-output-channels (pa-get-device-info i))))

;; matches-latency? : natural real -> boolean
;; return true when the device has low latency
;; no greater than some threshold
(define (matches-latency? latency i)
  (<= (device-low-output-latency i) latency))

;; device-low-output-latency : natural -> real
;; return the low output latency of a device 
(define (device-low-output-latency i)
  (pa-device-info-default-low-output-latency (pa-get-device-info i)))


(define (display-device-table)
  (define host-apis (all-host-apis))
  (for ([i (pa-get-device-count)])
    (define device-info (pa-get-device-info i))
    (printf "device index ~s: api = ~s, device name = ~s, ~s input channels, ~s output channels\n"
            i 
            (list-ref host-apis (pa-device-info-host-api device-info))
            (pa-device-info-name device-info)
            (pa-device-info-max-input-channels device-info)
            (pa-device-info-max-output-channels device-info))))

;; returns the number of input channels available for the default
;; input device
(define (default-input-device-channels)
  (define i (pa-get-default-input-device))
  (define device-info (pa-get-device-info i))
  (pa-device-info-max-input-channels device-info))