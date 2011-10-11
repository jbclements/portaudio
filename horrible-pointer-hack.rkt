#lang racket/base

(require ffi/unsafe)

(provide pointer->num
         num->pointer)

;; this horrible hack is necessary to make passing pointers 
;; possible in 5.1.3; this hack is unnecessary in 5.1.3.9
;; (and presumably later)

(define ptr-size (ctype-sizeof _pointer))
(define int-type 
  (cond [(= ptr-size 4) _uint32]
        [(= ptr-size 8) _uint64]
        [else 
         (error 'pointer->num
                "pointer-size isn't 4 or 8: ~e" ptr-size)]))

;; pointer->num : _pointer -> number?
(define (pointer->num ptr)
  (cast ptr _pointer int-type))

;; num->pointer : exact-integer? -> _pointer
(define (num->pointer num)
  (cast num int-type _pointer))


;; comment this out... it leaks memory.
(define my-ptr (malloc 14 'raw))
(unless (equal? my-ptr (num->pointer (pointer->num my-ptr)))
  (error 'horrible-hack "round-tripping is broken."))
(free my-ptr)