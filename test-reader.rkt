#lang racket

(require ffi/unsafe
         ffi/vector)

(define lib (ffi-lib "reader.dylib"))

(define old-fetch
  (get-ffi-obj "oldFetchFirstWord" lib
               (_fun _pointer -> _ulong)))

#|unsigned long oldFetchFirstWord(unsigned long *data){
  return (unsigned long)data;
}
|#

(define new-fetch
  (get-ffi-obj "fetchFirstWord" lib
               (_fun _pointer -> _ulong)))


(define u64vec (make-u64vector 1000))

(for ([i (in-range 1000)])
  (s16vector-set! u64vec i (random 100)))


(number->string (old-fetch (s16vector->cpointer s16vec)) 16)
(check-e(number->string (new-fetch (s16vector->cpointer s16vec)) 16))