#lang racket

(require ffi/unsafe
         ffi/vector
         rackunit)

(define lib (ffi-lib "reader.dylib"))


(define-struct info (a b c))

(define my-info (make-info 12 13 14))

(define info-holder (malloc-immobile-cell my-info))

(define info-holder-ptr (ptr-ref info-holder _racket))

(define find-elements
  (get-ffi-obj "findElements" lib
               (_fun _racket _racket -> _ulong)))

(eq? info-holder-ptr my-info)

(find-elements info-holder-ptr info-holder)

