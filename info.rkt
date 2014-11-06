#lang setup/infotab

(define collection 'multi)
(define version "0.1") ; version bump for add of s16vec-record

(define deps
  (list "base"))

(define build-deps
  (list "rackunit-lib"
        "scribble-lib"
         "racket-doc"))

