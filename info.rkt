#lang setup/infotab

(define collection 'multi)
(define version "0.2")

(define deps
  '("base"
    ("portaudio-x86_64-macosx" #:platform "x86_64-macosx")
    ("portaudio-win32-x86-64"  #:platform "x86_64-win32")
    ))

(define build-deps
  (list "rackunit-lib"
        "scribble-lib"
         "racket-doc"))

