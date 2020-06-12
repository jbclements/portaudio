#lang setup/infotab

(define collection 'multi)
(define version "0.2")

(define deps
  '("base"
    ("portaudio-x86_64-macosx" #:platform "x86_64-macosx")
    ;; still working on this one...
    ;("portaudio-win32-x86-64"  #:platform "win32\\x86_64")
    ))

(define build-deps
  (list "rackunit-lib"
        "scribble-lib"
         "racket-doc"))

