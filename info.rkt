#lang setup/infotab

(define collection 'multi)
(define version "0.2")

(define deps
  '("base"
    ;; NB: not supporting 386 platforms at this point, unless requested...
    ("portaudio-x86_64-macosx"  #:platform "x86_64-macosx")
    ("portaudio-x86_64-linux"   #:platform "x86_64-linux")
    ("portaudio-x86_64-windows" #:platform "win32\\x86_64")
    ))

(define build-deps
  (list "rackunit-lib"
        "scribble-lib"
         "racket-doc"))

