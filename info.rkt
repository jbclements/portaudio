#lang setup/infotab

(define name "PortAudio")

(define blurb '((p "PortAudio is a cross-platform library "
                   "for audio output and input. It runs "
                   "on Windows, Mac OS X, and linux. "
                   "This package provides Racket bindings "
                   "for these functions. This package is "
                   "unsafe. Use RSound for safety.")))

(define scribblings '(("portaudio.scrbl" () (tool))))
(define categories '(media))
(define version "2011-10-20-09:57")
(define release-notes '((p "trying to get rid of stray error messages")))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

