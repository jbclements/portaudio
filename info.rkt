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
(define version "2011-10-18-09:36")
(define release-notes '((p "fixed malloc/free under windows")))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

