#lang setup/infotab

(define name "PortAudio")

(define blurb '((p "PortAudio is a cross-platform library "
                   "for audio output and input. It runs "
                   "on Windows, Mac OS X, and linux. "
                   "This package provides Racket bindings "
                   "for these functions.")))

#;(define scribblings '(("rsound.scrbl" () (tool))))
(define categories '(media))
(define version "2011-09-11-08:01")
(define release-notes '((p "initial release")))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

