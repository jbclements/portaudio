#lang racket

;; WIP, let's use existing racket build infrastructure to
;; recompile portaudio for windows?

;; status : wow! I got a dll. No idea if it works yet.

(require remote-shell/docker)

(define name "zazzoo-1")

;; success!
#;(docker-create #:name name
               #:image-name "racket/distro-build:crosswin")

;; success!
#;(docker-start #:name name)

(define portaudio-src-bundle "pa_stable_v190700_20210406.tgz")

;; I happen to have it in /tmp/right now. This is all totally WIP:
#;(docker-copy #:name name
             #:src (build-path "/tmp" portaudio-src-bundle)
             #:dest (string-append name ":"
                                   (path->string
                                    (build-path "/home/racket"
                                                portaudio-src-bundle))))

;; did this interactively, use docker-exec in future?
;; tar xvzf pa-stable...
;; cd portaudio
;; mkdir my-build # build already exists...
;; cd my-build
;; ../configure --host=x86_64-w64-mingw32 --with-winapi=wasapi
;; make

;; .. I'm guessing I'll need headers? for wasapi or directx?

(define result-dll-name "libportaudio-2.dll")

(docker-copy #:name name
             #:src (string-append
                    name ":"
                    "/home/racket/portaudio/my-build/lib/.libs/"
                    result-dll-name)
             #:dest (build-path "/tmp" result-dll-name))