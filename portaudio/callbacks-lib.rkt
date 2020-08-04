#lang racket/base

(require setup/collection-search
         ffi/unsafe)

(provide callbacks-lib
         (struct-out stream-rec)
         ;; no obvious way to provide names for the underscore
         ;; version, add as needed...
         _stream-rec
         _stream-rec-pointer)

(define not-false? (λ (x) x))


;; the library containing the C copying callbacks
(define callbacks-lib
  (let ()
    (or
     ;; search in all portaudio/lib collection dirs:
     (collection-search
      '(lib "portaudio/lib")
      #:combine
      (λ (_ path)
        (ffi-lib (build-path path "callbacks")
                 #:fail (λ () #f)))
      #:break?
      not-false?)
     ;; also look in "standard locations". useful
     ;; for people building executables.
     (ffi-lib "callbacks"))))

;; STREAMING CALLBACK STRUCT

(define-cstruct _stream-rec
  (;; the number of frames in the circular buffer
   [buffer-frames _int]
   ;; the circular buffer
   [buffer _pointer]
   ;; the last frame read by the callback
   [last-frame-read _uint]
   ;; the offset of the last byte read by the callback.
   [last-offset-read _uint]
   ;; the last frame written by Racket
   [last-frame-written _uint]
   ;; the offset of the last byte written by Racket.
   [last-offset-written _uint]
   ;; number of faults:
   [fault-count _int]
   ;; a pointer to a 4-byte cell; when it's nonzero,
   ;; the supplying procedure should shut down, and
   ;; free this cell. If it doesn't get freed, well,
   ;; that's four bytes wasted until the next store-prompt.
   [all-done _pointer]))
