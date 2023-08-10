#lang racket

;; Compile "callbacks.c" using Racket's suggested compiler and flags.

;; Heavily based on Sam Tobin-Hochstadt's bcrypt/private/install.rkt
;; https://github.com/samth/bcrypt.rkt

(require racket/file
         racket/runtime-path
         (for-syntax syntax/transformer))

(define-syntax-rule (require-without-pkg-dep lib name)
  (begin (define tmp
           (let* ([v #f]
                  [name (λ ()
                          (unless v
                            (set! v (dynamic-require 'lib 'name)))
                          v)])
             name))
         (define-syntax name
           (make-variable-like-transformer #'(tmp)))))

(require-without-pkg-dep dynext/file append-extension-suffix)
(require-without-pkg-dep dynext/link current-use-mzdyn)
(require-without-pkg-dep dynext/link current-extension-linker-flags)
(require-without-pkg-dep dynext/link link-extension)

(define-runtime-path callbacks.c
  "callbacks.c")

(define lib/
  (path-only callbacks.c))

(define (so-path-elem)
  (append-extension-suffix "callbacks"))

(define (cc [so-path (build-path lib/
                                 (system-library-subpath #f)
                                 (so-path-elem))]
            #:overwrite? [overwrite? #t])
  (parameterize ([current-use-mzdyn #f]
                 [current-extension-linker-flags
                  (if (eq? 'macosx (system-type 'os))
                      (list* "-mmacosx-version-min=10.5"
                             (current-extension-linker-flags))
                      (current-extension-linker-flags))])
    (when (and overwrite? (file-exists? so-path))
      (delete-file so-path))
    (make-parent-directory* so-path)
    (link-extension #f ;; not quiet
                    (list callbacks.c)
                    so-path)))

(define (cc/i386-macosx)
  (unless (eq? 'macosx (system-type 'os))
    (raise-arguments-error 'cc/i386-macosx
                           "unsupported system type"
                           "expected" 'macosx
                           "given" (system-type 'os)))
  (parameterize ([current-extension-linker-flags
                  (list* "-arch"
                         "i386"
                         (current-extension-linker-flags))])
    (cc (build-path lib/ "i386-macosx" (so-path-elem)))))
