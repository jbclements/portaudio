#lang racket

(require (submod "../callback-support.rkt" test))
(provide (all-from-out (submod "../callback-support.rkt" test)))

(module+ test
  (require rackunit/text-ui)
  
  (run-tests the-test-suite))