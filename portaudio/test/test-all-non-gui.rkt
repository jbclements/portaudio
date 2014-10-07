#lang racket

(require rackunit
         rackunit/text-ui
         (prefix-in cs: (submod "../callback-support.rkt" test))
         (prefix-in tsc: "test-stream-callback.rkt"))

(run-tests
 (test-suite "non-gui-tests"
             cs:the-test-suite
             tsc:the-test-suite))