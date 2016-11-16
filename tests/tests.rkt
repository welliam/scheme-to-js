#lang racket

(require rackunit rackunit/text-ui
         "object-code-tests.rkt"
         "expand-tests.rkt"
         "rename-tests.rkt")

(run-tests object-code-tests)
(run-tests rename-tests)
(run-tests expand-tests)
