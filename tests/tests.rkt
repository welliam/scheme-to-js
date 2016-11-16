#lang racket

(require rackunit rackunit/text-ui "object-code-tests.rkt" "expand-tests.rkt")
 
(run-tests (test-begin object-code-tests expand-tests))
