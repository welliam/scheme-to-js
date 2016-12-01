#lang racket

(require rackunit "../src/rename.rkt")

(define/provide-test-suite scope-rename-tests
  symbols)

;; to feature (protect var1 ...) form that compiles to some value but
;; updates dictionary with variables bound to themselves

(define-test-suite symbols
  (check-not-equal? (scope-rename 'a) 'a)
  (check-not-equal? (scope-rename 'quux) 'quux)
  (check-not-equal? (scope-rename 'foobar) (scope-rename 'quux))
  (check-equal? (scope-rename 'quux) (scope-rename 'quux)))
