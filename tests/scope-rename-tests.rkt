#lang racket

(require rackunit "../src/rename.rkt")

(define/provide-test-suite scope-rename-tests
  symbols
  lists
  quotes)

;; to feature (protect var1 ...) form that compiles to some value but
;; updates dictionary with variables bound to themselves

(define-test-suite symbols
  (check-not-equal? (scope-rename 'a) 'a)
  (check-not-equal? (scope-rename 'quux) 'quux)
  (check-not-equal? (scope-rename 'foobar) (scope-rename 'quux))
  (check-equal? (scope-rename 'quux) (scope-rename 'quux)))

(define-test-suite lists
  (check-not-equal? (car (scope-rename '(a b))) 'a)
  (check-not-equal? (second (scope-rename '(a b))) 'b)
  (check-equal? (car (scope-rename '(a b))) (scope-rename 'a))
  (check-equal? (second (scope-rename '(a b))) (scope-rename 'b)))

(define-test-suite quotes
  (check-equal? (scope-rename '(quote a)) '(quote a))
  (check-equal? (scope-rename '(quote (a b c))) '(quote (a b c))))
