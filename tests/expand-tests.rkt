#lang racket

(require rackunit "../src/expand.rkt")

(define/provide-test-suite expand-tests
  fix-points
  defun
  operators
  single-arm-if
  compound-expansions)

(define-test-suite fix-points
  (check-equal? (expand 'x) 'x)
  (check-equal? (expand 5) 5)
  (check-equal? (expand '(lambda (x) (x x))) '(lambda (x) (x x)))
  (check-equal? (expand '(if 1 2 3)) '(if 1 2 3)))

(define-test-suite defun
  (check-equal? (expand '(define (f) x)) '(define f (lambda () x)))
  (check-equal? (expand '(define (f x) x)) '(define f (lambda (x) x)))
  (check-equal? (expand '(define (f x y) x)) '(define f (lambda (x y) x)))
  (check-equal? (expand '(define (f . foo) x)) '(define f (lambda foo x))))

(define-test-suite operators
  (check-equal? (expand '(and lhs rhs)) '(operator && lhs rhs))
  (check-equal? (expand '(or lhs rhs)) '(operator "||" lhs rhs)))

(define-test-suite single-arm-if
  (check-equal? (expand '(if 1 2)) '(if 1 2 #f)))

(define-test-suite compound-expansions
  (check-equal? (expand '(f (if 1 2) (if 1 2)))
                '(f (if 1 2 #f) (if 1 2 #f))))
