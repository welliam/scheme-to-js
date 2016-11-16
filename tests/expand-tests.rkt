#lang racket

(require rackunit "../src/expand.rkt")

(define/provide-test-suite expand-tests
  (test-suite "fix points"
    (check-equal? (expand 'x) 'x)
    (check-equal? (expand 5) 5)
    (check-equal? (expand '(lambda (x) (x x))) '(lambda (x) (x x)))
    (check-equal? (expand '(if 1 2 3)) '(if 1 2 3)))

  (test-suite "defun"
    (check-equal? (expand '(define (f) x)) '(define f (lambda () x)))
    (check-equal? (expand '(define (f x) x)) '(define f (lambda (x) x)))
    (check-equal? (expand '(define (f x y) x)) '(define f (lambda (x y) x)))
    (check-equal? (expand '(define (f . foo) x)) '(define f (lambda foo x))))

  (test-suite "operators"
    (check-equal? (expand '(and lhs rhs)) '(operator && lhs rhs))
    (check-equal? (expand '(or lhs rhs)) '(operator "||" lhs rhs))
    (check-equal? (expand '(+ lhs rhs)) '(operator + lhs rhs))
    (check-equal? (expand '(- lhs rhs)) '(operator - lhs rhs))
    (check-equal? (expand '(* lhs rhs)) '(operator * lhs rhs))
    (check-equal? (expand '(/ lhs rhs)) '(operator / lhs rhs))
    (check-equal? (expand '(eq? lhs rhs)) '(operator == lhs rhs))
    (check-equal? (expand '(define (eq? a b) (eq? a b)))
                  '(define eq? (lambda (a b) (operator == a b)))))

  (test-suite "single arm if"
    (check-equal? (expand '(if 1 2)) '(if 1 2 #f)))

  (test-suite "compound expansions"
    (check-equal? (expand '(f (if 1 2) (if 1 2)))
                  '(f (if 1 2 #f) (if 1 2 #f)))))

