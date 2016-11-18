#lang racket

(require rackunit "../src/expand.rkt")

(define/provide-test-suite expand-tests
  fix-points
  defun
  implicit-begin
  operators
  single-arm-if
  compound-expansions
  begins
  conds)

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

(define-test-suite implicit-begin
  (check-equal? (expand '(lambda (foo) foo foo foo))
                '(lambda (foo)
                   ((lambda (x)
                      ((lambda (x)
                         foo)
                       foo))
                    foo)))
  (check-equal? (expand '(define (f) 1 2))
                '(define f
                   (lambda ()
                     ((lambda (x)
                        2)
                      1)))))

(define-test-suite operators
  (check-equal? (expand '(and lhs rhs)) '(operator "&&" lhs rhs))
  (check-equal? (expand '(or lhs rhs)) '(operator "||" lhs rhs)))

(define-test-suite single-arm-if
  (check-equal? (expand '(if 1 2)) '(if 1 2 #f)))

(define-test-suite compound-expansions
  (check-equal? (expand '(f (if 1 2) (if 1 2)))
                '(f (if 1 2 #f) (if 1 2 #f))))

(define-test-suite begins
  (check-equal? (expand '(begin 1)) 1)
  (check-equal? (expand '(begin 1 2))
                '((lambda (x) 2) 1))
  (check-equal? (expand '(begin 1 2 3))
                '((lambda (x)
                    ((lambda (x)
                       3)
                     2))
                  1)))

(define-test-suite conds
  (check-equal? (expand '(cond (else 0)))
                0)
  (check-equal? (expand '(cond (0 1)))
                '(if 0 1 #f))
  (check-equal? (expand '(cond (0 1) (else 2)))
                '(if 0 1 2))
  (check-equal? (expand '(cond (0 1) (2 3)))
                '(if 0 1 (if 2 3 #f)))
  (check-equal? (expand '(cond (0 1) (2 3) (else 4)))
                '(if 0 1 (if 2 3 4)))
  (check-equal? (expand '(cond (else 0 1)))
                '((lambda (x) 1) 0))
  (check-equal? (expand '(cond (0 1 2)))
                '(if 0 ((lambda (x) 2) 1) #f))
  (check-equal? (expand '(cond (0 1 2) (else 3 4)))
                '(if 0 ((lambda (x) 2) 1) ((lambda (x) 4) 3)))
  (check-equal? (expand '(cond (0 1 2) (3 4 5)))
                '(if 0 ((lambda (x) 2) 1) (if 3 ((lambda (x) 5) 4) #f)))
  (check-equal? (expand '(cond (0 1 2) (3 4 5) (else 6 7)))
                '(if 0
                     ((lambda (x) 2) 1)
                     (if 3
                         ((lambda (x) 5) 4)
                         ((lambda (x) 7) 6))))
  (check-equal? (expand '(cond (0 1 2 3) (4 5 6 7) (else 8 9 10)))
                '(if 0
                     ((lambda (x) ((lambda (x) 3) 2)) 1)
                     (if 4
                         ((lambda (x) ((lambda (x) 7) 6)) 5)
                         ((lambda (x) ((lambda (x) 10) 9)) 8))))
  (check-equal? (expand '(cond (e => f)))
                '((lambda (x)
                    (if x
                        (f x)
                        #f))
                  e))
  (check-equal? (expand '(cond (e => f) (else quux)))
                '((lambda (x)
                    (if x
                        (f x)
                        quux))
                  e)))
