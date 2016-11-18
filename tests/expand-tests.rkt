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
  conds
  lets
  let*s
  letrecs
  named-let
  polyadic)

(define-test-suite fix-points
  (check-equal? (expand 'x) 'x)
  (check-equal? (expand 5) 5)
  (check-equal? (expand '(lambda (x) (x x))) '(lambda (x) (x x)))
  (check-equal? (expand '(if 1 2 3)) '(if 1 2 3)))

(define-test-suite defun
  (check-equal? (expand '(define (f) x)) '(define f (lambda () x)))
  (check-equal? (expand '(define (f x) x)) '(define f (lambda (x) x)))
  (check-equal? (expand '(define (f x y) x)) '(define f (lambda (x y) x))))

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

(define-test-suite lets
  (check-equal? (expand '(let () 0))
                '((lambda () 0)))
  (check-equal? (expand '(let ((x 0)) x))
                '((lambda (x) x) 0))
  (check-equal? (expand '(let ((k v) (kk vv)) (+ k kk)))
                '((lambda (k kk) (+ k kk))
                  v vv))
  (check-equal? (expand '(let () a b c))
                '((lambda ()
                    ((lambda (x)
                       ((lambda (x) c)
                        b))
                     a))))
  (check-equal? (expand '(let ((k v) (kk vv)) a b c))
                '((lambda (k kk)
                    ((lambda (x)
                       ((lambda (x) c)
                        b))
                     a))
                  v vv)))

(define-test-suite let*s
  (check-equal? (expand '(let* () 0))
                '((lambda () 0)))
  (check-equal? (expand '(let* ((x 0)) x))
                '((lambda (x)
                    ((lambda () x)))
                  0))
  (check-equal? (expand '(let* ((x 0) (y (+ x 1))) (+ x y)))
                '((lambda (x)
                    ((lambda (y)
                       ((lambda () (+ x y))))
                     (+ x 1)))
                  0)))

(define-test-suite letrecs
  (check-equal? (expand '(letrec () 0))
                '((lambda () 0)))
  (check-equal? (expand '(letrec ((a 0)) a))
                '((lambda (a)
                    ((lambda (x)
                       a)
                     (set! a 0)))
                  #f))
  (check-equal? (expand '(letrec ((a 1) (b 2)) (+ a b)))
                '((lambda (a b)
                    ((lambda (x)
                       ((lambda (x)
                          (+ a b))
                        (set! b 2)))
                     (set! a 1)))
                  #f #f)))

(define-test-suite named-let
  (check-equal? (expand '(let loop () 0))
                '((lambda (loop)
                    ((lambda (x)
                       (loop))
                     (set! loop (lambda () 0))))
                  #f))
  (check-equal? (expand '(let loop ((quux 1)) quux))
                '((lambda (loop)
                    ((lambda (x)
                       (loop 1))
                     (set! loop (lambda (quux) quux))))
                  #f)))

(define-test-suite polyadic
  (check-equal? (expand '(lambda xs xs))
                '(lambda ()
                   ((lambda (xs) xs)
                    (get-rest-arguments arguments 0))))
  (check-equal? (expand '(define (list . xs) xs))
                '(define list
                   (lambda ()
                     ((lambda (xs)
                        xs)
                      (get-rest-arguments arguments 0))))))
