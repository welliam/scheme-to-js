#lang racket

(require rackunit "../src/rename.rkt")

(define/provide-test-suite rename-tests
  symbols
  forms
  fixpoints)

(define valid-js-char?
  (disjoin char-alphabetic?
           char-numeric?
           (curryr memq '(#\_ #\$))))

(define (renamed? sym)
  (for/and ((c (symbol->string sym)))
    (valid-js-char? c)))

(define-test-suite symbols
  (check-not-false (renamed? (js-rename 'eq?)))
  (check-not-false (renamed? (js-rename 'call/cc)))
  (check-not-false (renamed? (js-rename 'foobar)))
  (check-not-false (renamed? (js-rename '_@!#*????))))

(define-test-suite fixpoints
  (check-equal? 1 (js-rename 1))
  (check-equal? 'a (js-rename 'a))
  (check-equal? "hello" (js-rename "hello"))
  (check-equal? '(hello "hello" 0.7734) (js-rename '(hello "hello" 0.7734))))

(define-test-suite forms
  (check-not-false (null? (js-rename '())))
  (check-not-false (andmap renamed? (js-rename '(foo))))
  (check-not-false (andmap renamed? (js-rename '(foo eq?))))
  (check-not-false (andmap renamed? (js-rename '(eq?))))
  (check-not-false (andmap renamed? (js-rename '(eq?))))
  (check-not-false (andmap renamed? (car (js-rename '((eq? foo bar)))))))
