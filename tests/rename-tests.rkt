#lang racket

(require rackunit "../src/rename.rkt")

(define valid-js-char?
  (disjoin char-alphabetic?
           char-numeric?
           (curryr memq '(#\_ #\$))))

(define (renamed? sym)
  (for/and ((c (symbol->string sym)))
    (valid-js-char? c)))

(define/provide-test-suite rename-tests
  (check-equal? #t (renamed? (js-rename 'eq?)))
  (check-equal? #t (renamed? (js-rename 'call/cc)))
  (check-equal? #t (renamed? (js-rename 'foobar)))
  (check-equal? #t (renamed? (js-rename '_@!#*????))))
