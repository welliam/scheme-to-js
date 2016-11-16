#lang racket

(provide object-code-tests)
(require rackunit "../src/object-code.rkt")

(define object-code-tests
  (test-suite
   "Tests for object-code.rkt"
   (check-equal? (object-code 3) "3" "Basic number compilation.")

   (check-equal? (object-code "hello") "\"hello\"" "Basic string compilation.")

   (check-equal? (object-code 'x) "x" "Symbol compilation.")

   (check-equal? (object-code '(foo)) "foo()" "Basic function application.")
   (check-equal? (object-code '(foo bar)) "foo(bar)" "Function with argument.")
   (check-equal? (object-code '(foo bar baz))
                 "foo(bar, baz)"
                 "Function with two arguments.")
   (check-equal? (object-code '(foo bar baz quux))
                 "foo(bar, baz, quux)"
                 "Function with three arguments.")
   (check-equal? (object-code '((a 1) (a 2)))
                 "a(1)(a(2))"
                 "Embedded application.")

   (check-equal? (object-code '(lambda (x) x))
                 "(function (x) { return x; })"
                 "Anonymous identity function.")

   (check-equal? (object-code '(lambda (x) x))
                 "(function (x) { return x; })"
                 "Anonymous identity function.")

   (check-equal? (object-code '(define x 0))
                 "var x = 0;"
                 "Variable definition.")

   (check-equal? (object-code
                  '(define f
                     (lambda (x)
                       (x x x))))
                 "var f = (function (x) { return x(x, x); });")

   (check-equal? (object-code '(set-field! f "hello" 0))
                 "f[\"hello\"] = 0;"
                 "Field assignment.")

   (check-equal? (object-code '(set-field! (lambda (x) x) -1 (foo bar)))
                 "(function (x) { return x; })[-1] = foo(bar);"
                 "Field assignment.")))
