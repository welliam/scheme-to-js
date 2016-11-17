#lang racket

(require rackunit "../src/object-code.rkt")

(define/provide-test-suite object-code-tests
  simple-objects
  applications
  procedures
  variables
  object-fields
  ternary-operator)

(define-test-suite simple-objects
  (check-equal? (object-code 3) "3" "Basic number compilation.")
  (check-equal? (object-code "hello") "\"hello\"" "Basic string compilation.")
  (check-equal? (object-code 'x) "x" "Symbol compilation.")
  (check-equal? (object-code '(make-object)) "{}" "Object creation."))

(define-test-suite applications
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
                "Embedded application."))

(define-test-suite procedures
  (check-equal? (object-code '(lambda (x) x))
                "(function (x) { return x; })"
                "Anonymous identity function.")
  (check-equal? (object-code '(lambda (x) x))
                "(function (x) { return x; })"
                "Anonymous identity function.")
  (check-equal? (object-code '(lambda (f x) (f x)))
                "(function (f, x) { return f(x); })"
                "Multiarg function."))

(define-test-suite variables
  (check-equal? (object-code '(define x 0))
                "var x = 0;"
                "Variable definition.")
  (check-equal? (object-code
                 '(define f
                    (lambda (x)
                      (x x x))))
                "var f = (function (x) { return x(x, x); });"
                "Function definition."))

(define-test-suite object-fields
  (check-equal? (object-code '(field-set! f "hello" 0))
                "f[\"hello\"] = 0"
                "Field assignment.")
  (check-equal? (object-code '(field-set! (lambda (x) x) -1 (foo bar)))
                "(function (x) { return x; })[-1] = foo(bar)"
                "Field assignment.")
  (check-equal? (object-code '(field-ref foo bar))
                "foo[bar]"
                "Field reference.")
  (check-equal? (object-code '(field-ref (lambda (x) x) bar))
                "(function (x) { return x; })[bar]"
                "Field reference."))

(define-test-suite operators
  (check-equal? (object-code '(operator * 2 3))
                "(2 * 3)"
                "Multiplication.")
  (check-equal? (object-code '(operator + (operator * 2 3) (operator % 5 3)))
                "((2 * 3) + (5 % 3))"
                "Math"))

(define-test-suite ternary-operator
  (check-equal? (object-code '(if a b c))
                "(a ? b : c)")
  (check-equal? (object-code '(if (if a b c) (if d e f) (if g h i)))
                "((a ? b : c) ? (d ? e : f) : (g ? h : i))"))
