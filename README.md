# scheme->javascript

A Scheme to JavaScript compiler.

Currently, a Scheme with some key JavaScript features is
implemented. Through use of these features full Scheme features like
cons, vectors, hash tables, etc. are able to be defined. This is
partially done in
[examples/prelude.scm](https://github.com/welliam/scheme-to-js/blob/master/examples/prelude.scm)

## Examples
A simple webpage which generates prime numbers is available on GitHub
[here](http://welliam.github.io/primes/).

Input:
```scheme
(define (cons a b)
  ((lambda (o)
     (field-set! o "type" "pair")
     (field-set! o "car" a)
     (field-set! o "cdr" b)
     o)
   (make-object)))

(define (car p)
  (field-ref p "car"))

(define (cdr p)
  (field-ref p "cdr"))

(define (eq? a b)
  (operator "==" a b))
```

Output
```javascript
var cons = (function (a, b) { return (function (o) { return (function (x) { return (function (x) { return (function (x) { return o; })(o["cdr"] = b); })(o["car"] = a); })(o["type"] = "pair"); })({}); });
var car = (function (p) { return p["car"]; });
var cdr = (function (p) { return p["cdr"]; });
var eqp = (function (a, b) { return (a == b); });
```

Some examples:

- [Core Scheme functions](https://github.com/welliam/scheme-to-js/blob/master/examples/prelude.scm)
- [Factorial](https://github.com/welliam/scheme-to-js/blob/master/examples/fac.scm)
- [Prime generation](https://github.com/welliam/scheme-to-js/blob/master/examples/primes.scm)
- [A webpage](https://github.com/welliam/scheme-to-js/blob/master/examples/primespage.scm) ([accompanying HTML](https://github.com/welliam/scheme-to-js/blob/master/examples/primes.html))

[This directory](https://github.com/welliam/scheme-to-js/tree/master/examples)
has more successfully-compiling examples.

## See [the issues](https://github.com/welliam/scheme-to-js/issues) for some bugs/missing features/quirks/etc.
Currently the issues page is more like a development journal/to-do list.
