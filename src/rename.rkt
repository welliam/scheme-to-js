#lang racket

(provide js-rename)

(define special-char-table
  '((#\. . dot)
    (#\? . p)
    (#\! . bang)
    (#\@ . at)
    (#\# . hash)
    (#\% . percent)
    (#\^ . caret)
    (#\& . and)
    (#\* . star)
    (#\- . _)
    (#\+ . plus)
    (#\= . eq)
    (#\/ . fslash)
    (#\~ . tilde)
    (#\< . lt)
    (#\> . gt)))

(define valid-js-char?
  (disjoin char-alphabetic? char-numeric? (curryr memq '(#\_ #\$))))

(define (js-rename-char c)
  (cond
   ((valid-js-char? c) (string c))
   ((assq c special-char-table)
    => (lambda (search) (format "~A" (cdr search))))
   (else "")))

(define (js-rename-symbol sym)
  (define s
    (apply string-append
           (for/list ((c (symbol->string sym)))
             (js-rename-char c))))
  (string->symbol
   (if (zero? (string-length s)) "_" s)))

(define (js-rename x (allowed '(field-ref field-set! make-object set!)))
  (cond
   ((and (symbol? x) (not (memq x allowed)))
    (js-rename-symbol x))
   ((pair? x)
    (cons (js-rename (car x))
          (js-rename (cdr x))))
   (else x)))
