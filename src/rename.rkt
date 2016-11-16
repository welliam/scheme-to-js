#lang racket

(provide js-rename)

(define special-char-table
  '((#\. . dot)
    (#\? . q)
    (#\! . bang)
    (#\@ . at)
    (#\# . hash)
    (#\% . percent)
    (#\^ . caret)
    (#\& . and)
    (#\* . star)
    (#\- . minus)
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

(define (js-rename sym)
  (define s
    (apply string-append
           (for/list ((c (symbol->string sym)))
             (js-rename-char c))))
  (string->symbol
   (if (zero? (string-length s)) "_" s)))
