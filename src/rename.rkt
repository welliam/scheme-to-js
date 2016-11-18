#lang racket

(provide js-rename)

(define special-char-table
  '((#px"\\." . "dot")
    (#px"\\?" . "p")
    (#px"!" . "bang")
    (#px"@" . "at")
    (#px"#" . "hash")
    (#px"%" . "percent")
    (#px"\\^" . "caret")
    (#px"&" . "and")
    (#px"\\*" . "star")
    (#px"-" . "_")
    (#px"\\+" . "plus")
    (#px"=" . "eq")
    (#px"/" . "fslash")
    (#px"~" . "tilde")
    (#px"<" . "lt")
    (#px">" . "gt")))

(define (js-rename-symbol sym)
  (string->symbol
   (foldr (match-lambda*
           ((list (cons from to) res)
            (regexp-replace* from res to)))
          (symbol->string sym)
          special-char-table)))

(define (js-rename x (allowed '(field-ref field-set! make-object set!)))
  (cond
   ((and (symbol? x) (not (memq x allowed)))
    (js-rename-symbol x))
   ((pair? x)
    (cons (js-rename (car x))
          (js-rename (cdr x))))
   (else x)))
