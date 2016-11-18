#lang racket

(provide js-rename)

(define special-char-table
  '((#px"^->$" . "arrow")
    (#px"^->" . "to_")
    (#px"->$" . "_arrow")
    (#px"->" . "_to_")
    (#px"\\." . "dot")
    (#px"\\?" . "p")
    (#px"^!$" . "bang")
    (#px"^!" . "bang_")
    (#px"!$" . "_bang")
    (#px"!" . "_bang_")
    (#px"@" . "at")
    (#px"#" . "hash")
    (#px"%" . "percent")
    (#px"\\^" . "caret")
    (#px"&" . "and")
    (#px"\\*" . "star")
    (#px"^-$" . "minus")
    (#px"-" . "_")
    (#px"\\+" . "plus")
    (#px"=" . "eq")
    (#px"/" . "fslash")
    (#px"~" . "tilde")
    (#px"<" . "lt")
    (#px">" . "gt")))

(define (js-rename-symbol sym)
  (string->symbol
   (foldl (match-lambda*
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
