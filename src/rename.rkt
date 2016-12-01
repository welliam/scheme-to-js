#lang racket

(provide js-rename scope-rename)

;; js-rename (scheme-valid -> js-valid (with exceptions for core forms))

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

;; scope renamer (scheme-valid -> scheme-valid)

(define current-rename-dictionary (make-parameter (make-hash)))

(define (format-symbol dict sym)
  (string->symbol (format "~A_~A" sym (hash-count dict))))

(define (new-symbol! dict sym)
  (define renamed (format-symbol dict sym))
  (hash-set! dict sym renamed)
  renamed)

(define (symbol-rename form dict)
  (hash-ref dict form (lambda () (new-symbol! dict form))))

(define (pair-rename pair dict)
  (cons (scope-rename (car pair) dict)
        (scope-rename (cdr pair) dict)))

(define (scope-rename form (dict (current-rename-dictionary)))
  (cond
   ((symbol? form) (symbol-rename form dict))
   ((pair? form) (pair-rename form dict))
   (else form)))
