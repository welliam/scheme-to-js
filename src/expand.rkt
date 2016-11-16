#lang racket

(provide expand)

(define-syntax-rule (expansion . match-form)
  (lambda (form)
    (match form
      match-form
      (x x))))

(define function-define-expansion
  (expansion (list 'define (cons op args) body)
             `(define ,op (lambda ,args ,body))))

(define operators
  `((and . &&)
    (or . "||")
    (+ . +)
    (- . -)
    (* . *)
    (/ . /)))

(define operator-expansion
  (expansion (list op lhs rhs)
             #:when (memq op (map car operators))
             `(operator ,(cdr (assq op operators)) ,lhs ,rhs)))

(define expand
  (compose function-define-expansion
           operator-expansion))
