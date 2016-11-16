#lang racket

(provide expand)

(define-syntax-rule (expansion pattern result)
  (lambda (form)
    (match form
      (pattern result)
      (x x))))

(define function-define-expansion
  (expansion (list 'define (cons op args) body)
             `(define ,op (lambda ,args ,body))))

(define expand function-define-expansion)
