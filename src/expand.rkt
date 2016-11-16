#lang racket

(provide expand)

(define-syntax-rule (expansion . match-form)
  (lambda (form)
    (match form
      match-form
      (x x))))

(define (function-define-expansion op args body)
  `(define ,op ,(expand `(lambda ,args ,body))))

(define operators
  `((and . &&)
    (or . "||")
    (+ . +)
    (- . -)
    (* . *)
    (/ . /)
    (eq? . ==)))

(define (operator-expansion op lhs rhs)
  `(operator ,(cdr (assq op operators)) ,(expand lhs) ,(expand rhs)))

(define (if-expansion a b)
  `(if ,(expand a) ,(expand b) #f))

(define/match (expand form)
  (((list 'define (cons op args) body))
   (function-define-expansion op args body))
  (((list 'if pred then))
   (if-expansion pred then))
  (((list op lhs rhs))
   #:when (memq op (map car operators))
   (operator-expansion op lhs rhs))
  (((cons f args))
   (cons (expand f) (expand args)))
  ((x) x))