#lang racket

(provide expand)

(define (function-define-expansion op args bodies)
  `(define ,op ,(expand `(lambda ,args . ,bodies))))

(define operators
  `((and . "&&")
    (or . "||")))

(define (operator-expansion op lhs rhs)
  `(operator ,(cdr (assq op operators)) ,(expand lhs) ,(expand rhs)))

(define (if-expansion a b)
  `(if ,(expand a) ,(expand b) #f))

(define (begin-expansion expr exprs)
  (if (null? exprs)
      (expand expr)
      `((lambda (x)
          ,(begin-expansion (car exprs) (cdr exprs)))
        ,(expand expr))))

(define/match (expand form)
  (((list* 'define (cons op args) body body*))
   (function-define-expansion op args (cons body body*)))
  (((list 'if pred then))
   (if-expansion pred then))
  (((list op lhs rhs))
   #:when (assq op operators)
   (operator-expansion op lhs rhs))
  (((list* 'lambda args body body*))
   #:when (not (null? body*))
   `(lambda ,args ,(expand `(begin ,body . ,body*))))
  (((list* 'begin expr exprs))
   (begin-expansion expr exprs))
  (((cons f args))
   (cons (expand f) (expand args)))
  ((x) x))
