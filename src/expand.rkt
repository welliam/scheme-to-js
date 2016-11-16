#lang racket

(provide expand)

(define (function-define-expansion op args bodies)
  `(define ,op ,(expand `(lambda ,args . ,bodies))))

(define operators
  `((and . &&)
    (or . "||")))

(define (operator-expansion op lhs rhs)
  `(operator ,(cdr (assq op operators)) ,(expand lhs) ,(expand rhs)))

(define (if-expansion a b)
  `(if ,(expand a) ,(expand b) #f))

(define/match (expand form)
  (((list* 'define (cons op args) body body*))
   (function-define-expansion op args (cons body body*)))
  (((list 'if pred then))
   (if-expansion pred then))
  (((list op lhs rhs))
   #:when (memq op (map car operators))
   (operator-expansion op lhs rhs))
  (((list* 'lambda args body body*))
   #:when (not (null? body*))
   `(lambda ,args (begin ,body . ,body*)))
  (((cons f args))
   (cons (expand f) (expand args)))
  ((x) x))
