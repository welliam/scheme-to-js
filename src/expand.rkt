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

(define (cond-expansion cond-forms)
  (match-define (cons form forms) cond-forms)
  (match form
    ((list 'else body)
     (expand body))
    ((list* 'else body bodies)
     (expand `(begin ,body . ,bodies)))
    ((list pred then)
     (if (null? forms)
         (expand `(if ,pred ,then))
         (expand `(if ,pred ,then ,(cond-expansion forms)))))
    ((list* pred then thens)
     (if (null? forms)
         (expand `(if ,pred (begin ,then . ,thens)))
         (expand `(if ,pred
                      (begin ,then . ,thens)
                      ,(cond-expansion forms)))))))

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
  (((list* 'cond cond-forms))
   #:when (not (null? cond-forms))
   (cond-expansion cond-forms))
  (((cons f args))
   (cons (expand f) (expand args)))
  ((x) x))
