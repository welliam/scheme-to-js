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
  (expand
   (if (null? exprs)
       expr
       `(let ((x ,expr))
          ,(begin-expansion (car exprs) (cdr exprs))))))

(define (cond-expansion cond-forms)
  (match-define (cons form forms) cond-forms)
  (match form
    ((list 'else body)
     (expand body))
    ((list* 'else body bodies)
     (expand `(begin ,body . ,bodies)))
    ((list exp '=> then-f)
     (expand
      (if (null? forms)
          `(let ((x ,exp)) (if x (,then-f x)))
          `(let ((x ,exp))
             (if x (,then-f x) ,(cond-expansion forms))))))
    ((list pred then)
     (expand
      (if (null? forms)
          `(if ,pred ,then)
          `(if ,pred ,then ,(cond-expansion forms)))))
    ((list* pred then thens)
     (expand
      (if (null? forms)
          `(if ,pred (begin ,then . ,thens))
          `(if ,pred
               (begin ,then . ,thens)
               ,(cond-expansion forms)))))))

(define (let-expansion key-values bodies)
  (define keys (map car key-values))
  (define values (map cadr key-values))
  (expand `((lambda ,keys . ,bodies) . ,values)))

(define (let*-expansion key-values bodies)
  (match key-values
    ('() (expand `(let () . ,bodies)))
    ((list* (list k v) key-values)
     (expand
      `(let ((,k ,v))
         ,(let*-expansion key-values bodies))))))

(define (letrec-expansion key-values bodies)
  (define keys (map car key-values))
  (define values (map cadr key-values))
  (define let-keys (map (lambda (key) `(,key #f)) keys))
  (define set!s (map (lambda (key value) `(set! ,key ,value))
                     keys values))
  (define let-body (append set!s bodies))
  (expand `(let ,let-keys . ,let-body)))

(define (separate-cars improper)
  (let loop ((t improper) (res '()))
    (if (pair? t)
        (loop (cdr t) (cons (car t) res))
        (values (reverse res) t))))

(define (polyadic-expansion args body)
  (define-values (proper-args rest-arg) (separate-cars args))
  (expand
   `(lambda ,proper-args
      (let ((,rest-arg (get-rest-arguments arguments ,(length proper-args))))
        ,body))))

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
   (expand `(lambda ,args (begin ,body . ,body*))))
  (((list 'lambda args body))
   #:when (if (pair? args) (not (list? args)) (symbol? args))
   (polyadic-expansion args body))
  (((list* 'begin expr exprs))
   (begin-expansion expr exprs))
  (((list* 'cond cond-forms))
   #:when (not (null? cond-forms))
   (cond-expansion cond-forms))
  (((list* 'let (list* key-values) bodies))
   (let-expansion key-values bodies))
  (((list* 'let* (list* key-values) bodies))
   (let*-expansion key-values bodies))
  (((list* 'letrec (list* key-values) bodies))
   (letrec-expansion key-values bodies))
  (((cons f args))
   (cons (expand f) (expand args)))
  ((x) x))
