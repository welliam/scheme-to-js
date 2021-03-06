#lang racket

(provide object-code)

(define/match (object-code x)
  (((list 'lambda args body))
   (format-procedure args body))
  (((list 'define id exp))
   (format-definition id exp))
  (((list 'if pred then else))
   (format-if pred then else))
  (((list 'field-ref of field))
   (format-field-ref of field))
  (((list 'field-set! of field to))
   (format-field-set! of field to))
  (((list 'operator operator lhs rhs))
   (format-operator operator lhs rhs))
  (((list 'make-object)) "({})")
  (((list 'set! op exp))
   (format-set! op exp))
  (((list* op args))
   (format-application op args))
  ((x) (format-self-evaluating x)))

(define (intersperse-commas strs)
  (apply string-append
         (car strs)
         (map (curry string-append ", ") (cdr strs))))

(define (format-argument-list args)
  (if (null? args) "" (intersperse-commas (map object-code args))))

(define (format-procedure params body)
  (define js-params (format-argument-list params))
  (define js-body (object-code body))
  (format "(function (~A) { return ~A; })" js-params js-body))

(define (format-application op arg-list)
  (format "~A(~A)"
          (object-code op)
          (format-argument-list arg-list)))

(define (format-field-ref of field)
  (format "~A[~A]"
          (object-code of)
          (object-code field)))

(define (format-field-set! of field to)
  (format "~A[~A] = ~A"
          (object-code of)
          (object-code field)
          (object-code to)))

(define (format-operator operator lhs rhs)
  (format "(~A ~A ~A)"
          (object-code lhs)
          operator
          (object-code rhs)))

(define (format-self-evaluating x)
  (cond
   ((boolean? x)
    (if x "true" "false"))
   (else (format "~S" x))))

(define (format-definition id exp)
  (format "var ~S = ~A;" id (object-code exp)))

(define (format-if pred then else)
  (format "(~A ? ~A : ~A)"
          (object-code pred)
          (object-code then)
          (object-code else)))

(define (format-set! op exp)
  (format "(~A = ~A)"
          (object-code op)
          (object-code exp)))
