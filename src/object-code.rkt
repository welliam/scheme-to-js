#lang racket

(provide object-code)

(define (intersperse-commas strs)
  (apply string-append
         (car strs)
         (map (curry string-append ", ") (cdr strs))))

(define (format-argument-list args)
  (if (null? args) "" (intersperse-commas (map object-code args))))

(define (format-function params body)
  (define js-params (apply string-append (map object-code params)))
  (define js-body (object-code body))
  (format "(function (~A) { return ~A; })" js-params js-body))

(define (format-application op arg-list)
  (format "~A(~A)"
          (object-code op)
          (format-argument-list arg-list)))

(define/match (object-code x)
  (((list 'lambda args body))
   (format-function args body))
  (((list 'define id exp))
   (format "var ~S = ~A;" id (object-code exp)))
  (((list 'set-field! of field to))
   (format "~A[~A] = ~A;"
           (object-code of)
           (object-code field)
           (object-code to)))
  (((list* op args))
   (format-application op args))
  ((x) (format "~S" x)))
