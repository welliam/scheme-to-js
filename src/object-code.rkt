#lang racket

(provide object-code)

(define (intersperse-commas strs)
  (apply string-append
         (car strs)
         (map (curry string-append ", ") (cdr strs))))

(define (format-argument-list args)
  (if (null? args) "" (intersperse-commas (map object-code args))))

(define (format-function arg-list)
  (define params (apply string-append (map object-code (car arg-list))))
  (define body (object-code (second arg-list)))
  (format "(function (~A) { return ~A; })" params body))

(define (format-application op arg-list)
  (format "~A(~A)"
          (object-code op)
          (format-argument-list arg-list)))

(define (object-code x)
  (cond
   ((list? x)
    (match-define (cons op arg-list) x)
    (if (equal? op 'lambda)
        (format-function arg-list)
        (format-application op arg-list)))
   (else
    (format "~S" x))))
