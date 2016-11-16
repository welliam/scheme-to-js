#lang racket

(provide compile-file)

(require "object-code.rkt")

(define (string-join strings)
  (foldr (lambda (x s) (string-append x "\n" s))
         ""
         strings))

(define (read-all)
  (let loop ()
    (define x (read))
    (if (eof-object? x)
        '()
        (cons x (loop)))))

(define (compile-file filename)
  (string-join (map object-code (with-input-from-file filename read-all))))
