#lang racket

(provide compile compile-file)

(require "object-code.rkt" "expand.rkt" "rename.rkt")

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

(define compile (compose object-code js-rename expand))

(define (compile-file filename)
  (string-join
   (map compile (with-input-from-file filename read-all))))
