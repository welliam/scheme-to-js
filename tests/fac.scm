(define mult (lambda (x y) (operator * x y)))

(define sub1 (lambda (x) (operator - x 1)))

(define zero (lambda (x) (operator == x 0)))

(define fac
  (lambda (x)
    (if (zero x)
        1
        (mult x (fac (sub1 x))))))
