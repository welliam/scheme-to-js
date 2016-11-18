(define (fac x)
  (if (zero? x)
      1
      (* x (fac (- x 1)))))
