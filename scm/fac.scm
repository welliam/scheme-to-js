(define (fac x)
  (if (eq? x 0)
      1
      (* x (fac (- x 1)))))
