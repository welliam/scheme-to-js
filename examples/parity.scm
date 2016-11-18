;; relies upon prelude.scm

(letrec ((even? (lambda (n) (or (zero? n) (odd? (- n 1)))))
         (odd? (lambda (n) (and (not (zero? n)) (even? (- n 1))))))
  (displayln (even? 16)))
