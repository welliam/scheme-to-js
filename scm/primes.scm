; relies upon prelude.scm

(define (generate-primes-help primes num-primes i)
  (if (= (length primes) num-primes)
      primes

      (generate-primes-help
       (if (ormap (lambda (prime)
                    (zero? (modulo i prime)))
                  primes)
           primes
           (cons i primes))
       num-primes
       (+ i 1))))

(define (generate-primes num)
  (generate-primes-help null num 2))

(define (display-list t)
  (if (not (null? t))
      (begin (displayln (car t))
             (display-list (cdr t)))
      false))

(display-list (generate-primes 15))
