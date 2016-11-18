; relies upon prelude.scm

(define (ormap f t)
  (and (not (null? t))
       (or (f (car t))
           (ormap f (cdr t)))))

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
  (reverse (generate-primes-help null num 2)))
