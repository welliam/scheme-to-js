; relies upon prelude.scm

(define (ormap f t)
  (and (not (null? t))
       (or (f (car t))
           (ormap f (cdr t)))))

(define (generate-primes-help primes num-primes i)
  (cond
   ((zero? num-primes) primes)
   ((ormap (lambda (prime)
             (zero? (modulo i prime)))
           primes)
    (generate-primes-help primes num-primes (+ i 1)))
   (else
    (generate-primes-help (cons i primes) (- num-primes 1) (+ i 1)))))

(define (generate-primes num)
  (reverse (generate-primes-help '() num 2)))
