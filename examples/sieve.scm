(define (sqrt n)
  ((field-ref Math "sqrt") n))

(define (sieve-prime! v p)
  (letrec ((rec (lambda (n)
                  (cond
                   ((< n (vector-length v))
                    (vector-set! v n #f)
                    (rec (+ n p)))))))
    (rec (* p 2))))

(define (collect-primes primes i)
  ; (displayln (vector-ref primes i))
  (cond
   ((= i (vector-length primes)) null)
   ((vector-ref primes i)
    (cons i (collect-primes primes (+ i 1))))
   (else (collect-primes primes (+ i 1)))))

(define (prime-sieve limit)
  (let ((is-prime? (make-vector limit #t))
        (imax (sqrt limit)))
    (letrec ((rec (lambda (i)
                    (cond
                     ((> i imax)
                      (collect-primes is-prime? i))
                     ((vector-ref is-prime? i)
                      (sieve-prime! is-prime? i)
                      (cons i (rec (+ i 1))))
                     (else
                      (rec (+ i 1)))))))
      (rec 2))))

(define generate-primes prime-sieve)
