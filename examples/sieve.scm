(define (sieve-prime! v p)
  (let loop ((n (* p 2)))
    (cond
     ((< n (vector-length v))
      (vector-set! v n #f)
      (loop (+ n p))))))

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
    (let loop ((i 2))
      (cond
       ((> i imax)
        (collect-primes is-prime? i))
       ((vector-ref is-prime? i)
        (sieve-prime! is-prime? i)
        (cons i (loop (+ i 1))))
       (else
        (loop (+ i 1)))))))

(define generate-primes prime-sieve)
