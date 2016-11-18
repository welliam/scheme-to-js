; relies upon prelude.scm and primes.scm

(define (by-id id)
  ((field-ref document "getElementById") id))

(define (set-on-click! elem f)
  (field-set! elem "onclick" f))

(define (to-string i)
  (operator "+" "" i))

(define (string-append a b)
  (operator "+" a b))

(define (generate-output t)
  (if (null? t)
      ""
      (string-append (to-string (car t))
        (string-append "\n"
          (generate-output (cdr t))))))

(define (run)
  (field-set! (by-id "o")
              "innerHTML"
              (generate-output
               (generate-primes
                (parseInt (field-ref (by-id "n") "value"))))))

(set-on-click! (by-id "b") run)
