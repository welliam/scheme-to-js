; relies upon prelude.scm and primes.scm

(define (by-id id)
  ((field-ref document "getElementById") id))

(define (set-on-click! elem f)
  (field-set! elem "onclick" f))

(define (->string i)
  (operator "+" "" i))

(define (string-append a b)
  (operator "+" a b))

(define (generate-output t)
  (cond
   ((null? t) "")
   ((null? (cdr t))
    (car t))
   (else
    (string-append (->string (car t))
      (string-append ", "
        (generate-output (cdr t)))))))

(define (run)
  (field-set! (by-id "o")
              "innerHTML"
              (generate-output
               (generate-primes
                (parseInt (field-ref (by-id "n") "value"))))))

(set-on-click! (by-id "b") run)
