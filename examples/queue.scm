;; relies upon prelude.scm

(define emptyqueue (cons null null))

(define (queue-empty? q)
  (and (null? (car q))
       (null? (cdr q))))

(define (enqueue x q)
  (cons (cons x (car q))
        (cdr q)))

(define (dequeue q)
  (if (null? (cdr q))
      (dequeue (cons null (reverse (car q))))
      (cons (car q)
            (cdr (cdr q)))))

(define (queue-peek q)
  (if (null? (cdr q))
      (and (not (null? (car q)))
           (queue-peek (cons null (reverse (car q)))))
      (car (cdr q))))
