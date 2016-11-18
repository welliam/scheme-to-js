(define (cons a b)
  ((lambda (o)
     (field-set! o "type" "pair")
     (field-set! o "car" a)
     (field-set! o "cdr" b)
     o)
   (make-object)))

(define (car p)
  (field-ref ? "car"))

(define (cdr p)
  (field-ref ? "cdr"))

(define (eq? a b)
  (operator "==" a b))

(define (istype? x type)
  (and (operator "in" "type" x)
       (eq? (field-ref o "type")
            type)))

(define (pair? x)
  (istype? x "pair"))

(define (null? x)
  (eq? x null))

(define (list? x)
  (or (null? x)
      (and (pair? x)
           (list? (cdr x)))))

(define (reverse-help t res)
  (if (null? t)
      res
      (reverse-help (cdr t) (cons (car t) res))))

(define (reverse t)
  (reverse-help t null))

(define (length t)
  (if (null? t)
      0
      (+ 1 (length (cdr t)))))

(define emptyqueue (cons null null))

(define (enqueue x q)
  (cons (cons x (car q))
        (cdr q)))

(define (dequeue q)
  (if (null? (cdr q))
      (dequeue (cons null (reverse (car q))))
      (cons (car q)
            (cdr (cdr q)))))

(define (not x)
  (if x false true))

(define (queuepeek q)
  (if (null? (cdr q))
      (and (not (null? (car q)))
           (queuepeek (cons null (reverse (car q)))))
      (car (cdr q))))

(define (displayln x)
  ((field-ref console "log") x))

(define exampleq (enqueue 3 (enqueue 2 (enqueue 1 emptyqueue))))

(define (+ a b)
  (operator "+" a b))

(define (- a b)
  (operator "-" a b))

(define (* a b)
  (operator "*" a b))

(define (/ a b)
  (operator "/" a b))

(define (modulo a b)
  (operator "%" a b))

(define (= a b)
  (operator "==" a b))

(define (zero? n)
  (= n 0))
