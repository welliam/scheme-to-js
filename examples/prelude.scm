(define (cons a b)
  ((lambda (o)
     (field-set! o "type" "pair")
     (field-set! o "car" a)
     (field-set! o "cdr" b)
     o)
   (make-object)))

(define (car p)
  (field-ref p "car"))

(define (cdr p)
  (field-ref p "cdr"))

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

(define (not x)
  (if x false true))

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

(define (displayln x)
  ((field-ref console "log") x))
