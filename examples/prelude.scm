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

(define (get-rest-arguments a i)
  ;; convert a javascript object with number slots and a length attr to a list
  ;; used in expand.scm
  (if (= i (field-ref a "length"))
      null
      (cons (field-ref a i) (get-rest-arguments a (+ i 1)))))

(define (list . xs) xs)

(define (not x)
  (if x false true))

(define (for-each f t)
  (cond
   ((not (null? t))
    (f (car t))
    (for-each f (cdr t)))))

(define (map f t)
  (if (null? t)
      null
      (cons (f (car t)) (map f (cdr t)))))

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

(define (< a b)
  (operator "<" a b))

(define (> a b)
  (operator ">" a b))

(define (<= a b)
  (operator "<=" a b))

(define (>= a b)
  (operator ">=" a b))

(define (zero? n)
  (= n 0))

(define (displayln x)
  ((field-ref console "log") x))

(define (vector-length v)
  (field-ref v "length"))

(define (vector-ref v i)
  (field-ref v i))

(define (vector-set! v i x)
  (field-set! v i x))

(define (vector-fill! v x)
  (letrec ((rec (lambda (i)
                  (cond
                   ((< i (vector-length v))
                    (vector-set! v i x)
                    (rec (+ i 1)))))))
    (rec 0)))

(define (make-vector size init)
  (let ((v (Array size)))
    (vector-fill! v init)
    v))

(define (string-append . strings)
  (letrec ((rec (lambda (strs res)
                  (if (null? strs)
                      res
                      (rec (cdr strs)
                           (operator "+" res (car strs)))))))
    (rec strings "")))
