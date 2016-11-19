(define (is-type? o type)
  (and (operator "in" "type" (Object o))
       (eq? (field-ref o "type")
            type)))

(define (set-type! o type)
  (field-set! o "type" type))

(define (cons a b)
  (let ((o (make-object)))
    (set-type! o "pair")
    (field-set! o "car" a)
    (field-set! o "cdr" b)
    o))

(define (list . xs) xs)

(define (car p)
  (field-ref p "car"))

(define (cdr p)
  (field-ref p "cdr"))

(define (eq? a b)
  (operator "==" a b))

(define (pair? x)
  (is-type? x "pair"))

(define (null? x)
  (eq? x '()))

(define (list? x)
  (or (null? x)
      (and (pair? x)
           (list? (cdr x)))))

(define (reverse-help t res)
  (if (null? t)
      res
      (reverse-help (cdr t) (cons (car t) res))))

(define (reverse t)
  (reverse-help t '()))

(define (length t)
  (if (null? t)
      0
      (+ 1 (length (cdr t)))))

(define (get-rest-arguments a i)
  ;; convert a javascript object with number slots and a length attr to a list
  ;; used in expand.scm
  (if (= i (field-ref a "length"))
      null  ; dirty secret..!
      (cons (field-ref a i) (get-rest-arguments a (+ i 1)))))

(define (not x)
  (if x false true))

(define (for-each f t)
  (cond
   ((not (null? t))
    (f (car t))
    (for-each f (cdr t)))))

(define (map f t)
  (if (null? t)
      '()
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

(define (sqrt n)
  ((field-ref Math "sqrt") n))

(define (displayln x)
  ((field-ref console "log") x))

(define (vector-length v)
  (field-ref v "length"))

(define (vector-ref v i)
  (field-ref v i))

(define (vector-set! v i x)
  (field-set! v i x))

(define (vector-fill! v x)
  (let loop ((i 0))
    (cond
     ((< i (vector-length v))
      (vector-set! v i x)
      (loop (+ i 1))))))

(define (make-vector size init)
  (let ((v (Array size)))
    (vector-fill! v init)
    v))

(define (string-append . strings)
  (let loop ((strs strings) (res ""))
    (if (null? strs)
        res
        (loop (cdr strs)
              (operator "+" res (car strs))))))
