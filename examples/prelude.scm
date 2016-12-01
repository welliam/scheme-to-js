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

(define (set-car! p x)
  (cond
   ((pair? p)
    (set-field! p "car" x))))

(define (set-cdr! p x)
  (cond
   ((pair? p)
    (set-field! p "cdr" x))))

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
      (cons (field-ref a i) (get-rest-arguments a (operator "+" i 1)))))

(define (not x)
  (if x false true))

(define (list->array t)
  (let ((res (Array)))
    (let loop ((t t))
      (cond
       ((null? t) res)
       (else
        ((field-ref res "push") (car t))
        (loop (cdr t)))))))

(define (print-list t)
  (displayln "(")
  (let loop ((t t))
    (cond
     ((not (null? t))
      (displayln (car t))
      (loop (cdr t)))))
  (displayln ")"))

(define (apply f arg . args)
  (let ((args (let loop ((args (cons arg args)))
                (let ((arg (car args)))
                  (if (null? (cdr args))
                      arg
                      (cons arg (loop (cdr args))))))))
    ((field-ref f "apply") '() (list->array args))))  ; relies upon '() compiling to "null"

(define (foldl f x t)
  (if (null? t)
      x
      (foldl f (f (car t) x) (cdr t))))

(define (foldr f x t)
  (if (null? t)
      x
      (f (car t) (foldr f x (cdr t)))))

(define (for-each f t)
  (foldr (lambda (a res) (f a) res) #f t))

(define (map f t)
  (if (null? t)
      '()
      (cons (f (car t)) (map f (cdr t)))))

(define (+ . xs)
  (foldl (lambda (a b) (operator "+" a b)) 0 xs))

(define (- x . xs)
  (cond
   ((null? xs) (operator "-" 0 x))
   (else
    (foldl (lambda (b a) (operator "-" b a)) x xs))))

(define (* . xs)
  (foldl (lambda (a b) (operator "*" a b)) 0 xs))

(define (/ x . xs)
  (if (null? xs)
      (operator "/" 1 x)
      (foldl (lambda (b a) (operator "/" a b)) x xs)))


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
  (foldr (lambda (a b) (operator "+" a b)) "" strings))

;; symbols

(define (string->symbol s)
  (let ((o (make-object)))
    (field-set! o "string" s)
    (field-set! o "type" "symbol")
    o))

(define (symbol->string sym)
  (field-ref sym "string"))

(define (symbol? x)
  (is-type? x "symbol"))

(field-ref (make-object) "a")
