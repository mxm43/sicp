(define (div-interval x y)
  (if (OR (< (* (lower-bound x) (upper-bound x)) 0)
          (< (* (lower-bound y) (upper-bound y)) 0))
      (error "The definition is not clear."))
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound x)
  (let ((a (car x))
        (b (cdr x)))
        (if (> a b)
            a
            b)))

(define (lower-bound x)
  (let ((a (car x))
        (b (cdr x)))
        (if (> a b)
            b
            a)))
