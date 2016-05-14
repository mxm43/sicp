(define (make-center-percent c p)
  (let ((t (* c (/ p 100.0))))
    (make-interval (- c t) (+ c t))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (percent x)
  (* (/ (/ (- (upper-bound x) (lower-bound x)) 2) (center x)) 100.0))

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
