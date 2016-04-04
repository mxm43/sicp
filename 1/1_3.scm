(define (compare a b)
  (cond ((> a b) a)
        (else b)))

(define (plus-biggers a b c)
  (cond ((> a b) (+ a (compare b c)))
        (else (+ b (compare a c)))))
