(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (mul a b)
  (define (iterator r a b)
    (cond ((= b 0) r)
          ((even? b) (iterator r (double a) (halve b)))
          (else (iterator (+ r a) a (- b 1)))))
  (iterator 0 a b))
