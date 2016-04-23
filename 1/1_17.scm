(define (double n) (+ n n))
(define (halve n) (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ (mul a (- b 1)) a))))
