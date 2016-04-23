(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (define (iterator a b n)
    (cond ((= n 0) a)
          ((even? n) (iterator a (* b b) (/ n 2)))
          (else (iterator (* a b) b (- n 1)))))
  (iterator 1 b n))
