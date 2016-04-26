(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (- (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (* x x)))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac n d k))
