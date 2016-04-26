(define (d i)
  (if (= (remainder (- i 2) 3) 0)
      (* (/ (+ i 1) 3) 2)
      1))
    
(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(+ (cont-frac (lambda (x) 1.0) d 100) 2)
