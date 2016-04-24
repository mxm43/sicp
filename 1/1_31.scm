(define (product-recu term a next b)
  (if (> a b)
      1
      (* (term a) (product-recu term (next a) next b))))

(define (square x) (* x x))

(define (factorial-recu n)
  (define (term k)
    (/ (square k) (square (- k 1))))
  (define (next k)
    (+ k 2))
  
  (/ (* 8.0 (product-recu term 4 next n)) n))
