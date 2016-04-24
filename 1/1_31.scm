(define (product-recu term a next b)
  (if (> a b)
      1
      (* (term a) (product-recu term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result 
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (square x) (* x x))

(define (factorial n)
  (define (term k)
    (/ (square k) (square (- k 1))))
  (define (next k)
    (+ k 2))
  
  (/ (* 8.0 (product-iter term 4 next n)) n))
