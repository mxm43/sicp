(define (cont-frac-recu n d k)
  (define (recu i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recu (+ i 1))))))
  (recu 1))

(define (cont-frac-iter n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(cont-frac-recu (lambda (i) 1.0) (lambda (i) 1.0) 100)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100)
