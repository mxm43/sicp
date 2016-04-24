(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate filter-func combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter-func (term a))
                                            (term a)
                                            null-value)))))
  (iter a null-value))

(define (keep n) n)
(define (inc n) (+ n 1))
(filtered-accumulate prime? + 0 keep 2 inc 18)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? a b) (= (gcd a b) 1))
(define (relatively-prime-product n)
  (define (fil k) (relatively-prime? k n))
  (filtered-accumulate fil * 1 keep 1 inc n))

(relatively-prime-product 10)
