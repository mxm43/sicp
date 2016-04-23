(define (square n) (* n n))

(define (check-nontrivial-sqrt n m)
   (let ((r (remainder (square n) m)))
        (if (and (not (= n 1)) (not (= n (- m 1))) (= r 1))
            0
            r)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iterator count)
    (cond ((= count n) #t)
          ((try-it count) (iterator (+ count 1)))
          (else #f)))
  (iterator 1))

(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)
