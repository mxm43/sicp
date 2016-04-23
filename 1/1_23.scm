(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime) (tms:clock (times)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display "(")
  (display elapsed-time)
  (display ")")
  (newline)
  #t)

(define (search-for-primes start-number)
  (define (iterator n number)
    (if (< n 3)
      (iterator (if (timed-prime-test number) (+ n 1) n)
                (+ number 2))))
  (if (even? start-number) (iterator 0 (+ start-number 1)) (iterator 0 start-number)))

(search-for-primes 100000000000)
(search-for-primes 1000000000000)
(search-for-primes 10000000000000)
(search-for-primes 100000000000000)
