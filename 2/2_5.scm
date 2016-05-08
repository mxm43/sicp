(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (define (div r)
    (if (not (= (remainder r 3) 0))
        r
        (div (/ r 3))))
  (define (iter n r)
    (if (= n 1)
        r
        (iter (/ n 2) (+ r 1))))
  (iter (div z) 0))

(define (cdr z)
  (define (div r)
    (if (not (= (remainder r 2) 0))
        r
        (div (/ r 2))))
  (define (iter n r)
    (if (= n 1)
        r
        (iter (/ n 3) (+ r 1))))
  (iter (div z) 0))
