(define (get-numbers l judge-func)
  (define (get-numbers-iter c r)
    (if (null? c)
        r
        (get-numbers-iter (cdr c)
                          (if (not (judge-func (car c)))
                              r
                              (cons (car c) r)))))
  (reverse (get-numbers-iter l '())))

(define (same-parity . l)
  (cond ((null? l) '())
        ((even? (car l)) (get-numbers l even?))
        ((odd? (car l)) (get-numbers l odd?))))
