(define (recursion n)
  (cond ((< n 3) n)
        (else ( + (recursion (- n 1))
                  (* 2 (recursion (- n 2)))
                  (* 3 (recursion (- n 3)))))))

(define (iteration n)
  (define (iterator fn-1 fn-2 fn-3 i)
    (cond ((> i n) fn-1)
          (else (iterator (+ fn-1
                             (* 2 fn-2)
                             (* 3 fn-3))
                          fn-1
                          fn-2
                          (+ i 1)))))
  (cond ((< n 3) n)
        (else (iterator 2 1 0 3))))
