(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
    
(define (enumerate-interval low high)
  (define (iter l h r)
    (if (> l h)
        r
        (iter l (- h 1) (cons h r))))
  (iter low high '()))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (flatmap (lambda (j)
                             (map (lambda (k) (list j k))
                                  (enumerate-interval 1 (- j 1))))
                           (enumerate-interval 1 (- i 1)))))
           (enumerate-interval 1 n)))

(define (triples-sum-s n s)
  (filter (lambda (triples)
            (= (+ (car triples)
                  (cadr triples)
                  (caddr triples))
               s))
          (unique-triples n)))
