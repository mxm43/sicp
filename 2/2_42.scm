(define (enumerate-interval low high)
  (define (iter l h r)
    (if (> l h)
        r
        (iter l (- h 1) (cons h r))))
  (iter low high '()))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (safe? k positions)
  (define (iter new-col position n)
    (if (= n 0)
        #t
        (cond ((= new-col (car position)) #f)
              ((= (- k n) (- new-col (car position))) #f)
              ((= (- k n) (- (car position) new-col)) #f)
              (else (iter new-col (cdr position) (- n 1))))))
  (iter (car positions) (cdr positions) (- k 1)))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
