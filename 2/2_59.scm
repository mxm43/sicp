(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (define (union-set-iter set result)
    (if (null? set)
        result
        (union-set-iter (cdr set) (adjoin-set (car set) result))))
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set-iter set1 set2))))
