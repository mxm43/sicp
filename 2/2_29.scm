(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (weight? structure)
  (not (pair? structure)))

(define (branch-weight b)
  (if (weight? (branch-structure b))
      (branch-structure b)
      (total-weight (branch-structure b))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balance? mobile)
  (and (if (weight? (branch-structure (left-branch mobile)))
           #t
           (balance? (branch-structure (left-branch mobile))))
       (if (weight? (branch-structure (right-branch mobile)))
           #t
           (balance? (branch-structure (right-branch mobile))))
       (= (* (branch-length (left-branch mobile))
             (branch-weight (left-branch mobile)))
          (* (branch-length (right-branch mobile))
             (branch-weight (right-branch mobile))))))
