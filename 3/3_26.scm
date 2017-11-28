(define (make-tree) (list '() '() '()))

(define (key tree) (car tree))
(define (set-key! tree value) (set-car! tree value))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (set-left-branch! tree value) (set-car! (cdr tree) value))
(define (set-right-branch! tree value) (set-car! (cddr tree) value))

(define (insert-tree! tree value compare)
  (if (null? tree)
    (set! tree (make-tree)))
  (cond ((null? (key tree)) (set-key! tree value))
        ((> (compare (key tree) value) 0)
         (if (null? (right-branch tree))
             (set-right-branch! tree (make-tree)))
         (insert-tree! (right-branch tree) value compare))
        (else
         (if (null? (left-branch tree))
             (set-left-branch! tree (make-tree)))
         (insert-tree! (left-branch tree) value compare)))
  tree)

(define (lookup-tree tree value compare)
  (cond ((null? tree) false)
        ((= (compare (key tree) value) 0) (key tree))
        ((> (compare (key tree) value) 0)
         (lookup-tree (right-branch tree) value compare))
        ((< (compare (key tree) value) 0)
         (lookup-tree (left-branch tree) value compare))))

(define (make-table same-key?)
  (let ((local-table (list '())))
    (define (assoc key records)
      (cond ((null? records) false)
            (else (lookup-tree records key same-key?))))
    (define (lookup keys)
      (if (null? keys)
          (if (null? (car local-table))
              false
              (car local-table))
          (let ((subtable (assoc (car keys) (cdr local-table))))
            (if subtable
                (((cadr subtable)'lookup-proc) (cdr keys))
                false))))
    (define (insert! keys value)
      (if (null? keys)
          (begin (set-car! local-table value) 'ok)
          (let ((subtable (assoc (car keys) (cdr local-table))))
            (if subtable
                (let ((record (cadr subtable)))
                  ((record 'insert-proc!) (cdr keys) value))
                (set-cdr! local-table
                          (insert-tree! (cdr local-table)
										(list (car keys)                                    
                                              (let
                                                ((table (make-table same-key?)))         
                                                ((table 'insert-proc!)
                                                   (cdr keys) value)               
                                                table))                                     
                                        same-key?)))
            'ok)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation =-- TABLE" m))))
    dispatch))
