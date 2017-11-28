(define (make-table same-key?)
  (let ((local-table (list '())))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
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
                          (cons (list (car keys)
                                      (let ((table (make-table same-key?)))
                                        ((table 'insert-proc!) (cdr keys)
                                                               value)
                                        table))
                                (cdr local-table))))
            'ok)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation =-- TABLE" m))))
    dispatch))
