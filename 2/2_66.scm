(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))
    
(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (lookup given-key set-of-records)
  (let ((record (entry set-of-records)))
    (cond ((null? set-of-records) #f)
          ((= given-key (key record)) record)
          ((< given-key (key record)) (lookup given-key (left-branch set-of-records)))
          ((> given-key (key record)) (lookup given-key (right-branch set-of-records))))))
