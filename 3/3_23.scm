(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-item (list item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-car! (cdr (front-ptr deque)) new-item)
           (set-car! (cddr new-item) (front-ptr deque))
           (set-front-ptr! deque new-item)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-item (list item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-car! (cdr new-item) (rear-ptr deque))
           (set-car! (cddr (rear-ptr deque)) new-item)
           (set-rear-ptr! deque new-item)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE FRONT called with an empty deque" deque))
        (else
         (set-front-ptr! deque (caddr (front-ptr deque)))
         (cond ((empty-deque? deque)
                (set-rear-ptr! deque '())
                deque)
               (else
                 (set-car! (cdr (front-ptr deque)) '())
                 deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE REAR called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque '()))
         (cond ((empty-deque? deque) deque)
               (else
                 (set-car! (cddr (rear-ptr deque)) '())
                 deque)))))

