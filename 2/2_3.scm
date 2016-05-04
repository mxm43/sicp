(define (make-rectangle l w)
  (cons l w))

(define (rectangle-length rectangle)
  (car rectangle))

(define (rectangle-width rectangle)
  (cdr rectangle))

(define (rectangle-perimeter rectangle)
  (* 2 (+ (rectangle-length rectangle)
          (rectangle-width rectangle))))

(define (rectangle-area rectangle)
  (* (rectangle-length rectangle) (rectangle-width rectangle)))
