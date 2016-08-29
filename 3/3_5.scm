(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (/ range 1.0)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (square x) (* x x))

(define (in-unit-circle? x y)
  (>= 1 (+ (square x) (square y))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (* (- x1 x2) (- y1 y2))
     (monte-carlo trials (lambda () (P (random-in-range x2 x1)
                                       (random-in-range y2 y1))))))
(define PI
  (estimate-integral in-unit-circle? 1.0 -1.0 1.0 -1.0 10000000))
