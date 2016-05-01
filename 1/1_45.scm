(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fast-expt b n)
  (define (iterator a b n)
    (cond ((= n 0) a)
          ((even? n) (iterator a (* b b) (/ n 2)))
          (else (iterator (* a b) b (- n 1)))))
  (iterator 1 b n))

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (fast-expt y (- n 1))))
                            (repeated average-damp 2)
                            1.0))
