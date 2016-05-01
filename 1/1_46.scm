(define (iterative-improve improve-func good-enough?)
  (lambda (x) (if (good-enough? x)
                  x
                  ((iterative-improve improve-func good-enough?) (improve-func x)))))

(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (square x)
  (* x x))

(define tolerance 0.00001)

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  ((iterative-improve improve good-enough?) 1.0))

(define (fixed-point f first-guess)
  (define (improve guess)
    (f guess))
  (define (good-enough? guess)
    (display guess)
    (newline)
    (< (abs (- guess (improve guess))) tolerance))
  ((iterative-improve improve good-enough?) first-guess))
