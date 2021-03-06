(define (count-pairs x)
  (let ((visited '()))
    (define (add-visited pair)
      (begin (set! visited (cons pair visited))
             1))
    (define (is-visited? p l)
      (cond ((null? l) #f)
            ((eq? p (car l)) #t)
            (else (is-visited? p (cdr l)))))
    (define (recu p)
      (cond ((not (pair? p)) 0)
            ((is-visited? p visited) 0)
            (else (+ (recu (car p))
                     (recu (cdr p))
                     (add-visited p)))))
    (recu x)))
