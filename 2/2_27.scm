(define (deep-reverse l)
  (define (iter items r)
    (if (null? items)
        r
        (iter (cdr items) (cons (deep-reverse (car items)) r))))
  (if (not (pair? l))
      l
      (iter l '())))
