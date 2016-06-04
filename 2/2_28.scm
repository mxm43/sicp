(define (fringe-helper items result)
  (define (iter l r)
    (if (null? l)
        r
        (iter (cdr l) (fringe-helper (car l) r))))
  (if (not (pair? items))
      (cons items result)
      (iter items result)))

(define (fringe items)
  (reverse (fringe-helper items '())))
