(define f
  (let ((called #f))
    (lambda (p)
      (if (equal? called #f)
          (begin (set! called #t) p)
          0))))
