(define (square n) (* n n))
    
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))
