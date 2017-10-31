#lang sicp


(define (square-tree tree)
    (define (square x) (* x x))
    (cond ((null? tree) nil)
          ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
          (else (square tree))))

(define (square-tree2 tree)
    (define (square x) (* x x))
    (map (lambda (sub-tree)
             (if (pair? sub-tree)
                 (square-tree2 sub-tree)
                 (square sub-tree)))
         tree))

(list 1
      (list 2 (list 3 4) 5)
      (list 6 7))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
