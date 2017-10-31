#lang sicp


(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
    (cond ((null? tree) nil)
          ((pair? tree) (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))
          (else (proc tree))))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
