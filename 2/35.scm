#lang sicp


;; accumulate boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))


;; actual code

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 4) (list 5 6)))

(count-leaves x)
(count-leaves (list x x))
(count-leaves y)
(count-leaves (list y y))
