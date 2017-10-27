#lang sicp


(define (fringe t)
    (cond ((null? t) nil)
          ((not (pair? t)) (list t))
          (else (append (fringe (car t))
                        (fringe (cdr t))))))


(define x (list (list 1 2) (list 3 4)))

(fringe x)

(fringe (list x x))
