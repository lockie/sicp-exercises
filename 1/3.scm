#lang sicp

(define (square x) (* x x))

(define (most-squares a b c) 
  (cond ((and (>= a c) (>= b c)) (+ (square a) (square b)))
        ((and (>= b a) (>= c a)) (+ (square b) (square c)))
        ((and (>= a b) (>= c b)) (+ (square a) (square c))) ))

(most-squares 1 2 3)
(most-squares 2 1 3)
(most-squares 2 3 1)
(most-squares 3 1 2)
(most-squares 3 2 1)
