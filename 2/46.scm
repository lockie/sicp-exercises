#lang sicp


(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
               (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
    (add-vect v1 (make-vect (- (xcor-vect v2)) (- (ycor-vect v2)))))

(define (scale-vect v s)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))


(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))

(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect v1 2)
