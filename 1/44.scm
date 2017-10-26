#lang sicp


(define dx 0.00001)

(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated f n)
    (define (compose f g)
        (lambda (x) (f (g x))))
    (cond ((= n 1) f)
          (else (compose f (repeated f (- n 1))))))

(define (square x) (* x x))

((smooth square) 2)

(((repeated smooth 10) square) 2)
