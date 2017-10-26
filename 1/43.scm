#lang sicp


(define (repeated f n)
    (define (compose f g)
        (lambda (x) (f (g x))))
    (cond ((= n 1) f)
          (else (compose f (repeated f (- n 1))))))

(define (square x) (* x x))


((repeated square 2) 5)  ; 625
