#lang sicp


(define (iterative-improve good-enough? improve)
    (define (improve-step x)
        (if (good-enough? x)
            x
            (improve-step (improve x))))
    (lambda (x) (improve-step x)))

(define (sqrt x)
    (define (square x) (* x x))
    (define (average a b) (/ (+ a b) 2))
    ((iterative-improve
     (lambda (guess) (< (abs (- (square guess) x)) 0.001))
     (lambda (guess) (average guess (/ x guess)))) 1.))

(sqrt 16)


(define tolerance 0.00001)

(define (fixed-point f first-guess)
    ((iterative-improve
     (lambda (guess) (< (abs (- guess (f guess))) tolerance))
     (lambda (guess) (f guess))) first-guess))


(fixed-point cos 1.0)  ; .7390822985224023
