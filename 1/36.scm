#lang sicp


(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (display next)
            (newline)
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))


(define (f x)
    (/ (log 1000) (log x)))

(define (average a b)
    (/ (+ a b) 2))

(define (f-with-average-damping x)
    (average (f x) x))

(fixed-point f 4.5)  ; 24 steps

(display "*** with average damping")
(newline)

(fixed-point f-with-average-damping 5)  ; 9 steps
