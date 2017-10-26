#lang sicp


(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (midpoint-segment segment)
    (define (average x y) (/ (+ x y) 2))
    (make-point
     (average (x-point (start-segment segment)) (x-point (end-segment segment)))
     (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(print-point (midpoint-segment (make-segment
                                (make-point 2 3)
                                (make-point 10 15))))
