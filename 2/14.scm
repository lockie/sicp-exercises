#lang sicp


(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
    (make-center-width c (* c p 0.01)))

(define (percent i)
    (* 100 (/ (width i) (center i))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (print-percent-interval i)
    (display (center i))
    (display " ± ")
    (display (percent i))
    (display " %")
    (newline))

(define a (make-center-percent 10 0.4))
(define b (make-center-percent 10 0.5))

(define d1 (div-interval a a))
(define d2 (div-interval a b))

(print-percent-interval d1)
(print-percent-interval d2)


(define c (make-center-percent 1000 0.04))
(define d (make-center-percent 100 0.05))

(define dd1 (div-interval c c))
(define dd2 (div-interval c d))

(print-percent-interval dd1)
(print-percent-interval dd2)
