#lang sicp


;; boilerplate

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))


;; actual code

(define (mul-rat x y)
    (define (is-negative?) (< (* (numer x) (numer y) (denom x) (denom y)) 0))
    (make-rat (* (abs (numer x)) (abs (numer y)) (cond ((is-negative?) -1) (else 1)))
              (* (abs (denom x)) (abs (denom y)))))

(print-rat (mul-rat (make-rat -1 3) (make-rat 1 2)))
(print-rat (mul-rat (make-rat 1 3) (make-rat -1 2)))
(print-rat (mul-rat (make-rat -1 3) (make-rat -1 2)))
(print-rat (mul-rat (make-rat 1 -3) (make-rat 1 2)))
(print-rat (mul-rat (make-rat 1 -3) (make-rat -1 2)))
(print-rat (mul-rat (make-rat -1 -3) (make-rat -1 2)))  ; wut
