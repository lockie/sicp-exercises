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


(define test (make-center-percent 100 1))

(display test)
(newline)

(percent test)
