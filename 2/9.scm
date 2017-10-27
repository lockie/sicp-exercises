#lang sicp


(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

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


(define (sub-interval x y)
    (let ((p1 (- (lower-bound x) (lower-bound y)))
          (p2 (- (upper-bound x) (upper-bound y)))
          (p3 (- (lower-bound x) (upper-bound y)))
          (p4 (- (upper-bound x) (lower-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))


(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))


(define (radius interval) (/ (- (upper-bound interval) (lower-bound interval)) 2.))


(define a (make-interval 0.1 0.5))
(define b (make-interval 1 5))

(define a1 (make-interval 5.1 5.5))
(define b1 (make-interval 10 14))

; 2.2
(radius (add-interval a b))
(radius (add-interval a1 b1))

; 1.2 vs 13
(radius (mul-interval a b))
(radius (mul-interval a1 b1))
