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
    (let ((y1 (lower-bound y))
          (y2 (upper-bound y)))
        (cond ((or (= y1 0) (= y2 0))
              (error "Division by zero"))
              (else (mul-interval x
                                  (make-interval (/ 1.0 y2)
                                                 (/ 1.0 y1)))))))


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

(div-interval (make-interval 1 2)
              (make-interval 0 1))
