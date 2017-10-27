#lang sicp


(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((x1 (lower-bound x))
          (x2 (upper-bound x))
          (y1 (lower-bound y))
          (y2 (upper-bound y)))
        (cond ((and (positive? x1) (positive? y1)) (make-interval (* x1 y1) (* x2 y2)))
              ((and (positive? x1) (negative? y1)) (make-interval (* x2 y1) (* y2 (if (negative? y2) x1 x2))))
              ((and (negative? x1) (positive? y1)) (make-interval (* x1 y2) (* x2 (if (negative? x2) y1 y2))))
              ((and (positive? x2) (positive? y2)) (make-interval (min (* x1 y2) (* x2 y1)) (max (* x1 y1) (* x2 y2))))
              ((and (positive? x2) (negative? y2)) (make-interval (* x2 y1) (* x1 y1)))
              ((and (negative? x2) (positive? y2)) (make-interval (* x1 y2) (* x1 y1)))
              (else (make-interval (* x2 y2) (* x1 y1))))))


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

(mul-interval (make-interval -1 2)
              (make-interval -0.6 -0.5))
