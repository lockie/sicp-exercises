#lang sicp


(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car p)
    (define (car-iter i n)
        (cond ((not (= (remainder n 2) 0)) i)
              (else (car-iter (+ i 1) (/ n 2)))))
    (car-iter 0 p))

(define (cdr p)
    (define (cdr-iter i n)
        (cond ((not (= (remainder n 3) 0)) i)
              (else (cdr-iter (+ i 1) (/ n 3)))))
    (cdr-iter 0 p))


(car (cons 1 2))
(cdr (cons 1 2))
(car (cons 2 1))
(cdr (cons 2 1))
