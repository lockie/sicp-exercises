#lang sicp


;(define (cont-frac n d k)
;    (define (frac i)
;        (cond ((= i k) (/ (n i) (d i)))
;              (else (/ (n i) (+ (d i) (frac (+ i 1)))))))
;    (frac 1))

(define (cont-frac n d k)
    (define (frac-iter i frac)
        (cond ((= i 0) frac)
              (else (frac-iter (- i 1) (/ (n i) (+ frac (d i)))))))
    (frac-iter k 0))


(define (reverse-phi steps)
    (cont-frac (lambda (i) 1.)
               (lambda (i) 1.) steps))

(reverse-phi 1)
(reverse-phi 2)
(reverse-phi 3)
(reverse-phi 4)
(reverse-phi 5)
(reverse-phi 6)
(reverse-phi 7)
(reverse-phi 8)
(reverse-phi 9)
(reverse-phi 10)
(reverse-phi 11)  ; <--- k
