#lang sicp


(define (cont-frac n d k)
    (define (frac-iter i frac)
        (cond ((= i 0) frac)
              (else (frac-iter (- i 1) (/ (n i) (+ frac (d i)))))))
    (frac-iter k 0))


(define (square x) (* x x))

(define (tan-cf x k)
    (cont-frac (lambda (i) (cond ((= i 1) x)
                                 (else (- (square x)))))
               (lambda (i) (- (* 2 i) 1))
               k))

(tan-cf 3.14159265359 100)
