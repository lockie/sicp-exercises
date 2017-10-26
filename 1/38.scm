#lang sicp


(define (cont-frac n d k)
    (define (frac-iter i frac)
        (cond ((= i 0) frac)
              (else (frac-iter (- i 1) (/ (n i) (+ frac (d i)))))))
    (frac-iter k 0))


(+ 2 (cont-frac (lambda (i) 1.)
                (lambda (i) (cond ((= 0 (remainder (+ i 1) 3)) (+ 1(floor (/ (* i 2) 3))))
                                  (else 1)))
                20))
