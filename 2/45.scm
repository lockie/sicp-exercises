#lang sicp
(#%require sicp-pict)


(define (split op1 op2)
    (define (split-inner painter n)
        (if (= n 0)
            painter
            (let ((smaller (split-inner painter (- n 1))))
                (op1 painter (op2 smaller smaller)))))
    split-inner)

(define right-split (split beside below))
(define up-split (split below beside))


(paint (up-split einstein 2))
(paint (right-split einstein 3))
