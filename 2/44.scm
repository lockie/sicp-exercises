#lang sicp
(#%require sicp-pict)


;; boilerplate
(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00)))))


(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

(paint (up-split wave 1))
;(paint (up-split einstein 2))

