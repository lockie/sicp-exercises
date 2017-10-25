#lang sicp

(define (pascal-triangle row col)
  (cond ((= row 1) 1)
        ((= row 2) 1)
        ((= col 1) 1)
        ((= col row) 1)
        (else (+ 
                 (pascal-triangle (- row 1) (- col 1))
                 (pascal-triangle (- row 1) col) 
               )
        )
  )
)

(pascal-triangle 3 2)

(pascal-triangle 5 1)
(pascal-triangle 5 2)
(pascal-triangle 5 3)
(pascal-triangle 5 4)
(pascal-triangle 5 5)
