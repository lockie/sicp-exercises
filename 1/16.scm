#lang sicp

(define (fast-expt b n)
  (fast-expt-iter b n 1 0))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt-iter b n a cntr)
  (cond ((= cntr n) a)
        (else (cond ((even? n) (fast-expt-iter (square b) (/ n 2) a cntr))
                    (else (fast-expt-iter b n (* a b) (+ cntr 1)))
              )
        )
   )
)

(fast-expt 2 2)
(fast-expt 2 3)
(fast-expt 2 4)
(fast-expt 2 10)

(fast-expt 3 2)
(fast-expt 3 3)
(fast-expt 3 4)

(fast-expt 10 5)
;(fast-expt 10 1000)
