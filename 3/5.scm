#lang sicp
(#%require (only racket random))


(define (sqr x) (* x x))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (* range (random)))))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((experiment)
               (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else
               (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (estimateintegral P x1 y1 x2 y2 trials)
    (exact->inexact (* (+ (- x2 x1) (- y2 y1))
                       (monte-carlo trials
                                    (lambda () (P (random-in-range x1 x2)
                                                  (random-in-range y1 y2)))))))


(* 2 (estimateintegral (lambda (x y) (< (+ (sqr x) (sqr y)) 1)) 0 0 1 1 1000000))
