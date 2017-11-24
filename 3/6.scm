#lang sicp
(#%require (only racket random-seed random))


(define rand
    (let ((seed 0))
        (lambda (action)
            (cond ((eq? action 'generate)
                   (begin
                       (random-seed (inexact->exact (floor (* 1000 seed))))
                       (set! seed (random))
                       seed))
                  ((eq? action 'reset) (lambda (s) (set! seed s)))
                  (else (error "Uknown action"))))))

((rand 'reset) 100)
(rand 'generate)
(rand 'generate)
(newline)

((rand 'reset) 100)
(rand 'generate)
(rand 'generate)
(newline)

((rand 'reset) 200)
(rand 'generate)
(rand 'generate)
