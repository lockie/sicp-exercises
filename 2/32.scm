#lang sicp


(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (l) (cons (car s) l)) rest)))))

(subsets (list 1 2 3))
