#lang sicp


(define (has-cycle x)
    (define (visited-pair? p pairs)
        (cond ((null? pairs) false)
              ((eq? p (car pairs)) true)
              (else (visited-pair? p (cdr pairs)))))
    (let ((visited-pairs '()))
        (define (has-cycle-internal x)
            (if (not (pair? x))
                false
                (let ((visited (visited-pair? x visited-pairs)))
                    (begin (set! visited-pairs (append visited-pairs (list x)))
                           (if visited
                               true
                               (has-cycle-internal (cdr x)))))))
        (has-cycle-internal x)))


(has-cycle (list 1 2 3))

(define t (list 1 2))
(has-cycle (cons t t))

(define a (list 1 2 3))
(set-cdr! (cddr a) a)
(has-cycle a)
