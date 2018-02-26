#lang sicp


(define (let*->nested-lets exp)
    (let ((vars (car exp))
          (first-var (caar exp))
          (body (cadr exp)))
        (cond ((null? (cdr vars))
               (list 'let (list first-var) body))
              (else
               (list 'let (list first-var) (let*->nested-lets (list (cdr vars) body)))))))


(display (let*->nested-lets '(
                              ((a 1)
                               (b (* 2 a)))
                              (+ a b))))
