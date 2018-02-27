#lang sicp


(define (let->combination exp)
    (cond ((symbol? (car exp))  ; named let
           (let ((name (car exp))
                 (vars (cadr exp)))
               (list
                (list 'define (list name (map car vars)) (caddr exp))
                (list name (map cadr vars)))))
          (else  ; usual let
           (let ((vars (car exp)))
               (cons (make-lambda (map car vars) (cadr exp))
                     (map cadr vars))))))
