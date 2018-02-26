#lang sicp


(define (let? exp) (tagged-list? exp 'let))

(define (make-lambda args body) (list 'lambda args body))

(define (let->combination exp)
    (let ((vars (car exp)))
        (cons (make-lambda (map car vars) (cadr exp))
              (map cadr vars))))
