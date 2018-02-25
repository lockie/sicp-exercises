#lang sicp


;; а

(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exp env)
    (cond ((null? exp) true)
          (else (let ((e (eval (car exp) env)))
                    (cond ((eq? e false) false)
                          (else (eval-and (cdr exp) env)))))))

(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
    (cond ((null? exp) false)
          (else (let ((e (eval (car exp) env)))
                    (cond ((eq? e true) true)
                          (else (eval-or (cdr exp) env)))))))


;; б
;; через make-if
