#lang sicp


;; a

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


;; b

(define (and->if exp)
    (cond ((null? exp) 1)
          (else (make-if (car exp)
                         (and->if (cdr exp))
                         0))))

(define (or->if exp)
    (cond ((null? exp) 0)
          (else (make-if (car exp)
                         1
                         (or->if (cdr exp))))))
