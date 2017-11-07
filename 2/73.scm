#lang sicp


;; get/put boilerplate

(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *op-table* (make-hash))

(define (put op type proc)
    (hash-set! *op-table* (list op type) proc))

(define (get op type)
    (hash-ref *op-table* (list op type) '()))


;; type tag boilerplate

(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Некорректные помеченные данные -- TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Некорректные помеченные данные -- CONTENTS" datum)))


;; other boilerplate

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) (operands exp)
                                             var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;; actual code

; b part

(define (make-sum x1 x2) (attach-tag '+ (list x1 x2)))
(define (addend s) (car s))
(define (augend s) (cadr s))

(define (install-sum-package)
    (define (deriv-sum sum var)
        (make-sum (deriv (addend sum) var)
                  (deriv (augend sum) var)))

    ;; interface
    (put 'deriv '+ deriv-sum))


(define (make-product x1 x2) (attach-tag '* (list x1 x2)))
(define (multiplier s) (car s))
(define (multiplicand s) (cadr s))

(define (install-product-package)
    (define (deriv-product product var)
        (make-sum (make-product (deriv (multiplier product) var)
                                (multiplicand product))
                  (make-product (multiplier product)
                                (deriv (multiplicand product) var))))

    ;; interface
    (put 'deriv '* deriv-product))


(install-sum-package)
(install-product-package)

(deriv '(+ x 2) 'x)
(newline)
(deriv '(* x y) 'x)
(newline)


; c part

(define (make-exponentiation x n) (attach-tag '** (list x n)))
(define (base e) (car e))
(define (exponent e) (cadr e))

(define (install-exp-package)
    (define (deriv-exp exp var)
        (make-product (make-product (exponent exp)
                                    (make-exponentiation (base exp) (- (exponent exp) 1)))
                      (deriv (base exp) var)))


    ;; interface
    (put 'deriv '** deriv-exp))


(install-exp-package)

(deriv '(** x 3) 'x)
