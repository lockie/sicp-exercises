#lang sicp


(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
          (else
           (error "неизвестный тип выражения -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum . args)
    (cond ((=number? (car args) 0) (cadr args))
          ((=number? (cadr args) 0) (car args))
          (else (cons '+ args))))

(define (make-product . args)
    (cond ((=number? (car args) 0) 0)
          ((=number? (cadr args) 0) 0)
          ((=number? (car args) 1) (cadr args))
          ((=number? (cadr args) 1) (car args))
          (else (cons '* args))))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (cons '+ (cddr s))))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (cons '* (cddr p))))


(deriv '(+ x 3) 'x)
(newline)
(deriv '(* 2 x y) 'x)
(newline)
(deriv '(* x y (+ x 3)) 'x)
