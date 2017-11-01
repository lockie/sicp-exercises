#lang sicp


;; boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

; wut
(define (filter predicate l)
    (cond ((null? l) nil)
          (else (let ((head (car l))
                      (rest (cdr l)))
                    (if (predicate head)
                        (cons head (filter predicate rest))
                        (filter predicate rest))))))


;; actual code

(define (pairs n)
    (flatmap (lambda (i)
             (flatmap (lambda (j)
                      (map (lambda (k) (list i j k))
                           (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

;(pairs 4)

(define (solution n s)
    (filter (lambda (x) (= s (+ (car x) (cadr x) (caddr x)))) (pairs n)))

(solution 4 6)
