#lang sicp


;; (define (make-frame origin edge1 edge2)
;;     (list origin edge1 edge2))

;; (define (origin-frame f) (car f))

;; (define (edge1-frame f) (cadr f))

;; (define (edge2-frame f) (caddr f))


(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame f) (car f))

(define (edge1-frame f) (cadr f))

(define (edge2-frame f) (cddr f))


(define f (make-frame 1 2 3))

(origin-frame f)
(edge1-frame f)
(edge2-frame f)
