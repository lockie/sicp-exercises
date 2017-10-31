#lang sicp


;; accumulate boilerplate
(define (accumulate proc initial sequence)
    (if (null? sequence)
        initial
        (proc (car sequence)
              (accumulate proc initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))


;; actual code

(define (dot-product v w)
    (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector (list (list 2 0 0 0)
                       (list 0 2 0 0)
                       (list 0 0 2 0)
                       (list 0 0 0 2))
                 (list 1 2 3 4))

(define (transpose mat)
    (accumulate-n (lambda (x y) (cons x y)) nil mat))

(define M (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(transpose M)


(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(matrix-*-matrix M (transpose M))
