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

(define (queens board-size)
    (define empty-board nil)
    (define (adjoin-position row col board)
        (cons (cons row col) board))
    (define (safe? col board)
        (let ((queen (car board))) ; new queen is always at head of list
            (not (accumulate (lambda (x y) (or x y)) false
                        (map (lambda (q)
                                 (or (= (car q) (car queen))
                                     (= (cdr q) (cdr queen))
                                     (= (- (car q) (cdr q)) (- (car queen) (cdr queen)))))
                                 (cdr board))))))
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
             (lambda (positions) (safe? k positions))
             (flatmap
              (lambda (rest-of-queens)
                  (map (lambda (new-row)
                           (adjoin-position new-row k rest-of-queens))
                       (enumerate-interval 1 board-size)))
              (queen-cols (- k 1))))))
    (queen-cols board-size))

(queens 8)
