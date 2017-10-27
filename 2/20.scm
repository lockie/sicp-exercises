#lang sicp


(define (same-parity n . args)
    (define (filter-list l predicate)
        (cond ((eq? l nil) nil)
              (else (let ((head (car l))
                          (rest (cdr l)))
                        (if (predicate head)
                            (cons head (filter-list rest predicate))
                            (filter-list rest predicate))))))
    (cons n
          (cond ((even? n) (filter-list args even?))
                ((odd? n) (filter-list args odd?)))))


(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
