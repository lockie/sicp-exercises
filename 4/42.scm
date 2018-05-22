
;; boilerplate
(define (require p)
    (if (not p) (amb)))
(define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

;; actual code
(define (liar-grades)
    (let ((betty (amb 1 2 3 4 5))
          (ethel (amb 1 2 3 4 5))
          (joan (amb 1 2 3 4 5))
          (kitty (amb 1 2 3 4 5))
          (mary (amb 1 2 3 4 5)))
        (require
         (distinct? (list betty ethel joan kitty mary)))
        (require (or
                  (and (= kitty 2) (not (= betty 3)))
                  (and (not (= kitty 2)) (= betty 3))))
        (require (or
                  (and (= ethel 1) (not (= joan 2)))
                  (and (not (= ethel 1)) (= joan 2))))
        (require (or
                  (and (= joan 3) (not (= ethel 5)))
                  (and (not (= joan 3)) (= ethel 5))))
        (require (or
                  (and (= kitty 2) (not (= mary 4)))
                  (and (not (= kitty 2)) (= mary 4))))
        (require (or
                  (and (= mary 4) (not (= betty 1)))
                  (and (not (= mary 4)) (= betty 1))))
        (list (list 'betty betty)
              (list 'ethel ethel)
              (list 'joan joan)
              (list 'kitty kitty)
              (list 'mary mary))))

(liar-grades)
