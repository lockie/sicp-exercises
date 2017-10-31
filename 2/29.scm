#lang sicp


;; (define (make-mobile left right)
;;     (list left right))

;; (define (make-branch length structure)
;;     (list length structure))

;; (define (left-branch mobile)
;;     (car mobile))

;; (define (right-branch mobile)
;;     (cadr mobile))

;; (define (branch-length branch)
;;     (car branch))

;; (define (branch-structure branch)
;;     (cadr branch))


(define (make-mobile left right)
    (cons left right))

(define (make-branch length structure)
    (cons length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cdr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cdr branch))


(define (total-weight mobile)
    (define (branch-weight branch)
        (let ((structure (branch-structure branch)))
            (cond ((pair? structure) (total-weight structure))
                  (else structure))))
    (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (balanced? mobile)
    (define (branch-weight branch)
        (let ((structure (branch-structure branch)))
            (cond ((pair? structure) (total-weight structure))
                  (else structure))))
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
        (and (= (* (branch-weight left) (branch-length left))
                (* (branch-weight right) (branch-length right)))
             (cond ((pair? (branch-structure left)) (balanced? (branch-structure left)))
                   (else true))
             (cond ((pair? (branch-structure right)) (balanced? (branch-structure right)))
                   (else true)))))


(define test-mobile (make-mobile
                     (make-branch 4 3)
                     (make-branch 3
                                  (make-mobile (make-branch 2 2) (make-branch 2 2)))))

(define other-test-mobile (make-mobile
                           (make-branch 4 4)
                           (make-branch 4
                                        (make-mobile (make-branch 1 2) (make-branch 2 2)))))


(total-weight test-mobile)
(total-weight other-test-mobile)
(balanced? test-mobile)
(balanced? other-test-mobile)
