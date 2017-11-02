#lang sicp


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (list->tree elements)
    (define (partial-tree elts n)
        (if (= n 0)
            (cons '() elts)
            (let ((left-size (quotient (- n 1) 2)))
                (let ((left-result (partial-tree elts left-size)))
                    (let ((left-tree (car left-result))
                          (non-left-elts (cdr left-result))
                          (right-size (- n (+ left-size 1))))
                        (let ((this-entry (car non-left-elts))
                              (right-result (partial-tree (cdr non-left-elts)
                                                          right-size)))
                            (let ((right-tree (car right-result))
                                  (remaining-elts (cdr right-result)))
                                (cons (make-tree this-entry left-tree right-tree)
                                      remaining-elts))))))))
    (car (partial-tree elements (length elements))))

(define (tree->list tree)
    (if (null? tree)
        '()
        (append (tree->list (left-branch tree))
                (cons (entry tree)
                      (tree->list (right-branch tree))))))

;; (define (element-of-set? x set)
;;     (cond ((null? set) false)
;;           ((= x (entry set)) true)
;;           ((< x (entry set))
;;            (element-of-set? x (left-branch set)))
;;           ((> x (entry set))
;;            (element-of-set? x (right-branch set)))))
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set))))))

;; actual code

(define (union-set tree1 tree2)
    (define (union-set-list set1 set2)  ; lawls
        (cond ((null? set1) set2)
              ((null? set2) set1)
              (else (let ((x1 (car set1)) (x2 (car set2)))
                        (cond ((= x1 x2) (cons x1 (union-set-list (cdr set1) (cdr set2))))
                              ((< x1 x2) (cons x1 (union-set-list (cdr set1) set2)))
                              ((> x1 x2) (cons x2 (union-set-list set1 (cdr set2)))))))))
    (list->tree (union-set-list (tree->list tree1)
                                (tree->list tree2))))

(define (intersection-set tree1 tree2)
    (define (intersection-set-list set1 set2)
        (cond ((or (null? set1) (null? set2)) '())
              ((element-of-set? (car set1) set2)
               (cons (car set1)
                     (intersection-set-list (cdr set1) set2)))
              (else (intersection-set-list (cdr set1) set2))))
    (list->tree (intersection-set-list (tree->list tree1)
                                       (tree->list tree2))))


(define set1
    (adjoin-set 11
                (adjoin-set 7
                            (adjoin-set 9
                                        (adjoin-set 3
                                                    (adjoin-set 5 '()))))))

(define set2
    (adjoin-set 6
                (adjoin-set 8
                            (adjoin-set 2
                                        (adjoin-set 4
                                                    (adjoin-set 10 '()))))))

(union-set set1 set2)
(intersection-set set1 set2)
