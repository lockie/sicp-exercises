#lang sicp


;; tree boilerplate

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))


;; record boilerplate

(define (make-record key value) (cons key value))

(define (value record) (cdr record))

(define (key record) (car record))

(define (add-record x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= (key x) (key (entry set))) set)
          ((< (key x) (key (entry set)))
           (make-tree (entry set)
                      (add-record x (left-branch set))
                      (right-branch set)))
          ((> (key x) (key (entry set)))
           (make-tree (entry set)
                      (left-branch set)
                      (add-record x (right-branch set))))))

;; actual code

(define (lookup given-key set-of-records)
    (if (null? set-of-records)
        false
        (let ((top-key (key (entry set-of-records))))
            (cond ((= given-key top-key) (value (entry set-of-records)))
                  ((< given-key top-key) (lookup given-key (left-branch set-of-records)))
                  ((> given-key top-key) (lookup given-key (right-branch set-of-records)))))))


(define records
    (add-record (make-record 2 "Коля")
                (add-record (make-record 1 "Петя")
                            (add-record (make-record 4 "Маша")
                                        (add-record (make-record 3 "Вася") nil)))))

(lookup 1 records)
(lookup 2 records)
(lookup 3 records)
(lookup 4 records)
(lookup 5 records)
