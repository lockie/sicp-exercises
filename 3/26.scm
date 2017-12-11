#lang sicp


(define (make-table)
    (let ((local-table '()))
        ;; tree boilerplate
        (define (entry tree) (car tree))
        (define (left-branch tree) (cadr tree))
        (define (right-branch tree) (caddr tree))
        (define (make-tree entry left right)
            (list entry left right))
        ;; recordset boilerplate
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
        ; external procedures
        (define (lookup k)
            (define (lookup-iter table)
                (if (null? table)
                    false
                    (let ((top-key (key (entry table))))
                        (cond ((= k top-key) (value (entry table)))
                              ((< k top-key) (lookup-iter (left-branch table)))
                              ((> k top-key) (lookup-iter (right-branch table)))
                              (else false)))))
            (lookup-iter local-table))
        (define (insert! k v)
            (let ((record (lookup k)))
                (if record
                    (set-cdr! record v)
                    (set! local-table (add-record (make-record k v) local-table)))))
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Неизвестная операция -- TABLE" m))))
        dispatch))


(define t (make-table))

((t 'insert-proc!) 1 "Вася")
((t 'insert-proc!) 2 "Петя")

((t 'lookup-proc) 1)
((t 'lookup-proc) 2)
((t 'lookup-proc) 3)
