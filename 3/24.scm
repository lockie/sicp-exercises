#lang sicp


(define (make-table same-key?)
    (let ((local-table (list '*table*)))
        (define (lookup key)
            (let ((record (assoc key (cdr local-table))))
                (if record
                    (cdr record)
                    false)))
        (define (assoc key records)
            (cond ((null? records) false)
                  ((same-key? key (caar records)) (car records))
                  (else (assoc key (cdr records)))))
        (define (insert! key value)
            (let ((record (assoc key (cdr local-table))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! local-table
                              (cons (cons key value) (cdr local-table))))))
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Неизвестная операция -- TABLE" m))))
        dispatch))


(define t (make-table equal?))

((t 'insert-proc!) 1 "Вася")
((t 'insert-proc!) 2 "Петя")

((t 'lookup-proc) 1)
((t 'lookup-proc) 2)
