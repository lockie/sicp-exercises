#lang sicp


(define (make-table)
    (let ((local-table (list '*table*)))
        (define (assoc key records)
            (cond ((null? records) false)
                  ((equal? key (caar records)) (car records))
                  (else (assoc key (cdr records)))))
        (define (lookup keys)
            (define (lookup-iter keys subtable)
                (cond ((null? keys) (error "Нет ключей -- LOOKUP"))
                      ((null? subtable) false)
                      ((null? (cdr keys))
                       ; car keys. lol.
                       (let ((record (assoc (car keys) (cdr subtable))))
                           (if record
                               (cdr record)
                               false)))
                      (else
                       (let ((next-subtable (assoc (car keys) (cdr subtable))))
                           (if next-subtable
                               (lookup-iter (cdr keys) next-subtable)
                               false)))))
            (lookup-iter keys local-table))
        (define (insert! keys value)
            (define (insert-iter keys subtable)
                (cond ((null? keys) (error "Нет ключей -- INSERT"))
                      ((null? (cdr keys))
                       (let ((record (assoc (car keys) (cdr subtable))))
                           (if record
                               (set-cdr! record value)
                               (set-cdr! subtable
                                         (cons (cons (car keys) value)
                                               (cdr subtable))))))
                      (else
                       (let ((next-subtable (assoc (car keys) (cdr subtable))))
                           (if (not next-subtable)
                               (begin
                                   (set-cdr! subtable
                                             (cons (list (car keys)
                                                         '(*table*))
                                                   (cdr subtable)))
                                   (set! next-subtable (cadr subtable))))
                           (insert-iter (cdr keys) next-subtable)))))
            (insert-iter keys local-table))
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Неизвестная операция -- TABLE" m))))
        dispatch))


(define t (make-table))

((t 'insert-proc!) '(1 2 3 4) "Вася")
((t 'insert-proc!) '(1 2 3 5) "Петя")
((t 'insert-proc!) '(6 7) "Маша")

((t 'lookup-proc) '(1 2 3 4))
((t 'lookup-proc) '(1 2 3 5))
((t 'lookup-proc) '(3 4))
((t 'lookup-proc) '(6 7))
