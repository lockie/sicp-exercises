#lang sicp


;; get/put boilerplate

(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *op-table* (make-hash))

(define (put op type proc)
    (hash-set! *op-table* (list op type) proc))

(define (get op type)
    (hash-ref *op-table* (list op type) '()))


; type tag boilerplate

(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Некорректные помеченные данные -- TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Некорректные помеченные данные -- CONTENTS" datum)))

(define (division file) (type-tag file))


; example: west division

(define (install-west-package)
    (define (get-name record) (car record))
    (define (get-salary record) (cadr record))
    (define (get-record employee-name file)
        (cond ((null? file) false)
              ((eq? employee-name (get-name (car file))) (car file))
              (else (get-record employee-name (cdr file)))))
    ; interface
    (put 'west 'record get-record)
    (put 'west 'salary get-salary))


; example: east divison - different order
;  could be even a tree, but I don't know how to compare employee name strings

(define (install-east-package)
    (define (get-name record) (cadr record))
    (define (get-salary record) (car record))
    (define (get-record employee-name file)
        (cond ((null? file) false)
              ((eq? employee-name (get-name (car file))) (car file))
              (else (get-record employee-name (cdr file)))))
    ; interface
    (put 'east 'record get-record)
    (put 'east 'salary get-salary))


; actual code

(define (get-record employee-name file)
    ((get (division file) 'record) employee-name (contents file)))

(define (get-salary record file)
    ((get (division file) 'salary) record))

(define (find-employee-record employee-name division-list)
    (if (null? division-list)
        nil
        (or (get-record employee-name (car division-list))
            (find-employee-record employee-name (cdr division-list)))))


(install-west-package)
(install-east-package)

(define west-file '(west ("Вася" 1000) ("Петя" 2000)))
(define east-file '(east (1000 "Маша") (2000 "Света")))

(get-salary (get-record "Петя" west-file) west-file)
(get-salary (get-record "Маша" east-file) east-file)
(find-employee-record "Маша" (list west-file east-file))
