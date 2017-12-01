#lang sicp


;; queue boilerplate

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT вызвана с пустой очередью" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
               (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
              (else
               (set-cdr! (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
           (error "DELETE! вызвана с пустой очередью" queue))
          (else
           (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))


;; actual code

(define (print-queue queue)
    (define (print-list list)
        (cond ((null? list) (display ""))
              (else (begin (display (car list))
                           (if (not (eq? (cdr list) nil))
                               (display " ")
                               (display ""))
                           (print-list (cdr list))))))
    (display "{")
    (print-list (front-ptr queue))
    (display "}"))


(define q1 (make-queue))

(print-queue (insert-queue! q1 'a)) (newline)

(print-queue (insert-queue! q1 'b)) (newline)

(print-queue (delete-queue! q1)) (newline)

(print-queue (delete-queue! q1)) (newline)
