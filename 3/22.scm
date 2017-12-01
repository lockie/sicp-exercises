#lang sicp


(define (empty-queue? queue) (null? (front-ptr queue)))

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


;; actual code

(define (make-queue)
    (let ((front-ptr nil)
          (rear-ptr nil))
        (define (dispatch m)
            (cond ((eq? m 'front-ptr) (lambda () front-ptr))
                  ((eq? m 'rear-ptr) (lambda () rear-ptr))
                  ((eq? m 'set-front-ptr!) (lambda (item) (set! front-ptr item)))
                  ((eq? m 'set-rear-ptr!) (lambda (item) (set! rear-ptr item)))
                  )
            )
        dispatch))

(define (front-ptr queue) ((queue 'front-ptr)))
(define (rear-ptr queue) ((queue 'rear-ptr)))
(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item))


(define q1 (make-queue))

(print-queue (insert-queue! q1 'a)) (newline)

(print-queue (insert-queue! q1 'b)) (newline)

(print-queue (delete-queue! q1)) (newline)

(print-queue (delete-queue! q1)) (newline)
