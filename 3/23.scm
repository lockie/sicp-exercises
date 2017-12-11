#lang sicp


;; it's doubly linked list internally, list of (item (prev next)) pairs
(define (set-prev! element new-prev) (set-car! (cdr element) new-prev))
(define (set-next! element new-next) (set-cdr! (cdr element) new-next))

(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))
(define (set-front-deque! deque item) (set-car! deque item))
(define (set-rear-deque! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (and (null? (car (front-deque deque)))
                                  (eq? (front-deque deque) (rear-deque deque))))

(define (make-deque)
    (let ((front (cons '() ; item = nil
                       (cons '() '())))) ; prev & next = nil
        (cons front front)))

(define (front-insert-deque! deque item)
    (cond ((empty-deque? deque)
           (set-car! (front-deque deque) item)
           deque)
          (else
           (let ((new-pair (cons item (cons '() (front-deque deque)))))
               (set-prev! (front-deque deque) new-pair)
               (set-front-deque! deque new-pair)
               deque))))

(define (rear-insert-deque! deque item)
    (cond ((empty-deque? deque)
           (set-car! (rear-deque deque) item)
           deque)
          (else
           (let ((new-pair (cons item (cons (rear-deque deque) '()))))
               (set-next! (rear-deque deque) new-pair)
               (set-rear-deque! deque new-pair)
               deque))))

(define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "FRONT-DELETE! вызвана с пустым деком" deque))
          ((eq? (front-deque deque) (rear-deque deque)) ; only one element
           (set-car! (front-deque deque) '())
           deque)
          (else
           (let ((new-front (cddr (front-deque deque))))
               (set-prev! new-front '())
               (set-front-deque! deque new-front)
               deque))))

(define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "REAR-DELETE! вызвана с пустым деком" deque))
          ((eq? (front-deque deque) (rear-deque deque)) ; only one element
           (set-car! (rear-deque deque) '())
           deque)
          (else
           (let ((new-rear (cadr (rear-deque deque))))
               (set-next! new-rear '())
               (set-rear-deque! deque new-rear)
               deque))))

;; actual code

(define (print-deque deque)
    (define (print-iter element)
        (cond ((null? element) (display ""))
              ((null? (car element)) (display ""))
              (else (begin (display (car element))
                           (if (not (eq? (cddr element) nil))
                               (display " ")
                               (display ""))
                           (print-iter (cddr element))))))
    (display "{")
    (print-iter (front-deque deque))
    (display "}"))


(define d1 (make-deque))

(print-deque d1) (newline)

(print-deque (front-insert-deque! d1 2)) (newline)

(print-deque (front-insert-deque! d1 1)) (newline)

(print-deque (rear-insert-deque! d1 3)) (newline)

(print-deque (rear-insert-deque! d1 4)) (newline)

(print-deque (rear-delete-deque! d1)) (newline)

(print-deque (front-delete-deque! d1)) (newline)

(print-deque (front-delete-deque! d1)) (newline)

(print-deque (front-delete-deque! d1)) (newline)
