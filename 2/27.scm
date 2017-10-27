#lang sicp


(define (deep-reverse l)
    (cond ((null? l) nil)
          (else (let ((head (car l)))
                    (append (deep-reverse (cdr l))
                            (cond ((pair? head) (list (deep-reverse head)))
                                  (else (list head))))))))


(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
