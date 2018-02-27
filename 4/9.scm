#lang sicp


;; (while (> i 0)
;;     (display i))
;; ->
;; (define (while-loop)
;;     (cond ((> i 0)
;;            (begin
;;                (display i)
;;                (while-loop)))
;;           (else 'done)))

(define (while->combination exp)
    (let ((condition (car exp))
          (body (cadr exp)))
        (list 'define (list 'while-loop)
              (list 'cond (list condition
                                'begin
                                body
                                (list 'while-loop))
                    (list 'else (list 'quote 'done))))))
