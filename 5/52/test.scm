(define factorial (lambda(n) (if (= n 0) 1 (* n (factorial (- n 1))))))
(factorial 10)

(define (filter pred lst) (if (null? lst) nil (begin (if (pred (car lst)) (cons (car lst) (filter pred (cdr lst))) (filter pred (cdr lst))))))
(define (even? x) (= (remainder x 2) 0))
(filter even? (quote (1 2 3 4 5 6 7 8 9 10)))

(exit)

