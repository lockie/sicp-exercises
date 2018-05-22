#lang sicp


(define (caddddr l)
    (car (cddddr l)))

(define (permutations s)
    (cond ((null? s) '())
          ((null? (cdr s)) (list s))
          (else
           (let splice ((l '()) (m (car s)) (r (cdr s)))
               (append
                (map (lambda (x) (cons m x))
                     (permutations (append l r)))
                (if (null? r)
                    '()
                    (splice (cons m l) (car r) (cdr r))))))))

(define (multiple-dwelling)
    (define (fits? baker cooper fletcher miller smith)
        (and (not (= baker 5))
             (not (= cooper 1))
             (not (= fletcher 5))
             (not (= fletcher 1))
             (> miller cooper)
             (not (= (abs (- smith fletcher)) 1))
             (not (= (abs (- fletcher cooper)) 1))))
    (define (multiple-dwelling-iter p)
        (if (not (null? p))
            (let* ((variant (car p))
                   (baker (car variant))
                   (cooper (cadr variant))
                   (fletcher (caddr variant))
                   (miller (cadddr variant))
                   (smith (caddddr variant)))
                (if (fits? baker cooper fletcher miller smith)
                    (begin
                        (display (list (list 'baker baker)
                                       (list 'cooper cooper)
                                       (list 'fletcher fletcher)
                                       (list 'miller miller)
                                       (list 'smith smith)))
                        (newline)))
                (multiple-dwelling-iter (cdr p)))))
    (multiple-dwelling-iter (permutations '(1 2 3 4 5))))

(multiple-dwelling)
