; boilerplate
(define (require p)
    (if (not p) (amb)))
(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

; actual code
(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
    (if (= low high)
        (amb)
        (amb low (an-integer-between (+ low 1) high))))

(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high)))
        (let ((j (an-integer-between i high)))
            (let ((k (an-integer-between j high)))
                (require (= (+ (* i i) (* j j)) (* k k)))
                (list i j k)))))

(define (a-pythagorean-triple-greater-than low)
    (let ((high (an-integer-starting-from low)))
        (a-pythagorean-triple-between 1 high)))


(a-pythagorean-triple-greater-than 1)
