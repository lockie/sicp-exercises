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


;; some required boilerplate

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

(define (apply-generic op . args)
    (define (coerce-arg arg type)
        (if (eq? (type-tag arg) type)
            arg
            (let ((raise (get 'raise (list (type-tag arg)))))
                (if (null? raise)
                    nil
                    (coerce-arg (raise (contents arg)) type)))))
    (define (coerce-args args type)
        (if (null? args)
            nil
            (let ((coerced (coerce-arg (car args) type)))
                (if (null? coerced)
                    false
                    (let ((tail (coerce-args (cdr args) type)))
                        (if (not tail)
                            false
                            (cons coerced tail)))))))
    (define (try-coerce args type-tags)
        (if (null? type-tags)
            false
            (let ((coerced (coerce-args args (car type-tags))))
                (if (not coerced)
                    (try-coerce args (cdr type-tags))
                    coerced))))
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if (not (null? proc))
                (apply proc (map contents args))
                (let ((coerced (try-coerce args type-tags)))
                    (if (not coerced)
                        (error "Нет метода для этих типов"
                               (list op type-tags))
                        (apply apply-generic (cons op coerced))))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (sqroot x) (apply-generic 'sqroot x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctg y x) (apply-generic 'arctg y x))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (raise x) (apply-generic 'raise x))

(define (square x) (mul x x))


;; scheme number boilerplate

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x))
    (define (raise x) (make-rational x 1))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'sqroot '(scheme-number) (lambda (x) (tag (sqrt x))))
    (put 'sine '(scheme-number) (lambda (x) (tag (sin x))))
    (put 'cosine '(scheme-number) (lambda (x) (tag (cos x))))
    (put 'arctg '(scheme-number scheme-number) (lambda (y x) (tag (atan y x))))
    (put 'raise '(scheme-number) raise)
    (put 'make 'scheme-number
         (lambda (x) (tag x))))

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))


;;  rational boilerplate

(define (install-rational-package)
    ;; внутренние процедуры
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (raise n) (make-complex-from-real-imag (/ (numer n) (denom n)) 0))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))
    ;; интерфейс к остальной системе
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
    (put 'sqroot '(rational) (lambda (x) (tag (make-rat (sqrt (numer x)) (sqrt (denom x))))))
    (put 'sine '(rational) (lambda (x) (make-scheme-number (sin (/ (numer x) (denom x))))))
    (put 'cosine '(rational) (lambda (x) (make-scheme-number (cos (/ (numer x) (denom x))))))
    (put 'arctg '(rational rational) (lambda (y x) (make-scheme-number
                                                    (atan (/ (numer y) (denom y))
                                                          (/ (numer x) (denom x))))))
    (put 'raise '(rational) raise)
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d)))))

(define (make-rational n d)
    ((get 'make 'rational) n d))


;; complex boilerplate

(define (install-rectangular-package)
    ;; внутренние процедуры
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqroot (add (square (real-part z))
                   (square (imag-part z)))))
    (define (angle z)
        (arctg (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (mul r (cos a)) (mul r (sin a))))
    ;; интерфейс к остальной системе
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (install-polar-package)
    ;; внутренние процедуры
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
        (mul (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (mul (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
        (cons (sqroot (add (square x) (square y)))
              (arctg y x)))
    ;; интерфейс к остальной системе
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (install-complex-package)
    ;; процедуры, импортируемые из декартова
    ;; и полярного пакетов
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))
    ;; внутренние процедуры
    (define (add-complex z1 z2)
        (make-from-real-imag (add (real-part z1) (real-part z2))
                             (add (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (sub (real-part z1) (real-part z2))
                             (sub (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                           (add (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                           (sub (angle z1) (angle z2))))
    ;; интерфейс к остальной системе
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'magnitude '(complex) (lambda (z) (magnitude z)))
    (put 'angle '(complex) (lambda (z) (angle z)))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))


;; actual code


(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


(add (make-complex-from-real-imag (make-scheme-number 1) (make-scheme-number 2))
     (make-complex-from-real-imag (make-scheme-number 3) (make-scheme-number 4)))
(magnitude (make-complex-from-real-imag (make-scheme-number 0) (make-rational 3 2)))
(mul (make-scheme-number 2)
   (angle (make-complex-from-real-imag (make-scheme-number 0) (make-rational 3 2))))
