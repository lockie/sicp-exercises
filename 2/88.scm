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
    (if (eq? type-tag 'scheme-number)
        contents
        (cons type-tag contents)))

(define (type-tag datum)
    (cond ((pair? datum) (car datum))
          ((number? datum) 'scheme-number)
          (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
    (cond ((pair? datum) (cdr datum))
          ((number? datum) datum)
          (else (error "Некорректные помеченные данные -- CONTENTS" datum))))

(define (apply-generic op . args)
    (define (coerce-arg arg type)
        (cond ((not (symbol? type)) (error "Некорректный тип -- APPLY_GENERIC" type))
              ((eq? (type-tag arg) type) arg)
              (else (let ((raise (get 'raise (list (type-tag arg)))))
                        (if (null? raise)
                            nil
                            (coerce-arg (raise (contents arg)) type))))))
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
(define (negate x) (apply-generic 'negate x))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))


;; scheme-number boilerplate

(define (install-scheme-number-package)
    (put 'add '(scheme-number scheme-number) +)
    (put 'sub '(scheme-number scheme-number) -)
    (put 'mul '(scheme-number scheme-number) *)
    (put 'div '(scheme-number scheme-number) /)
    (put 'negate '(scheme-number) -)
    (put '=zero? '(scheme-number)
         (lambda (x) (= x 0)))
    (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
    (put 'make 'scheme-number identity))

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
    (define (negate-rat x)
        (make-rat (negate (numer x))
                  (denom x)))
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
    (put 'negate '(rational)
         (lambda (x) (tag (negate-rat x))))
    (put '=zero? '(rational)
         (lambda (x) (= (numer x) 0)))
    (put 'raise '(rational)
         (lambda (x) (make-polynomial 'y (adjoin-term
                                          (make-term 0 (/ (numer x) (denom x)))
                                          (the-empty-termlist)))))
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d)))))

(define (make-rational n d)
    ((get 'make 'rational) n d))


;; poly boilerplate

; uhm.
(define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
(define (the-empty-termlist) '())
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-polynomial-package)
    ;; внутренние процедуры
    ;; представление poly
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (variable? x) (symbol? x))
    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else
               (let ((t1 (first-term L1)) (t2 (first-term L2)))
                   (cond ((> (order t1) (order t2))
                          (adjoin-term
                           t1 (add-terms (rest-terms L1) L2)))
                         ((< (order t1) (order t2))
                          (adjoin-term
                           t2 (add-terms L1 (rest-terms L2))))
                         (else
                          (adjoin-term
                           (make-term (order t1)
                                      (add (coeff t1) (coeff t2)))
                           (add-terms (rest-terms L1)
                                      (rest-terms L2)))))))))
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (add-terms (term-list p1)
                                  (term-list p2)))
            (error "Многочлены от разных переменных -- ADD-POLY"
                   (list p1 p2))))
    (define (mul-poly p1 p2)
        (define (mul-terms L1 L2)
            (if (empty-termlist? L1)
                (the-empty-termlist)
                (add-terms (mul-term-by-all-terms (first-term L1) L2)
                           (mul-terms (rest-terms L1) L2))))
        (define (mul-term-by-all-terms t1 L)
            (if (empty-termlist? L)
                (the-empty-termlist)
                (let ((t2 (first-term L)))
                    (adjoin-term
                     (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms L))))))
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (mul-terms (term-list p1)
                                  (term-list p2)))
            (error "Многочлены от разных переменных -- MUL-POLY"
                   (list p1 p2))))
    (define (negate-poly p)
        (define (negate-terms L)
            (cond ((empty-termlist? L) L)
                  (else (let ((t (first-term L)))
                            (adjoin-term
                             (make-term (order t) (negate (coeff t)))
                             (negate-terms (rest-terms L)))))))
        (make-poly (variable p) (negate-terms (term-list p))))
    ;; интерфейс к остальной системе
    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
         (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(polynomial polynomial)
         (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
    (put 'mul '(polynomial polynomial)
         (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
    (put '=zero? '(polynomial)
         (lambda (p) (empty-termlist? (term-list p))))
    (put 'make 'polynomial
         (lambda (var terms) (tag (make-poly var terms)))))

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))


;; actual code

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)

(define A (make-polynomial 'x (adjoin-term (make-term 2 2)
                                           (adjoin-term (make-term 1 1)
                                                        (adjoin-term (make-term 0 0)
                                                                     (the-empty-termlist))))))
(define B (make-polynomial 'x (adjoin-term (make-term 1 1)
                                           (adjoin-term (make-term 0 1)
                                                        (the-empty-termlist)))))

(display A) (newline)
(display B) (newline)
(display (sub B A))
