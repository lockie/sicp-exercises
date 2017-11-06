#lang sicp


;; boilerplate

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? object)
    (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (successive-merge (adjoin-set
                           (make-code-tree (car leaf-set) (cadr leaf-set))
                           (cddr leaf-set)))))

(define (encode-symbol symbol tree)
    (define (in? x l)
        (cond ((null? l) false)
              ((equal? x (car l)) true)
              (else (in? x (cdr l)))))
    (cond ((and (leaf? tree) (equal? symbol (car (symbols tree)))) nil)
          ((in? symbol (symbols (left-branch tree))) (cons '0 (encode-symbol symbol (left-branch tree))))
          ((in? symbol (symbols (right-branch tree))) (cons '1 (encode-symbol symbol (right-branch tree))))
          (else (error "Unknown symbol " symbol))))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))


;; actual code

(define tree (generate-huffman-tree
              '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

; lold hardly :D
(define result (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
                       tree))

(display result)
(newline)
(display (length result))  ; 84 vs 36 * 3bits = 108
