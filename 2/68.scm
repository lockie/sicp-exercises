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


;; actual code

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


(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree (make-leaf 'D 1)
                                     (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree)
