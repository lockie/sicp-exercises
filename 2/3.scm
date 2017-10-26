#lang sicp


; point boilerplate

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

; segment boilerplate

(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))


; first implementation

;(define (make-rectangle top-left bottom-right)
;    (cons top-left bottom-right))
;
;(define (rectangle-top-left rectangle)
;    (car rectangle))
;
;(define (rectangle-bottom-right rectangle)
;    (cdr rectangle))

(define (rectangle-width rectangle)
    (- (x-point (rectangle-bottom-right rectangle)) (x-point (rectangle-top-left rectangle))))

(define (rectangle-height rectangle)
    (- (y-point (rectangle-top-left rectangle)) (y-point (rectangle-bottom-right rectangle))))

(define (perimeter rectangle)
    (* 2 (+
          (rectangle-width rectangle)
          (rectangle-height rectangle))))

(define (square rectangle)
    (*
     (rectangle-width rectangle)
     (rectangle-height rectangle)))


;(define test-rectangle (make-rectangle (make-point 0 5) (make-point 10 0)))
;(perimeter test-rectangle)  ; 30
;(square test-rectangle)  ; 50


; another implementation

(define (make-rectangle base apex)
    (cons base apex))

(define (rectangle-base rectangle)
    (car rectangle))

(define (rectangle-apex rectangle)
    (cdr rectangle))

(define (rectangle-top-left rectangle)
    (end-segment (rectangle-apex rectangle)))

(define (rectangle-bottom-right rectangle)
    (end-segment (rectangle-base rectangle)))


(define test-rectangle (make-rectangle
                        (make-segment (make-point 0 0) (make-point 10 0))
                        (make-segment (make-point 0 0) (make-point 0 5))))
(perimeter test-rectangle)  ; 30
(square test-rectangle)  ; 50
