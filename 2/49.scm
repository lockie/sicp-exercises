#lang sicp
(#%require sicp-pict)


(define (top-left frame)
    (vector-add (frame-origin frame) (frame-edge1 frame)))

(define (bottom-right frame)
    (vector-add (frame-origin frame) (frame-edge2 frame)))

(define (top-right frame)
    (vector-add (vector-add (frame-origin frame) (frame-edge1 frame)) (frame-edge2 frame)))

(define (x-half v)
    (make-vect (/ (vector-xcor v) 2.) (vector-ycor v)))

(define (y-half v)
    (make-vect (vector-xcor v) (/ (vector-ycor v) 2.)))

(define (outline frame)
  (segments->painter
   (list
    (make-segment (frame-origin frame) (top-left frame))
    (make-segment (top-left frame) (top-right frame))
    (make-segment (top-right frame) (bottom-right frame))
    (make-segment (bottom-right frame) (frame-origin frame)))))

(define (cross frame)
    (segments->painter
     (list
      (make-segment (frame-origin frame) (top-right frame))
      (make-segment (top-left frame) (bottom-right frame)))))

(define (diamond frame)
    (segments->painter
     (list
      (make-segment (y-half (top-left frame)) (x-half (top-right frame)))
      (make-segment (x-half (top-right frame)) (y-half (top-right frame)))
      (make-segment (y-half (top-right frame)) (x-half (bottom-right frame)))
      (make-segment (x-half (bottom-right frame)) (y-half (top-left frame))))))

;; TODO okay I'm too lazy to re-create wave painter :|


(define frame (make-frame (make-vect 0 0) (make-vect 0 0.8) (make-vect 0.8 0)))

(paint (outline frame))
(paint (cross frame))
(paint (diamond frame))
