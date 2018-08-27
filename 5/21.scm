
;; a
(define count-leaves-recursive-machine
    (make-machine
     '(t tmp r continue)  ; registers
     (list
      (list 'null? null?)
      (list 'pair? pair?)
      (list 'car car)
      (list 'cdr cdr)
      (list '+ +))  ; ops
     '(
       (assign continue (label count-leaves-done))
       count-leaves-loop
       (test (op null?) (reg t))
       (branch (label null))
       (test (op pair?) (reg t))
       (branch (label recurse))
       (assign r (const 1))
       (goto (reg continue))
       recurse
       (save continue)
       (assign continue (label after-count-1))
       (save t)
       (assign t (op car) (reg t))
       (goto (label count-leaves-loop))
       after-count-1
       (restore t)
       (restore continue)
       (save r)
       (save continue)
       (assign continue (label after-count-2))
       (assign t (op cdr) (reg t))
       (goto (label count-leaves-loop))
       after-count-2
       (restore continue)
       (assign tmp (reg r))
       (restore r)
       (assign r (op +) (reg r) (reg tmp))
       (goto (reg continue))
       null
       (assign r (const 0))
       (goto (reg continue))
       count-leaves-done)))

(set-register-contents! count-leaves-recursive-machine 't '((1 (2 3)) 4))
(start count-leaves-recursive-machine)
(get-register-contents count-leaves-recursive-machine 'r)


;; b

(define count-leaves-recursive-machine2
    (make-machine
     '(t n r continue)  ; registers
     (list
      (list 'null? null?)
      (list 'pair? pair?)
      (list 'car car)
      (list 'cdr cdr)
      (list '+ +))  ; ops
     '(
       (assign n (const 0))
       (assign continue (label count-leaves-done))
       count-iter
       (test (op null?) (reg t))
       (branch (label null))
       (test (op pair?) (reg t))
       (branch (label recurse))
       (assign r (op +) (reg n) (const 1))
       (goto (reg continue))
       recurse
       (save t)
       (assign t (op car) (reg t))
       (save continue)
       (assign continue (label after-count-1))
       (goto (label count-iter))
       after-count-1
       (restore continue)
       (restore t)
       (assign t (op cdr) (reg t))
       (assign n (reg r))
       (goto (label count-iter))
       null
       (assign r (reg n))
       (goto (reg continue))
       count-leaves-done)))

(set-register-contents! count-leaves-recursive-machine2 't '((1 (2 3)) 4))
(start count-leaves-recursive-machine2)
(get-register-contents count-leaves-recursive-machine2 'r)
