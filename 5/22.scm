
;; a

(define append-machine
    (make-machine
     '(x y tmp r continue)  ; registers
     (list
      (list 'null? null?)
      (list 'cons cons)
      (list 'car car)
      (list 'cdr cdr))  ; ops
     '(
       (assign continue (label append-done))
       append-loop
       (test (op null?) (reg x))
       (branch (label x-null))
       (save x)
       (assign x (op cdr) (reg x))
       (save continue)
       (assign continue (label after-recurse))
       (goto (label append-loop))
       after-recurse
       (restore continue)
       (restore x)
       (assign tmp (op car) (reg x))
       (assign r (op cons) (reg tmp) (reg r))
       (goto (reg continue))
       x-null
       (assign r (reg y))
       (goto (reg continue))
       append-done)))

(set-register-contents! append-machine 'x '(1 2 3))
(set-register-contents! append-machine 'y '(4 5 6))
(start append-machine)
(get-register-contents append-machine 'r)
(get-register-contents append-machine 'x)


;; b

(define append!-machine
    (make-machine
     '(x y tmp r continue)  ; registers
     (list
      (list 'null? null?)
      (list 'cdr cdr)
      (list 'set-cdr! set-cdr!))  ; ops
     '(
       (save x)
       last-pair-loop
       (assign tmp (op cdr) (reg x))
       (test (op null?) (reg tmp))
       (branch (label x-null))
       (assign x (reg tmp))
       (goto (label last-pair-loop))
       x-null
       (assign r (reg x)) ;; r = last-pair x
       (restore x)
       (assign tmp (op set-cdr!) (reg r) (reg y))
       (assign r (reg x)))))

(set-register-contents! append!-machine 'x '(1 2 3))
(set-register-contents! append!-machine 'y '(4 5 6))
(start append!-machine)
(get-register-contents append!-machine 'r)
(get-register-contents append!-machine 'x)

