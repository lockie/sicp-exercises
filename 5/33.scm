#lang sicp

(#%require "compiler.scm")

(display (compile
          '(define (factorial-alt n)
               (if (= n 1)
                   1
                   (* n (factorial-alt (- n 1)))))
          'val
          'next))


;; original code compiled
;;
;; (assign val (op make-compiled-procedure) (label entry35) (reg env))
;; (goto (label after-lambda36))
;; entry35
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; (save continue)
;; (save env)
;; (assign proc (op lookup-variable-value) (const =) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch40))
;; compiled-branch41
;; (assign continue (label after-call42))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch40
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call42
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch38))
;; true-branch37
;; (assign val (const 1))
;; (goto (reg continue))
;; false-branch38
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; (save continue)
;; (save proc)
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op list) (reg val))
;; (save argl)
;; (assign proc (op lookup-variable-value) (const factorial) (reg env))
;; (save proc)
;; (assign proc (op lookup-variable-value) (const -) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch43))
;; compiled-branch44
;; (assign continue (label after-call45))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch43
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call45
;; (assign argl (op list) (reg val))
;; (restore proc)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch46))
;; compiled-branch47
;; (assign continue (label after-call48))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch46
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call48
;; (restore argl)
;; (assign argl (op cons) (reg val) (reg argl))
;; (restore proc)
;; (restore continue)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch49))
;; compiled-branch50
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch49
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call51
;; after-if39
;; after-lambda36
;; (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; (assign val (const ok))

;; modified code compiled
;;
;; (assign val (op make-compiled-procedure) (label entry1) (reg env))
;; (goto (label after-lambda2))
;; entry1
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; (save continue)
;; (save env)
;; (assign proc (op lookup-variable-value) (const =) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch6))
;; compiled-branch7
;; (assign continue (label after-call8))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch6
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call8
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch4))
;; true-branch3
;; (assign val (const 1))
;; (goto (reg continue))
;; false-branch4
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; (save continue)
;; (save proc)
;; (save env)
;; (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
;; (save proc)
;; (assign proc (op lookup-variable-value) (const -) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch9))
;; compiled-branch10
;; (assign continue (label after-call11))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch9
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call11
;; (assign argl (op list) (reg val))
;; (restore proc)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch12))
;; compiled-branch13
;; (assign continue (label after-call14))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch12
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call14
;; (assign argl (op list) (reg val))
;; (restore env)
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (restore proc)
;; (restore continue)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch15))
;; compiled-branch16
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch15
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call17
;; after-if5
;; after-lambda2
;; (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
;; (assign val (const ok))
