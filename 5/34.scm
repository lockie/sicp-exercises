#lang sicp

(#%require "compiler.scm")

(display (compile
          '(define (factorial n)
               (define (iter product counter)
                   (if (> counter n)
                       product
                       (iter (* counter product)
                             (+ counter 1))))
               (iter 1 1))
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


;; iterative code compiled
;;
;; (assign val (op make-compiled-procedure) (label entry1) (reg env))
;; (goto (label after-lambda2))
;; entry1
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; (assign val (op make-compiled-procedure) (label entry3) (reg env))
;; (goto (label after-lambda4))
;; entry3
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;; (save continue)
;; (save env)
;; (assign proc (op lookup-variable-value) (const >) (reg env))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch8))
;; compiled-branch9
;; (assign continue (label after-call10))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch8
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call10
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch6))
;; true-branch5
;; (assign val (op lookup-variable-value) (const product) (reg env))
;; (goto (reg continue))
;; false-branch6
;; (assign proc (op lookup-variable-value) (const iter) (reg env))
;; (save continue)
;; (save proc)
;; (save env)
;; (assign proc (op lookup-variable-value) (const +) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch14))
;; compiled-branch15
;; (assign continue (label after-call16))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch14
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call16
;; (assign argl (op list) (reg val))
;; (restore env)
;; (save argl)
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; (assign val (op lookup-variable-value) (const product) (reg env))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch11))
;; compiled-branch12
;; (assign continue (label after-call13))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch11
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; after-call13
;; (restore argl)
;; (assign argl (op cons) (reg val) (reg argl))
;; (restore proc)
;; (restore continue)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch17))
;; compiled-branch18
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch17
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call19
;; after-if7
;; after-lambda4
;; (perform (op define-variable!) (const iter) (reg val) (reg env))
;; (assign val (const ok))
;; (assign proc (op lookup-variable-value) (const iter) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (const 1))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch20))
;; compiled-branch21
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))
;; primitive-branch20
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))
;; after-call22
;; after-lambda2
;; (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; (assign val (const ok))
