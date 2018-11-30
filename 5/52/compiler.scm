#lang racket


(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
    (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
    (tagged-list? exp 'define))

(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        (quote 'false)))

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
    (let ((vars (cadr exp)))
        (append (list (make-lambda (map car vars) (cddr exp)))
                (map cadr vars))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((let? exp) (compile (let->combination exp) target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
    (list needs modifies statements))

(define (empty-instruction-sequence)
    (make-instruction-sequence '() '() ""))

(define (compile-linkage linkage)
    (cond ((eq? linkage 'return)
           (make-instruction-sequence '(cont) '()
                                      "goto *cont;"))
          ((eq? linkage 'next)
           (empty-instruction-sequence))
          (else
           (make-instruction-sequence '() '()
                                      (format "goto ~a;" linkage)))))

(define (end-with-linkage linkage instruction-sequence)
    (preserving
     '(cont)
     instruction-sequence
     (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
    (end-with-linkage
     linkage
     (make-instruction-sequence '() (list target)
                                (let ((ctor
                                       (cond ((number? exp) "g_variant_new_int64")
                                             ((real? exp) "g_variant_new_double")
                                             ((string? exp) "g_variant_new_string")
                                             (else
                                              (error "Unknown data type" exp)))))
                                    (format "~a = ~a(~s);" target ctor exp)))))

(define (format-value val)
    (define (element-type el)
        (cond ((number? el) "x")
              ((real? el) "d")
              ((string? el) "s")
              ((symbol? el) "h")
              ((boolean? el) "h")  ;; HACK: booleans are awkward
              ((pair? el) "v")
              (else
               (error "Unsupported element type" el))))
    (define (element-format el)
        (cond ((or (symbol? el) (boolean? el)) (format "g_quark_from_string(\"~a\")" el))
              ((pair? el) (format "g_variant_new(~a)" (format-value el)))
              ((number? el) (format "~al" el))
              (else
               (~s el))))
    (cond ((pair? val)
           (string-join
            (map element-format
                 (cons
                  (string-join
                   (append (list "(")
                           (map element-type val)
                           (list ")"))
                   "")
                  val))
            ", "))
          ((null? val)
           "\"()\"")
          (else
           (string-join
            (list
             (format "~s" (element-type val))
             (element-format val))
            ", "))))

(define (compile-quoted exp target linkage)
    (end-with-linkage
     linkage
     (make-instruction-sequence '() (list target)
                                (format
                                 "~a = g_variant_new(~a);"
                                 target
                                 (format-value (text-of-quotation exp))))))

(define (compile-variable exp target linkage)
    (end-with-linkage
     linkage
     (make-instruction-sequence '(env) (list target)
                                (format
                                 "~a = lookup_variable_value(\"~a\", env);"
                                 target exp))))

(define (compile-assignment exp target linkage)
    (let ((var (assignment-variable exp))
          (get-value-code
           (compile (assignment-value exp) 'val 'next)))
        (end-with-linkage
         linkage
         (preserving '(env)
                     get-value-code
                     (make-instruction-sequence
                      '(env val)
                      (list target)
                      (format
                       "set_variable_value(\"~a\", val, env);~%~a = NULL;"
                       var target))))))

(define (compile-definition exp target linkage)
    (let ((var (definition-variable exp))
          (get-value-code
           (compile (definition-value exp) 'val 'next)))
        (end-with-linkage
         linkage
         (preserving '(env)
                     get-value-code
                     (make-instruction-sequence
                      '(env val)
                      (list target)
                      (format
                       "define_variable(\"~a\", val, env);~%~a = NULL;"
                       var target))))))

(define label-counter 0)

(define (new-label-number)
    (set! label-counter (+ 1 label-counter))
    label-counter)

(define (make-label name)
    (string->symbol
     (string-append (symbol->string name)
                    (number->string (new-label-number)))))

(define (compile-if exp target linkage)
    (let ((t-branch (make-label 'true_branch))
          (f-branch (make-label 'false_branch))
          (after-if (make-label 'after_if)))
        (let ((consequent-linkage
               (if (eq? linkage 'next) after-if linkage)))
            (let ((p-code
                   (compile (if-predicate exp) 'val 'next))
                  (c-code
                   (compile (if-consequent exp) target consequent-linkage))
                  (a-code
                   (compile (if-alternative exp) target linkage)))
                (preserving
                 '(env cont)
                 p-code
                 (append-instruction-sequences
                  (make-instruction-sequence
                   '(val) '()
                   (format "if(is_false(val)) goto ~a;" f-branch))
                  (parallel-instruction-sequences
                   (append-instruction-sequences t-branch c-code)
                   (append-instruction-sequences f-branch a-code))
                  after-if))))))

(define (compile-sequence seq target linkage)
    (if (last-exp? seq)
        (compile (first-exp seq) target linkage)
        (preserving '(env cont)
                    (compile (first-exp seq) target 'next)
                    (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
    (let* ((proc-entry (make-label 'entry))
           (after-lambda (make-label 'after_lambda))
           (lambda-linkage
            (if (eq? linkage 'next) after-lambda linkage)))
        (append-instruction-sequences
         (tack-on-instruction-sequence
          (end-with-linkage
           lambda-linkage
           (make-instruction-sequence
            '(env) (list target)
            (format
             "~a = make_compiled_procedure(&&~a, env);"
             target proc-entry)))
          (compile-lambda-body exp proc-entry))
         after-lambda)))

(define (compile-lambda-body exp proc-entry)
    (let ((formals (lambda-parameters exp)))
        (append-instruction-sequences
         (make-instruction-sequence
          '(env proc argl) '(env)
          (format
           "~a:~%env = extend_environment(g_variant_new(~a), argl, compiled_procedure_env(proc));"
           proc-entry
           (format-value formals)))
         (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
    (let ((proc-code (compile (operator exp) 'proc 'next))
          (operand-codes
           (map (lambda (operand) (compile operand 'val 'next))
                (operands exp))))
        (preserving
         '(env cont)
         proc-code
         (preserving
          '(proc cont)
          (construct-arglist operand-codes)
          (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
    (let ((operand-codes (reverse operand-codes)))
        (if (null? operand-codes)
            (make-instruction-sequence
             '() '(argl)
             "argl = g_variant_new(\"()\");")
            (let ((code-to-get-last-arg
                   (append-instruction-sequences
                    (car operand-codes)
                    (make-instruction-sequence
                     '(val) '(argl)
                     "argl = cons(val, NIL);"))))
                (if (null? (cdr operand-codes))
                    code-to-get-last-arg
                    (preserving
                     '(env)
                     code-to-get-last-arg
                     (code-to-get-rest-args
                      (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
    (let ((code-for-next-arg
           (preserving
            '(argl)
            (car operand-codes)
            (make-instruction-sequence
             '(val argl) '(argl)
             "argl = cons(val, argl);"))))
        (if (null? (cdr operand-codes))
            code-for-next-arg
            (preserving
             '(env)
             code-for-next-arg
             (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
    (let ((primitive-branch (make-label 'primitive_branch))
          (compiled-branch (make-label 'compiled_branch))
          (after-call (make-label 'after_call)))
        (let ((compiled-linkage
               (if (eq? linkage 'next) after-call linkage)))
            (append-instruction-sequences
             (make-instruction-sequence
              '(proc) '()
              (format
               "if(is_primitive_procedure(proc)) goto ~a;"
               primitive-branch))
             (parallel-instruction-sequences
              (append-instruction-sequences
               compiled-branch
               (compile-proc-appl target compiled-linkage))
              (append-instruction-sequences
               primitive-branch
               (end-with-linkage
                linkage
                (make-instruction-sequence
                 '(proc argl) (list target)
                 (format
                  "~a = apply_primitive_procedure(proc, argl);"
                  target)))))
             after-call))))

(define (compile-proc-appl target linkage)
    (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
           (make-instruction-sequence
            '(proc) all-regs
            (format
             "cont = &&~a;~%goto *compiled_procedure_entry(proc);"
             linkage)))
          ((and (not (eq? target 'val))
                (not (eq? linkage 'return)))
           (let ((proc-return (make-label 'proc_return)))
               (make-instruction-sequence
                '(proc) all-regs
                (format
                 "cont = &&~a;~%goto compiled_procedure_entry(proc);~%~a: ~a = val; goto ~a;"
                 proc-return
                 proc-return
                 target
                 linkage))))
          ((and (eq? target 'val) (eq? linkage 'return))
           (make-instruction-sequence
            '(proc cont) all-regs
            "val = compiled_procedure_entry(proc); goto *val;"))
          ((and (not (eq? target 'val)) (eq? linkage 'return))
           (error "return linkage, target not val -- COMPILE" target))))

(define all-regs '(env proc val argl cont))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (format "~a:" s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
    (define (append-2-sequences seq1 seq2)
        (make-instruction-sequence
         (list-union (registers-needed seq1)
                     (list-difference (registers-needed seq2)
                                      (registers-needed seq1)))
         (list-union (registers-modified seq1)
                     (registers-modified seq2))
         (format "~a~%~a" (statements seq1) (statements seq2))))
    (define (append-seq-list seqs)
        (if (null? seqs)
            (empty-instruction-sequence)
            (append-2-sequences (car seqs)
                                (append-seq-list (cdr seqs)))))
    (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
    (if (null? regs)
        (append-instruction-sequences seq1 seq2)
        (let ((first-reg (car regs)))
            (if (and (needs-register? seq2 first-reg)
                     (modifies-register? seq1 first-reg))
                (preserving
                 (cdr regs)
                 (make-instruction-sequence
                  (list-union (list first-reg)
                              (registers-needed seq1))
                  (list-difference (registers-modified seq1)
                                   (list first-reg))
                  (format
                   "g_queue_push_tail(stack, ~a);~%~a~%~a = g_queue_pop_tail(stack);"
                   first-reg
                   (statements seq1)
                   first-reg))
                 seq2)
                (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
    (make-instruction-sequence
     (registers-needed seq)
     (registers-modified seq)
     (format "~a~%~a" (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (registers-needed seq2))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (format "~a~%~a" (statements seq1) (statements seq2))))

(define prologue "#include \"runtime.h\"
int main()
{
void* cont;
GQueue* stack = g_queue_new();
GSList* env = setup_environment();
GVariant* val, *argl, *proc;")

(define epilogue "display(val, NULL);
g_slist_free_full(env, free_frame);
g_queue_free(stack);
return 0;
}
")

(define (full-compile exp)
    (format "~a~%~a~%~a"
            prologue
            (statements (compile exp 'val 'next))
            epilogue))

(provide (rename-out [full-compile compile]))

(display (full-compile
          (read (open-input-file "interpreter.scm"))))
