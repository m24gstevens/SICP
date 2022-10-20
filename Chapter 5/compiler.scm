#lang sicp
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

(define (split-body proc-body)
  (define (helper defns rest body)
    (if (null? body)
        (cons defns rest)
        (if (definition? (car body))
            (helper (append defns (list (car body))) rest (cdr body))
            (helper defns (append rest (list (car body))) (cdr body)))))
  (helper '() '() proc-body))

(define (defns-to-bindings defn-list)
  (map (lambda (d) (if (not (pair? (cadr d)))
                    (list (cadr d) ''*unassigned*)
                    (list (caadr d) ''*unassigned*))) defn-list))
(define (defns-to-sets defn-list)
  (map (lambda (d) (if (not (pair? (cadr d)))
                       (cons 'set! (cdr d))
                       (let ((name (caadr d))
                             (params (cdadr d))
                             (body (cddr d)))
                         (list 'set! name
                               (make-lambda params body)))
                                   )) defn-list))
                 
(define (scan-out-defines proc-body)
  (let ((two-halves (split-body proc-body)))
    (let ((defn-list (car two-halves))
          (rest-list (cdr two-halves)))
      (let ((new-body
             (append (defns-to-sets defn-list) rest-list))
            (bindings (defns-to-bindings defn-list)))
        (if (null? bindings)
            proc-body
        (list (append (list 'let bindings)
                new-body)))))))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (two-arg-open-coder? exp)
  (and (memq (operator exp) '(+ * - / =))
       (= (length exp) 3)))

(define (mult-arg-open-coder? exp)
  (and (memq (operator exp) '(+ *))
       (> (length exp) 3)))

(define (mult->two-arg exp)
  (let ((operation (operator exp))
        (operand-list (operands exp)))
    (define (make-operation first second)
      (list operation first second))
    (define (inner-loop seq)
      (if (= (length seq) 1) (car seq)
          (make-operation (car seq)
                          (inner-loop (cdr seq)))))
    (inner-loop operand-list)))

(define (can-override? exp compiler-env)
  (let ((search-result
         (find-variable exp compiler-env)))
    (if (eq? search-result 'not-found) false
        true)))

;;;**following needed only to implement COND as derived expression,
;;; not needed by eceval machine in text.  But used by compiler

;; from 4.1.2
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
      'false                          ; no else clause
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

(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (cadr exp)))
(define (let-inits exp) (map cadr (cadr exp)))

(define (reduce-body body)
  (if (let? body)
      (let->combination body)
      body))

(define (named-let? exp) (symbol? (cadr exp)))
(define (named-var exp) (cadr exp))
(define (named-bindings exp) (caddr exp))
(define (named-body exp) (cdddr exp))
(define (named-let->combination exp)
  (define proc
    (make-lambda (map car (named-bindings exp))
                 (cons (reduce-body (car (named-body exp)))
                       (cdr (named-body exp))))) 
   (make-lambda '()
                (list (list 'define (named-var exp) proc)
                      (map cadr (named-bindings exp)))))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (cons (reduce-body (car (let-body exp)))
                           (cdr (let-body exp))))
        (let-inits exp))))

(define (lexical-address-value address env)
  (define (get-to-frame frame-seq depth-counter)
    (if (= depth-counter 0) (car frame-seq)
        (get-to-frame (cdr frame-seq) (- depth-counter 1))))
  (define (iter vals counter)
    (if (= counter 0) (car vals)
        (iter (cdr vals) (- counter 1))))
  (let ((depth (frames-back address))
        (width (variables-wide address)))
    (let ((target-frame (get-to-frame env depth)))
      (let ((values (frame-vals target-frame)))
        (iter values width)))))

(define (lexical-address-lookup address env)
  (let ((val (lexical-address-value address env)))
    (if (eq? val '*unassigned)
        (error "Unassigned value at address " address)
        val)))

(define (lexical-address-set! address env new-value)
  (define (get-to-frame frame-seq depth-counter)
    (if (= depth-counter 0) (car frame-seq)
        (get-to-frame (cdr frame-seq) (- depth-counter 1))))
  (define (iter vals counter)
    (if (= counter 0)
        (begin (set-car! vals new-value) new-value)
        (iter (cdr vals) (- counter 1))))
  (let ((depth (frames-back address))
        (width (variables-wide address)))
    (let ((target-frame (get-to-frame env depth)))
      (let ((values (frame-vals target-frame)))
        (iter values width)))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame vars vals) (cons vars vals))
(define (frame-vars frame) (car frame))
(define (frame-vals frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (add-new-variable! var address-frame)
  (set! address-frame (cons var address-frame)))
(define (extend-address-environment vars base-env)
  (cons vars base-env))

(define (frames-back address) (car address))
(define (variables-wide address) (cadr address))

(define (find-variable var compile-env)
  (let ((depth-counter 0) (width-counter 0))
    (define (scan-frame frame)
      (cond ((null? frame) false)
            ((eq? (car frame) var) true)
            (else (set! width-counter (+ width-counter 1))
                  (scan-frame (cdr frame)))))
    (define (iter list-of-frames)
      (cond ((null? list-of-frames) 'not-found)
            ((scan-frame (car list-of-frames))
        (list depth-counter width-counter))
        (else
         (begin (set! depth-counter (+ depth-counter 1))
                   (set! width-counter 0)
                   (iter (cdr list-of-frames))))))
    (iter compile-env)))

(define (compiled-code exp target linkage compile-env)
  (caddr (compile exp target linkage compile-env)))

(define (compile exp target linkage compile-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage compile-env))
        ((quoted? exp) (compile-quoted exp target linkage compile-env))
        ((variable? exp)
         (compile-variable exp target linkage compile-env))
        ((assignment? exp)
         (compile-assignment exp target linkage compile-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-env))
        ((if? exp) (compile-if exp target linkage compile-env))
        ((lambda? exp) (compile-lambda exp target linkage compile-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-env))
        ((cond? exp) (compile (cond->if exp) target linkage compile-env))
        ((let? exp) (compile (let->combination exp) target linkage compile-env))
        ((two-arg-open-coder? exp)
         (compile-open-code exp target linkage compile-env))
        ((mult-arg-open-coder? exp)
         (compile (mult->two-arg exp) target linkage compile-env))
        ((application? exp)
         (compile-application exp target linkage compile-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))


(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


;;;SECTION 5.5.2

;;;linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))


;;;simple expressions

(define (compile-self-evaluating exp target linkage compile-env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage compile-env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage compile-env)
  (let ((var-addr (find-variable exp compile-env)))
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    (if (eq? var-addr 'not-found)
        `((assign lexical (op get-global-environment))
          (assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg lexical)))
        `((assign ,target
              (op lexical-address-lookup)
              (const ,var-addr)
              (reg env))))))))

(define (compile-assignment exp target linkage compile-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next compile-env)))
    (let ((compile-env-search (find-variable var compile-env)))
      (let ((assgn-instruction-sequence
             (if (eq? (find-variable var compile-env) 'not-found)
                 (make-instruction-sequence '(env val) (list target)
       `((assign lexical (op get-global-environment))
         (perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg lexical))
         (assign ,target (const ok))))
                 (make-instruction-sequence '(env val) (list target)
       `((assign lexical ,compile-env-search)
         (perform (op lexical-address-set!)
                  (reg lexical)
                  (reg env)
                  (reg val))
         (assign ,target (const ok)))))))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code assgn-instruction-sequence))))))

(define (compile-definition exp target linkage compile-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next compile-env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))


;;;conditional expressions

;;;labels (from footnote)
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))
;; end of footnote

(define (compile-if exp target linkage compile-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-env))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage compile-env))
            (a-code
             (compile (if-alternative exp) target linkage compile-env)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;;; sequences

(define (compile-sequence seq target linkage compile-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-env)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next compile-env)
       (compile-sequence (rest-exps seq) target linkage compile-env))))

;;;lambda expressions

(define (compile-lambda exp target linkage compile-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry compile-env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry compile-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp))
                       'val 'return
                       (extend-address-environment formals compile-env)))))


;;;SECTION 5.5.3

;;;combinations

(define (compile-application exp target linkage compile-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-env))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-env))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage compile-env)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;;;applying procedures

(define (compile-procedure-call target linkage compile-env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage compile-env))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define (spread-arguments operand-list compile-env)
(append-instruction-sequences
 (compile (car operand-list) 'arg1 'next compile-env)
 (protect '(arg1)
  (compile (cadr operand-list) 'arg2 'next compile-env))))
            

(define (compile-open-code exp target linkage compile-env)
  (let ((operation (operator exp)))
    (if (can-override? operation compile-env)
        (compile-application exp target linkage compile-env)
(end-with-linkage linkage
   (append-instruction-sequences
    (spread-arguments (operands exp) compile-env)
    (make-instruction-sequence '(arg1 arg2) '(arg1 arg2)
     `((assign ,target (op ,operation) (reg arg1) (reg arg2)))))))))

;;;applying compiled procedures

(define (compile-proc-appl target linkage compile-env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; footnote
(define all-regs '(env proc val argl continue arg1 arg2 lexical))


;;;SECTION 5.5.4

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
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
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (protect regs seq)
  (if (null? regs)
      seq
      (let ((first (car regs)))
        (if (modifies-register? seq first)
            (protect (cdr regs)
             (make-instruction-sequence
              (list-union (list first)
                          (registers-needed seq))
              (list-difference (registers-modified seq)
                               (list first))
              (append `((save ,first))
                      (statements seq)
                      `((restore ,first)))))
            (protect (cdr regs) seq)))))
                        
  
  
                  
                 