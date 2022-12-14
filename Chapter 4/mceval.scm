#lang sicp
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((while? exp) (eval-while exp env))
        ((for? exp) (eval-for exp env))
        ((application? exp)
         (mc-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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

(define (make-let pairings body)
  (list 'let pairings body))
(define (let*->nested-lets exp)
  (nest-lets (cadr exp) (cddr exp)))
(define (nest-lets pairings body)
  (if (null? (cdr pairings))
      (make-let pairings body)
      (make-let (list (car pairings))
                (nest-lets (cdr pairings) body))))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (and? exp) (tagged-list? exp 'and))
(define (and-expressions exp) (cdr exp))
(define (first-and-pred exp) (cadr exp))
(define (remaining-and-pred exp) (cddr exp))
(define (and->if exp)
  (if (null? (and-expressions exp))
      true
      (make-if (first-and-pred exp)
               (and->if
                (cons 'and (remaining-and-pred exp)))
               false)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-expressions exp) (cdr exp))
(define (first-or-pred exp) (cadr exp))
(define (remaining-or-pred exp) (cddr exp))
(define (or->if exp)
  (if (null? (or-expressions exp))
      false
      (make-if (first-or-pred exp)
               true
               (or->if
                (cons 'or (remaining-or-pred exp))))))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-test->recipient? clause)
  (if (pair? (cond-actions clause))
      (eq? (cadr clause) '=>)
      false))

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
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (if (cond-test->recipient? first)
                (let ((test-evaluated (eval (cond-predicate first))))
                  (make-if test-evaluated
                     (mc-apply (caddr first) (list test-evaluated))
                     (expand-clauses rest)))
                
             (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest)))))))

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-actions exp) (cddr exp))
(define (eval-while exp env)
  (if (eval (while-predicate exp) env)
      (begin (eval-sequence (while-actions exp) env)
             (eval-while exp env))
      'ok))

(define (for? exp) (tagged-list? exp 'for))
(define (for-list exp) (cadr exp))
(define (for-function exp) (caddr exp))
(define (make-for for-lst func)
  (list 'for for-lst func))
(define (eval-for exp env)
  (let ((app-list (for-list exp)) (func (for-function exp)))
    (define (iter lst)
       (if (null? lst)
      'done
      (begin
        (display (eval (list func (car lst)) env))
        (iter (cdr lst)))))
    (iter app-list)))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (search vars vals var)
  (cond ((null? vars) false)
        ((eq? (car vars) var)
         (cons (cons (car vars) (car vals))))
        (else (traverse (cdr vars) (cdr vals) var))))

(define (traverse var env on-find on-fail on-empty)
  (let ((first (first-frame env)))
    (if (eq? first the-empty-environment)
        (on-empty)
    (let ((search-result
           (search (frame-variables first)
                   (frame-values first)
                   var)))
      (if (search-result)
          (on-find search-result)
          (on-fail env))))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (let ((search-result (env-loop env)))
    (if (eq? search-result '*unassigned*)
        (error "Unassigned variable")
        search-result)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (make-unbound! var env)
  (let ((first (first-frame env)))
    (define (remover vars vals)
      (cond ((null? vars) 'not-in-env)
            ((eq? (car vars) var)
             (set! vars (cdr vars))
             (set! vals (cdr vals)))
            (else
             (remover (cdr vars)
                      (cdr vals)))))
    (remover (frame-variables first)
             (frame-values first))))

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

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec->let exp)
  (let ((bindings (letrec-bindings exp))
        (body (letrec-body exp)))
    (let ((initializations
           (map (lambda (b) (list (car b) '*unassigned*)) bindings))
          (new-body
           (append (map (lambda (b) (cons 'set! b)) bindings)
                   body)))
      (append (list 'let initializations) new-body))))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'remainder remainder)
        (list 'quotient quotient)
        (list 'display display)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
             
;(driver-loop)    