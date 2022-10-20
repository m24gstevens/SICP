#lang sicp
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-insts-and-labs)
     (assemble controller-text machine))
    machine))

(define (flatten-out-instructions instruction-list)
  (if (null? instruction-list) '()
      (append (car instruction-list)
              (flatten-out-instructions (cdr instruction-list)))))

(define (remove lst item)
  (cond ((null? lst) lst)
        ((equal? (car lst) item) (cdr lst))
        (else (cons (car lst) (remove (cdr lst) item)))))

(define (in-list pair list-of-pairs)
  (let ((firsts (map (lambda (p)
                       (if (pair? p) (car p) '()))
                     list-of-pairs))
        (seconds (map (lambda (p)
                       (if (pair? p) (cdr p) '()))
                     list-of-pairs)))
  (if (and (memq (car pair) firsts) (memq (cdr pair) seconds))
      true false)))
  
         
(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-status false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'trace) trace-status)
            ((eq? message 'set-trace)
             (lambda (val) (set! trace-status val)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (reg-trace register)
  (register 'trace))
(define (set-reg-trace! register val)
  ((register 'set-trace) val))

;;**monitored version from section 5.2.4
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-ct 0)
        (trace-flag false)
        (breakpoint-info '())
        (current-label '())
        (label-depth 0))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (if (not (pair? (car insts)))
                  (begin (set! current-label (car insts))
                         (set! label-depth 1)
                         (if trace-flag
                             (begin (display (car insts))
                                    (newline)))
                         (set-contents! pc (cdr insts))
                         (execute))
                  (if (in-list (cons current-label label-depth)
                            breakpoint-info)
                      (begin (display "BREAK")
                             (display current-label)
                             (display " at depth ")
                             (display label-depth))

                  (begin
                    (set! label-depth (+ label-depth 1))
                ((instruction-execution-proc (car insts)))
                (execute)))))))
      (define (proceed)
        (set! label-depth (+ label-depth 1))
        (execute))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc (flatten-out-instructions
                                  the-instruction-sequence))
               (execute))
              ((eq? message 'proceed)
               (proceed))
              ((eq? message 'install-insts-and-labs)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'instruction-ct) instruction-ct)
              ((eq? message 'set-inst-ct)
               (lambda (val) (set! instruction-ct val)))
              ((eq? message 'operations) the-ops)
              ((eq? message 'trace) trace-flag)
              ((eq? message 'set-trace)
               (lambda (val) (set! trace-flag val)
                 (if val 'trace-ON 'trace-OFF)))
              ((eq? message 'pc) (assoc 'pc register-table))
              ((eq? message 'add-breakpoint)
               (lambda (label n)
                 (set! breakpoint-info (cons (cons label n)
                                             breakpoint-info))))
              ((eq? message 'cancel-breakpoint)
               (lambda (label n)
                 (set! breakpoint-info (remove breakpoint-info
                                               (cons label n)))))
              ((eq? message 'delete-breakpoints)
               (lambda () (set! breakpoint-info '())))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(define (start machine)
  (machine 'start))

(define (proceed-machine machine)
  (machine 'proceed))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register-trace machine register-name)
  (reg-trace (get-register machine register-name)))

(define (set-register-trace! machine register-name value)
  (set-reg-trace! (get-register machine register-name) value)
  value)

(define (get-instruction-ct machine)
  (machine 'instruction-ct))
(define (inc-instruction-ct machine)
  (let ((current-ct (get-instruction-ct machine)))
  ((machine 'set-inst-ct) (+ current-ct 1))))
(define (reset-instruction-ct machine)
  ((machine 'set-inst-ct) 0))

(define (trace-status machine)
  (machine 'trace))
(define (trace-on machine)
  ((machine 'set-trace) true))
(define (trace-off machine)
  ((machine 'set-trace) false))

(define (set-breakpoint machine label n)
  ((machine 'add-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  ((machine 'delete-breakpoints)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      labels)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (execution-proc-with-ct
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (execution-proc-with-ct inst labels machine pc
                                flag stack ops)
  (let ((normal-proc (make-execution-procedure inst labels machine
                                  pc flag stack ops)))
    (lambda () (inc-instruction-ct machine)
      (if (trace-status machine) (begin (display inst)
                                        (newline)))
      (normal-proc))))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'break) '())
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))


(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (if (reg-trace target)
            (begin (display (assign-reg-name inst))
                   (display " Old value: ")
                   (display (get-contents target))
                   (newline)
                   (display " New value: ")
                   (display (value-proc))
                   (newline)))
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc (cons (label-exp-label dest) insts))
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc (cons (label-exp-label dest) insts)))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;(define (operation-exp? exp)
  ;(and (pair? exp) (tagged-list? (car exp) 'op)))
;(define (operation-exp-op operation-exp)
  ;(cadr (car operation-exp)))
;(define (operation-exp-operands operation-exp)
        ;(cdr operation-exp))

;(define (lookup-prim symbol operations)
  ;(let ((val (assoc symbol operations)))
    ;(if val
        ;(cadr val)
        ;(error "Unknown operation -- ASSEMBLE" symbol))))

;(define (tagged-list? exp tag)
  ;(if (pair? exp)
      ;(eq? (car exp) tag)
      ;false))