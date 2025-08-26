(in-package :advent-of-code-2024.sicp)

;; Chapter 4 - Metalinguistic Abstraction

(define-condition eval-error ()
  ())

(define-condition unknown-expression (eval-error)
  ((exp
    :initarg :exp)))

(define-condition unknown-procedure-type (eval-error)
  ((proc
    :initarg :proc)))

(defun eval* (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)
         (lookup-variable exp env))
        ((quoted? exp) (quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-params exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval* (cond->if exp) env))
        ((application? exp)
         (apply* (eval* (operator exp) env)
                 (list-of-values (operands exp) env)))
        (t (error 'unknown-expression :exp exp))))

(defun apply* (proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence (procedure-body proc)
                        (extend-env (procedure-params proc)
                                    args
                                    (procedure-env proc))))
        (t (error 'unknown-procedure-type :proc proc))))

(defun list-of-values (exps env)
  (if (no-operands? exps)
      nil
      (cons (eval* (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


#+(or) ;; Ex. 4.1
(defun list-of-values-lr (exps env)
  (if (no-operands? exps)
      nil
      (let ((head (eval* (first-operand exps) env))
            (tail (list-of-values (rest-operands exps) env)))
        (cons head tail))))

#+(or) ;; Ex. 4.1
(defun list-of-values-rl (exps env)
  (if (no-operands? exps)
      nil
      (let ((tail (list-of-values (rest-operands exps) env))
            (head (eval* (first-operand exps) env)))
        (cons head tail))))

(defun eval-if (exp env)
  (if (true? (eval* (if-predicate exp) env))
      (eval* (if-consequent exp) env)
      (eval* (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (if (last-exp? exps)
      (eval* (first-exp exps) env)
      (progn (eval* (first-exp exps) env)
             (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval* (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                       (eval* (definition-value exp) env)
                       env)
  'ok)



;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Expression representations
(defun self-evaluating? (exp)
  (or (numberp exp)
      (stringp exp)))

(defun variable? (exp)
  (symbolp exp))

(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun quotation (exp)
  (second exp))

;; assignment
(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp)
  (second exp))

(defun assignment-value (exp)
  (third exp))

;; definition
(defun definition? (exp)
  (tagged-list? exp 'define))

(defun definition-variable (exp)
  (if (symbolp (second exp))
      (second exp)
      (caadr exp)))

(defun definition-value (exp)
  (if (symbolp (second exp))
      (third exp)
      (make-lambda (cdadr exp) ; (cadr exp) is a list, so its rest is a list of params
                   (cddr exp)))) ; after the first two items, the rest is function body

;; lambda
(defun lambda? (exp)
  (tagged-list? exp 'lambda))

(defun lambda-params (exp)
  (second exp))

(defun lambda-body (exp)
  (cddr exp))

(defun make-lambda (params body)
  (list* 'lambda params body))

;; if
(defun if? (exp)
  (tagged-list? exp 'if))

(defun if-predicate (exp)
  (second exp))

(defun if-consequent (exp)
  (third exp))

(defun if-alternative (exp)
  (if (cdddr exp)
      (fourth exp)
      'false))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(defun begin? (exp)
  (tagged-list? exp 'begin))

(defun begin-actions (exp)
  (assert (begin? exp))
  (rest exp))

(defun last-exp? (exps)
  (not (rest exps)))

(defun first-exp (exps)
  (first exps))

(defun rest-exps (exps)
  (rest exps))



(defun no-operands? (exps)
  (null exps))

(defun first-operand (exps)
  (first exps))

(defun rest-operands (exps)
  (rest exps))

(defun true? (exp)
  (not (not exp)))

(defun tagged-list? (exp tag)
  (when (consp exp)
    (eql tag (first exp))))

;; TBC: (sicp) 4-1-2
