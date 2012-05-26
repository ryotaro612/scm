;;; ##################################### MEMO #####################################
;;; my-do bind-step -> step
;;; rename
;;; my-define-macro var-info -> bound
;;; my-begin x -> exp mapではなく再帰を利用
;;; my-ifの x -> exp , "<#undef#>"追加
;;; my-letrecの x -> exp
;;; my-let*の x -> exp
;;; my-letの x -> exp
;;; my-named-letの x -> exp
;;; my-defineで info -> bound
;;; my-set!で id-info -> bound
;;; get-varinfo                -> get-bound
;;; *macro-symbols*            -> *macro-vars*
;;; delete-macrodef            -> delete-macrodef!
;;; (extend-env vars vals env) -> (extend-env parms args env)
;;; macro-name?                -> macro-var?
;;; macro-expand               -> expand-macro
;;; ################################################################################
'()

(load "./init.scm")
(load "./misc.scm")
(load "./syntax.scm")
(load "./variable.scm")

;;; eval TopLevel
(define (my-eval x env)
  (cond 
   ((symbol? x)                   (eval-exp x env))
   ((not (pair? x))               (eval-exp x env))
   ((macro-var? (car x))          (my-eval (expand-macro x env) env))
   (else
    (case (car x)
      ((define)                   (my-define       x env))
      ((define-macro)             (my-define-macro x env))
      ((load)                     (my-load         x    ))
      (else                       (eval-exp        x env))))))

;;; eval Exp
(define (eval-exp exp env)
  (cond
   ((symbol? exp)                 (get-val exp env))
   ((not (pair? exp))             exp)
   ((macro-var? (car exp))        (my-eval (expand-macro exp env) env))
   (else
    (case (car exp)
      ((lambda)                   (my-lambda       exp env))
      ((quote)                    (cadr            exp    ))
      ((set!)                     (my-set!         exp env))
      ((let)                      (my-let          exp env))
      ((let*)                     (my-let*         exp env))
      ((letrec)                   (my-letrec       exp env))
      ((if)                       (my-if           exp env))
      ((cond)                     (my-cond         exp env))
      ((and)                      (my-and          exp env))
      ((or)                       (my-or           exp env))
      ((begin)                    (my-begin        exp env))
      ((do)                       (my-do           exp env))
      ((define define-macro load) (abort `("error Exp syntax: " ,exp)))       
      (else
       (apply (eval-exp (car exp) env)
	      (map (lambda (e) (eval-exp e env)) (cdr exp))))))))

;;; eval Body
(define (eval-body body env)
  (eval-exp (conv-to-exp body env) env))

;;; imitate a scheme interpreter
(define (my-scm)
  (call/cc (lambda (cc) (set! init cc)))
  (set! *global-env* (init-global-env))
  (newline) (display "initialize my-scm") (newline)
  (let loop ()
    (display "my-scm> ") (flush)
    (display (my-eval (read) '()))
    (newline)
    (loop)))

(my-scm)

; Toplevel ::= Exp
;            | Define
;            | Define-Macro
;            | (load String)
; 
; Define ::= (define Id Exp)                   
;          | (define (Id Id* [. Id]) Body) 
; 
; Define-Macro ::= (define-macro Id Exp)
;                | (define-macro (Id Id* [. Id]) Body)
;
; Exp ::= Const                                 
;       | Id                                    
;       | (lambda Arg Body)                     
;       | (Exp Exp*)                            
;       | (quote S-Exp)                         
;       | (set! Id Exp)                         
;       | (let [Id] Bindings Body)              
;       | (let* Bindings Body)                  
;       | (letrec Bindings Body)                
;       | (if Exp Exp [Exp])                    
;       | (cond (Exp Exp+)* [(else Exp+)])      
;       | (and Exp*)                            
;       | (or Exp*)                             
;       | (begin Exp*)                          
;       | (do ((Id Exp Exp)*) (Exp Exp*) Body)  

; Body ::= Define* Exp+
;
; Arg ::= Id
;       | (Id* [Id . Id])
;
; Bindings ::= ((Id Exp)*)
;
; S-Exp ::= Const
;         | Id
;         | (S-Exp* [S-Exp . S-Exp])
;
; Const ::= Num
;         | Bool
;         | String
;         | ()