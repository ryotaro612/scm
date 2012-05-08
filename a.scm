;;; ##################################### MEMO #####################################
;;; ################################################################################
'()

(load "./init.scm")
(load "./misc.scm")
(load "./syntax.scm")
(load "./variable.scm")

;;; eval TopLevel
(define (my-eval x env)
  (cond 
   ((symbol? x)           (eval-exp x env))
   ((not (pair? x))       (eval-exp x env))
   ((macro-name? (car x)) (my-eval (macro-expand x env) env))
   (else
    (case (car x)
      ((define)       (my-define       x env))
      ((define-macro) (my-define-macro x env))
      ((load)         (my-load         x))
      (else           (eval-exp        x env))))))

;;; eval Exp
(define (eval-exp x env)
  (cond
   ((symbol? x)     (get-val x env))
   ((not (pair? x)) x)
   ((macro-name? (car x)) (my-eval (macro-expand x env) env))
   (else
    (case (car x)
      ;((define)       (my-define       x env))
      ((lambda)       (my-lambda       x env))
      ((quote)        (cadr            x    ))
      ((set!)         (my-set!         x env))
      ((let)          (my-let          x env))
      ((let*)         (my-let*         x env))
      ((letrec)       (my-letrec       x env))
      ((if)           (my-if           x env))
      ((cond)         (my-cond         x env))
      ((and)          (my-and          x env))
      ((or)           (my-or           x env))
      ((begin)        (my-begin        x env))
      ((do)           (my-do           x env))
      (else
       (apply (my-eval (car x) env)
	      (map (lambda (y) (my-eval y env)) (cdr x))))))))


;;; eval Body
(define (eval-body body env)
  ;; ここに 構文チェックを入れる
  ;(eval-exp (trans-body body env) env)
  (eval-exp (conv-to-exp body env) env))


;;; インタプリタ呼出し
(define (my-scm)
  (call/cc (lambda (cc) (set! init cc)))
  (set! *global-env* (init-global-env))
  (newline) (display "initialize my-scm") (newline)
  (let loop ()
    (display "my-scm> ") (flush) ; flushはGaucheの処理系依存の手続き
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