;;; #################################### NOTE ####################################

;;; 2. and or cond をletrecを用いたアサーションを追加する. 
;;; 3. eval-bodyを

;;; (symbol? 1) ==> #f アトミックかどうかの検証はnot pair?を使うこと

;;; eval-expからmy-defineを取り除くこと  

;;; evel-bodyが必要か? my-define中のBodyの評価にeval-expを使用しており構文的に問題が出てくるおそれが高い

;;; do文でbeginを使用してはいけないExpではなくBodyである

;;; my-define-macroのeval-expを検討

;;; ###############################################################################
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
      ((load)         (abort           `("UNDER CONSTRUCTION")))
      (else           (eval-exp        x env))))))

;;; eval Exp
;;; xよりもexpの方がよいのでは?
(define (eval-exp x env)
  (cond
   ((symbol? x)     (get-val x env))
   ((not (pair? x)) x)
   ((macro-name? (car x)) (my-eval (macro-expand x env) env))
   (else
    (case (car x)
      ;; DefineはExpに含まれないからmy-defineは消さなければならない
      ((define)       (my-define       x env))
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
  (eval-exp (trans-body body env) env))

;;;   Add some variables and values to an environment.
(define (extend-env vars vals env)
  (cond
   ;; (lambda () <body>)
   ((null? vars) 
      env)
   ;; ((lambda (x y z) <body>) a b c)
   ((and (list? vars) (list? vals) (= (length vars) (length vals)))
      (let ((a (map cons vars vals)) (b env))
	(set-cdr! (last a) b)
	a))
   ;; (lambda x <body>)
   ((and (symbol? vars) (list? vals))
      (let ((newenv (list (cons vars vals))))
	(set-cdr! newenv env)
	newenv))
   ;; ((lambda (x y z . w) <body>) a b c d)
   ((and (not (symbol? vars)) (not (list? vars)))
      (extend-env-sub vars vals env)))) 

;;; extend-envで, ((lambda (x y z . w) <body>) a b c d) のときに使用
(define (extend-env-sub vars vals env)
  (cond
   ((symbol? (cdr vars))
      (cons 
       (cons (cdr vars) (cdr vals)) 
       (cons (cons (car vars) (car vals)) env)))
   (else
    (extend-env-sub (cdr vars)
		    (cdr vals)
		    (cons (cons (car vars) (car vals)) env)))))

;;; マクロ展開
(define (macro-expand x env)
  (if (and (list? x) (macro-name? (car x)))
      (macro-expand (apply (get-val (car x) env) (cdr x)) env)
      x))

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