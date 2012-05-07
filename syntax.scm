
'()
;;; (define Id Exp) | (define (Id Id* [. Id]) Body)
(define (my-define x env)
  (assert `("error define syntax: " ,x) 
	  (and (list? x) (eq? (car x) 'define) (>= (length x) 3)))
  (cond
   ;; (define Id Exp) のケース
   ((symbol? (cadr x))
    (let ((info (get-varinfo (cadr x) env)))
      (cond
       ;; シンボルが既にマクロとして定義されているケース
       ((and info (macro-name? (car info)))
	  (delete-macrodef (car info))
	  (set-cdr! info (eval-exp (caddr x) env))
 	  (car info))
       ;; シンボルがマクロ以外(変数/手続き)として定義されているケース
       (info
	  (set-cdr! info (eval-exp (caddr x) env))
	  (car info))
       ;; それ以外(シンボルが未定義の場合)
       (else
	(set! *global-env*
	      (cons (cons (cadr x) (eval-exp (caddr x) env))
		    *global-env*))
	(cadr x)))))
   ;; (define (<変数> <仮引数部>) <本体>) -> (define <変数> (lambda (<仮引数部>) <本体>))
   ;; (define (<変数> . <仮引数>) <本体>) -> (define <変数> (lambda <仮引数> <本体>))
   ((pair? (cadr x))
    (my-define (get-bitterdef x) env))
   (else
    (abort `("define misc error: " ,x)))))

;;; (define (Id Id* [. Id]) Body) -> (define Id Exp)
(define (desugar-def def)
  (assert `("error define syntax: " ,def) (define? def))
  (if (symbol? (cadr def))
      def
      `(define ,(caadr def) (lambda ,(cdadr def) ,@(cddr def)))))

;;; (define Id Exp) | (define (Id Id* [. Id]) Body) ???
(define (define? def)
  (if (and (list? def) (>= (length def) 3) (eq? (car def) 'define)
	   (or (symbol? (cadr def)) (and (pair? (cadr def)) (symbol? (caadr def)))))
      #t
      #f))

;;; (set! Id Exp)
(define (my-set! x env)
  (assert `("error set! syntax: " ,x) 
	  (and (list? x) (eq? (car x) 'set!) (= (length x) 3) (symbol? (cadr x))))
  (let* ((id (cadr x)) (exp (caddr x)) (id-info (get-varinfo id env)))
    (cond
     ;; idがマクロとして定義されている場合
     ((and id-info (macro-name? id))
      (delete-macrodef id)
      (set-cdr! id-info (eval-exp exp env)))
     ;; idがマクロ以外(値/手続き)として定義されている場合
     (id-info
      (set-cdr! id-info (eval-exp exp env)))
     ;; idが未定義の場合
     (else
      (abort `(,id " is not defined."))))))

;;; xがBindings ::= ((Id Exp)*) の形式になっているか調べる
(define (bindings? x)
  (cond
   ((null? x)
    #t)
   ((and (list? x) (list? (car x)) (= (length (car x)) 2)) ; <- modified
    (bindings? (cdr x)))
   (else
    #f)))

;;; (let [Id] Bindings Body)
;;; 名前付きletの処理はmy-namedletに依頼する
(define (my-let x env)
  (assert `("error let syntax: " ,x)
	  (and (list? x) (eq? (car x) 'let) (>= (length x) 3)))
  (cond
   ;; 名前付きletのケース
   ((symbol? (cadr x))
    (my-namedlet x env))
   ;; 束縛がないケース
   ;; (let () Body)            -> (lambda () Body)
   ((null? (cadr x))
    (eval-exp `((lambda () ,@(cddr x))) env))
   ;; 束縛があるケース
   ;; (let ((x a) (y b)) Body) -> ((lambda (x y) Body) a b)
   (else
    (assert `("error let syntax: " ,x) (bindings? (cadr x)))
    (eval-exp `((lambda ,(map car (cadr x)) ,@(cddr x)) ,@(map cadr (cadr x)))
	      env))))

;;; (let Id Bindings Body)
;;; 名前付きlet, my-letで利用される補助関数
;;; (let Id Bindigs Body) -> (letrec ((Id (lambda (args) Body))) (Id actuals))
(define (my-namedlet x env)
  (assert `("error named let syntax: " ,x) 
	  (and (list? x) (bindings? (caddr x)) (>= (length x) 4)))
  (let ((id (cadr x)) (bindings (caddr x)) (body (cdddr x)))
    (if (null? bindings)
	;; 束縛がないケース
	(my-letrec 
	 `(letrec ((,id (lambda () ,@body))) (,id)) env)
	;; 束縛があるケース
	(my-letrec 
	 `(letrec ((,id (lambda ,(map car bindings) ,@body))) 
	    (,id ,@(map cadr bindings))) env))))

;;; (let* Bindings Body)
(define (my-let* x env)
  (assert `("error let* syntax: " ,x) 
	  (and (list? x) (eq? (car x) 'let*) 
	       (>= (length x) 3) (bindings? (cadr x))))
  (let ((bindings (cadr x)) (body (cddr x)))
    (if (null? bindings)
	;; 束縛がないケース
	(eval-exp `((lambda () ,@body)) env)
	;; 束縛があるケース	
	(eval-exp
	 `((lambda (,(caar bindings)) (let* ,(cdr bindings) ,@body)) 
	   ,(cadar bindings)) 
	 env))))

;;; (letrec Bindings Body)
(define (my-letrec x env)
  (assert `("error letrec syntax: " ,x)
	  (and (list? x) (eq? (car x) 'letrec)
	       (>= (length x) 3) (bindings? (cadr x))))
  (if (null? (cadr x))
      ;; 束縛がないケース
      (eval-exp `((lambda () ,@(cddr x))) env)
      ;; 束縛があるケース
      (my-let
       `(let ,(map (lambda (lst) (list (car lst) "<#undef#>")) (cadr x))
	  ,@(map (lambda (lst) (cons 'set! lst)) (cadr x)) ,@(cddr x))
       env)))

;;; (if Exp Exp [Exp])
(define (my-if x env)
  (assert `("error if syntax: " ,x) 
	  (and (list? x) (eq? (car x) 'if) (or (= (length x) 3) (= (length x) 4))))
  (if (eval-exp (cadr x) env)
      (eval-exp (caddr x) env)
      (if (not (null? (cdddr x)))
	  (eval-exp (cadddr x) env))))

;;; (cond (Exp Exp+)* [(else Exp+)])
(define (my-cond exp env)
  (define (cond? expr) 
    (cond ((null? expr)
	   #t)
	  ;; (Exp Exp+)*
	  ((and (list? (car expr)) (>= (length (car expr)) 2) 
		(not (eq? (caar expr) 'else)))		
	   (cond? (cdr expr)))
	  ;; (else Exp+)*
	  ((and (list? (car expr)) (>= (length (car expr)) 2)
		(eq? (caar expr) 'else) (null? (cdr expr)))
	   #t)
	  (else
	   #f)))
  (define (cond-sub expr env)
    (cond
     ;; 最後の節のテストが#f
     ((null? expr)
      "<#undef#>")
     ;; (else Exp+)
     ((eq? (caar expr) 'else)
      (eval-exp `(begin ,@(cdar expr)) env))
     ;; (Exp Exp+) 
     ((eval-exp (caar expr) env)
      (eval-exp `(begin ,@(cdar expr)) env))
     ;; 節のテストが#f
     (else
      (cond-sub (cdr expr) env))))

  (assert `("error cond syntax: " ,exp)
	  (and (list? exp) (eq? (car exp) 'cond) (>= (length exp) 2) 
	       (list? (cadr exp)) (not (null? (cadr exp))) (cond? (cdr exp))))

  (cond-sub (cdr exp) env))

;;; (and Exp*)
(define (my-and exp env)
  (assert `("error and syntax: " ,exp) (and (list? exp) (eq? (car exp) 'and)))
  (letrec ((and-sub (lambda (expr env) (cond
				       ;; 式が一つもない場合
				       ((null? expr)
					#t)
				       ;; 最後のテスト
				       ((null? (cdr expr))
					(eval-exp (car expr) env))
				       ;; テストが真の場合
				       ((not (eq? (eval-exp (car expr) env) #f))
					(and-sub (cdr expr) env))
				       ;; テストが偽の場合
				       (else
					#f)))))
    (and-sub (cdr exp) env)))

;;; (or Exp*)
(define (my-or exp env)
  (assert `("error or syntax: " ,exp) (and (list? exp) (eq? (car exp) 'or)))
  (letrec ((or-sub (lambda (expr env)   (cond
					;; 式が一つもない場合
					((null? expr)
					 #f)
					;; 最後のテスト
					((null? (cdr expr))
					 (eval-exp (car expr) env))
					;; 
					(else
					 (let ((val (eval-exp (car expr) env)))
					   (if val
					       val
					       (or-sub (cdr expr) env))))))))
    (or-sub (cdr exp) env)))

;;; (begin Exp*)
(define (my-begin x env)
  (assert `("error begin syntax: " ,x) (and (list? x) (eq? (car x) 'begin)))
  (if (not (null? (cdr x)))
      (car (last (map (lambda (y) (eval-exp y env))
		      (cdr x))))))

;;; (do ((Id Exp Exp)*) (Exp Exp*) Body)
(define (my-do exp env)
  (define (iter-specs? iter-specs)
    (cond ((null? iter-specs)
	   #t)
	  ((and (list? (car iter-specs)) (= (length (car iter-specs)) 3) 
		(symbol? (caar iter-specs)))
	   (iter-specs? (cdr iter-specs)))
	  (else
	   #f)))

  (assert `("error do syntax: " ,exp)
	  (and (list? exp) (eq? (car exp) 'do) (>= (length exp) 4)
	       (iter-specs? (cadr exp)) (list? (caddr exp)) 
	       (not (null? (caddr exp)))))
  
  (let loop ((iter-specs  (cadr   exp))
	     (test        (caaddr exp))
	     (tail-seq    (cdaddr exp)) ; nullのときは?
	     (body        (cdddr  exp))
	     (ex-env (extend-env 
		      (map car (cadr exp))
		      (map (lambda (iter-spec) (eval-exp (cadr iter-spec) env)) 
			   (cadr exp))
		      env)))
    (if (eval-exp test                ex-env)
	(eval-exp `(begin ,@tail-seq) ex-env)
	(begin (eval-body body ex-env)
	       (loop iter-specs test tail-seq body
		     (extend-env (map car iter-specs)
				 (map (lambda (iter-spec) (eval-exp (caddr iter-spec) ex-env)) 
				      iter-specs)
				 ex-env))))))

;;; (define-macro Id Exp) | (define-macro (Id Id* [. Id]) Body)
(define (my-define-macro x env)
  (assert `("error define-macro syntax: " ,x)
	  (and (list? x) (eq? (car x) 'define-macro) (>= (length x) 3)))
  (cond
   ;; (define-macro Id Exp) のケース
   ((symbol? (cadr x))
    (let ((var-info (get-varinfo (cadr x) env)))
      (cond
       ;; varがマクロとして定義されているケース
       ((and var-info (macro-name? (cdr var-info))) 
        (set-cdr! var-info (eval-exp (caddr x) env))
	(cadr x))
       ;; varが手続として定義されているケース
       (var-info                                    
        (set-macroname! (cadr x)) 
	(set-cdr! var-info (eval-exp (caddr x) env))
	(cadr x))
       ;; varが未定義のケース
       (else
        (set! *global-env*
	      (cons (cons (cadr x) (eval-exp (caddr x) env)) *global-env*))
	(set-macroname! (cadr x))
	(cadr x)))))
   ;; (define-macro (<変数> <仮引数部>) <本体>) 
   ;; -> (define <変数> (lambda (<仮引数部>) <本体>))
   ;; (define (<変数> . <仮引数>) <本体>) -> (define <変数> (lambda <仮引数> <本体>))
   ((pair? (cadr x))
    (my-define-macro (get-bitterdefmacro x) env))
   (else
    (abort `("define-macro misc error: " ,x)))))

;;; define-macroの糖衣構文を基本形に直したものを返す
(define (get-bitterdefmacro x)
  (assert `("error get-bitterdefmacro: " ,x)
	  (and (list? x) (eq? (car x) 'define-macro) (>= (length x) 3)))
  (if (symbol? (cadr x))
      x
      `(define-macro ,(caadr x) (lambda ,(cdadr x) ,@(cddr x)))))

;;; (define-macro (Id Id* [. Id]) Body) -> (define-macro Id Exp)
(define (desugar-defmcr defmcr)
  (assert `("error define-macro syntax: " ,defmcr)
	  (and (list? defmcr) (not (null? defmcr)) (eq? (car defmcr) 'define-macro)
	       (>= (length defmcr) 3)))
  (if (symbol? (cadr defmcr))
      defmcr
      (begin
	(assert `("error define-macro syntax: " ,defmcr)
		(and (pair? (cadr defmcr)) (symbol? (caadr def))))
	`(define-macro ,(caadr defmcr) (lambda ,(cdadr defmcr) ,@(cddr defmcr))))))

;;; (lambda Arg Body)
;;; Arg  ::= Id | (Id* [Id . Id])
;;; Body ::= Define* Exp+
(define (my-lambda x env)
  (let ((parms (cadr x))
	(code (trans-body (cddr x) env)))
    (lambda args (eval-exp code (extend-env parms args env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bodyがDefine+Exp*であれば等価なletrecに変換された式を返す
(define (trans-body body env)
  (let* ((bitterbody (get-bitterbody body env))
	 (defpart    (get-defpart bitterbody))
	 (exppart    (get-exppart bitterbody)))
    (cond
     ((and (null? defpart) (null? exppart))
      '())
     ((and (null? defpart) (= (length exppart) 1))
      (car exppart))
     ((null? defpart)
      (cons 'begin exppart))
     (else
      `(letrec ,defpart ,@exppart)))))

;;; 
(define (get-bitterbody body env)
  (assert `("error get-bitterbody: " ,body) (list? body))
  (cond
   ((null? body)
    body)
   (else
    (cons (make-bodydefbitter (car body) env)
	  (get-bitterbody (cdr body) env)))))

;;; xが'defineで始まるリストなら糖衣構文を解いて返し, それ以外は与えられたxをそのまま返す.
;;; xがマクロであれば展開され, 糖衣構文を解いたものが返される. 
(define (make-bodydefbitter x env)
  (let ((maybedef (macro-expand x env)))
    (if (and (list? maybedef) (not (null? x))(eq? (car maybedef) 'define))
	(get-bitterdef maybedef)
	maybedef)))

;;; 先頭がdefineのdefine構文のリストが糖衣構文であれば基本形(define Id Exp) に変形したものを返す
(define (get-bitterdef x)
  (assert `("error get-bitterdef: " ,x) 
	  (and (list? x) (eq? (car x) 'define) (>= (length x) 3)))
  (if (symbol? (cadr x))
      x
      `(define ,(caadr x) (lambda ,(cdadr x) ,@(cddr x)))))

;;; Def部がすべて非糖衣構文からなるBodyを受取り, Defineで表わされる部分をリストで返す. 
;;; Defにあたるものが存在しなければ, ()を返す. 
(define (get-defpart bitterbody)
  (cond
   ((or (null? bitterbody) (not (list? (car bitterbody))))
    '())
   ((and (not (null? (car bitterbody))) (eq? (caar bitterbody) 'define))
    (cons (cdar bitterbody) (get-defpart (cdr bitterbody))))
   (else
    '())))

;;; Def部がすべて非糖衣構文からなるBodyを受取り, Exp+で表わされる部分をリストで返す. 
(define (get-exppart bitterbody)
  (cond
   ((null? bitterbody)
    bitterbody)
   ((and (list? (car bitterbody)) (not (null? (car bitterbody)))
	 (eq? (caar bitterbody) 'define))
    (get-exppart (cdr bitterbody)))
   (else
    (cons (car bitterbody) (get-exppart (cdr bitterbody))))))