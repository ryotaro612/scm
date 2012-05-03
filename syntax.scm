;;; BodyのDefineの変形はmy-lambdabodyで処理させる?
;;; シンタックスの検査は非終端記号別に作成した関数で処理させる?
;;; Bindingsのシンタックスの検査はlet系で行う?
;;; my-defineにBody中のDefineが渡る前にletrecに変形する必要がある
;;; my-lambdaでDefineの検査と変形を行う?
;;; 非終端記号別にevalを作り変える ?
'()
;;; (define Id Exp) | (define (Id Id* [. Id]) Body)
(define (my-define x env)
  (cond
   ;; 下二つ同じ処理をしている 
   ;; (define (<変数> <仮引数部>) <本体>) -> (define <変数> (lambda (<仮引数部>) <本体>))
   ((and (pair? (cadr x)) (pair? (cdadr x)))
    (my-define `(define ,(caadr x) (lambda ,(cdadr x) ,@(cddr x))) env))
   ;; (define (<変数> . <仮引数>) <本体>) -> (define <変数> (lambda <仮引数> <本体>))
   ((and (pair? (cadr x)) (symbol? (cdadr x)))
    (my-define `(define ,(caadr x) (lambda ,(cdadr x) ,@(cddr x))) env))
   ;; (define Id Exp) のケース
   ((symbol? (cadr x))
    (let ((info (get-varinfo (cadr x) env)))
      (cond
       ;; シンボルが既にマクロとして定義されている場合
       ((and info (macro-name? (car info)))
	  (delete-macrodef (car info))
	  (set-cdr! info (my-eval (caddr x) env))
	  (car info))
       ;; シンボルがマクロ以外(変数/手続き)として定義されている場合
       (info
	  (set-cdr! info (my-eval (caddr x) env))
	  (car info))
       ;; それ以外(シンボルが未定義の場合)
       (else
          (set! *global-env*
		(cons (cons (cadr x) (my-eval (caddr x) env))
		      *global-env*))
	  (cadr x)))))
   (else
    (abort `("define error: " ,x)))))

;;; (set! Id Exp) 
;;; r5rsでは、set!の式の結果は未規定
(define (my-set! x env)
  ;; xが長さ3のリストではない, またはidがシンボルではないケース
  (if (not (and (= (length x) 3) (symbol? (cadr x))))
      (abort `("set! syntax error: " ,x)))
  (let* ((id (cadr x)) (exp (caddr x)) (id-info (get-varinfo id env)))
    (cond
     ;; idがマクロとして定義されている場合
     ((and id-info (macro-name? id))
      (delete-macrodef id)
      (set-cdr! id-info (my-eval exp env)))
     ;; idがマクロ以外(値/手続き)として定義されている場合
     (id-info
      (set-cdr! id-info (my-eval exp env)))
     ;; idが未定義の場合
     (else
      (abort `(,id " is not defined."))))))

;;; (let [Id] Bindings Body)
;;; 名前付きletの処理はmy-namedletに依頼する
(define (my-let x env)
  (cond
   ;; 名前付きletのケース
   ((symbol? (cadr x))
    (my-namedlet x env))
   ;; 束縛がないケース
   ;; (let () Body)            -> (lambda () Body)
   ((null? (cadr x))
    (my-eval `((lambda () ,@(cddr x))) env))
   ;; 束縛があるケース
   ;; (let ((x a) (y b)) Body) -> ((lambda (x y) Body) a b)
   (else
    (my-eval `((lambda ,(map car (cadr x)) ,@(cddr x)) ,@(map cadr (cadr x)))
	     env))))

;;; (let Id Bindings Body)
;;; 名前付きlet, my-letで利用される補助関数
;;; (let Id Bindigs Body) -> (letrec ((Id (lambda (args) Body))) (Id actuals))
(define (my-namedlet x env)
  (let ((id (cadr x)) (bindings (caddr x)) (body (cdddr x)))
    (if (null? bindings)
	;; 束縛がないケース
	(my-letrec 
	 `(letrec ((,id (lambda () ,@body))) ,id) env)
	;; 束縛があるケース
	(my-letrec 
	 `(letrec ((,id (lambda ,(map car bindings) ,@body))) (,id ,@(map cadr bindings))) env))))

;;; (let* Bindings Body)
(define (my-let* x env)
  (let ((bindings (cadr x)) (body (cddr x)))
    (if (null? bindings)
	;; 束縛がないケース
	(my-eval `((lambda () ,@body)) env)
	;; 束縛があるケース	
	(my-eval 
	 `((lambda (,(caar bindings)) (let* ,(cdr bindings) ,@body)) 
	   ,(cadar bindings)) 
	 env))))

;;; (letrec Bindings Body)
(define (my-letrec x env)
  (if (null? (cadr x))
      ;; 束縛がないケース
      (my-eval `(,`(lambda () ,@(cddr x))) env)
      ;; 束縛があるケース
      (my-let
       `(let ,(map (lambda (lst) (cons (car lst) '(*undef*)) lst) (cadr x)) 	    
	  ,@(append (map (lambda (lst) (cons 'set! lst)) (cadr x)) (cddr x)))
       env)))

;;; (if Exp Exp [Exp])
;;; testが#f、かつalternateが未定義であれば式の結果は未規定
(define (my-if x env)
  (if (my-eval (cadr x) env)
      (my-eval (caddr x) env)
      (if (not (null? (cdddr x)))
	  (my-eval (cadddr x) env))))

;;; (cond (Exp Exp+)* [(else Exp+)])
;;; すべてのテストの値が#fでelse節がなければ式の値は未規定
(define (my-cond x env)
  (cond 
   ;; (else Exp+)
   ((eq? (caadr x) 'else)
    (my-eval `(begin ,@(cdadr x)) env))
   ;; (Exp Exp+) 
   ((my-eval (caadr x) env)
    (my-eval `(begin ,@(cdadr x)) env))
   ;; 最後の節のテストが#f
   ((null? (cddr x))
    "unspecified")
   ;; 節のテストが#f
   (else
    (my-cond (cdr x) env))))
   
;;; (and Exp*)
(define (my-and x env)
  (if (not (list? x))
      (abort `("and syntax error: " ,x)))
  (cond
   ;; 式が一つもない場合
   ((null? (cdr x)) 
      #t) 
   ;; 最後のテスト
   ((and (list? (cdr x)) (null? (cddr x)))
    (my-eval (cadr x) env))
   ;; テストが真の場合
   ((not (eq? (my-eval (cadr x) env) #f)) 
    (my-and (cdr x) env))
   ;; テストが偽の場合
   (else 
    #f)))

;;; (or Exp*)
(define (my-or x env)
  (if (not (list? x))
      (abort `("or syntax error: " ,x)))
  (cond
   ;; 式が一つもない場合
   ((null? (cdr x))
      #f)
   ;; 最後のテスト
   ((and (list? (cdr x)) (null? (cddr x)))
      (my-eval (cadr x) env))
   ;;
   (else
      (let ((value (my-eval (cadr x) env)))
	(if value
	    value
	    (my-or (cdr x) env))))))

;;; (begin Exp*)
(define (my-begin x env)
  (if (not (null? (cdr x)))
      (car (last (map (lambda (y) (my-eval y env))
		      (cdr x))))))

;;; (do ((Id Exp Exp)*) (Exp Exp*) Body)
;;; letrecに変形
(define (my-do x env)
  (let ((a (cadr x)) (b (caddr x)) (body (cdddr x)))
    (if (null? a)
	(my-letrec
	 `(letrec ((do-iter (lambda () (if ,(car b)
					   (begin ,@(cdr b))
					   (begin ,@body (do-iter))))))
	    (do-iter))
	 env)
	
	(my-letrec
	 `(letrec ((do-iter 
		    (lambda ,(map car a) 
		      (if ,(car b)
			  (begin ,@(cdr b))
			  (begin 
			    ,@body 
			    (do-iter 
			     ,@(map (lambda (lst) (if (< (length lst) 3)
						      (car lst)
						      (caddr lst)))
				    a)))))))
	    (do-iter ,@(map cadr a)))
	 env))))

;;; (define-macro Id Exp) | (define-macro (Id Id* [. Id]) Body)
;;; Body内部のdefineは考慮しない
(define (my-define-macro x env)
  (cond
   ;;    (define-macro (<変数> <仮引数部>) <本体>)
   ;; -> (define <変数> (lambda (<仮引数部>) <本体>))
   ((and (pair? (cadr x)) (pair? (cdadr x)))
    (my-define-macro `(define-macro ,(caadr x) (lambda ,(cdadr x) ,@(cddr x))) env))
   ;; (define (<変数> . <仮引数>) <本体>) -> (define <変数> (lambda <仮引数> <本体>))
   ((and (pair? (cadr x)) (symbol? (cdadr x)))
    (my-define-macro `(define-macro ,(caadr x) (lambda ,(cdadr x) ,@(cddr x))) env))
   ;; (define-macro Id Exp) のケース
   ((symbol? (cadr x))
    (let ((var-info (get-varinfo (cadr x) env)))
      (cond
       ;; varがマクロとして定義されているケース
       ((and var-info (macro-name? (cdr var-info))) 
        (set-cdr! var-info (my-eval (caddr x) env))
	(cadr x))
       ;; varが手続として定義されているケース
       (var-info                                    
        (set-macroname! (cadr x)) 
	(set-cdr! var-info (my-eval (caddr x) env))
	(cadr x))
       ;; varが未定義のケース
     (else
        (set! *global-env*
	      (cons (cons (cadr x) (my-eval (caddr x) env)) *global-env*))
	(set-macroname! (cadr x))
	(cadr x)))))))

;;; (lambda Arg Body)
;;; Arg  ::= Id | (Id* [Id . Id])
;;; Body ::= Define* Exp+

(define (my-lambda x env)
  (let ((parms (cadr x))
	(code  (lambda-body (cddr x))))
    (lambda args (my-eval code (extend-env parms args env)))))

;;; my-lambdaで使う
;;; 下の関数で置き換える予定
(define (lambda-body lamb)
  (if (= (length lamb) 1)
      (car lamb)
      (cons 'begin lamb)))

;;; bodyがDefineだけで構成されていた場合も考慮しろ
(define (make-bodydefbitter x env)
  (let ((maybedef (macro-expand x env)))
    (if (eq? (car maybef) 'define)
	(make-defbitter maybedef)
	maybedef)))
	
;;; 先頭がdefineのdefine構文のリストが糖衣構文であれば基本形に変形したものを返す
;;; get-bitterdefの方が適切
(define (make-defbitter x)
  (if (not (eq? (car x) 'define))
      (abort `("error make bitter: " ,x)))
  (if (symbol? (cadr x))
      x
      `(define ,(caadr x) (lambda ,(cdadr x) ,@(cddr x)))))