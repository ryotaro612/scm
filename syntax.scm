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
      (abort `(,id "is nort defined."))))))

;;; (let [Id] Bindings Body)
;;; 名前付きletの処理はmy-namedletに依頼する
(define (my-let x env)
  (cond
   ;; 名前付きlet 
   ((symbol? (cadr x))
    (my-namedlet x env))
   ;; 束縛がないケース
   ((null? (cadr x))
    (my-eval `(,`(lambda () ,@(cddr x))) env))
   ;; 束縛があるケース
   (else
    (my-eval `(,`(lambda ,(map car (cadr x)) ,@(cddr x)) ,@(map cadr (cadr x)))
	     env))))

;;; (let Id Bindings Body)
;;; 名前付きlet, my-letでの補助関数
(define (my-namedlet x env)
  (if (null? (caddr x))
      ;; 束縛がない場合
      (my-letrec `(letrec ,`(,`(,(cadr x) ,`(lambda () ,@(cdddr x)))) ,`(,(cadr x)))
		 env)
      ;; 束縛がある場合
      (my-letrec `(letrec 
		    ,`(,`(,(cadr x) ,`(lambda ,(map car (caddr x)) ,@(cdddr x))))
		    ,`(,(cadr x) ,@(map cadr (caddr x))))
		  env)))

;;; (let* Bindings Body)
(define (my-let* x env)
  (if (null? (cadr x))
      ;  束縛がないケース
      (my-eval `(,`(lambda () ,@(cddr x))) env)
      ;; 束縛があるケース
      (my-eval `(,`(lambda ,`(,(caaadr x)) ,`(let* ,(cdadr x) ,@(cddr x))) 
		 ,(car (cdaadr x)))
	       env)))

;;; (letrec Bindings Body)
(define (my-letrec x env)
  (if (null? (cadr x))
      ;  束縛がないケース
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
;;; UNDER CONSTRUCTION
(define (my-do x env)
  (if 
   (null? (cadr x))
   (my-letrec `(letrec ((do-iter
			 (lambda () (if ,(caaddr x)
					(begin ,@(cdaddr x))
					(begin ,@(cdddr x) (do-iter))))))
		 (do-iter))
	      env)
   (my-letrec 
    `(letrec ((do-iter 
	       (lambda ,(map car (cadr x))
		 (if ,(caaddr x)
		     (begin ,@(cdaddr x))
		     (begin
		       ,@(cdddr x)
		       (do-iter 
			,@(map (lambda (lst) (if (< (length lst) 3)
						 (car lst)
						 (caddr lst)))
			       (cadr x))))))))
       (do-iter ,@(map cadr (cadr x))))
    env)))

;;; (define Id Exp) | (define (Id Id* [. Id]) Body)
;;; 返値をあてにしてはならない
;;; Body内部のdefineは考慮しない
(define (my-define x env)
  (cond
   ;; (define (<変数> <仮引数部>) <本体>) -> (define <変数> (lambda (<仮引数部>) <本体>))
   ((and (pair? (cadr x)) (pair? (cdadr x)))
      (my-define `(define ,(caadr x) ,`(lambda ,(cdadr x) ,@(cddr x))) env))
   ;; (define (<変数> . <仮引数>) <本体>) -> (define <変数> (lambda <仮引数> <本体>))
   ((and (pair? (cadr x)) (symbol? (cdadr x)))
      (my-define `(define ,(caadr x) ,`(lambda ,(cdadr x) ,@(cddr x))) env))
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
    (display "define error: ") (display x))))

;;; (define-macro Id Exp) | (define-macro (Id Id* [. Id]) Body)
;;; Body内部のdefineは考慮しない
;;; UNDER CONSTRUCTION
(define (my-define-macro x env)
  (let ((var-info (get-varinfo (cadr x) env)))
    (cond
     ((and var-info (macro-name? (cdr var-info))) ;; 'varがマクロとして定義されている
        (set-cdr! var-info (my-eval (caddr x) env)))
     (var-info                                    ;; 'varが手続として定義されている
        (set-macroname! (cadr x)) 
	(set-cdr! var-info (my-eval (caddr x) env)))
     (else                                        ;; 'varが未定義
        (set! *global-env*
	      (cons (cons (cadr x) (my-eval (caddr x) env)) *global-env*))
	(set-macroname! (cadr x))))))

;;; (lambda Arg Body)
;;; Arg  ::= Id | (Id* [Id . Id])
;;; Body ::= Define* Exp+
(define (my-lambda x env)
  (let ((parms (cadr x))
	(code  (lambda-body (cddr x))))
    (lambda args (my-eval code (extend-env parms args env)))))

;;; my-lambdaで使う
(define (lambda-body lamb)
  (if (= (length lamb) 1)
      (car lamb)
      (cons 'begin lamb)))