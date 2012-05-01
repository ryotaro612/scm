;;; (last list)
;;  last returns the last cons (not the last element!) of list. 
;;  If list is (), it returns ().
(define (last lst)
  (cond ((null? lst) lst)
	((and (pair? lst) (not (pair? (cdr lst)))) lst)
	(else (last (cdr lst)))))

;;; (neq? obj1 obj2) はschemeのプリミティブな手続きではない
(define (neq? a b) (not (eq? a b)))


(define *global-env* '())
(define *macro-lst* '())

;;; 'varがマクロとして定義されていたら#t, それ以外は#fを返す.
(define (macro-name? var)
  (if (member var *macro-lst*)
      #t
      #f))

;;; *macro-lst*から'varを削除する
(define (delete-macrodef var)
  (let ((num-defmacro (length *macro-lst*)) 
	(varlst       (member var *macro-lst*)))
    (cond
     ((eq? varlst #f) 
        #f)
     ((eq? var (car *macro-lst*)) 
        (set! *macro-lst* (cdr *macro-lst*)) #t)
     (else 
        (set-cdr! (list-tail *macro-lst* (- num-defmacro (length varlst) 1))
		  (cdr varlst)) 
	#t))))

;;; *macro-lst*にvarが存在しなければ*macro-lst*に追加する.
(define (set-macroname! var)
  (if (member var *macro-lst*)
      var
      (set! *macro-lst* (cons var *macro-lst*))))

;;; (変数 . 値) のペアを返す. 変数が未定義の場合は#fを返す.
(define (get-varinfo var env)
 (let ((info (assoc var env)))
   (cond
    (info                     info)
    ((assoc var *global-env*) (assoc var *global-env*))
    (else                     #f))))

;;; 変数が未定義の場合の返値は不定.
(define (get-val var env)
  (let ((info (get-varinfo var env)))
    (if info
	(cdr info)
	(begin (display var) (display " is not defined.") (newline)))))

;;; (set! id exp) 
;;; r5rsでは、set!の式の結果は未規定
(define (my-set! x env)
  (let ((id-info (get-varinfo (cadr x) env)))
    (cond
     ;; idがマクロとして定義されている場合
     ((and id-info (macro-name? (car id-info)))
         (delete-macrodef (car id-info))
	 (set-cdr! id-info (my-eval (caddr x) env)))
     ;; idが定義されている場合
     (id-info
         (set-cdr! id-info (my-eval (caddr x) env)))
     (else
         (begin (display (cadr x)) (display " is not defined.") (newline))))))

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
(define (my-or exps env)
  (cond
   ;; 式が一つもない場合
   ((null? (cdr exps))
      #f)
   ;; 最後のテスト
   ((and (list? (cdr exps)) (null? (cddr exps)))
      (my-eval (cadr exps) env))
   ;;
   (else
      (let ((value (my-eval (cadr exps) env)))
	(if value
	    value
	    (my-or (cdr exps) env))))))


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

;;; ((lambda (x y z . w) <body>) a b c d) のときに使用
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
;;;        

;;; (define Id Exp) | (define (Id Id* [. Id]) Body)
;;; Body内部のdefineは考慮しない
;;; UNDER CONSTRUCTION
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
       ((and info (macro-name? (car info)))
	  (delete-macrodef (car info))
	  (set-cdr! info (my-eval (caddr x) env)))
       (info
	  (set-cdr! info (my-eval (caddr x) env)))
       (else
          (set! *global-env*
		(cons (cons (cadr x) (my-eval (caddr x) env))
		      *global-env*))))))
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

;;; マクロ展開
(define (macro-expand x env)
  (if (and (list? x) (macro-name? (car x)))
      (macro-expand (apply (get-val (car x) env) (cdr x)) env)
      x))

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
;;;

;;; UNDER CONSTRUCTION
(define (my-eval x env)
  (cond
   ((symbol? x)           (get-val x env))
   ((not (pair? x))       x)
   ((macro-name? (car x)) (my-eval (macro-expand x env) env))
   ((pair? x)
    (case (car x)
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
      ((define-macro) (my-define-macro x env))
      (else
       (apply (my-eval (car x) env)
	      (map (lambda (y) (my-eval y env))
		   (cdr x))))))))

(define (init-global-env)
  (list
   (cons 'number?        number?)
   (cons '+              +)
   (cons '-              -)
   (cons '*              *)
   (cons '/              /)
   (cons '=              =)
   (cons '<              <)
   (cons '<=             <=)
   (cons '>              >)
   (cons '>=             >=)   
   (cons 'null?          null?)
   (cons 'pair?          pair?)
   (cons 'list?          list?)
   (cons 'symbol?        symbol?)
   (cons 'car            car)
   (cons 'cdr            cdr)
   (cons 'cons           cons)
   (cons 'list           list)
   (cons 'length         length)
   (cons 'memq           memq)
   (cons 'last           last)
   (cons 'append         append)
   (cons 'set-car!       set-car!)
   (cons 'set-cdr!       set-cdr!)
   (cons 'boolean?       boolean?)
   (cons 'not            not)
   (cons 'string?        string?)
   (cons 'string-append  string-append)
   (cons 'symbol->string symbol->string)
   (cons 'string->symbol string->symbol)
   (cons 'string-number  string->number)
   (cons 'number->string number->string)
   (cons 'procedure?     procedure?)
   (cons 'eq?            eq?)
   (cons 'neq?           neq?)
   (cons 'equal?         equal?)
   (cons 'load           load)
   (cons 'call/cc        call/cc)
   ))

(define (my-scm)
  (set! *global-env* (init-global-env))
  (let loop ()
    (display "my-scm> ")
    (display (my-eval (read) '())) ; UNDER CONSTRUCTION
    (newline)
    (loop)))

(my-scm)

; Toplevel ::= Exp
;            | Define
;            | (load String) 
; 
; Define ::= (define Id Exp)                    △
;          | (define (Id Id* [. Id]) Body)      △
; 
; Exp ::= Const                                 △
;       | Id                                    △
;       | (lambda Arg Body)                     △
;       | (Exp Exp*)                            △ 
;       | (quote S-Exp)                         △
;       | (set! Id Exp)                         △
;       | (let [Id] Bindings Body)              △
;       | (let* Bindings Body)                  △
;       | (letrec Bindings Body)                △ 
;       | (if Exp Exp [Exp])                    △
;       | (cond (Exp Exp+)* [(else Exp+)])      △
;       | (and Exp*)                            △
;       | (or Exp*)                             △
;       | (begin Exp*)                          △
;       | (do ((Id Exp Exp)*) (Exp Exp*) Body)  △

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