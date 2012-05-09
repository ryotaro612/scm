;;;   Add some variables and values to an environment.
(define (extend-env parms args env) 
  (define (proper-numargs? parms num-args)
    (cond
     ((and (symbol? (cdr parms)) (>= num-args 1))
      #t)
     ((= num-args 0)
      #f)
     (else
      (proper-numargs? (cdr parms) (- num-args 1)))))  
  ;; extend-envで, ((lambda (x y z . w) <body>) a b c d) のときに使用
  (define (extend-env-sub parms args env)
    (assert `("error wrong num of args: " ,parms ", " ,args) 
	    (proper-numargs? parms (length args)))
    (cond
     ((symbol? (cdr parms))
      (cons 
       (cons (cdr parms) (cdr args)) 
       (cons (cons (car parms) (car args)) env)))
     (else
      (extend-env-sub (cdr parms)
		      (cdr args)
		      (cons (cons (car parms) (car args)) env)))))
  (cond
   ;; (lambda () <body>)
   ((null? parms) 
      env)
   ;; ((lambda (x y z) <body>) a b c)
   ((and (list? parms) (list? args) (= (length parms) (length args)))
      (let ((a (map cons parms args)) (b env))
	(set-cdr! (last a) b)
	a))
   ;; (lambda x <body>)
   ((and (symbol? parms) (list? args))
      (let ((newenv (list (cons parms args))))
	(set-cdr! newenv env)
	newenv))
   ;; ((lambda (x y z . w) <body>) a b c d)
   ((and (not (symbol? parms)) (not (list? parms)))
      (extend-env-sub parms args env))
   (else
    (abort `("error wrong num of args: " ,parms ", " ,args)))))

;;; マクロ展開
(define (expand-macro x env)
  (if (and (list? x) (not (null? x)) (macro-var? (car x)))
      (expand-macro (apply (get-val (car x) env) (cdr x)) env)
      x))

;;; varがマクロとして定義されていたら#t, それ以外は#fを返す.
(define (macro-var? var)
  (if (member var *macro-vars*)
      #t
      #f))

;;; *macro-vars*からvarを削除する
;;; 返値をあてにしてはならない
(define (delete-macrodef! var)
  (let ((num-defmacro (length *macro-vars*))
	(varlst       (member var *macro-vars*)))
    (cond
     ;; シンボルが未定義の場合
     ((eq? varlst #f)
        #f)
     ;; *macro-vars*の先頭にvarが格納されている場合
     ((eq? var (car *macro-vars*)) 
        (set! *macro-vars* (cdr *macro-vars*)) var)
     ;; *macro-vars*の2番目以降にvarが格納されている場合
     (else 
        (set-cdr! (list-tail *macro-vars* (- num-defmacro (length varlst) 1))
		  (cdr varlst)) 
	var))))

;;; *macro-vars*にvarが存在しなければ, *macro-vars*に追加する.
(define (set-macroname! var)
  (if (not (member var *macro-vars*))
      (set! *macro-vars* (cons var *macro-vars*)))
  var)

;;; (変数 . 値) のペアを返す. 変数が未定義の場合は#fを返す.
(define (get-bound var env)
  (let ((info (assoc var env)))
    (cond
     (info                     info)
     ((assoc var *global-env*) (assoc var *global-env*))
     (else                     #f))))

;;; 変数が未定義の場合の返値はエラー
(define (get-val var env)
  (let ((info (get-bound var env)))
    (if info
	(cdr info) ; 値が見つかったケース
	(abort `(,var " is unbound. ")))))