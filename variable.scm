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
  (if (and (list? x) (not (null? x)) (macro-name? (car x)))
      (macro-expand (apply (get-val (car x) env) (cdr x)) env)
      x))

;;; varがマクロとして定義されていたら#t, それ以外は#fを返す.
(define (macro-name? var)
  (if (member var *macro-symbols*)
      #t
      #f))

;;; *macro-symbols*からvarを削除する
;;; 返値をあてにしてはならない
(define (delete-macrodef var)
  (let ((num-defmacro (length *macro-symbols*)) 
	(varlst       (member var *macro-symbols*)))
    (cond
     ;; シンボルが未定義の場合
     ((eq? varlst #f)
        #f)
     ;; *macro-symbols*の先頭にvarが格納されている場合
     ((eq? var (car *macro-symbols*)) 
        (set! *macro-symbols* (cdr *macro-symbols*)) var)
     ;; *macro-symbols*の2番目以降にvarが格納されている場合
     (else 
        (set-cdr! (list-tail *macro-symbols* (- num-defmacro (length varlst) 1))
		  (cdr varlst)) 
	var))))

;;; *macro-symbols*にvarが存在しなければ, *macro-symbols*に追加する.
(define (set-macroname! var)
  (if (not (member var *macro-symbols*))
      (set! *macro-symbols* (cons var *macro-symbols*)))
  var)

;;; (変数 . 値) のペアを返す. 変数が未定義の場合は#fを返す.
(define (get-varinfo var env)
  (let ((info (assoc var env)))
    (cond
     (info                     info)
     ((assoc var *global-env*) (assoc var *global-env*))
     (else                     #f))))

;;; 変数が未定義の場合の返値はエラー
(define (get-val var env)
  (let ((info (get-varinfo var env)))
    (if info
	(cdr info) ; 値が見つかったケース
	(abort `(,var " is unbound. ")))))