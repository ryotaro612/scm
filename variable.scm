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