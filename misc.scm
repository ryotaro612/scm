;;; (last list)
;;; last returns the last cons (not the last element!) of list. 
;;; If list is (), it returns ().
(define (last lst)
  (cond ((null? lst) lst)
	((and (pair? lst) (not (pair? (cdr lst)))) lst)
	(else (last (cdr lst)))))

;;; (neq? obj1 obj2)
(define (neq? a b) (not (eq? a b)))

;;; エラーメッセージを表示して, インタプリタを初期化する
(define (abort info)
  (cond
   ((null? info)
    (newline) (init))
   ((list? info)
    (display (car info)) (abort (cdr info)))
   (else
    (display info) (newline) (init))))

;;; predが#fの場合にabortを呼び出す
(define (assert info pred)
  (if (not pred)
      (abort info)))

;;; 使用している？
;;; xがDefineで表わされるならば#t, それ以外では#fを返す
;;; Defineである条件は, 先頭が'defineで始まること.
;;; xがマクロであれば展開後のリストを調べる. 
(define (define? x env)
  (if (and (list? x) (eq? (car (macro-expand x env)) 'define))
      #t
      #f))