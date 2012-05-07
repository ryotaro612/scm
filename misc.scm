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