;;; (load String)
(define (my-load x)
  (assert `("error load syntax: " ,x) 
	  (and (list? x) (= (length x) 2) (eq? (car x) 'load) (string? (cadr x))))
  (call-with-input-file (cadr x) ; filename
    (lambda (port) 
      (let ((s-exps (port->sexp-list port)))
	(define (eval-reading reading)
	  (cond ((null? reading)
		 #t)
		(else
		 (my-eval (car reading) '()) 
		 (eval-reading (cdr reading)))))	
	(eval-reading s-exps)))))

;;; (last list)
;;; last returns the last cons (not the last element!) of list. 
;;; If list is (), it returns ().
(define (last lst)
  (cond ((null? lst) lst)
	((and (pair? lst) (not (pair? (cdr lst)))) lst)
	(else (last (cdr lst)))))

;;; (neq? obj1 obj2)
(define (neq? a b) (not (eq? a b)))

;;; predが#fの場合にabortを呼び出す
(define (assert info pred)
  (if (not pred)
      (abort info)))

;;; エラーメッセージを表示して, インタプリタを初期化する
(define (abort info)
  (cond
   ((null? info)
    (newline) (init))
   ((list? info)
    (display (car info)) (abort (cdr info)))
   (else
    (display info) (newline) (init))))