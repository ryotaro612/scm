;;; (load String)
(define (my-load x)
  (assert `("error load syntax: " ,x) 
	  (and (list? x) (= (length x) 2) (eq? (car x) 'load) (string? (cadr x))))
  (call-with-input-file file 
    (lambda (port) (let ((exprs (port->sexp-list port)))      
		     (define (eval-iter x)
		       (cond ((null? x)
			      (newline))
			     (else
			      (my-eval (car x) '()) (eval-iter (cdr x)))))
		     (eval-iter exprs)))))

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