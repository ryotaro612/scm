(define-macro positive (lambda (x) (list '> x 0)))

(define-macro (set-nil! x) (list 'define x ()))

(define (fact n)
  (let iter ((n n) (a 1))
    (if (= n 0)
	a
	(iter (- n 1) (* a n)))))

(define (my-reverse ls)
  (let iter ((ls ls) (a '()))
    (if (null? ls)
	a
	(iter (cdr ls) (cons (car ls) a)))))

(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p 
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))
    (iter n n)))

(define (fact-do n)
  (do ((n1 n (- n1 1)) (p n (* p (- n1 1))))
      ((= n1 1) p)
    ()))