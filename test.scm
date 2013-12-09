(define-macro positive (lambda (x) (list '> x 0)))

(define-macro (set-nil! x) (list 'define x ()))

(define (even? x)
  (if (= x 0) #t (odd? (- x 1))))
(define (odd? x)
  (if (= x 1) #t (even? (- x 1))))

(define (fact n)
  (let loop ((n n) (a 1))
    (if (= n 0)
	a
	(loop (- n 1) (* a n)))))

(define (my-reverse lst)
  (let loop ((lst lst) (a '()))
    (if (null? lst)
	a
	(loop (cdr lst) (cons (car lst) a)))))

(define (fact-letrec n)
  (letrec ((loop (lambda (n m)
		   (if (= n 0)
		       m
		       (loop (- n 1) (* m n))))))
    (loop n 1)))

(define (fact-do n)
  (do ((n n (- n 1)) (m 1 (* m n)))
      ((= n 0) m)
    ()))