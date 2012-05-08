;;; 継続
(call/cc (lambda (cont) cont)) ; ==> #<subr continuation>

(+ 1 (* 2 (call/cc (lambda (cont) 3)))) ; ==> 7

(+ 1 (* 2 (call/cc (lambda (cont) (cont 4) 3)))) ; ==> 9

(define *cont* #f) ; ==> *cont*
(+ 1 (* 2 (call/cc (lambda (cont) (set! *cont* cont) 3)))) ; ==> 7

(*cont* 10) ; ==> 21

(*cont* 100) ; ==> 201

;;; マクロ
(define a 1)
;;; (positive a) ==> (> 1 0)
(define positive (lambda (x) (list '> x 0)))
;;; (psitive a)  ==> #t
(define-macro positive (lambda (x) (list '> x 0)))

(define a 4) ; ==> a

(define-macro (def! x) (list 'define x ''())) ; ==> def!

(define (f x) (def! x) x) ; ==> f
(f a) ; ==> ()

a ;==> 4

;;; let
(let ((x 2) (y 4)) (define z (+ x y)) z) ; ==> 6

;;; 名前付きlet
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

;;; letrec
(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p 
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))
    (iter n n)))

;;; do
(define (fact-do n)
  (do ((n1 n (- n1 1)) (p n (* p (- n1 1))))
      ((= n1 1) p)
    ()
    ))