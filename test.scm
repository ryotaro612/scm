;;; 継続のテスト
(call/cc (lambda (cont) cont))
(+ 1 (* 2 (call/cc (lambda (cont) 3))))

(+ 1 (* 2 (call/cc (lambda (cont) (cont 4) 3))))

(define *cont* #f)

(+ 1 (* 2 (call/cc (lambda (cont) (set! *cont* cont) 3))))

(*cont* 10)

(*cont* 100)

;;; マクロのテスト
(define a 1)
;;; (positive a) ==> (> 1 0)
(define positive (lambda (x) (list '> x 0)))
;;; (psitive a)  ==> #t
(define-macro positive (lambda (x) (list '> x 0)))
