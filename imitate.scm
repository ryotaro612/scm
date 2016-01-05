(define my-number?
  (lambda obj (begin (assert `("wrong num of args: number?", obj) (= (length  obj) 1))
		     (apply number? obj))))

(define my-+
  (lambda num-lst (begin 
		    (assert `("error + ") 
			    (and (>= (length num-lst) 2)
				 (not (member #f (map number? num-lst)))))
		    (apply + num-lst))))

(define my--
  (lambda num-lst (begin 
		    (assert `("error - ", num-lst)
			    (and (>= (length num-lst) 2)
				 (not (member #f (map number? num-lst)))))
		    (apply - num-lst))))

(define my-*
  (lambda num-lst (begin 
		    (assert `("error * ", num-lst)
			    (and (>= (length num-lst) 2)
				 (not (member #f (map number? num-lst)))))
		    (apply * num-lst))))