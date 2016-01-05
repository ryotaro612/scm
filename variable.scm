(define (extend-env vars vals env) 
  (define (proper-numvals? vars num-vals)
    (cond
     ((and (symbol? (cdr vars)) (>= num-vals 1))
      #t)
     ((= num-vals 0)
      #f)
     (else
      (proper-numvals? (cdr vars) (- num-vals 1)))))  
  ;; in case ((lambda (x y z . w) <body>) a b c d)
  (define (extend-env-sub vars vals env)
    (assert `("error wrong num of vals: " ,vars ", " ,vals) 
	    (proper-numvals? vars (length vals)))
    (cond
     ((symbol? (cdr vars))
      (cons 
       (cons (cdr vars) (cdr vals)) 
       (cons (cons (car vars) (car vals)) env)))
     (else
      (extend-env-sub (cdr vars)
		      (cdr vals)
		      (cons (cons (car vars) (car vals)) env)))))
  (cond
   ;; (lambda () <body>)
   ((null? vars) 
      env)
   ;; ((lambda (x y z) <body>) a b c)
   ((and (list? vars) (list? vals) (= (length vars) (length vals)))
      (let ((a (map cons vars vals)) (b env))
	(set-cdr! (last a) b)
	a))
   ;; (lambda x <body>)
   ((and (symbol? vars) (list? vals))
      (let ((newenv (list (cons vars vals))))
	(set-cdr! newenv env)
	newenv))
   ;; ((lambda (x y z . w) <body>) a b c d)
   ((and (not (symbol? vars)) (not (list? vars)))
      (extend-env-sub vars vals env))
   (else
    (abort `("error wrong num of vals: " ,vars ", " ,vals)))))

;;; 
(define (expand-macro x env)
  (if (and (list? x) (not (null? x)) (macro-var? (car x)))
      (expand-macro (apply (get-val (car x) env) (cdr x)) env)
      x))

;;;
(define (macro-var? var)
  (if (member var *macro-vars*)
      #t
      #f))

;;; remove var from *macro-vars*
(define (delete-macrodef! var)
  (let ((num-defmacro (length *macro-vars*))
	(varlst       (member var *macro-vars*)))
    (cond
     ;; var is unbound.
     ((eq? varlst #f)
        #f)
     ;; *macro-vars* == (var ...)
     ((eq? var (car *macro-vars*)) 
        (set! *macro-vars* (cdr *macro-vars*)) var)
     ;; *macro-vars* == (.. var ..)
     (else 
        (set-cdr! (list-tail *macro-vars* (- num-defmacro (length varlst) 1))
		  (cdr varlst)) 
	var))))

(define (set-macroname! var)
  (if (not (member var *macro-vars*))
      (set! *macro-vars* (cons var *macro-vars*)))
  var)

;;; return (var . val). in case var is unbound, return #f. 
(define (get-bound var env)
  (let ((info (assoc var env)))
    (cond
     (info                     info)
     ((assoc var *global-env*) (assoc var *global-env*))
     (else                     #f))))

(define (get-val var env)
  (let ((info (get-bound var env)))
    (if info
	(cdr info)
	(abort `(,var " is unbound. ")))))