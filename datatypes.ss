;; Parsed expression datatypes

;Expression types.
;Based on the simple expression grammar, EoPL-2 p6
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	(var-exp
		(id symbol?))
	(set!-exp
		(change symbol?)
		(to expression?))
	[lambda-exp
		(id check-lam?)
		(body (list-of expression-o?))]
	(let-exp
		(vars (list-of symbol?))
		(vals (list-of expression-o?))
		(body (list-of expression?)))
	[let*-exp 
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))]
	[letrec-exp 
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(body (list-of expression?))]
	[cond-exp 
		(tests (list-of expression?))
		(vals (list-of expression?))]
	[begin-exp 
		(items (list-of expression?))]
	(if-else-exp 
		(condition expression?)
		(if-true expression?)
		(if-false expression?))
	(if-exp-null
		(condition expression?)
		(if-true expression?))
	(app-exp
		(rator expression-o?)
		(rand (list-of expression-o?)))
	(lit-exp 
		(item lit?)))

	

(define list-of? 
	(lambda (pred) 
		(lambda (ls)
			(cond
				[(not(list? ls)) (pred? ls)]
				[else
					(or(andmap pred ls) (pred ls))]))))
	
;Checks if something is an expression or proc-val.
(define expression-o?
	(lambda (v)
		(or (expression? v) (proc-val? v))))

(define-datatype proc-val proc-val?
	[prim-proc
		(name test-prim?)]
	[lambda-proc-with-env
		(id check-lam?)
		(body (list-of expression-o?))
		(env environment?)]
	[proc-in-list-exp
		(id (list-of expression-o?))]
	; [unevaluated-proc
		; (bodys (list-of proc-val?))
		; ]
	)

; (define proc-in-list?
	; (lambda (x)
		; (cases expression x
			; (proc-in-list-exp (id) #t)
			; (var-exp (id) #f)
			; (lambda-exp (id body) #f)
			; (set!-exp (change to) #f)
			; (multi-lambda-exp (id body) #f)
			; (let-exp (vars vals body) #f)
			; (let*-exp (vars vals body) #f)
			; (letrec-exp (vars vals body) #f)
			; (if-else-exp (con true false) #f)
			; (if-exp-null (con true) #f)
			; (app-exp (rator rand) #f)
			; (lit-exp (item) #f)
			; )))
(define test-prim?
	(lambda (x)
		(prim-proc? (list x))))
	 
(define prim-proc?
	(lambda (sym)
		; (newline)
		; (newline)
		; (printf "\t\tprim-proc?:\t")
		; (display sym)
		; (newline)
		; (newline)
		(let loop ([sym sym]
				[prim-procs *prim-proc-names*])
			(cond [(null? prim-procs) #f]
				[(eqv? (car prim-procs) (car sym)) #t]
				[else (loop sym (cdr prim-procs))]))))
	 
;Checks the lambda.	
(define check-lam?
	(lambda (item)
		(or (symbol? item) (null? item) (pair? item) (list? item))))
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms list?)
		(vals (list-of scheme-value?))
		(env environment?)))