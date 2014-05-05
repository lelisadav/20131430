


;; Parsed expression datatypes

;Expression types.
;Based on the simple expression grammar, EoPL-2 p6
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
	(var-exp
		(id symbol?))
	(lambda-exp
		(id check-lam?)
		(body (list-of expression-o?)))
	(set!-exp
		(change expression?)
		(to expression?))
	(multi-lambda-exp
		(id check-lam?)
		(body (list-of expression?)))
	; (namedlet-exp
		; (name symbol?)
		; (id (list-of? list?))
		; (body (list-of? expression)))
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

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define list-of? 
	(lambda (pred) 
		(lambda (ls)
	; (printf "list-of?\n\n")
	; (display ls)
	(cond
	[(not(list? ls)) (pred? ls)]
	[else
	(or(andmap pred ls) (pred ls))]))))
(define expression-o?
	(lambda (v)
		(or (expression? v) (proc-val? v))))

(define-datatype proc-val proc-val?
	[prim-proc
		(name test-prim?)]
	[lambda-proc
		(exp expression-o?)])
	 
(define test-prim?
	(lambda (x)
		(prim-proc? (list x))))
	 
(define prim-proc?
	(lambda (sym)
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