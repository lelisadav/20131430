
;; Parsed expression datatypes

;Expression types.
;Based on the simple expression grammar, EoPL-2 p6
(define-datatype expression expression?  
	[var-exp
		(id symbol?)]
	[lambda-exp
		(id pair?)
		(body (list-of? expression-o?))]
	(thunk-exp
		(id null?)
		(body (list-of? expression?)))
	(multi-lambda-exp
		(id symbol?)
		(body (list-of? expression?)))
	(app-exp
		(rator expression-o?)
		(rand (list-of? expression-o?)))
	(if-exp-null
		(condition expression?)
		(truebody expression?))
	(if-else-exp
		(condition expression?)
		(truebody expression?)
		(falsebody expression?))
	(namedlet-exp
		(name symbol?)
		(vars (list-of? symbol?))
		(vals (list-of? expression?))
		(body (list-of? expression?)))
	(let-exp
		(vars (list-of? symbol?))
		(vals (list-of? expression?))
		(body (list-of? expression?)))
	[let*-exp 
		(vars (list-of? symbol?))
		(vals (list-of? expression?))
		(body (list-of? expression?))]
	[letrec-exp 
		(vars (list-of? symbol?))
		(vals (list-of? expression?))
		(body (list-of? expression?))]
	[set!-exp
		(var symbol?)
		(expr expression?)]
	[lit-exp
		(id literal?)])

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

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
	 

	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
	(empty-env-record)
	(extended-env-record
		(syms list?)
		(vals (list-of scheme-value?))
		(env environment?)))