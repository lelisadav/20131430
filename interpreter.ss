; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
		[lit-exp (datum) 
			(if (and (pair? datum) (eqv? (car datum) 'quote))
				(cadr datum)
				datum)]
		[var-exp (id)
			(apply-env env id
				(lambda (x) x)
				(lambda () (apply-env global-env id
					(lambda (x) x) ;procedure to call if id is in the environment 
					(lambda () (display env) (begin (eopl:error 'apply-env ; procedure to call if id not in env
							"variable not found in environment: ~s" id) (newline) (display env))))))]
		[let-exp (vars exp bodies)
			(printf "I shouldn't be here, ever!")]
		[letrec-exp (vars idss vals body)
			(eval-exp body
				(extend-env-recursively vars idss vals env))]
		[lambda-exp (id body)
			(lambda-proc-with-env id body env)]
		[if-else-exp (test-exp then-exp else-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env)
				(eval-exp else-exp env))]
		[if-exp-null (test-exp then-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env))]
		[app-exp (rator rands) 
			(let* ([proc-value (eval-exp rator env)]
					[args (eval-rands rands env)])
				(apply-proc proc-value args))]
		[case-exp (var cases body)
			(printf "I should never be here!")]
		[set!-exp (id body)
			(set! global-env (change-env id (eval-exp body env) env))]
		[while-exp (test body)
			(if (eval-exp test env)
				(begin (loop-through body env)
					(eval-exp exp env)))]
				
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))
		

					
;Gets the last element in a list.
(define last 
	(lambda (ls)
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
			
(define loop-through
	(lambda (ls env)
		(if (null? (cdr ls)) 
			(eval-exp (car ls) env)
			(begin (eval-exp (car ls) env)
				(loop-through (cdr ls) env)))))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x)
	(eval-exp x env))
		rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc-with-env (id body envi) (apply-lambda id body args envi)]
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
;Evaluates the lambda.
(define apply-lambda
	(lambda (id body args env)
		(let ([envi 
			(if (or (symbol? id) (not (list? id)))
				(with-lists id args env)
				(extend-env 
					id
					args env))])
			(loop-through body envi))))
						
(define with-lists 
	(lambda (vars args env)
		(cond [(symbol? vars) 
				(extend-env (list vars) 
					(list args) env)]
			[(not (list? vars)) 
				(let* ([x-vars (get-nice-vars vars)]
						[x-args (find-correct-args args (get-list-placement vars 0) 0)])
					(extend-env x-vars x-args env))])))
					
(define get-nice-vars
	(lambda (nls)
		(cond [(not (pair? nls)) (cons nls '())]
			[else (cons (car nls) (get-nice-vars (cdr nls)))])))
			
(define get-list-placement 
	(lambda (vars count)
		(cond [(not (pair? vars)) count]
			[else (get-list-placement (cdr vars) (+ 1 count))])))
			
(define find-correct-args
	(lambda (args place count)
		(cond [(equal? count place) 
				(list args)]
			[else (cons (car args) (find-correct-args (cdr args) place (+ 1 count)))])))



(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) 
		(top-level-eval (syntax-expand (parse-exp x)))))

(define syntax-expand
	(lambda (datum)
		; (newline)
		; (display datum)
		; (newline)
		(cases expression datum
			[var-exp (id) (var-exp id)]
			[lit-exp (id) (lit-exp id)]
			[lambda-exp (id body) 
				(lambda-exp id
					(map syntax-expand body))]
			[let-exp (vars vals body)
				(app-exp (lambda-exp vars (map syntax-expand body)) (map syntax-expand vals))]
			[let*-exp (vars vals body)
				(if (null? vars)
					(map syntax-expand body)
					(app-exp 
						(lambda-exp (list (car vars)) 
							(syntax-expand 
								(let*-exp (cdr vars) (cdr vals) body))) 
						(list (car vals))))]
			[letrec-exp (vars idss vals body)
				(letrec-exp vars idss (map syntax-expand vals) (syntax-expand body))]
			[define-exp (name body)
				(lambda-exp (list name) (list (syntax-expand body)))]
			[cond-exp (tests vals)
				(cond [(and (null? tests) (not (null? vals)))
						(if-exp-null (parse-exp '(lambda () #t)) (syntax-expand (car vals)))]
					[(and (null? (cdr tests)) (not (null? (cdr vals))))
						(if-else-exp (syntax-expand (car tests)) (syntax-expand (car vals)) (syntax-expand (cadr vals)))]
					[(and (null? (cdr tests)) (null? (cdr vals)))
						(if-exp-null (syntax-expand (car tests)) (syntax-expand (car vals)))]
					[else 
						(if-else-exp (syntax-expand (car tests)) (syntax-expand (car vals))
							(syntax-expand (cond-exp (cdr tests) (cdr vals))))])]
			
			[begin-exp (items)
				(app-exp (lambda-exp '() 
					(map syntax-expand items)) '())]
			[app-exp (rator rands)
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[set!-exp (id body)
				(set!-exp id (syntax-expand body))]
			[if-else-exp (test success fail)
				(if-else-exp test 
					(syntax-expand success) (syntax-expand fail))]
			[if-exp-null (test success)
				(if-exp-null test (syntax-expand success))]
			[while-exp (test body)
				(while-exp (syntax-expand test) (map syntax-expand body))]
			[or-exp (body) 
				(if (null? body)
					(lit-exp '#f)
					(app-exp (lambda-exp (list 'v)
						(list (if-else-exp (var-exp 'v)
							(var-exp 'v)
							(syntax-expand (or-exp (cdr body)))))) (list (car body))))]
			[and-exp (body) 
				(if (null? body)
					(lit-exp '#t)
					(app-exp (lambda-exp (list 'v)
						(list (if-else-exp (var-exp 'v)
							(syntax-expand (and-exp (cdr body)))
							(var-exp 'v)))) (list (car body))))]
			[case-exp (var cases next)
				(syntax-expand (cond-exp (map (lambda (x) (change-to-or var x)) cases) (map syntax-expand next)))]
									
						
						; Error here with it not correctly going onto the next list of statements. Needs better if-exp-null
						; integration! Something like doing a cond exp of all the cond expressions? Or a multi-bodied lambda would probably work better
						; as long as it short-circuited. It will get the correct answer if it's odd! But nothing for anything else... XD
						)))
						
(define place-let-into
	(lambda (val vars vals)
		(app-exp (lambda-exp vars
			(list (syntax-expand val))) (map syntax-expand vals))))
			
		
(define change-to-or
	(lambda (var ls)
		(syntax-expand 
			(or-exp (map (lambda (x) (app-exp (var-exp 'equal?) (list var x))) ls)))))


		










