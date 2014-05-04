; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

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
						(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
							"variable not found in environment: ~s")))))]
		[let-exp (vars exp bodies)
			(let ([new-env 
					(extend-env vars 
						(map (lambda (x) (eval-exp x env)) exp) env)])
					(let loop ([bodies bodies])
						(if (null? (cdr bodies))
							(eval-exp (car bodies) new-env)
							(begin (eval-exp (car bodies) new-env)
								(loop (cdr bodies))))))]
		[if-else-exp (test-exp then-exp else-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env)
				(eval-exp else-exp env))]
		[if-exp-null (test-exp then-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env))]
		[lambda-exp (params body)
			(eval-exp (car body) env)]
		[multi-lambda-exp (param bodies)
			(lambda params
				(let loop ([bodies bodies])
					(if (null? (cdr bodies))
						(eval-exp (car bodies) new-env)
						(begin (eval-exp (car bodies) new-env)
							(loop (cdr bodies))))))]
		[app-exp (rator rands) 
			(let ([proc-value rator]
					[args (eval-rands rands env)])
				(apply-proc proc-value args env))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args env)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc (la) (apply-lambda la args env)]
			; You will add other cases
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
(define apply-lambda
	(lambda (exp args env)
		(eval-exp exp
			(extend-env 
				(cadr exp)
				args env))))

(define *prim-proc-names* '(+ - * add1 sub1 cons =))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

; +, -, *, /, add1, sub1, zero?, not, = and or < <= >= >
; cons, car, cdr, list, null?,
; assq, eq?, equal?, atom?, length, list->vector, list?, pair?, procedure?, 
; vector->list, vector, make-vector, vector-ref, vector?, number?, symbol?, set-car! , 
; set-cdr!, vector-set! , display , newline
; Add the c**r and c***r 
; procedures (where each "*" stands for an "a" or "d"). 
(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
		    [(+) (apply-all + + args 0)]
		    [(-) (apply-all - + args 0)]
		    [(add1) (+ (car args) 1)]
		    [(sub1) (- (car args) 1)]
		    [(cons) (cons (car args) (cadr args))]
		    [(=) (apply-all = = args #t)]
			[(*) (apply-all * * args 1)]
			[(/) (apply-all / / args 1)]
			[(zero?) (eq? (car args) 0)]
			[(not) (apply-all not not args #f)]
			[(and) (apply-all and and args '())]
			[(or) (apply-all or or args '())]
			[(<) (< (car args) (cadr args))]
			[(>) (> (car args) (cadr args))]
			[(<=) (<= (car args) (cadr args))]
			[(>=) (>= (car args) (cadr args))]
			[(car) (car (car args))]
			[(caar) (caar (car args))]
			[(caaar) (caaar (car args))]
			[(caaaar) (caaaar (car args))]
			[(
		    [else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-op)])))
				
(define apply-all
	(lambda (proc1 proc2 args null-value)
			(if (null? args) 
				null-value
				(proc1 (car args) (apply-all proc2 proc1 (cdr args) null-value)))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (parse-exp x))))










