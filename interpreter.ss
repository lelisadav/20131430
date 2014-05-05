; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 
(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms vals env)))
(define empty-env
	(lambda ()
		(empty-env-record)))
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
							"variable not found in environment: ~s" id)))))]
		[let-exp (vars exp bodies)
			(let ([new-env 
					(extend-env vars 
						(map (lambda (x) 
							(if (and (list? x) (proc-val? x))
								(lambda-proc 
									(eval-exp (cadr x)
										(strike-from-env 
											(cadr (cadr x))
											env 0)))
								(eval-exp x env)))
							exp) env)])
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
			(last (map (lambda (x) 
				(if (expression? x)
					(eval-exp x env)
					x))
				body))]
		[multi-lambda-exp (param bodies)
			(lambda params
				(let loop ([bodies bodies])
					(if (null? (cdr bodies))
						(eval-exp (car bodies) new-env)
						(begin (eval-exp (car bodies) new-env)
							(loop (cdr bodies))))))]
		[app-exp (rator rands) 
			(let ([proc-value rator]
					[args 
						(if (andmap expression? rands)
							(eval-rands rands env)
							rands)])
				(if (proc-val? proc-value)
					(apply-proc proc-value args env)
					(apply-proc (eval-exp proc-value env) args env)))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;Gets the last element in a list.
(define last 
	(lambda (ls)
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env))
		rands)))

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

(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))

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
		;(display x)
		;(newline)
	(top-level-eval (parse-exp x))))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3


		
(define global-env 
	init-env)


(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
	(lambda (env sym succeed fail) 
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env env sym succeed fail)))))))












