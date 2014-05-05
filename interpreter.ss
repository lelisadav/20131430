; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 

(define extend-env
	(lambda (syms vals env)
		;(printf "extend-env\n")
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
  ;(printf "eval-exp\n")
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
											env)))
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
						(if (and (list? rands) (andmap expression? rands))
							(eval-rands rands env)
							rands)])
				(if (proc-val? proc-value)
					(apply-proc proc-value args env)
					(apply-proc (eval-exp proc-value env) args env)))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;Gets the last element in a list.
(define last 
	(lambda (ls)
	;(printf "last\n")
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
  	;(printf "eval-rands\n")
    (map (lambda (x) (eval-exp x env))
		rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args env)
		;(printf "apply-proc\n")
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc (la) (apply-lambda la args env)]
			; You will add other cases
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
(define apply-lambda
	(lambda (exp args env)
		;(printf "apply-lambda\t\t")
		(display exp)
		(newline)
		(eval-exp exp
			(if (or (symbol? (cadr exp)) (not (list? (cadr exp))))
					(with-lists (cadr exp) args env)
					(extend-env 
						(cadr exp)
						args env)))))
						
(define with-lists 
	(lambda (vars args env)
		;(printf "with-lists\n")
		(cond [(symbol? vars) 
				(extend-env (list vars) 
					(list args) env)]
			[(not (list? vars)) 
				(let* ([x-vars (get-nice-vars vars)]
						[x-args (find-correct-args args (get-list-placement vars 0) 0)])
					(extend-env x-vars x-args env))])))
					
(define get-nice-vars
	(lambda (nls)
		;(printf "get-nice-vars\n")
		(cond [(not (pair? nls)) (cons nls '())]
			[else (cons (car nls) (get-nice-vars (cdr nls)))])))
			
(define get-list-placement 
	(lambda (vars count)
		;(printf "get-list-placement\n")
		(cond [(not (pair? vars)) count]
			[else (get-list-placement (cdr vars) (+ 1 count))])))
			
(define find-correct-args
	(lambda (args place count)
		;(printf "find-correct-args\n")
		(cond [(equal? count place) 
				(list args)]
			[else (cons (car args) (find-correct-args (cdr args) place (+ 1 count)))])))

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
		; (printf "\t")
		; (display x)
		; (newline)
		; (newline)
		
	(top-level-eval (parse-exp x))))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3


		
(define strike-from-env
	(lambda (vars env)
		;(printf "strike-from-env\n")
		(let* ([ls-co (map (lambda (x) (find-front x (cadr env) 0)) vars)]
				[sort-ls (sort (car ls-co) (cdr ls-co))])
			(if (null? sort-ls)
				env
				((car env) (strike ls-co (cadr env) 0) (strike ls-co (caddr env) 0))))))
		
(define find-front
	(lambda (var env count)
		;(printf "find-front\n")
		(if (null? env)
			-1
			(if (equal? var (car env))
				count
				(find-front var (cdr env) (+ 1 count))))))
			
(define strike
	(lambda (ls env count)
		;(printf "strike\n")
		(cond [(null? ls) env]
			[(equal? count (car ls)) 
				(strike (cdr ls) (cdr env) (+ 1 count))]
			[else (cons (car env) (strike ls (cdr env) (+ 1 count)))])))
			
(define sort
	(lambda (ls)
		;(printf "sort\n")
		(if (null? ls) 
			'()
			(let ([smallest (find-smallest (car ls) (cdr ls))])
				(if (equal? smallest -1)
					(sort (cdr ls))
					(cons smallest (sort (cdr ls))))))))
				
(define find-smallest
	(lambda (cur ls)
		;(printf "find-smallest\n")
		(cond [(null? ls) cur]
			[(> cur (car ls)) (find-smallest (car ls) (cdr ls))]
			[else (find-smallest cur (cdr ls))])))
			
(define global-env 
	init-env)

; (define extend-env
	; (lambda (syms vals env)
		; (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
  	;(printf "list-find-position\t\t")
	(display sym)
	(newline)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
  	; (printf "list-index\t\t")
	; (display pred)
	; (newline)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
	(lambda (env sym succeed fail) 
		;(printf "apply-env\n")
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env env sym succeed fail)))))))


		










