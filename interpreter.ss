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
;

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
			[(list) (apply-all list append args '())]
			[(null?) (car args)]
			[(assq) (car args) (cadr args)]
			[(eq?) (eq? (car args) (cadr args))]
			[(equal?) (equal? (car args) (cadr args))]
			[(atom?) (atom? (car args))]
			[(length) (length (car args))]
			[(list->vector) (list->vector (car args))]
			[(list?) (list? (car args))]
			[(pair?) (pair? (car args))]
			[(procedure?) (procedure? (car args))]
			[(vector->list) (vector->list (car args))]
			[(vector) (apply-all vector append args '())]
			[(make-vector) (if (length (= 2 (length args))) 
				(make-vector (car args) (cadr args)) (make-vector (car args)))]
			[(vector-ref) (vector-ref (car args) (cadr args))]
			[(vector?) (vector? (car args))]
			[(number?) (number? (car args))]
			[(symbol?) (symbol? (car args))]
			[(set-car!) (set-car! (car args) (cadr args))]
			[(set-cdr!) (set-cdr! (car args) (cadr args))]
			[(set-car!) (set-car! (car args) (cadr args))]
			[(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
			[(display) (display (car args))]
			[(mdisplay) (apply-all display display args newline)]
			[(newline) (newline)]
			[(caaaar)	(caaaar (car args))]
			[(caaadr)	(caaadr (car args))]
			[(caaar)	(caaar (car args))]
			[(caadar)	(caadar (car args))]
			[(caaddr)	(caaddr (car args))]
			[(caadr)	(caadr (car args))]
			[(caar)	(caar (car args))]
			[(cadaar)	(cadaar (car args))]
			[(cadadr)	(cadadr (car args))]
			[(cadar)	(cadar (car args))]
			[(caddar)	(caddar (car args))]
			[(cadddr)	(cadddr (car args))]
			[(caddr)	(caddr (car args))]
			[(cadr)	(cadr (car args))]
			[(car)	(car (car args))]
			[(cdaaar)	(cdaaar (car args))]
			[(cdaadr)	(cdaadr (car args))]
			[(cdaar)	(cdaar (car args))]
			[(cdadar)	(cdadar (car args))]
			[(cdaddr)	(cdaddr (car args))]
			[(cdadr)	(cdadr (car args))]
			[(cdar)	(cdar (car args))]
			[(cddaar)	(cddaar (car args))]
			[(cddadr)	(cddadr (car args))]
			[(cddar)	(cddar (car args))]
			[(cdddar)	(cdddar (car args))]
			[(cddddr)	(cddddr (car args))]
			[(cdddr)	(cdddr (car args))]
			[(cddr)	(cddr (car args))]
			[(cdr)	(cdr (car args))]
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










