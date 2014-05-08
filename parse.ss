;Rose Reatherford, Assignment 3
;Problem #2

;Sees if the item is a literal.
(define lit?
	(lambda (x)
		(cond
		[(quoted? x)  #t]
		[(list? x) #f]
		[(pair? x) (quoted? x)] 
		[(number? x)  #t]
		[(string? x) #t]
		[(boolean? x)  #t]
		[(char? x)  #t]
		
		[(vector? x)  #t]
		[else #f]
		)))
		
;Sees if the item is quoted.
(define quoted? 
  (lambda (exp)
    (and (pair? exp) (eq? (car exp) 'quote))))		

(define parse-exp
	(lambda (datum . childoflist)
		(cond
			[(symbol? datum)
				(var-exp datum)]
			[(lit? datum) (lit-exp datum)]
			[(not (list? datum)) 
				(eopl:error 'parse-exp
					"Error in parse-exp: application ~s is not a proper list" datum)]
			[(pair? datum)
				(cond [(eqv? (car datum) 'set!)
						(if (check-set? (cdr datum))
							(set!-exp (parse-exp (cadr datum) ) (parse-exp (caddr datum) ))
							(eopl:error 'parse-exp 
								"Error in parse-exp: set! expression: ~s" datum))]
					[(eqv? (car datum) 'lambda) 
						(if (check-valid-arg? (cadr datum))
							(lambda-exp (cadr datum) (map parse-exp (cddr datum)))
							(eopl:error 'parse-exp 
								"Error in parse-exp: lambda expression: ~s" datum))]
					[(eqv? (car datum) 'let)
							(cond
								[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'let datum)]
								[(list? (cadr datum))
									(cond
									[(not (andmap list? (cadr datum))) (eopl:error 'parse-exp
										"declarations in ~s-expression not a proper list ~s" 'let datum)]
									[(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
										(eopl:error 'parse-exp
											"declaration in ~s-exp must be a list of length 2 ~s" 'let datum)]
									[(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
										(eopl:error 'parse-exp
											"vars in ~s-exp must be symbols ~s" 'let datum)]
									[else
										(let* (
										[varvals (cadr datum)]
										[body (cddr datum)]
										[splitls (split varvals)]
										[vars (car splitls)]
										[vals (cadr splitls)]
										)
									(let-exp vars (map parse-exp vals) (map parse-exp body )))])]
								[else 
									(eopl:error 'parse-exp
									"declarations in ~s-expression not a list ~s" 'let datum)])]
					[(eqv? (car datum) 'let*)
						(cond
							[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'let* datum)]
							[(not(list? (cadr datum)))
								(eopl:error 'parse-exp
								"declarations in ~s-expression not a list ~s" 'let* datum)]
							[(not (andmap list? (cadr datum))) (eopl:error 'parse-exp
								"declarations in ~s-expression not a proper list ~s" 'let* datum)]
							[(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
								(eopl:error 'parse-exp
								"declaration in ~s-exp must be a list of length 2 ~s" 'let* datum)]
							[(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
								(eopl:error 'parse-exp
								"vars in ~s-exp must be symbols ~s" 'let* datum)]
							[else
								(let* (
									[varvals (cadr datum)]
									[body (cddr datum)]
									[splitls (split varvals)]
									[vars (car splitls)]
									[vals (cadr splitls)])
									(let*-exp vars (map parse-exp vals ) (map parse-exp body )))])]
					[(eqv? (car datum) 'letrec)
						(cond
							[(= 1 (length(cdr datum))) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" 'letrec datum)]
							[(not (list? (cadr datum)))
								(eopl:error 'parse-exp
									"declarations in ~s-expression not a list ~s" 'letrec datum)]
							[(not (andmap list? (cadr datum))) (eopl:error 'parse-exp
								"declarations in ~s-expression not a proper list ~s" 'letrec datum)]
							[(not (andmap (lambda (x) (eq? 2 (length x))) (cadr datum)))
								(eopl:error 'parse-exp
								"declaration in ~s-exp must be a list of length 2 ~s" 'letrec datum)]
							[(not (andmap (lambda (x) (symbol? (car x))) (cadr datum)))
								(eopl:error 'parse-exp
								"vars in ~s-exp must be symbols ~s" 'letrec datum)]
							[else
								(let* (
									[varvals (cadr datum)]
									[body (cddr datum)]
									[splitls (split varvals)]
									[vars (car splitls)]
									[vals (cadr splitls)])
									(letrec-exp vars (map parse-exp vals ) (map parse-exp body )))])]
					[(eqv? (car datum) 'if)
						(if (check-if? datum)
							(if (null? (cdddr datum)) 
								(if-exp-null (parse-exp (cadr datum) ) (parse-exp (caddr datum) ))
								(if-else-exp (parse-exp (cadr datum) ) (parse-exp (caddr datum) ) (parse-exp (cadddr datum) )))
							(eopl:error 'parse-exp
								"Error in parse-exp: if expression: ~s" datum))]
					[else (app-exp
						(parse-exp (car datum))
						(map parse-exp (cdr datum) 
						; (make-list (length (cdr datum)) #t)
						))])]
			[else (eopl:error 'parse-exp
				"Invalid concrete syntax ~s" datum)])))

(define unparse-exp ; an inverse for parse-exp
  (lambda (exp)
    (cases expression exp
      (var-exp (id) id)
      (lambda-exp (id body) 
        (append (list 'lambda id)
          (map unparse-exp body)))
		(multi-lambda-exp (id body)
			(append (list 'lambda id)
			(map unparse-exp body)))
		(let-exp (vars vals body)
				(let* ([unparsevals (map unparse-exp vals)]
				[merged (map (lambda (x y) (list x y)) vars unparsevals)])
				(append (list 'let merged) 
							(map unparse-exp body)))
			)
		(let*-exp (vars vals body)
			(let* ([unparsevals (map unparse-exp vals)]
			[merged (map (lambda (x y) (list x y)) vars unparsevals)])
				(append (list 'let* merged) 
					(map unparse-exp body)))
		)
		(letrec-exp (vars vals body)
			(let* ([unparsevals (map unparse-exp vals)]
			[merged (map (lambda (x y) (list x y)) vars unparsevals)])
				(append (list 'letrec merged) 
					(map unparse-exp body)))
			)
		(app-exp (rator rand)
			(append (list (unparse-exp rator))
			(map unparse-exp rand)))
		(set!-exp (change to)
			(list 'set! (unparse-exp change) 
			(unparse-exp to)))
		(if-else-exp (boolean if-true if-false)
			(list 'if (unparse-exp boolean) (unparse-exp if-true) (unparse-exp if-false)))
		(if-exp-null (boolean if-true)
			(list 'if boolean if-true))
		(lit-exp (item)
			item))))

(define occurs-free? ; in parsed expression
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))
			
(define check-let?
	(lambda (datum)
		(cond [(null? datum) #t]
			[(not (list? datum)) #f]
			[(number? (car datum)) #f]
			[(symbol? (car datum)) (and (equal? (length datum) 2) (expression? (parse-exp (cadr datum))))]
			[else (and (check-let? (cdr datum)) (check-let? (car datum)))])))


		
(define check-valid-arg?
	(lambda (item)
		(cond [(null? item) #t]
			[(symbol? item) #t]
			[(pair? item) #t]
			[(not (list? item)) #f]
			[else (and (check-valid-arg? (car item)) (check-valid-arg? (cdr item)))])))
			
(define check-if?
	(lambda (datum)
	;	(printf "check-if?\n")
		(cond [(null? (cddr datum)) #f]
			[else #t])))
			
(define check-set?
	(lambda (datum)
	;	(printf "check-set?\n")
		(equal? (length datum) 2)))
(define split 
    (lambda (ls)
	;	(printf "split\n")
      (list (map (lambda (x) (car x)) ls) (map (lambda (y) (cadr y)) ls))))
; (define merge 
	; (lambda (ls1 ls2)
	 ; (map (lambda (x y) (list x y)) ls1 ls2)))