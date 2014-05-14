; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
			
(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (list->vector vals) env)))
		
(define empty-env
	(lambda ()	
		(empty-env-record)))
		
(define top-level-eval
  (lambda (form)
	(let ([x (eval-exp form (empty-env))])
    ; later we may add things that are not expressions.
		(set! global-env init-env)
		x)))
			
(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * quotient / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display newline
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr map apply append list-tail eqv?))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))
			
(define global-env 
	init-env)

(define new-env
	(lambda (env)
		env))
; (define extend-env
	; (lambda (syms vals env)
		; (extended-env-record syms vals env)))

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
		 
(define extend-env-recursively 
	(lambda (vars idss vals old-env)
		(let ([len (length vars)])
			(let ([vec (make-vector len)])
				(let ([env (extended-env-record vars vec old-env)])
					(for-each
						(lambda (pos ids body)
							(vector-set! vec pos (lambda-proc-with-env ids (list body) env)))
						
						(iotass len 0) idss vals) env)))))
				
(define iotass
	(lambda (pos count)
		(if (equal? (+ 1 count) pos)
			(list count)
			(cons count (iotass pos (+ 1 count))))))

(define apply-env
	(lambda (env sym succeed fail) 
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (vector-ref vals pos))
						(apply-env env sym succeed fail)))))))

