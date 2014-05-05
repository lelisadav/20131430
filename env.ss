; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

; (define empty-env
	; (lambda ()
		; (empty-env-record)))
		
(define strike-from-env
	(lambda (vars env)
		(let* ([ls-co (map (lambda (x) (find-front x (cadr env) 0)) vars)]
				[sort-ls (sort (car ls-co) (cdr ls-co))])
			(if (null? sort-ls)
				env
				((car env) (strike ls-co (cadr env) 0) (strike ls-co (caddr env) 0))))))
		
(define find-front
	(lambda (var env count)
		(if (null? env)
			-1
			(if (equal? var (car env))
				count
				(find-front var (cdr env) (+ 1 count))))))
			
(define strike
	(lambda (ls env count)
		(cond [(null? ls) env]
			[(equal? count (car ls)) 
				(strike (cdr ls) (cdr env) (+ 1 count))]
			[else (cons (car env) (strike ls (cdr env) (+ 1 count)))])))
			
(define sort
	(lambda (ls)
		(if (null? ls) 
			'()
			(let ([smallest (find-smallest (car ls) (cdr ls))])
				(if (equal? smallest -1)
					(sort (cdr ls))
					(cons smallest (sort (cdr ls))))))))
				
(define find-smallest
	(lambda (cur ls)
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

