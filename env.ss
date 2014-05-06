; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

; (define empty-env
	; (lambda ()
		; (empty-env-record)))
		
(define strike-from-env
	(lambda (var env)
		(strike-from-e var env)))
		
(define strike-from-e
	(lambda (var env)
		(cases environment env
			(empty-env-record () env)
			(extended-env-record (syms vals envi)
				(let ([pos (remove-not-number (map (lambda (x) (list-find-position x syms)) var))])
					(if (andmap number? pos)
						(extended-env-record 
							(strike pos syms 0) (strike pos vals 0)
							(strike-from-e var envi))
						(extended-env-record syms vals
							(extended-env-record syms vals
								(strike-from-e var envi)))))))))
				
(define strike
	(lambda (pos ls count)
		(cond [(null? ls) '()]
			[(ormap (lambda (x) (equal? x count)) pos)
				(strike pos (cdr ls) (+ 1 count))]
			[else (cons (car ls) (strike pos (cdr ls) (+ 1 count)))])))
			
(define remove-not-number
	(lambda (ls)
		(cond [(null? ls) '()]
			[(not (number? (car ls))) (remove-not-number (cdr ls))]
			[else (cons (car ls) (remove-not-number (cdr ls)))])))
			
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

