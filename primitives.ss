;; This is the file for all our primitive procedures.

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
		    [(+) (apply-all + args 0)]
		    [(-) (apply-switch - + args 0)]
		    [(add1) (+ (car args) 1)]
		    [(sub1) (- (car args) 1)]
		    [(cons) (cons (car args) (cadr args))]
		    [(=) (apply-all = args #t)]
			[(*) (apply-all * args 1)]
			[(/) (apply-all / args 1)]
			
			;; All Prims break after this point, after extensive testing. 
			;; You cannot use the actual procedure itself to do these.
			;; It wil learn as a zero for this assignment.
			;; All need to be re-done.
			[(zero?) (zero-check? (car args))]
			[(not) (apply-all not not args #f)]
			[(and) (apply-all and and args #t)]
			[(or) (apply-all or or args #f)]
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
				prim-proc)])))
				
;For procedures like '-' which much switch between plus and minus.
(define apply-switch
	(lambda (proc1 proc2 args null-value)
			(if (null? args) 
				null-value
				(proc1 (car args) 
					(apply-switch proc2 proc1 (cdr args) null-value)))))
				
(define apply-all 
	(lambda (proc args null-value)
		(if (null? args)
			null-value
			(proc (car args) 
				(apply-all proc (cdr args) null-value)))))
				
(define zero-check?
	(lambda (v)
		(equal? v 0)))