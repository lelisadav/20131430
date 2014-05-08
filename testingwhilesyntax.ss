(define-syntax while
	(syntax-rules ()
		[(_ test b1 b2 ...) 
			(letrec ([_*temp*_ (lambda () (if test (begin b1 b2 ... (_*temp*_))))])
				(_*temp*_))]))