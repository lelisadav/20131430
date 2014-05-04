; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
	;(printf "datatypes.ss loaded properly.\n")
    (load "parse.ss")
	;(printf "parse.ss loaded properly.\n")
	(load "primitives.ss")
	;(printf "primitives.ss loaded properly.\n")
	(load "interpreter.ss")
	;(printf "interpreter.ss loaded properly.\n")
    (load "env.ss")
	;(printf "env.ss loaded properly.\n")
	
	
	
	))

(load-all)

(define l load-all) ; even easier!
