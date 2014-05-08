; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
	(load "primitives.ss")
	(load "env.ss")
	(load "interpreter.ss")
    (load "A15-test-code.ss")
	(r)
	(load "A16-test-code.ss")
	(r)
	))


	
(load-all)

(define l load-all) ; even easier!
