; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

;(load "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/datatypes.ss")
    (load "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/parse.ss")
	(load "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/primitives.ss")
    (load "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/interpreter.ss")
	(load "C:/Users/daveyle/Desktop/CSSE304/Interpreter/20131430/env.ss")))

(load-all)

(define l load-all) ; even easier!
