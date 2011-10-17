(system::%putd 'hook (function hook
	(lambda (expander form env) 
		(funcall expander form))))

(setq *macroexpand-hook* #'hook)

(defmacro nil! (var)
	(list 'setq var nil))

(setq a 85)
(nil! a)
a

(defmacro make-list (a) (list a))
(macroexpand-1 '(make-list b))


(macro-function 'symbol) ;; this function is what defmacro creates
=> #'(lambda (form env) ...)

(funcall *macroexpand-hook* (macro-function 'symbol) macro-form env)	
	; returns a function that when evaluated returns a	
	
(system::%putd 'defmacro 
	(function defmacro (lambda (name lambdalist form)
		(setf (symbol-function name) 
			; this is the macro function
			#'(lambda (form env)
				;; this is the expansion function, it must
				;; it will be invoked by the *macroexpansion-hook*
				(eval form env))))))
				
(defun defmacro (name lambdalist form)
	(setf (symbol-function name) 
		; this is the macro function
		#'(lambda (form)
			;; this is the expansion function, it must
			;; it will be invoked by the *macroexpansion-hook*
			(eval form))))


(defmacro nil! (var)
	(list 'setq var nil))