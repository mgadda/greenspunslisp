(system::%putd 'hook (function hook
	(lambda (expander form env) 
		(funcall expander form))))

(setq *macroexpand-hook* #'hook)
