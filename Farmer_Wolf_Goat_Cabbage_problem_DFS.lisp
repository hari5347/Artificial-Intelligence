(defun dfs (state goal output)
	(cond 
		((null state) nil)
		((equal state goal) (reverse (cons state output)))
		((not (is-present output state))
		(or (dfs (farmer state) goal (cons state output))
		(dfs (farmerwolf state) goal (cons state output))
		(dfs (farmergoat state) goal (cons state output))
		(dfs (farmercabbage state) goal (cons state output))
		)
		)
	)
)                                                                                                                                                                                                   
(defun valid (state)
	(cond 
		((or (equal state '(g w)) (equal state '(w g)) (equal state '(g c)) (equal state '(c g)) 
		(equal state '(f c)) (equal state '(c f)) (equal state '(f w)) (equal state '(w f))) nil)
	(t 
		state
	)
	)
)
(defun farmer (state)
	(cond 
		((is state 'f) (remov state 'f))
	(t 
		(cons 'f state))
	)
)
(defun farmerwolf (state)
	(cond 
		((and (is state 'f) (is state 'w)) (valid (remov (remov state 'f) 'w)))
		((and (not (is state 'f)) (not (is state 'w)) ) (valid (cons 'w (cons 'f state) )))
	(t 
		nil
	)
	)
)
(defun farmergoat (state)
	(cond 
		((and (is state 'f) (is state 'g)) (valid (remov (remov state 'f) 'g)))
		((and (not (is state 'f)) (not (is state 'g)) ) (valid (cons 'g (cons 'f state) )))
	(t
		 nil
	)
	)
)
(defun farmercabbage (state)
	(cond 
		((and (is state 'f) (is state 'c)) (valid (remov (remov state 'f) 'c)))
		((and (not (is state 'f)) (not (is state 'c)) ) (valid (cons 'c (cons 'f state) )))
			
	(t 
		nil
	)
	)
)
(defun is (state element)
	(cond 
		((null state) nil)
		((equal (car state) element) t)
	(t 
		(is (cdr state) element)
	)
	)
)
 
(defun remov (state element)
	(cond 
		((null state) nil)
		((equal (car state) element) (remov (cdr state) element))
	(t 
		(cons (car state) (remov (cdr state) element))
	)
	)
) 
(defun is-present (list element)
	(cond 
		((or (null list) (null element)) nil)
	(t
		(cond 
			((equalto (car list) element) t)
		
		(t 
			(is-present (cdr list) element)
		)
		)
	)
	)
)
(defun equalto (list element)
	(cond 
		((null list) nil)
		((= (length list) (length element)) (equalto1 list element))
	(t 
		nil
	)
	)
)
(defun equalto1 (list element)
	(cond 
		((equal element nil) t)
		((is-present1 list (car element)) (equalto1 list (cdr element)))
	(t 
		nil
	)
	)
)
(defun is-present1 (list element)
	(cond 
		((null list) nil)
		((equal (car list) element) t)
	(t 
		(is-present1 (cdr list) element)
	)
	)
)
(defun FGWC()
	(print "----FARMER(F) WOLF(W) GOAT(G) CABBAGE(C) PROBLEM----")
	(terpri)
	(append (dfs '(f g w c) '(f g) '() ) '(()))
)
