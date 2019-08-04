


	(defun bfs (state)
	(reverse (bfs1 (list (list state)))))


	(defun bfs1 (q)
	(cond
	((null q) nil)
	((equal (caar q) '(f g)) (car q))
	(t (let ((child (mtransit (valid(caar q)))))
	(bfs1
	(append
	(cdr q)
	(mapcar #'(lambda (state)
	(cons state (car q)))
	child)))))))







	(defun transit (s) 	
		(cond ((= (length s) 3)
		(append (list (cons (car s) (cons (cadr s) ())))
		       (list (cons (car s) (cons (caddr s) ())))
			(list (cons (cadr s) (cons (caddr s) ()))))
			)
		((= (length s) 2) 
		(list (list (car s))
		      	(list (cadr s))))
		(t '()))
	)

	(defun mtransit (s)
		(cond ((iso s 'f) (transit (remov s 'f)))
		((= (length s) 1) (ins s ))  
		(t (list (cons 'f s))))
	)

	(defun ins (s)
		(cond ((equal (car s) 'w) (list (list 'f 'g 'w) (list 'f 'c 'w) (list 'f 'w)))
		((equal (car s) 'g) (list (list 'f 'w 'g) (list 'f 'c 'g)(list 'f 'g)))
		(t(list (list 'f 'g 'c) (list 'f 'w 'c)(list 'f 'c)))
		)
	)

	(defun iso (l e)
	(cond ((null l) nil)
	((equal (car l) e) t)
	(t (iso (cdr l) e))))


		                                                                                                                                                                                   
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
	(defun farmerwolf   (state)
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
	(defun farmecabbage  (state)
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
		(print "----FARMER(F) farmerwolf  (W) GOAT(G) CABBAGE(C) PROBLEM----")
		(terpri)
		(append (bfs '(f g w c)) '(()))
	)
