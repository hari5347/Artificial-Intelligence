(defun remove-null (x)
    (cond
	((null x) nil)
	((null (car x)) (remove-null (cdr x)))
	((cons (car x) (remove-null (cdr x))))
    )
)


(defun fill-first (state)
	(cond 
		((< (car state) first) (list first (cadr state)))
	)
)

(defun fill-second (state)
	(cond 
		((< (cadr state) second) (list (car state) second))
	)
)

(defun pour-first-second (state)
	(cond
		((= (car state) 0) nil)
		((= (cadr state) second) nil)
		((<= (+ (car state) (cadr state)) second) (list 0 (+ (car state) (cadr state))))
	(t	
		(list (- (+ (car state) (cadr state)) second) second))
	)
)

(defun pour-second-first (state)
	(cond
		((= (cadr state) 0) nil)
		((= (car state) first) nil)
		((<= (+ (car state) (cadr state)) first) (list (+ (car state) (cadr state)) 0))
	(t
		(list first (- (+ (car state) (cadr state)) first)))
	)
)

(defun empty-first (state)
	(cond
		((> (car state) 0) (list 0 (cadr state))))
)

(defun empty-second (state)
	(cond
		((> (cadr state) 0) (list (car state) 0))
	)
)

(defun dfs (state op)
	(cond 
                ((null state) nil)		
		((and (= input 1)(= (car state) required)) (reverse (cons state op)))
		((and (= input 2)(= (cadr state) required)) (reverse (cons state op)))		
		((not (is-present op state))
		(or  (dfs  (fill-first state) (cons state op))
		     (dfs  (fill-second state) (cons state op))
		     (dfs  (pour-first-second state) (cons state op))
		     (dfs  (pour-second-first state) (cons state op))
		     (dfs  (empty-first state) (cons state op))
		     (dfs  (empty-second state) (cons state op))
		)
		)
	)
)

(defun is-present (list element)
	(cond ((or (null list) (null element)) nil)
	(t
		(cond ((equal (car list) element) t)
		(t (is-present (cdr list) element))
		)
	)
	)
)

(defun init ()
(print "-------------------------WATER JUG PROBLEM-------------------------")
(print "INPUT (Quantity1 Quantity2 Required_quantity Required_output_jug)")
(terpri)
(setq first (read) second (read) required (read) input (read))
(dfs '(0 0) '())
)

