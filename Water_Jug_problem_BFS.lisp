(defun transit (state)
    (remove-null
	(list
	    (fill-first state)
	    (fill-second state)
	    (pour-first-second state)
	    (pour-second-first state)
	    (empty-first state)
	    (empty-second state)
	)
     )
)


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

(defun bfs (state)
(reverse (bfs1 (list (list state)))))

(defun bfs1 (q)
(cond
((null q) nil)
((and (= input 1) (= (caaar q) required)) (car q))
((and (= input 2) (= (cadaar q) required) (car q)))
(t (let ((child (transit (caar q))))
(bfs1
(append
(cdr q)
(mapcar #'(lambda (state)
(cons state (car q)))
child)))))))



(defun init ()
(print "-------------------------WATER JUG PROBLEM-------------------------")
(print "INPUT (Quantity1 Quantity2 Required_quantity Required_output_jug)")
(terpri)
(setq first (read) second (read) required (read) input (read))
(bfs '(0 0))
)

