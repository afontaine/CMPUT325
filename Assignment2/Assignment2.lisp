(defun interp (E P)
  (interp-closure E
				  (mapcar (lambda (f)
							(list (car f) (get-args (cdr f))
								  (get-func f))) P)))

(defun interp-closure (E C)
  (cond
	((atom E)
	 (cond
	   ((contains-function E C) (find-args E C))
	   (T E)))
	(t
	  (let ((f (car E)) (arg (cdr E)))
		(cond
		  ; Built-in functions with one argument
		  ((eq f 'first)
		   (car (interp-closure (car arg) C)))
		  ((eq f 'rest)
		   (cdr (interp-closure (car arg) C)))
		  ((eq f 'null)
		   (null (interp-closure (car arg) C)))
		  ((eq f 'atom)
		   (atom (interp-closure (car arg) C)))
		  ((eq f 'number)
		   (numberp (interp-closure (car arg) C)))
		  ((eq f 'not)
		   (not (interp-closure (car arg) C)))
		  ; Built-in functions with two arguments
		  ((eq f 'eq)
		   (eq (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f 'cons)
		   (cons (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f 'equal)
		   (equal (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f '+)
		   (+ (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f '-)
		   (- (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f '*)
		   (* (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f '>)
		   (> (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f '<)
		   (< (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f '=)
		   (= (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f 'and)
		   (and (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ((eq f 'or)
		   (or (interp-closure (car arg) C) (interp-closure (cadr arg) C)))
		  ; Built-in functions with three arguments
		  ((eq f 'if)
		   (if (interp-closure (car arg) C) (interp-closure (cadr arg) C)
			 (interp-closure (caddr arg) C)))
		  ((contains-function f C)
		   (interp-closure (find-func f C)
						   (append (map-args (find-args f C) arg C) C)))
		  (T E))))))

(defun get-args (f)
  (cond
	((eq (car f) '=) ())
	(T (cons (car f) (get-args (cdr f))))))

(defun get-func (f)
  (cond
	((null f) ())
	((eq (car f) '=) (cadr f))
	(T (get-func (cdr f)))))

(defun contains-function (x L)
  (cond
	((null L) nil)
	((eq x (caar L)) T)
	(T (contains-function x (cdr L)))))

(defun find-args (f L)
  (cond
	((null L) nil)
	((eq f (caar L)) (cadar L))
	(T (find-args f (cdr L)))))

(defun find-func (f L)
  (cond
	((null L) nil)
	((eq f (caar L)) (caddar L))
	(T (find-func f (cdr L)))))

(defun map-args (A V C)
  (cond
	((null A) nil)
	(T (cons (list (car A) (interp-closure (car V) C))
			 (map-args (cdr A) (cdr V) C)))))
