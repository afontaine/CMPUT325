(defun interp (E P)
  (cond
	((atom E) E)
	(t
	  (let ( (f (car E)) (arg (cdr E)))
		(cond
		  ; Built-in functions with one argument
		  ((eq f 'first) (car (interp (car arg) P)))
		  ((eq f 'rest) (cdr (interp (car arg) P)))
		  ((eq f 'null) (null (interp (car arg) P)))
		  ((eq f 'atom) (atom (interp (car arg) P)))
		  ((eq f 'number) (numberp (interp (car arg) P)))
		  ((eq f 'not) (not (interp (car arg) P)))
		  ; Built-in functions with two arguments
		  ((eq f 'eq) (eq (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f 'cons) (cons (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f 'equal) (equal (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f '+) (+ (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f '-) (- (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f '*) (* (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f '>) (> (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f '<) (< (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f '=) (= (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f 'and) (and (interp (car arg) P) (interp (cadr arg) P)))
		  ((eq f 'or) (or (interp (car arg) P) (interp (cadr arg) P)))
		  ; Built-in functions with three arguments
		  ((eq f 'if) (if (interp (car arg) P) (interp (cadr arg) P)
						(interp (caddr arg) P)))
		  (T E))))))
