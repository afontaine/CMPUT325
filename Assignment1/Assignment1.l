;QUESTION 1
;This method takes a list and returns a list of pairs in reverse order. If there
;is an odd number of list items, the last item in the new list is paired with
;itself
(defun form-pair (L)
  (if (null (cdr L))
    (list (list (car L) (car L)))
    (if (null (cddr L))
      (list (list (cadr L) (car L)))
      (append (list (list (cadr L) (car L))) (form-pair (cddr L))))))

;QUESTION 2
;This method takes a list of pairs and returns the list with all expressions
;evaluated. If the pair contains the same value, the pair is dropped from the
;list
(defun drop-pair (L)
  (if (null (cdr L))
	(if (not (eq-pair (eval-list (car L))))
	  (list (eval-list (car L))))
	(if (not (eq-pair (eval-list (car L))))
			(cons (eval-list (car L))
		  (drop-pair (cdr L)))
			(drop-pair (cdr L)))))

(defun eval-list (L)
  (if (atom (car L))
	(if (not (numberp (car L)))
	  (apply (car L) (cdr L))
	  (cons (car L) (if (null (cdr L)) () (eval-list (cdr L)))))
	(cons (eval-list (car L)) (if (null (cdr L))
							()
							(eval-list (cdr L))))))

(defun eq-pair (L)
  (eq (car L) (cadr L)))

;QUESTION 3
;Takes a list of atoms and removes repeated atoms from the list.
(defun remove-duplicate (L)
  (if (null (cdr L))
	L
	(if (contains (car L) (cdr L))
	  (remove-duplicate (cdr L))
	  (cons (car L) (remove-duplicate (cdr L))))))

(defun contains (a L)
  (if (null (cdr L))
	(eq a (car L))
	(if (eq a (car L))
	  (eq a (car L))
	  (contains a (cdr L)))))
