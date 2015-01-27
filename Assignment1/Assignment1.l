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

;Checks to see if a is somewhere within L. Used in questions 3 and 4.
(defun contains (a L)
  (if (null (cdr L))
  (equal a (car L))
  (if (equal a (car L))
    (equal a (car L))
    (contains a (cdr L)))))

;Question 4
;Count the number of unique atoms in the list by iterating over the list once.
(defun my-count (L)
  (if (null L)
  0
  (if (contains (car L) (cdr L))
    (my-count (cdr L))
    (+ 1 (my-count (cdr L))))))

;Question 5
;This takes in a list and returns the powerset of that list.
(defun power-set (L)
  (if (null L)
	(list (list))
	(remove-duplicate
	  (append (power-set (cdr L)) (list (list (car L)))
			 (combo (power-set (cdr L)) (car L))))))

;This function takes an atom X and appends it to every element in the list L
(defun combo (L X)
  (if (null L)
	(list)
	(cons (append (car L) (list X)) (combo (cdr L) X))))

