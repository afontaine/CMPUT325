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
