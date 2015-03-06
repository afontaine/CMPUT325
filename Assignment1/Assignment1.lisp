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
  (cond
  ((null L) nil)
    ((null (cdr L))
     (equal a (car L)))
    ((equal a (car L)) T)
    (T (contains a (cdr L)))))

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
    (append (power-set (cdr L))
       (combo (power-set (cdr L)) (car L)))))

;This function takes an atom X and appends it to every element in the list L
(defun combo (L X)
  (if (null L)
  (list)
  (cons (append (car L) (list X)) (combo (cdr L) X))))

;Question 6
;A function that finds one best or worst grade for a specific student across all
;input to the function
(defun findOne (name isBest L)
  (cond ((string= 'worst isBest)
    (car (sort (findAll name L) 'sort-grade-negative)))
    ((string= 'best isBest)
    (car (sort (findAll name L) 'sort-grade-positive)))))

;Maps letter grades to numeric values for easier comparision. Grades listed are
;those in the UAlberta 2014-2015 Calendar
(defun grade-map (a)
  (cond
  ((string= a 'A+) 11)
  ((string= a 'A) 10)
  ((string= a 'A-) 9)
  ((string= a 'B+) 8)
  ((string= a 'B) 7)
  ((string= a 'B-) 6)
  ((string= a 'C+) 5)
  ((string= a 'C) 4)
  ((string= a 'C-) 3)
  ((string= a 'D+) 2)
  ((string= a 'D) 1)
  ((string= a 'F) 0)))

;Sorts grades in descending order (best first)
(defun sort-grade-positive (A B)
  (> (grade-map (cadr A)) (grade-map (cadr B))))

;Sorts grades in ascending order (worst first)
(defun sort-grade-negative (A B)
  (< (grade-map (cadr A)) (grade-map (cadr B))))

;This function finds all courses based off a given name and returns the results
;sorted by course number.
(defun findAll (name L)
  (sort (findName name L ()) 'sort-course-number))

;This function returns course-grade tuples for a given name
(defun findName (name L X)
  (cond
  ((null L) nil)
  ((string= (caar L) name)
    (append X (list (cdar L)) (findName name (cdr L) X)))
  (T (findName name (cdr L) X))))

;This function is the criteria used to sort course names
(defun sort-course-number (A B)
  (string-lessp (car A) (car B)))

;Question 7
;Takes an atom and a list of tuples and returns a list of the atoms that can
;"reach" the given atom from the list of tuples.
(defun reached (x L)
  (let
  ((R (reaches x L '())))
   (remove-duplicate (remove-atom x (reaching x L R R)))))

;This function takes an atom, a list of tuples, a list of nodes connected to the
;atom, and a list of nodes that are connected to nodes in the list R and
;computes a single list of all nodes reached by the atom x.
(defun reaching (x L R V)
  (cond
  ((null R) V)
  (T (reaching x L (cdr R) (append V (reaches (car R) L R))))))

;This function takes an atom, a list of tuples, and a list of nodes that
;the atom can reach, and appends new atoms to the list R
(defun reaches (x L R)
  (cond
  ((null L) R)
  ((and (equal x (caar L))
      (equal x (cadar L)))
   (reaches x (cdr L) R))
  ((and (equal x (caar L))
      (not (contains
         (cadar L)
         (reaches x (cdr L) R))))
   (append R (list (cadar L)) (reaches x (cdr L) R)))
  ((and (equal x (cadar L))
      (not (contains
         (caar L)
         (reaches x (cdr L) R))))
   (append R (list (cadar L)) (reaches x (cdr L) R)))
  (T (reaches x (cdr L) R))))

;This function takes an atom and a list and removes instances of the atom from
;the list.
(defun remove-atom (x L)
  (if (null L)
  L
  (if (equal x (car L))
    (remove-atom x (cdr L))
    (cons (car L) (remove-atom x (cdr L))))))

;Rank taks list of sites and a list of tuples and returns the original list of
;sites ordered by thier rank, determined by the list L. Rank is the number of
;of sites that link to the atom.
(defun rank (S L)
  (flatten-ranks (sort (get-ranks S L) 'rank-order)))

;This function takes a list of tuples with an atom and the number of links to
;the site and returns a flattened list of the atoms in the original order.
(defun flatten-ranks (S)
  (if (null S)
  nil
  (append (list (caar S)) (flatten-ranks (cdr S)))))

;This function is used for sorting the list of tuples containing a site and
;the number of links pointing to that site.
(defun rank-order (A B)
  (> (cadr A) (cadr B)))

;This function takes a list of atoms and a list of tuples representing links,
;and constructs a list of tuples containing the atoms in S and the number of
;links to that particular atom.
(defun get-ranks (S L)
  (cond
  ((null S) nil)
  (T (append (get-ranks (cdr S) L)
         (list (list (car S) (count-link (car S) L)))))))

;This function takes an atom and a list of tuples representing links and returns
;the number of links in the list L that point to the atom x
(defun count-link (x L)
  (cond
  ((null L) 0)
  ((and (equal (cadar L) x) (not (equal (caar L) x)))
   (+ 1 (count-link x (cdr L))))
  (T (count-link x (cdr L)))))
