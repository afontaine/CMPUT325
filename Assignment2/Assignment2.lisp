(defun interp (E P)
  (interp-closure E
          ; Map out the user-definied functions into a nicer format
          ; (func x = ...) => (func (x) (...)). The starting point for
          ; our closure
          (mapcar (lambda (f)
              (list (car f) (get-args (cdr f))
                  (get-func f))) P)))

(defun interp-closure (E C)
  (cond
  ((atom E)
   (cond
     ; See if the atom is a variable and try to find its value from the
     ; closure
     ((contains-symbol E C) (interp-closure (find-args E C) C))
     ; If not, return the atom
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
      ; See if there's a user-defined function
      ((contains-symbol f C)
       (interp-closure (find-func f C)
               ; Map the args of the new function to their values
               ; and add them to the current closure.
               (append (map-args (find-args f C) arg C) C)))
      (T (cons (interp-closure (car E) C) (interp-closure (cdr E) C))))))))


; Takes a function minus the name (x = ...) and returns a list of arguments for
; the function
;
; (x = ...) => (x)
(defun get-args (f)
  (cond
  ((eq (car f) '=) ())
  (T (cons (car f) (get-args (cdr f))))))

; Takes a function (func x = ...) and returns the body of the function
;
; (func x = ...) => (...)
(defun get-func (f)
  (cond
  ((null f) ())
  ((eq (car f) '=) (cadr f))
  (T (get-func (cdr f)))))

; See if our symbol happens to be in the closure, whether it's a function or a
; variable
(defun contains-symbol (x L)
  (cond
  ((null L) nil)
  ((eq x (caar L)) T)
  (T (contains-symbol x (cdr L)))))

; Extracts the args for a given function from the closure
;
; ((func (x y) (...))) => (x y)
(defun find-args (f L)
  (cond
  ((null L) nil)
  ((eq f (caar L)) (cadar L))
  (T (find-args f (cdr L)))))

; Extracs the function body for a given function from the closure
;
; ((func (x y) (...))) => (...)
(defun find-func (f L)
  (cond
  ((null L) nil)
  ((eq f (caar L)) (caddar L))
  (T (find-func f (cdr L)))))

; Maps a functions input variables to the values they will become
;
; (blah 1 2), (blah (x y) (...)) => ((x 1) (y 2))
(defun map-args (A V C)
  (cond
  ((null A) nil)
  (T (cons (list (car A) (interp-closure (car V) C))
       (map-args (cdr A) (cdr V) C)))))
