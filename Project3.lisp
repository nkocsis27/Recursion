; PROJECT 3
; Natalie Kocsis 	nkocsis@u.rochester.edu
; Meg Hanson	     mhanson8@u.rochester.edu
; Sarah Koehler 	skoehler@u.rochester.edu

; LIST FUNCTIONS

; Return true (T) if X is member of list L, otherwise (NIL)
(defun .member (X L) 
	(cond 
		((null L) 'NIL) 
		((equalp X (car L)) 't) 
		((equalp 't (.member X (cdr L))) 't) 
		((equalp 'NIL (.member X (cdr L))) 'NIL)
	)
)

; Returns list of all removed X's
(defun .remove-all (X L)
	(cond 
		((null L) L) 
		((equalp X (car L)) (.remove-all X (cdr L)))
		((equalp 'NIL (equalp X (car L))) 
			(cons (car L) (.remove-all X (cdr L)))
		)
	)
)	

; Fold function where Z is value, F is type of operator, L is list
(defun .foldl (L F Z)
	(cond
		((null L) Z)
		((.foldl (cdr L) F (funcall F Z (car L))))
	)
)

; Sum of items in list
(defun .sum (L)
	(cond
		((null L) 0)
		((car L) (+ (car L) (.sum (cdr L)))) 
	)
)

; SET FUNCTIONS

; Returns set by adding X to S
(defun .add-element (X S) 
	(cond 
		((.member X S) S)
		((and X S) (cons X S))
	)
)

; Returns the set that is the intersection of both sets
(defun .intersection (S1 S2)
	(cond
		((null S1) S1)
		((null S2) S2)
		((.member (car S1) S2) (cons (car S1) 
			(.intersection (cdr S1) S2)))
		((equalp 'NIL (.member (car S1) S2)) 
			(.intersection (cdr S1) S2))

	)
)


; Returns true if S1 is superset of or equal to S2
; Elements of S2 are in S1
(defun .supersetp (S1 S2)
	(cond 
		((null S2) 't)
		((equalp 't (.member (car S2) S1)) (.supersetp S1 (cdr S2)))
		((equalp 'NIL (.member (car S2) S1)) 'NIL)
	)
)

; Returns number of elements in set
(defun .cardinality (S)
	(cond
		((null S) 0)
		((car S) (+ 1 (.cardinality (cdr S))))
	)
)


; MATH FUNCTIONS

; Returns factorial
(defun .factorial (N)
	(cond
		((equalp N 0) 1)
		((* N (.factorial (- N 1))))
	)
)

; Returns greatest common divisor
(defun .gcd (X Y)
	(cond
		((equalp X 0) Y)
		((.gcd (.mod Y X) X))
	)
	
)

; Returns the mod of X and Y
; Helper function for (.gcd)
(defun .mod (X Y)
	(cond 
		((< X Y) X)
		(X (.mod (- X Y) Y))
	)
)


; Return value of X to the power Y
(defun .pow (X Y)
	(cond
		((equalp Y 0) 1)
		((< Y 0) (* (/ 1 X) (.pow X (+ Y 1))))
		((> Y 0) (* X (.pow X (- Y 1))))
	)
)

; Return value of V of formula
; formula is P(1+R)^N
(defun .with-annual-interest (P R N)
	(* P (.pow (+ 1 R) N))

)

