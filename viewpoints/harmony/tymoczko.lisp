;;;; ======================================================================
;;;; File:       tymoczko.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-13 10:18:30 peter>                     
;;;; Time-stamp: <2017-05-01 12:18:41 peter>                          
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines derived viewpoints corresponding to the
;;;; minimal voice-leading algorithm of Tymoczko (2006).
;;;;
;;;; Does not assume integer pitches or pitch classes.
;;;;
;;;; References:
;;;;
;;;; Tymoczko, D. (2006). The geometry of musical chords. Science,
;;;; 313(5783), 72–74. https://doi.org/10.1126/science.1126287

(cl:in-package #:viewpoints)

;;;==============
;;;* Viewpoints *
;;;==============

(define-viewpoint (h-cpc-vl-dist-p=1 derived (h-cpitch))
    ;; Pitch-class voice-leading distance between the preceding harmonic
    ;; slice and the current harmonic slice, after Tymoczko
    ;; (2006), using a p-norm of 1, i.e. a taxicab norm.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	       (if (or (null e1) (null e2)) +undefined+
		  (let ((pc-set-1 (h-cpitch-class-set (list e1)))
			(pc-set-2 (h-cpitch-class-set (list e2))))
		    (cdr (assoc :size
				(vl-get-minimal-voice-leading
				 pc-set-1 pc-set-2
				 :pitch-class :taxicab))))))
  :continuous t)

(define-viewpoint (h-cpc-vl-dist-p=2 derived (h-cpitch))
    ;; Pitch-class voice-leading distance between the preceding harmonic
    ;; slice and the current harmonic slice, after Tymoczko
    ;; (2006), using a p-norm of 2, i.e. an Euclidean norm.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	       (if (or (null e1) (null e2)) +undefined+
		  (let ((pc-set-1 (h-cpitch-class-set (list e1)))
			(pc-set-2 (h-cpitch-class-set (list e2))))
		    (cdr (assoc :size
				(vl-get-minimal-voice-leading
				 pc-set-1 pc-set-2
				 :pitch-class :euclidean))))))
  :continuous t)

(define-viewpoint (h-cpitch-vl-dist-p=1 derived (h-cpitch))
    ;; Pitch voice-leading distance between the preceding harmonic
    ;; slice and the current harmonic slice, after Tymoczko
    ;; (2006), using a p-norm of 1, i.e. a taxicab norm.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	       (if (or (null e1) (null e2)) +undefined+
		  (let ((p-set-1 (h-cpitch (list e1)))
			(p-set-2 (h-cpitch (list e2))))
		    (cdr (assoc :size
				(vl-get-minimal-voice-leading
				 p-set-1 p-set-2
				 :pitch :taxicab))))))
  :continuous t)

(define-viewpoint (h-cpitch-vl-dist-p=2 derived (h-cpitch))
    ;; Pitch voice-leading distance between the preceding harmonic
    ;; slice and the current harmonic slice, after Tymoczko
    ;; (2006), using a p-norm of 2, i.e. an Euclidean norm.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	       (if (or (null e1) (null e2)) +undefined+
		  (let ((p-set-1 (h-cpitch (list e1)))
			(p-set-2 (h-cpitch (list e2))))
		    (cdr (assoc :size
				(vl-get-minimal-voice-leading
				 p-set-1 p-set-2
				 :pitch :euclidean))))))
  :continuous t)

;;;===========
;;;* Classes *
;;;===========

(defclass vl-array ()
  ((cells :documentation "Array of cells corresponding to Fig S12 in Tymoczko (2006)"
	  :accessor cells)
   (set-1
    :initarg :set-1 :accessor set-1
    :documentation
    "Chord 1, list, arranged in ascending order,
no duplicates except for first and last. In Fig S12,
this would be (4 7 11 0 4).")
   (set-2
    :initarg :set-2 :accessor set-2
    :documentation
    "Chord 2, list, arranged in ascending order,
no duplicates except for first and last. In Fig S12,
this would be (4 8 11 3 4)." )
   (minimal-vl
    :accessor minimal-vl
    :documentation
    "Assoc-list containing the minimal voice-leading for the array.")))

(defclass vl-cell ()
  ((a-i
    :initarg :a-i :accessor a-i
    :documentation
    "Corresponds to a_i in Tymcoczko (2006). a_i
is the new element from chord 1 that is added
to the voice leading described by the current
cell. In the middle top cell of Fig S12, this 
would be 4.")
   (b-i
    :initarg :b-i :accessor b-i
    :documentation
    "Corresponds to b_i in Tymcoczko (2006). b_i
is the new element from chord 2 that is added
to the voice leading described by the current
cell. In the middle top cell of Fig S12, this 
would be 11.")
   (set-1
    :initarg :set-1 :accessor set-1
    :documentation
    "List corresponding to the starting chord in the voice-
leading described by the cell. In Fig S12, in the bottom
left cell, this list would be (4 7 11 0 4).")
   (set-2
    :initarg :set-2 :accessor set-2
    :documentation
    "List corresponding to the ending chord in the voice-
leading described by the cell. In Fig S12, in the bottom
left cell, this list would be (4 4 4 4 4).")
  (size
   :initarg :size :accessor size
   :documentation
   "Number corresponding to the size of the voice leading
between <set-1> and <set-2>.")))

;;;==========================
;;;* Initialisation methods *
;;;=========================

(defmethod initialize-instance :after ((array vl-array)
				       &key elt-type norm)
  (assert (equalp (car (set-1 array))
		  (car (last (set-1 array)))))
  (assert (equalp (car (set-2 array))
		  (car (last (set-2 array)))))
  (let ((height (length (set-1 array)))
	(width (length (set-2 array))))
    (assert (> height 0))
    (assert (> width 0))
    (setf (cells array)
	  (make-array (list height width)
		      :initial-element nil))
    (loop
       for i from 0 to (1- height)
       do (loop for j from 0 to (1- width)
	     do (let ((neighbours
		       (cond ((and (eql i 0) (eql j 0))
			      nil)
			     ((eql i 0)
			      (list (aref (cells array) i (1- j))))
			     ((eql j 0)
			      (list (aref (cells array) (1- i) j)))
			     (t
			      (list (aref (cells array) (1- i) (1- j))
				    (aref (cells array) i (1- j))
				    (aref (cells array) (1- i) j))))))
		  (setf (aref (cells array) i j)
			(make-instance 'vl-cell
				       :a-i (nth i (set-1 array))
				       :b-i (nth j (set-2 array))
				       :neighbours neighbours
				       :elt-type elt-type
				       :norm norm)))))
    (let* (;; Find the bottom-right cell in the array; this
	   ;; corresponds to the minimal voice-leading.
	   (minimal-vl-cell (aref (cells array)
				  (1- height) (1- width)))
	   ;; Note that the cell contains duplicates of the pair
	   ;; (a1, b1) as the last list elements. We need to remove
	   ;; these and recompute the size of the voice leading.
	   (minimal-vl-set-1 (butlast (set-1 minimal-vl-cell)))
	   (minimal-vl-set-2 (butlast (set-2 minimal-vl-cell)))
	   (minimal-vl-size (vl-set-distance minimal-vl-set-1
					     minimal-vl-set-2
					     elt-type norm)))
      (setf (minimal-vl array)
	    (list (cons :size minimal-vl-size)
		  (cons :start minimal-vl-set-1)
		  (cons :end minimal-vl-set-2))))))


(defmethod initialize-instance :after ((cell vl-cell)
				       &key
					 neighbours elt-type norm)
  (assert (member elt-type '(:pitch :pitch-class)))
  (assert (member norm '(:euclidean :taxicab :infinity)))  
  (assert (listp neighbours))
  (assert (every #'(lambda (x) (typep x 'vl-cell)) neighbours))
  (if neighbours
      (let* ((sizes (mapcar #'size neighbours))
	     (best-size (apply #'min sizes))
	     (best-neighbour (nth (position-if #'(lambda (x)
						   (eql (size x)
							best-size))
					       neighbours)
				  neighbours)))
	(setf (set-1 cell) (append (set-1 best-neighbour)
				   (list (a-i cell)))
	      (set-2 cell) (append (set-2 best-neighbour)
				   (list (b-i cell)))))
      (setf (set-1 cell) (list (a-i cell))
	    (set-2 cell) (list (b-i cell))))
  (assert (eql (length (set-1 cell))
	       (length (set-2 cell))))   
  (setf (size cell)
        (vl-set-distance (set-1 cell) (set-2 cell)
			 elt-type norm)))

;;;=================
;;;* Other methods *
;;;=================

;; (defgeneric extract-minimal-voice-leading (object &key values)
;;   (:documentation
;;    "Returns the minimal voice-leading represented within <object>."))

;; (defmethod extract-minimal-voice-leading ((array vl-array) &key values)
;;   (let* ((cells (cells array))
;; 	 (height (array-dimension cells 0))
;; 	 (width (array-dimension cells 1))
;; 	 (cell (aref cells (1- height) (1- width)))
;;     (if values
;; 	(values (size cell) (set-1 cell) (set-2 cell))
;; 	cell)))

(defgeneric extract-cell (object dim-0 dim-1 &key values))

(defmethod extract-cell ((array vl-array) dim-0 dim-1 &key values)
  (let ((cell (aref (cells array) dim-0 dim-1)))
    (if values
	(values (size cell) (set-1 cell) (set-2 cell))
	cell)))
    

;;;========================
;;;* Supporting functions *
;;;========================

;; Remember to remove the last voice-leading from the array

(defun vl-get-minimal-voice-leading (s1 s2 elt-type norm &key debug)
  "Computes the minimal voice-leading between 
two sets <s1> and <s2>. <s1> and <x2> should 
be lists of numbers, with each number corresponding
either to a pitch or to a pitch class. Duplicates 
within a list are permitted, and these will be retained.
Elements within each list may be provided
in any order. <elt-type> may be either :pitch or
:pitch-class; it denotes whether <s1> and <s2>
are lists of pitches or lists of pitch classes.
Returns three values: 1) the size of the minimal
voice-leading; 2) the start voicing; 3) the end voicing."
  ;; Notation is set to follow that of Tymoczko (2006)
  ;; where possible.
  ;; We choose <a1> as the numerically smallest element
  ;; of <a>, and then order <a> in increasing distance
  ;; from <a1>. In the case of pitch classes, we could
  ;; have chosen anything for <a1>; in the case of pitches, <a1>
  ;; must be the lowest pitch, because otherwise the
  ;; ascending distances to the other elements will be
  ;; undefined.
  ;; If we are dealing with pitches, then <b1> should
  ;; be set to the lowest element of <b>, because
  ;; the lowest pitch in the first chord must always
  ;; proceed to the lowest pitch in the second chord
  ;; (since voice crossings are prohibited).
  ;; With pitch classes, there is no such restriction,
  ;; and so we iterate over all possible choices of
  ;; <b1>.
  (assert (listp s1))
  (assert (listp s2))
  (assert (not (null s1)))
  (assert (not (null s2)))
  (assert (every #'numberp s1))
  (assert (every #'numberp s2))
  (assert (member elt-type
		  '(:pitch :pitch-class)))
  (let* ((a (copy-list s1)) (b (copy-list s2))
	 ;; (a (remove-duplicates a)) (b (remove-duplicates b))
	 (a (sort a #'<)) (b (sort b #'<))
	 (a-1 (car a))
	 (a (sort a #'<
		  :key #'(lambda (a-i)
			   (vl-ascending-distance a-1 a-i
						  elt-type))))
	 (a (append a (list a-1)))
	 (b-1-candidates (case elt-type
			   (:pitch (list (car b)))
			   (:pitch-class b)
			   (otherwise
			    (error "Unrecognised <elt-type>."))))
	 (best-voice-leading nil)
	 (best-voice-leading-dist nil))
    (assert (eql a-1 (car a)))
    (if debug (utils:message (format nil "Setting a = ~A" a)))
    (dolist (b-1 b-1-candidates)
      (let* ((b (sort (copy-list b) #'<
		      :key #'(lambda (b-i)
			       (vl-ascending-distance b-1 b-i
						      elt-type))))
	     (b (append b (list b-1)))

	     (voice-leading (vl-get-minimal-voice-leading-given-base
			     a b elt-type norm))
	     (voice-leading-dist (cdr (assoc :size voice-leading))))
	(assert (eql b-1 (car b)))
	(if debug (utils:message (format nil "Trying b = ~A" b)))
	(if debug (utils:message (format nil "Best distance = ~A"
					 voice-leading-dist)))
	(if (or (null best-voice-leading)
		(< voice-leading-dist best-voice-leading-dist))
	    (setf best-voice-leading voice-leading
		  best-voice-leading-dist voice-leading-dist))))
    best-voice-leading))


(defun vl-get-minimal-voice-leading-given-base (a b elt-type norm)
  "Gets the minimal voice-leading distance between
two lists <a> and <b>, where <a> and <b> comprise
numbers corresponding to pitches or pitch classes
ordered in increasing ascending distance from 
their first element, and with the first element
of each list being repeated as the last element.
<elt-type> and <norm> define what types of elements
are being used (pitch classes or pitches) and what
type of distance norm is used."
  (assert (listp a))
  (assert (listp b))
  (assert (eql (car a) (car (last a))))
  (assert (eql (car b) (car (last b))))
  (assert (member elt-type
		  '(:pitch :pitch-class)))
  (assert (member norm
		  '(:euclidean :taxicab :infinity)))
  (minimal-vl
   (make-instance 'vl-array :set-1 a :set-2 b
		  :elt-type elt-type :norm norm)))

(defun vl-ascending-distance (e1 e2 elt-type)
  "Returns the ascending distance from <e1> to <e2>,
after Tymoczko (2006): the smallest nonnegative real
number x such that, if p is a pitch with label e1,
then p + x has label e2. <elt-type> may be either
:pitch or :pitch-class; it denotes whether <e1>
and <e2> are pitches or pitch classes."
  (assert (numberp e1))
  (assert (numberp e2))
  (case elt-type
    (:pitch (if (< e2 e1)
		(error "Ascending distance is undefined for descending intervals.")
		(- e2 e1)))
    (:pitch-class (mod (- e2 e1) 12))
    (otherwise (error "Unrecognised <elt-type> argument."))))


(defun vl-elt-distance (e1 e2 elt-type)
  "Computes the voice-leading distance between two
elements <e1> and <e2> given <mode>. <mode>
can be :pitch-class, in which case the distance
is computed between two pitch classes, or 
:pitch, in which cases distance is computed
between two pitches."
  (case elt-type
    (:pitch
     (progn
       (assert (numberp e1))
       (assert (numberp e2))
       (abs (- e1 e2))))
     (:pitch-class
      (progn
	(assert (numberp e1))
	(assert (numberp e2))
	(assert (<= 0 e1))
	(assert (<= 0 e2))
	(assert (< e1 12))
	(assert (< e2 12))
	(min (abs (- e1 e2))
	     (- 12 (abs (- e1 e2))))))
     (otherwise (error "Unrecognised <elt-type> argument."))))

(defun vl-set-distance (s1 s2 elt-type norm)
  "Computes the voice-leading distance between two
ordered sets of the same length, <s1> and <s2>,
given <elt-type> (which can either be 
:pitch or :pitch-class; see vl-elt-distance for
definition) and <norm>, which determines the 
norm used: currently <norm> can take values 
of :euclidean, :taxicab, and :infinity."
  (assert (eql (length s1) (length s2)))
  (assert (member elt-type
		  '(:pitch :pitch-class)))
  (assert (member norm
		  '(:euclidean :taxicab :infinity)))  
  (let ((distances
	 (mapcar #'(lambda (e1 e2)
		     (vl-elt-distance e1 e2
				      elt-type))
		 s1 s2)))
    (assert (every #'(lambda (x) (>= x 0)) distances))
    (case norm
      (:euclidean (sqrt (loop for i in distances
			  summing (* i i))))
      (:taxicab (apply #'+ distances))
      (:infinity (apply #'max distances))
      (otherwise (error "Unrecognised value of <norm>.")))))

