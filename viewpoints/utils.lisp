(cl:in-package #:viewpoints)

;;; lists 

(defun flatten (list)
  "Recursively, flattens a list of lists into a single flat list."
  (labels ((flat (list result)
             (if (null list) (reverse result)
                 (let ((head (car list))
                       (tail (cdr list)))
                   (if (atom head)
                       (flat tail (cons head result))
                       (flat tail (append (flatten head) result)))))))
    (flat list '())))


(defun last-element (list)
  "Returns the last element of a list."
  (car (reverse list)))

(defun penultimate-element (list) 
  "Returns the penultimate element of a list."
  (nth 1 (reverse list)))

;;; Nested list functions
;;; (A mix of elements and lists of elements)

(defun nposition (x xs) 
  "Index of first element equal to or containing x"
  (position-if #'(lambda (y) (nmember x y)) xs))

(defun npositions (x xs &key (index 0))
  "Indices of all elements equal to or containing x"
  (if (or (not (listp xs)) (null xs))
      nil
      (let ((posns (npositions x (cdr xs) :index (+ index 1))))
	(if (nmember x (car xs))
	    (cons index posns)
	    posns))))


(defun nmember (x xs)
  "Is x member of a nested list?"
  (cond ((not (listp xs)) (eq x xs))
	((null xs) nil)
	(t (or (nmember x (car xs))
	       (nmember x (cdr xs))))))

(defun nmin (xs)
  "Minimum of nested lists"
  (cond ((not (listp xs)) xs)
	((null xs) 0)
	((null (cdr xs)) (nmin (car xs)))
	(t (min (nmin (car xs)) (nmin (cdr xs))))))


(defun nmapcar (f xs)
  "Map function over nested lists"
  (cond ((not (listp xs)) (apply f (list xs)))
	((null xs) nil)
	(t (cons (nmapcar f (car xs))
		 (nmapcar f (cdr xs))))))

(defun nselectfirst (xs)
  "Pick first element of any list element"
  (flet ((pick1 (x) (if (listp x) (first x) x)))
    (mapcar #'pick1 xs)))


;;; viewpoints 

(defun strip-until-true (test-viewpoint events)
  "Return the longest prefix of the list EVENTS such that
TEST-VIEWPOINT returns true (1 rather than 0)."
  (cond ((null events) '())
        ((undefined-p (viewpoint-element test-viewpoint events))
         (strip-until-true test-viewpoint (butlast events)))
        ((= (viewpoint-element test-viewpoint events) 1) events)
        (t (strip-until-true test-viewpoint (butlast events)))))
