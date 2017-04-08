;;;; ======================================================================
;;;; File:       general-chord-type.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-01 14:58:07 peter>                             
;;;; Time-stamp: <2017-04-08 19:38:02 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines derived viewpoints corresponding to the General 
;;;; Chord Type (GCT) representation of Cambouropoulos (2016).
;;;; 
;;;; Cambouropoulos, E. (2015). The Harmonic Musical Surface and Two
;;;; Novel Chord Representation Schemes. In Meredith, D., editor,
;;;; Computational Music Analysis, pages 31–56. Springer.
;;;;
;;;; Non-integer pitches are rounded to the nearest integer
;;;; before processing according to the GCT algorithm.

(cl:in-package #:viewpoints)

(defvar *common-practice-consonance-vector* '(1 0 0 1 1 1 0 1 1 1 0 0))
(defvar *harmonic-minor-scale-degrees* '(0 2 3 5 7 8 11))
(defvar *major-scale-degrees* '(0 2 4 5 7 9 11))


;;;======================
;;;* Derived viewpoints *
;;;======================

(define-viewpoint (h-cpitch-class derived (h-cpitch))
    ;; Pitches present in harmonic slice, mod 12, including duplicates
    ((events md:harmonic-sequence) element)
  :function (let* ((pc (mapcar #'(lambda (x) (mod x 12)) (h-cpitch events)))
		   (sort-pc (sort pc #'<)))
	      sort-pc))

(define-viewpoint (h-cpitch-class-set derived (h-cpitch))
    ;; Pitches present in harmonic slice, mod 12, not including duplicates
    ((events md:harmonic-sequence) element)
  :function (sort (remove-duplicates (h-cpitch-class events) :test #'=) #'<))

(define-viewpoint (h-gct-root-csd derived (h-cpitch))
    ;; Chromatic scale degree of root of chord (GCT representation)
    ;; Note that if the tonic is undefined then h-gct-root-csd
    ;; will be undefined.
    ((events md:harmonic-sequence) element)
  :function (let* ((gct (h-gct events))
		   (tonic (cdr (assoc :tonic gct))))
	      (if tonic
		  (cdr (assoc :root-csd gct))
		  +undefined+)))

(define-viewpoint (h-gct-root-cpc derived (h-cpitch))
    ;; Chromatic pitch class of root of chord (GCT representation)
    ((events md:harmonic-sequence) element)
  :function (let* ((gct (h-gct events))
		   (tonic (cdr (assoc :tonic gct)))
		   (root-csd (cdr (assoc :root-csd gct))))
	      (if tonic
		  (mod (+ tonic root-csd) 12)
		  root-csd)))

(define-viewpoint (h-gct-root-cpcint derived (h-cpitch))
    ;; Chromatic interval between the root of the current chord
    ;; and the root of the previous chord. Returns +undefined+
    ;; if the root of the previous chord is undefined.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((root1 (h-gct-root-cpc (list e1)))
                        (root2 (h-gct-root-cpc (list e2))))
                    (if (undefined-p root1 root2) +undefined+
                        (mod (- root2 root1) 12))))))

(define-viewpoint (h-gct-base derived (h-cpitch))
    ;; Base of chord (GCT representation) (chromatic scale degrees relative to root)
    ((events md:harmonic-sequence) element)
  :function (cdr (assoc :base (h-gct events))))

(define-viewpoint (h-gct-ext derived (h-cpitch))
    ;; Extension of chord (GCT representation) (chromatic scale degrees relative to root)
    ((events md:harmonic-sequence) element)
  :function (cdr (assoc :ext (h-gct events))))

(define-viewpoint (h-gct derived (h-cpitch))
    ;; GCT representation
    ((events md:harmonic-sequence) element)
  :function (let* ((pitch-class-set (h-cpitch-class-set events))
		   (pitch-class-set (mapcar #'round pitch-class-set))
		   (bass-pc (h-bass-cpc events))
		   (key (local-key events))
		   (key (if (undefined-p key)
			    nil
			    key))
		   (tonic (if key
			      (cdr (assoc :tonic key))
			      nil))
		   (mode (if key
			     (cdr (assoc :mode key))
			     nil))
                   ;; (mode (mode events))
                   ;; (tonic (referent events))
                   (pitch-scale-hierarchy 
                    (list tonic (if (eql mode 'minor)
                                    *harmonic-minor-scale-degrees*
                                    *major-scale-degrees*))))
	      (general-chord-type pitch-class-set bass-pc pitch-scale-hierarchy
				  *common-practice-consonance-vector*)))


;;;========================
;;;* Supporting functions *
;;;========================

(defun general-chord-type (pitch-class-set bass-pc pitch-scale-hierarchy consonance-vector)
  "Computes the GCT representation of PITCH-CLASS-SET, given BASS-PC, PITCH-SCALE-HIERARCHY and CONSONANCE-VECTOR.
   PITCH-SCALE-HIERARCHY can optionally be null, in which case the choose-best-chord function may produce
   slightly different results, and root-csd will be expressed relative to a tonic of C major."
  (let* ((pitch-class-set (sort pitch-class-set #'<))
	 (pitch-class-set (remove-duplicates pitch-class-set))
	 (maximal-subsets (maximal-subsets pitch-class-set consonance-vector))
         (base (mapcar #'make-compact maximal-subsets))
         (base-extension (mapcar #'(lambda (x) (add-extensions x pitch-class-set)) base))
         (root-base-extension (mapcar #'add-root base-extension))
         (relative-chord (mapcar #'(lambda (x) (relate-to-key x pitch-scale-hierarchy)) 
                                 root-base-extension))
	 (tonic (first pitch-scale-hierarchy))
	 (bass-csd (if tonic
		       (mod (- bass-pc tonic) 12)
		       bass-pc))
         (best-chord (choose-best-chord relative-chord base bass-csd pitch-scale-hierarchy
					pitch-class-set bass-pc consonance-vector)))
    ;; (when (> (length relative-chord) 1)
    ;;   (print (list pitch-class-set pitch-scale-hierarchy))
    ;;   (print maximal-subsets)
    ;;   (print base)
    ;;   (print base-extension)
    ;;   (print root-base-extension)
    ;;   (print relative-chord)
    ;;   (print best-chord))
    (list (cons :root-csd (first best-chord))
	  (cons :base (sort (second best-chord) #'<))
	  (cons :ext (sort (third best-chord) #'<))
	  (cons :tonic (first pitch-scale-hierarchy)))))

;; (defun combine-base-and-extension (chord)
;;  "This function is no longer needed."
;;  (if (= (length chord) 3)
;;      (list (root chord) (append (base chord) (extension chord)))
;;      chord))

(defun root (chord)
  "Get the root of CHORD (where root is the first element of CHORD)."
  (first chord))
(defun base (chord)
  "Get the base of CHORD (where base is the second element of CHORD)."
  (second chord))
(defun extension (chord)
  "Get the extension of CHORD (where extension is the third element of CHORD)."
  (third chord))

(defun as-pitch-class-set (chord)
  "Returns the pitch classes present within the base of CHORD (i.e. doesn't include extensions). Note: this function can similarly be applied to a pitch-scale-hierarchy object, returning the scale tones of the pitch-scale-hierarchy as pitch classes."
  (let* ((root (root chord))
         (base (base chord)))
    (mapcar #'(lambda (x) (mod (+ x root) 12)) base)))

(defun choose-best-chord (chords bases bass-csd pitch-scale-hierarchy
			  pitch-class-set bass-pc consonance-vector
			  &key (prefer-bass-roots t) (prefer-subset-overlap nil)
			    (prefer-scalar-base t))
  "Given a list of candidate chord interpretations (CHORDS), and their respective bases (BASES), and the current pitch scale hierarchy (PITCH-SCALE-HIERARCHY), this function selects the best chord interpretation according to certain heuristics. Which heuristics are used can be customised by providing options. These heuristics are a) prefer roots that are bass notes (prefer-bass-roots), prefer interpretations that have maximal subset overlap (prefer-subset-overlap), and prefer interpretations that have all scale tones in the base (prefer-scalar-base). Note that the maximal subset overlap option can be prohibitively computationally expensive for large chords. If more than one possible chord interpretation remains, the final chord interpretation is chosen pseudo-randomly, so that the interpretation should remain constant over different Lisp images."
  (let* ((candidates chords)
	 (candidate-bases bases)
         (n (length candidates)))
    ;; 1. bass note
    (when (and (> n 1) prefer-bass-roots)
      (let ((new-candidates nil)
	    (new-candidate-bases nil))
	(loop
	   for candidate in candidates
	   for base in candidate-bases
	   do (when (eql bass-csd (first candidate))
		(push candidate new-candidates)
		(push base new-candidate-bases)))
	(when (not (null new-candidates))
	  (setf candidates (reverse new-candidates))
	  (setf candidate-bases (reverse new-candidate-bases))
	  (setf n (length candidates)))))
    ;; 2. subset overlap 
    (when (and (> n 1) prefer-subset-overlap)
      (let* ((permutations (utils:permutations bases))
             (overlaps (mapcar #'total-overlap permutations))
             (max (apply #'max overlaps))
             (max-positions (positions max overlaps :test #'=)))
        (setf candidates nil)
        (dolist (mp max-positions)
          (let* ((permutation (nth mp permutations))
                 (base (car permutation))
                 (base-position (position base bases :test #'equal))
		 (chord (nth base-position chords)))
            (push chord candidates)))
	(setf candidates (remove-duplicates candidates :test #'equalp))
	(setf n (length candidates))))
    ;; 3. avoid non-scale tones in the base, if tonic was provided
    (when (and (> n 1)
	       (first pitch-scale-hierarchy)  ; i.e. tonic
	       prefer-scalar-base)
      (let ((pitch-scale-pc-set (as-pitch-class-set pitch-scale-hierarchy))
            (old-candidates candidates))
        (setf candidates 
              (remove-if #'(lambda (x) 
                             (some #'(lambda (y) 
                                       (not (member y pitch-scale-pc-set :test #'=)))
                                   (as-pitch-class-set x)))
                         candidates))
        ;; (print (list "candidates" candidates))
        (when (null candidates) (setf candidates old-candidates))
        (setf n (length candidates))))
    ;; 4. otherwise choose randomly
    (when (> n 1)
      ;; (format t "~&Warning: selecting chord pseudo-randomly from: ~A.~%" candidates)
      (let ((hash-input (list pitch-class-set bass-pc pitch-scale-hierarchy consonance-vector)))
	(setf candidates (list (nth (mod (sxhash hash-input) n) candidates)))))
    (car candidates)))

(defun positions (item list &key from-end test)
  "Returns the positions of the elements in LIST that are equal to ITEM under the equality predicate TEST. If FROM-END is true, then positions will be listed in reverse order."
  ;; (defun positions (item sequence &key from-end test test-not start end key)
  (let ((result nil)
        (i 0))
    (dolist (l list (if from-end result (nreverse result)))
      (when (funcall test item l)
        (push i result))
      (incf i))))

(defun total-overlap (list-of-lists)
  (reduce #'+ (mapcar #'overlap (butlast list-of-lists) (cdr list-of-lists))))

(defun overlap (l1 l2)
  "Returns the maximum number of overlapping elements between lists L1 and L2, where overlap is defined as when the end of L1 is equal to the beginning of L2."
  (let* ((length1 (length l1))
         (length2 (length l2))
         (n (max length1 length2)))
    (if (or (zerop length1) (zerop length2))
        0
        (do ((i 1 (1+ i)))
            ((= i n) 0)
          (when (and (<= i length1) (<= i length2)
                     (equal (subseq l1 (- length1 i)) (subseq l2 0 i)))
            (return i))))))

(defun relate-to-key (chord pitch-scale-hierarchy)
  "Expresses the root of CHORD (i.e. first element of CHORD) relative to the tonic of PITCH-SCALE-HIERARCHY (i.e. first element of PITCH-SCALE-HIERARCHY), specifically as an integer between 0 and 11 (inclusive). If the tonic of PITCH-SCALE-HIERARCHY is NULL, the root is left untransposed. CHORD is assumed to be of the form generated by the function ADD-ROOT."
  (let* ((root (first chord))
         (base (second chord))
         (extension (third chord))
         (tonic (first pitch-scale-hierarchy)))
    (if (null tonic)
        chord
        (list (subtract-mod-n root tonic 12) base extension))))

(defun add-root (chord)
  "Adds root to CHORD, and expresses base and extensions relative to the root. CHORD is assumed to be of the form generated by the function ADD-EXTENSIONS."
  (let* ((root (car (first chord)))
	 (base (cdr (first chord)))
         (extensions (second chord)))
    (list root 
          (mapcar #'(lambda (x) (subtract-mod-n x root 12)) base) 
          (mapcar #'(lambda (x) (subtract-mod-n x root 12)) extensions))))

(defun add-extensions (chord pc-set)
  "Given the pitch classes present in the base of the chord as argument CHORD, and the pitch classes present in the whole chord (PC-SET), returns a list with the first element listing the pitch classes in the base of the chord and the second element listing the pitch classes comprising the chord's extensions."
  (let* ((extensions (remove-if #'(lambda (x) 
                                    (member x chord))
                                pc-set))
         (extensions (sort extensions #'<)))
    (list chord extensions)))

(defun subtract-mod-n (x y n)
  "Subtracts Y from X and return the result modulo N. Assumes that both X and Y are already expressed modulo n."
  (if (<= y x)
      (- x y)
      (- (+ n x) y)))

(defun make-compact (list)
  "Takes a pitch-class set (LIST) arranged in ascending order of pitch class, and arranges it so as to minimise the interval between the first and the last pitch class (modulo 12)."
  (flet ((compactness (list n)
           (let ((first (car list))
                 (last (car (last list))))
             (subtract-mod-n last first n))))
  (let ((rotated-list list)
        (result list)
        (compactness (compactness list 12)))
    (dotimes (n (length result) result)
      (setf rotated-list (append (cdr rotated-list) (list (car rotated-list))))
      (let ((rotated-compactness (compactness rotated-list 12)))
        (when (< rotated-compactness compactness)
          (setf result rotated-list)
          (setf compactness rotated-compactness)))))))

(defun maximal-subsets (pc-set consonance-vector)
  "Finds the maximal subset(s) of pitch classes within PC-SET which are all pairwise consonant with each other according to CONSONANCE-VECTOR. Subsets are returned in ascending order of pitch class. Assumes that CONSONANCE-VECTOR is symmetric."
  (let ((subsets (mapcan #'(lambda (x) 
                             (longest-paths x pc-set consonance-vector))
                         pc-set))
        (maximal-subsets nil)
        (maximal-length 0))
    (dolist (s subsets maximal-subsets)
      (let ((s-length (length s)))
        (cond ((> s-length maximal-length)
               (setf maximal-subsets (list s))
               (setf maximal-length (length s)))
              ((= s-length maximal-length)
               (pushnew s maximal-subsets)))))))

(defun longest-paths (pc pc-set consonance-vector)
  "Finds the maximal subset(s) of pitch classes within PC-SET which a) are all pairwise consonant with each other according to CONSONANCE-VECTOR and b) contain PC. Subsets are returned in ascending order of pitch class. Assumes that CONSONANCE-VECTOR is symmetric."
  (let ((best-candidates nil))
    (labels ((generate-paths (current-pc path children)
               (let ((new-children (find-children current-pc children consonance-vector))) 
                 (if (null new-children)
                     ;; a leaf - no more children
                     (let ((path-length (length path))
                           (best-length (length (car best-candidates))))
                       (cond ((> path-length best-length)
                              (setf best-candidates (list (reverse path))))
                             ((= path-length best-length)
                              (pushnew (reverse path) best-candidates))))
                     ;; otherwise recurse
                     (mapcar #'(lambda (x) 
                                 (generate-paths x (cons x path) new-children)) 
                             new-children)))))
      (generate-paths pc (list pc) pc-set)
      best-candidates)))
    
(defun find-children (pc pc-set consonance-vector)
  "Takes a pitch-class (PC) and find its 'children' within a pitch-class set (PC-SET). Children of PC are defined as other members of PC-SET which a) are pairwise consonant with PC, according to CONSONANCE-VECTOR, and b) have higher pitch classes than PC."
  (remove-if #'(lambda (x) 
                 (or (<= x pc)
                     (not (pairwise-consonant-p x pc consonance-vector))))
             pc-set))

(defun pairwise-consonant-p (x y consonance-vector)
  "Determines whether two pitch classes X and Y are pairwise consonant with respect to CONSONANCE-VECTOR."
  (= 1 (elt consonance-vector (abs (- x y)))))


;;;===============
;;;* Unused code *
;;;===============

;; (defun consonance-matrix (pitch-class-set consonance-vector)
;;   (let* ((n (length pitch-class-set))
;;          (array (make-array (list n n))))
;;     (dotimes (i n)
;;       (dotimes (j n)
;;         (let ((xpi (elt pitch-class-set i))
;;               (xpj (elt pitch-class-set j)))
;;         (setf (aref array i j) (elt consonance-vector (abs (- xpi xpj)))))))
;;     array))

;;; ==============================================================================
;;; Stack of thirds (Sapp, 2007, Computing in Musicology, 15, 99-119)

;; (defun stack-of-thirds (pitch-class-set)
;;   (let ((min-score nil)
;;         (roots nil))
;;     (dotimes (test-root 12 roots)
;;       (let ((score 0))
;;         (dolist (pc pitch-class-set)
;;           (let ((weight (pc-weight test-root pc)))
;;             (incf score weight)))
;;         (when (or (null min-score) (<= score min-score))
;;           (setf min-score score)
;;           (push test-root roots))))))

;; (defun pc-weight (root pc)
;;   ;; but this requires pitch spelling (pitch-class is not enough).
;;   )
;;; ==============================================================================
