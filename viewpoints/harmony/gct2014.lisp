;;;; ======================================================================
;;;; File:       gct2014.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2022-07-06 12:57:27 marcusp>                           
;;;; ======================================================================

;;; An implementation of the General Chord Type (GCT) algorithm
;;; (Cambouropoulos et al., 2014, Proceedings of ICMC-SMC)

(cl:in-package #:viewpoints)

(defvar *common-practice-consonance-vector* '(1 0 0 1 1 1 0 1 1 1 0 0))
(defvar *harmonic-minor-scale-degrees* '(0 2 3 5 7 8 11))
(defvar *major-scale-degrees* '(0 2 4 5 7 9 11))

(defun general-chord-type-2014 (pitches pitch-scale-hierarchy consonance-vector)
  "Computes the GCT representation of PITCH-CLASS-SET, given BASS-PC,
PITCH-SCALE-HIERARCHY and CONSONANCE-VECTOR.  PITCH-SCALE-HIERARCHY
can optionally be null, in which case the choose-best-chord function
may produce slightly different results, and root-csd will be expressed
relative to a tonic of C major."
  (let* ((pitch-class-set (mapcar #'(lambda (x) (mod x 12)) pitches))
         (pitch-class-set (sort pitch-class-set #'<))
         (pitch-class-set (remove-duplicates pitch-class-set))
         (maximal-subsets (maximal-subsets pitch-class-set consonance-vector))
         (base (mapcar #'make-compact maximal-subsets))
         (root-base-extension (mapcar #'(lambda (x) (add-root-and-extensions x pitch-class-set))
                                      base))
         (relative-chord (mapcar #'(lambda (x) (relate-to-key x pitch-scale-hierarchy)) 
                                 root-base-extension))
         (tonic (first pitch-scale-hierarchy))
         (bass-pc (car pitch-class-set))
         (bass-csd (if tonic
                       (mod12- bass-pc tonic)
                       bass-pc))
         (best-chord (choose-best-chord relative-chord base bass-csd pitch-scale-hierarchy
                                        pitch-class-set bass-pc consonance-vector)))
    (combine-base-and-extension best-chord)))

(defun root (chord) (first chord))
(defun base (chord) (second chord))
(defun extension (chord) (third chord))

(defun as-pitch-class-set (chord)
  "Returns the pitch classes present within the base of
CHORD (i.e. doesn't include extensions). Note: this function can
similarly be applied to a pitch-scale-hierarchy object, returning the
scale tones of the pitch-scale-hierarchy as pitch classes."
  (let* ((root (car chord))
         (base (cdr chord)))
    (mapcar #'(lambda (x) (mod (+ x root) 12)) base)))

(defun combine-base-and-extension (chord)
  "For a chord represented as a triple (root, base, extension) returns
a pair (root, chord-type) where chord-type is the combination of base
and extension."
  (if (= (length chord) 3)
     (list (root chord) (append (list 0) (base chord) (extension chord)))
     chord))


(defun choose-best-chord (chords bases bass-csd pitch-scale-hierarchy
                          pitch-class-set bass-pc consonance-vector
                          &key (prefer-bass-roots t) (prefer-subset-overlap nil)
                            (prefer-scalar-base t))
  "Given a list of candidate chord interpretations (CHORDS), and their
respective bases (BASES), and the current pitch scale
hierarchy (PITCH-SCALE-HIERARCHY), this function selects the best
chord interpretation according to certain heuristics. Which heuristics
are used can be customised by providing options. These heuristics are
a) prefer roots that are bass notes (prefer-bass-roots), prefer
interpretations that have maximal subset
overlap (prefer-subset-overlap), and prefer interpretations that have
all scale tones in the base (prefer-scalar-base). Note that the
maximal subset overlap option can be prohibitively computationally
expensive for large chords. If more than one possible chord
interpretation remains, the final chord interpretation is chosen
pseudo-randomly, so that the interpretation should remain constant
over different Lisp images."
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
                                   (as-pitch-class-set (utils:flatten x))))
                         candidates))
        ;; (print (list "candidates" candidates))
        (when (null candidates) (setf candidates old-candidates))
        ;; (setf candidates (remove-duplicates candidates :test #'equalp))
        (setf n (length candidates))))
    ;; 4. otherwise choose randomly
    (when (> n 1)
      ;; (format t "~&Warning: selecting chord pseudo-randomly from: ~A.~%" candidates)
      ;; (setf candidates (list (nth (random n) candidates))))
      (let ((hash-input (list pitch-class-set bass-pc pitch-scale-hierarchy consonance-vector)))
        (setf candidates (list (nth (mod (sxhash hash-input) n) candidates)))))
    (car candidates)))

(defun positions (item list &key from-end test)
  "Returns the positions of the elements in LIST that are equal to
ITEM under the equality predicate TEST. If FROM-END is true, then
positions will be listed in reverse order."
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
  "Returns the maximum number of overlapping elements between lists L1
and L2, where overlap is defined as when the end of L1 is equal to the
beginning of L2."
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

(defun mod12- (x y) (mod (- x y) 12))

(defun relate-to-key (chord pitch-scale-hierarchy)
  "Expresses the root of CHORD (i.e. first element of CHORD) relative
to the tonic of PITCH-SCALE-HIERARCHY (i.e. first element of
PITCH-SCALE-HIERARCHY), specifically as an integer between 0 and
11 (inclusive). If the tonic of PITCH-SCALE-HIERARCHY is NULL, the
root is left untransposed. CHORD is assumed to be of the form
generated by the function ADD-ROOT."
  (let* ((root (root chord))
         (base (base chord))
         (extension (extension chord))
         (tonic (first pitch-scale-hierarchy)))
    (if (null tonic)
        chord
        (list (mod12- root tonic) base extension))))

(defun add-root-and-extensions (chord pc-set)
  "Given the pitch classes present in the base of the chord as
argument CHORD, and the pitch classes present in the whole
chord (PC-SET), returns a triple (root base extensions)."
  (let* ((root (car chord))
         (base (cdr chord))
         (base (mapcar #'(lambda (x) (mod12- x root)) base))
         (max (apply #'max base))
         (extensions (remove-if #'(lambda (x) 
                                    (member x chord))
                                pc-set))
         (extensions (mapcar #'(lambda (x)
                            (let ((y (mod12- x root)))
                              (if (< y max)
                                  (+ y 12)
                                  y)))
                        extensions)))
    (list root 
          base
          (sort extensions #'<))))

(defun make-compact (list)
  "Takes a pitch-class set (LIST) arranged in ascending order of pitch
class, and arranges it so as to minimise the interval between the
first and the last pitch class (modulo 12)."
  (flet ((compactness (list)
           (let ((first (car list))
                 (last (car (last list))))
             (mod12- last first))))
  (let ((rotated-list list)
        (result list)
        (compactness (compactness list)))
    (dotimes (n (length result) result)
      (setf rotated-list (append (cdr rotated-list) (list (car rotated-list))))
      (let ((rotated-compactness (compactness rotated-list)))
        (when (< rotated-compactness compactness)
          (setf result rotated-list)
          (setf compactness rotated-compactness)))))))

(defun maximal-subsets (pc-set consonance-vector)
  "Finds the maximal subset(s) of pitch classes within PC-SET which
are all pairwise consonant with each other according to
CONSONANCE-VECTOR. Subsets are returned in ascending order of pitch
class. Assumes that CONSONANCE-VECTOR is symmetric."
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
  "Finds the maximal subset(s) of pitch classes within PC-SET which a)
are all pairwise consonant with each other according to
CONSONANCE-VECTOR and b) contain PC. Subsets are returned in ascending
order of pitch class. Assumes that CONSONANCE-VECTOR is symmetric."
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
  "Takes a pitch-class (PC) and find its 'children' within a
pitch-class set (PC-SET). Children of PC are defined as other members
of PC-SET which a) are pairwise consonant with PC, according to
CONSONANCE-VECTOR, and b) have higher pitch classes than PC."
  (remove-if #'(lambda (x) 
                 (or (<= x pc)
                     (not (pairwise-consonant-p x pc consonance-vector))))
             pc-set))

(defun pairwise-consonant-p (x y consonance-vector)
  "Determines whether two pitch classes X and Y are pairwise consonant
with respect to CONSONANCE-VECTOR."
  (= 1 (elt consonance-vector (abs (- x y)))))


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
