;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2016-10-14 15:42:45 peter>                           
;;;; ======================================================================

(cl:in-package #:viewpoints)

(defvar *common-practice-consonance-vector* '(1 0 0 1 1 1 0 1 1 1 0 0))

;;;; Basic viewpoints

;; From melody, we also have:
;; 
;; * onset
;; * duration
;; * keysig
;; * mode
;; * pulses
;; * barlength
;; * tempo
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (let ((slice (make-instance 'md:music-slice))
;;         (types '(onset dur keysig mode pulses barlength tempo)))
;;     (mapcar #'(lambda (x) (viewpoints:register-basic-type x slice)) types)))

(define-basic-viewpoint h-cpitch ((events md:harmonic-sequence))
  (mapcar #'md:chromatic-pitch (coerce (car (last events)) 'list)))

;;;; Derived viewpoints

;; From melody we also have: 
;; 
;; * referent
;; * beatunit
;; * dur-ratio

;; * ioi, ioi-contour, posinbar, fib, crotchet, tactus, metaccent, met-interval, met-contour

(define-viewpoint (h-cpitch-class derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (mapcar #'(lambda (x) (mod x 12)) (h-cpitch events)))

(define-viewpoint (h-cpitch-class-set derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (sort (remove-duplicates (h-cpitch-class events) :test #'=) #'<))

(define-viewpoint (h-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (root (h-gct events)))

(define-viewpoint (h-base derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (base (h-gct events)))

(define-viewpoint (h-extension derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (extension (h-gct events)))

(define-viewpoint (h-gct derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((pitch-class-set (h-cpitch-class-set events))
                   (mode (mode events))
                   (tonic (referent events))
                   (pitch-scale-hierarchy 
                    (list tonic (if (= mode 9)
                                    ;; (0 2 3 5 7 8 10)   ;; natural minor (2 1 2 2 1 2 2)
                                    '(0 2 3 5 7 8 11)     ;; harmonic minor (2 1 2 2 1 3 1)
                                    '(0 2 4 5 7 9 11)))))  ;; major
              (general-chord-type pitch-class-set pitch-scale-hierarchy *common-practice-consonance-vector*)))

;;; General Chord Type (GCT) (Cambouropoulos et al., 2014, Proceedings of ICMC-SMC)

(defun general-chord-type (pitch-class-set pitch-scale-hierarchy consonance-vector)
  (let* ((maximal-subsets (maximal-subsets pitch-class-set consonance-vector))
         (base (mapcar #'make-compact maximal-subsets))
         (base-extension (mapcar #'(lambda (x) (add-extensions x pitch-class-set)) base))
         (root-base-extension (mapcar #'add-root base-extension))
         (relative-chord (mapcar #'(lambda (x) (relate-to-key x pitch-scale-hierarchy)) 
                                 root-base-extension))
         (best-chord (choose-best-chord relative-chord base pitch-scale-hierarchy)))
    ;; (when (> (length relative-chord) 1)
    ;;   (print (list pitch-class-set pitch-scale-hierarchy))
    ;;   (print maximal-subsets)
    ;;   (print base)
    ;;   (print base-extension)
    ;;   (print root-base-extension)
    ;;   (print relative-chord)
    ;;   (print best-chord))
    best-chord))

(defun combine-base-and-extension (chord)
  (if (= (length chord) 3)
      (list (root chord) (append (base chord) (extension chord)))
      chord))

(defun root (chord) (first chord))
(defun base (chord) (second chord))
(defun extension (chord) (third chord))

(defun as-pitch-class-set (chord)
  (let* ((root (root chord))
         (base (base chord)))
    (mapcar #'(lambda (x) (mod (+ x root) 12)) base)))

(defun choose-best-chord (chords bases pitch-scale-hierarchy)
  (let* ((candidates chords)
         (n (length candidates)))
    ;; 1. subset overlap 
    (when (> n 1)
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
    ;; 2. avoid non-scale tones in the base
    (when (> n 1)
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
    ;; 3. otherwise choose randomly
    (when (> n 1)
      ; (format t "~&Warning: selecting chord pseudo-randomly from: ~A.~%" candidates)
      (setf candidates (list (nth (mod (sxhash candidates) n) candidates))))
    (car candidates)))

(defun positions (item list &key from-end test)
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
  (let* ((root (first chord))
         (base (second chord))
         (extension (third chord))
         (tonic (first pitch-scale-hierarchy)))
    (if (null tonic)
        chord
        (list (subtract-mod-n root tonic 12) base extension))))

(defun add-root (chord)
  (let* ((root (car (first chord)))
	 (base (cdr (first chord)))
         (extensions (second chord)))
    (list root 
          (mapcar #'(lambda (x) (subtract-mod-n x root 12)) base) 
          (mapcar #'(lambda (x) (subtract-mod-n x root 12)) extensions))))

(defun add-extensions (chord pc-set)
  (let* ((max (apply #'max chord))
         (extensions (remove-if #'(lambda (x) 
                                    (member x chord))
                                pc-set))
         (extensions (sort extensions #'<)))
    (list chord extensions)))

(defun subtract-mod-n (x y n)
  (if (<= y x)
      (- x y)
      (- (+ n x) y)))

(defun make-compact (list)
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
  (remove-if #'(lambda (x) 
                 (or (<= x pc)
                     (not (pairwise-consonant-p x pc consonance-vector))))
             pc-set))

(defun pairwise-consonant-p (x y consonance-vector)
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
