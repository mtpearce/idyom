;;;; ======================================================================
;;;; File:       main.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2010-11-01 15:19:57 marcusp>
;;;; Time-stamp: <2015-02-26 18:17:02 marcusp>
;;;; ======================================================================

(cl:in-package #:idyom)

(defvar *cpitch-viewpoints*
  '(;; Chromatic pitch
    :cpitch       ; chromatic pitch (midi pitch number)
    :cpitch-class ; octave equivalent pitch class (chroma)
    :tessitura    ; 3 values: whether a note is between 66 (G#4) and 74 (D5), above, or below this range
    ;; Pitch interval
    :cpint        ; pitch interval in semitones 
    :cpint-size   ; absolute size of pitch interval
    :cpcint       ; pitch interval class (mod 12) 
    :cpcint-size  ; absolute size of pitch interval class
    ;; Contour
    :contour      ; contour (-1, 0, 1)
    :newcontour   ; boolean: whether or not contour is the same as the previous contour
    ;; Tonality
    :cpintfip     ; pitch interval from the first note in the piece
    :cpintfref    ; chromatic scale degree
    :inscale      ; boolean: whether or not the note is in the scale
    ))

(defvar *cpitch-viewpoints-short*
  '(:cpitch :cpint :contour :cpintfref))

(defvar *bioi-viewpoints*
  '(:bioi           ; inter-onset interval
    :bioi-ratio     ; ratio between consecutive inter-onset intervals
    :bioi-contour   ; contour between consecutive inter-onset intervals
    ))

(defvar *onset-viewpoints*
  '(;;:onset
    :ioi
    :ioi-ratio
    :ioi-contour
    :metaccent
    ;; :posinbar
    ))

;;; IDyOM top-level
;;;
(defun idyom (dataset-id target-viewpoints source-viewpoints
              &key
                ;; Dataset IDs for LTM pretraining
                pretraining-ids
                ;; Resampling
                (k 10) ; Number of cross-validation folds (:full = leave-one-out CV)
                resampling-indices ; Evaluate only certain resampling subsets
                ;; Model options
                (models :both+)
                (ltmo mvs::*ltm-params*) (stmo mvs::*stm-params*)
                ;; Viewpoint selection
                (basis :default)
                (dp nil) (max-links 2) (min-links 2)
                (vp-white '(:any))
                (vp-black nil)
                ;; Number of voices and texture (e.g., :melody :harmony)
                (voices nil)
                (texture :melody)
		(resolution 16)
                ;; Output
                (detail 3)
                (output-path nil)
                ;; Caching
                (use-resampling-set-cache? t)
                (use-ltms-cache? t))
  "IDyOM top level: computes information profiles for basic
   target-viewpoints over a dataset (dataset-id), using a set of
   source-viewpoints, which can be specified or selected
   automatically.  The LTM is optionally pretrained on multiple
   datasets (pretraining-ids) and/or other members of the target
   dataset using k-fold cross validation (AKA resampling).  The
   parameters <use-resampling-set-cache?> and <use-ltms-cache?> enable
   or disable respectively the caching of resampling-sets and LTMs."
  ;; Select source viewpoints, if requested
  (when (eq source-viewpoints :select)
    (format t "~&Selecting viewpoints for the ~A model on dataset ~A predicting viewpoints ~A.~%" 
            models dataset-id target-viewpoints)
    (let* (; Generate candidate viewpoint systems
	   (sel-basis (find-selection-basis target-viewpoints basis))
	   (viewpoint-systems (generate-viewpoint-systems sel-basis max-links min-links vp-white vp-black))
           ; Select viewpoint system
	   (selected (viewpoint-selection:dataset-viewpoint-selection
                      dataset-id target-viewpoints viewpoint-systems
                      :dp dp :pretraining-ids pretraining-ids
                      :k k :resampling-indices resampling-indices
                      :texture texture :voices voices
                      :models models :ltmo ltmo :stmo stmo)))
      (setf source-viewpoints selected)))
  ;; Derive target viewpoint IC profile from source viewpoints
  (multiple-value-bind (predictions filename)
      (resampling:idyom-resample dataset-id target-viewpoints source-viewpoints
                                 :pretraining-ids pretraining-ids
                                 :k k :resampling-indices resampling-indices
                                 :models models :ltmo ltmo :stmo stmo
                                 :voices voices :texture texture :resolution resolution
                                 :use-resampling-set-cache? use-resampling-set-cache?
                                 :use-ltms-cache? use-ltms-cache?)
    (when output-path
      (resampling:format-information-content predictions 
                                             (ensure-directories-exist
                                              (merge-pathnames
                                               filename (utils:ensure-directory output-path)))
                                             dataset-id detail))
    (resampling:output-information-content predictions detail)))


(defun find-selection-basis (targets basis)
  "Determine which viewpoints are to be used in selection process"
  (cond (; Auto mode: use all views derived from target viewpoints. 
	 (eq basis :auto) 
	 (let ((vps (viewpoints:predictors targets)))
	   (if (null vps)
	       (error "Auto viewpoint selection: no defined viewpoints found that might predict target viewpoints ~S" targets)
	       vps)))
	;; Default mode: use conservative default viewpoints for this target
	((eq basis :default) *cpitch-viewpoints-short*)
	;; Predefined viewpoint sets
	((eq basis :bioi) *bioi-viewpoints*)
        ((eq basis :onset) *onset-viewpoints*)
	((eq basis :pitch-full) *cpitch-viewpoints*)
	((eq basis :pitch-short) *cpitch-viewpoints-short*)
	;; Else use supplied viewpoints
	(t basis)))

;;
;; Each candidate viewpoint must match at least viewpoint patterns on
;; the whitelist, and none on the black.
;;
;; E.g. (generate-viewpoint-systems '(:cpitch :cpint :bioi :bioi-ratio) 3 '(:pitch (:pitch :pitch :ioi)) nil)
;; => (:CPITCH :CPINT (:CPITCH :CPINT :BIOI-RATIO) (:CPITCH :CPINT :BIOI))
;;
(defun generate-viewpoint-systems (basis-vps max-links min-links white black)
      (format t "Generating candidate viewpoints from: ~A~%Max. links ~A, whitelist ~A, blacklist ~A~%" basis-vps max-links white black)
      (let* ((links (remove-if #'(lambda (x) (or (null x) (< (length x) 2) 
						 (> (length x) max-links)
                                                 (< (length x) min-links)))
			       (utils:powerset basis-vps)))
	     (slinks (sort links #'(lambda (x y) (< (length x) (length y)))))
	     (candidates (append basis-vps slinks))
	     (filtered (remove-if-not #'(lambda (x) (and (match-vp-patterns x white)
						       (not (match-vp-patterns x black))))
				    candidates)))
	(progn (format t "Candidate viewpoints: ~A~%" filtered)
	       filtered)))

(defun match-vp-patterns (vp patterns)
  "Does the viewpoint match one of the patterns?"
  (if (or (not (listp patterns))
	  (null patterns))
      nil
      (or (match-vp-pattern vp (car patterns))
	  (match-vp-patterns vp (cdr patterns)))))

(defun match-vp-pattern (vp pattern)
  "Does the viewpoint match this pattern?"
  (or
   ;; ATOMIC PATTERNS
   ;;
   ;; Match any viewpoint
   (eq pattern :any)
   ;; Pitch viewpoint
   (and (eq pattern :pitch) (member vp *cpitch-viewpoints*))
   ;; IOI viewpoint
   (and (eq pattern :ioi) (member vp *bioi-viewpoints*))
   ;;
   ;; COMPOUND PATTERNS
   ;;
   (and (listp pattern)
	(if (eq (car pattern) :or)
	    ;; List of ground viewpoints
	    (member vp (cdr pattern))
	    ;; Match linked viewpoint
	    (and (listp vp)
		 (eq (length vp) (length pattern))
		 (reduce #'(lambda (a b) (and a b))
			 (mapcar #'match-vp-pattern vp pattern)))))))
		 

;;;===========================================================================
;;; Simulation of Conklin (1990), Conklin and Witten (1995), and Pearce (2005)
;;;===========================================================================

(defun conklin90 (&optional (dataset 2) (resampling-indices '(0)))
  (format t "~&Simulation of the pitch-based features of Conklin (1990, Experiment 8, p. 115).~%")
  (let ((viewpoints '(cpint
                      (cpint ioi)
                      (cpintfiph contour)
                      (cpintfref cpintfip)
                      (cpintfib barlength))))
    (idyom:idyom dataset '(cpitch) viewpoints :resampling-indices resampling-indices :detail 1)))

(defun conkwit95 (&optional (dataset 2) (resampling-indices '(1)))
  (format t "~&Simulation of the experiments of Conklin & Witten (1995, Table 4).~%")
  (let ((systems '((cpitch)
                   (cpint)
                   ((cpint ioi))
                   ((cpint ioi) cpitch)
                   ((cpintfref cpint))
                   ((cpintfref cpint) (cpint ioi))
                   ((cpintfref cpint) (cpint ioi) cpitch)
                   ((cpintfref cpint) (cpint ioi) cpitch (cpintfref fib))))
        (system-id 1))
    (dolist (system systems)
      (let ((mean-ic (idyom:idyom dataset '(cpitch) system :resampling-indices resampling-indices :detail 1)))
        (format t "~&System ~A; Mean Information Content: ~,2F ~%" system-id mean-ic)
        (incf system-id)))))

(defun pearce05 (&optional (dataset-id 1))
  (format t "~&Simulation of the experiments of Pearce (2005, Table 9.1/9.8, p. 191/206).~%")
  (let ((systems '((A (cpitch))
                   (B (cpintfip (cpintfref dur-ratio) thrfiph))
                   (C (thrfiph cpintfip (cpint dur-ratio) (cpintfref dur) 
                       thrtactus (cpintfref fib) (cpitch dur) 
                       (cpintfref cpintfip) (cpint dur)))
                   (D (cpintfiph (cpintfref dur) (cpint inscale) 
                       (cpint dur-ratio) (cpintfref liph) thrfiph 
                       (cpitch dur) (cpintfref cpintfip) 
                       (cpintfref mode) (cpint dur))))))
    (dolist (system systems)
      (let ((mean-ic (idyom:idyom dataset-id '(cpitch) (cadr system) :k 10 :detail 1)))
        (format t "~&System ~A; Mean Information Content: ~,2F ~%" (car system) mean-ic)))))



