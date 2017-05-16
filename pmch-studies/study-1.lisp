;;; =======================================================================
;;;; File:       study-1.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:37:26 peter>                          
;;;; Time-stamp: <2017-05-16 23:47:13 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides utility functions for Peter's study on harmony representations.

(cl:in-package #:pmch-s1)

(defparameter *harmony-viewpoints* '(h-cpitch
				     h-cpc-milne-sd-cont=min
				     h-hutch-rough
				     h-cpc-vl-dist-p=1
				     h-gct-root-csd
				     h-gct-root-cpcint
				     h-bass-cpc h-bass-cpcint h-bass-cpint
				     h-bass-cpitch h-bass-csd
				     h-bass-int-from-gct-root
				     h-cpc-identity h-cpc-int-from-bass
				     h-cpc-int-from-gct-root
				     h-cpitch-class-set
				     h-cpitch-identity
				     h-csd
				     h-gct h-gct-3rd-type
				     h-gct-7th-type h-gct-base
				     h-gct-ext h-gct h-gct-meeus-int
				     h-gct-root-5ths-dist
				     h-gct-root-cpc h-hedges-chord-type))

;; Splitting viewpoints into equal-sized sets

(defparameter *h-vp-1-of-3*
  (let* ((num-viewpoints (length *harmony-viewpoints*))
	 (one-third-threshold (1- (/ num-viewpoints 3))))
    (loop
       for v in *harmony-viewpoints*
       for i from 0 to (floor one-third-threshold)
       collect v)))

(defparameter *h-vp-2-of-3*
  (let* ((num-viewpoints (length *harmony-viewpoints*))
	 (one-third-threshold (1- (/ num-viewpoints 3)))
	 (two-third-threshold (1- (* 2 (/ num-viewpoints 3)))))
    (loop
       for i from (ceiling one-third-threshold) to (floor two-third-threshold)
       collect (nth i *harmony-viewpoints*))))

(defparameter *h-vp-3-of-3*
  (let* ((num-viewpoints (length *harmony-viewpoints*))
	 (two-third-threshold (1- (* 2 (/ num-viewpoints 3)))))
    (loop
       for i from (ceiling two-third-threshold) to (1- num-viewpoints)
       collect (nth i *harmony-viewpoints*))))

;; Analysis functions	 

(defun analyse-all-viewpoints
    (dataset reduce-harmony
     &key (output-path "/home/peter/idyom-output/study-1/")
       (k 10))
  (let ((viewpoints *harmony-viewpoints*))
    (analyse-viewpoints viewpoints dataset reduce-harmony
			:output-path output-path :k k)))

(defun analyse-viewpoints
    (viewpoints dataset reduce-harmony
     &key (output-path "/home/peter/idyom-output/study-1/")
       (k 10))
  "Analyses a set of viewpoints on a given dataset."
  (assert (listp viewpoints))
  (let ((num-viewpoints (length viewpoints)))
    (utils:message (format nil "Analysing ~A viewpoints with dataset ~A."
			   num-viewpoints dataset))
    (loop
       for viewpoint in viewpoints
       for i from 1
       do (progn
	    (utils:message (format nil "Analysing viewpoint ~A/~A (~A)."
				   i num-viewpoints viewpoint))
	    (analyse-viewpoint viewpoint dataset reduce-harmony
			       :output-path output-path :k k)))))

(defun analyse-viewpoint
    (viewpoint dataset reduce-harmony
     &key (output-path "/home/peter/idyom-output/study-1/")
       (k 10))
  "Analyses a derived viewpoint, identified by symbol <viewpoint>,
on dataset with ID <dataset>, saving the output to a sub-directory
of <output-path>, which will be created if it doesn't exist.
This subdirectory will be identified by the dataset and the viewpoint.
If <reduce-harmony> is true, harmonic reduction is applied to 
the dataset before analysis.
The analysis uses <k> cross-validation folds."
  (assert (integerp dataset))
  (assert (symbolp viewpoint))
  (let* ((output-root-dir (utils:ensure-directory output-path))
	 (output-dir (ensure-directories-exist
		      (merge-pathnames
		       (make-pathname :directory
				      (list :relative (format nil "~A" dataset)
					    (string-downcase (symbol-name viewpoint))))
		       output-root-dir)))
	 (output-resampling-set-path (namestring (merge-pathnames
						  (make-pathname :name "resampling" :type "lisp")
						  output-dir)))
	 (output-analysis-path (merge-pathnames
				(make-pathname :directory '(:relative "dat_from_idyom"))
				output-dir))
	 (viewpoints::*basic-types* (list :h-cpitch)))
    (idyom:idyom dataset '(h-cpitch) (list viewpoint)
		 :k k :texture :harmony :models :ltm
		 :harmonic-reduction (if reduce-harmony
					 :regular-harmonic-rhythm
					 :none)
		 :separator #\tab :overwrite t :detail 2.5
		 :use-resampling-set-cache? t
		 :slices-or-chords :chords
		 :resampling-set-cache-path output-resampling-set-path
		 :use-ltms-cache? nil
		 :overwrite nil
		 :output-path output-analysis-path)))

		 
