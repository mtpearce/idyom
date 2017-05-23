;;; =======================================================================
;;;; File:       study-1.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:37:26 peter>                          
;;;; Time-stamp: <2017-05-23 18:54:41 peter>                           
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

;;;; Splitting viewpoints into equal-sized sets
;;   Sets of 3
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

;; Splitting into 8 sets
(defparameter *h-vp-1-of-8*
  '(h-cpitch h-cpc-milne-sd-cont=min h-hutch-rough))

(defparameter *h-vp-2-of-8*
  '(h-cpc-vl-dist-p=1 h-gct-root-csd h-gct-root-cpcint))

(defparameter *h-vp-3-of-8*
  '(h-bass-cpc h-bass-cpcint h-bass-cpint h-hedges-chord-type))

(defparameter *h-vp-4-of-8*
  '(h-bass-cpitch h-bass-csd h-bass-int-from-gct-root))

(defparameter *h-vp-5-of-8*
  '(h-cpc-identity h-cpc-int-from-bass h-cpc-int-from-gct-root))

(defparameter *h-vp-6-of-8*
  '(h-cpitch-class-set h-cpitch-identity h-csd h-gct-meeus-int))

(defparameter *h-vp-7-of-8*
  '(h-gct h-gct-3rd-type h-gct-7th-type h-gct-root-5ths-dist))

(defparameter *h-vp-8-of-8*
  '(h-gct-base h-gct-ext h-gct-root-cpc))


;;;; Analysis functions	 

(defun analyse-all-viewpoints
    (dataset pretraining-ids reduce-harmony reduce-harmony-pretraining
     &key (output-path "/home/peter/idyom-output/study-1/")
       (k 10) training-set-size)
  (let ((viewpoints *harmony-viewpoints*))
    (analyse-viewpoints viewpoints dataset pretraining-ids
			:reduce-harmony reduce-harmony
			:reduce-harmony-pretraining reduce-harmony-pretraining
			:output-path output-path :k k
			:training-set-size training-set-size)))

(defun analyse-viewpoints
    (viewpoints dataset pretraining-ids &key reduce-harmony reduce-harmony-pretraining
					  (output-path "/home/peter/idyom-output/study-1/")
					  (k 10)
					  training-set-size)
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
	    (analyse-viewpoint viewpoint dataset pretraining-ids reduce-harmony
			       reduce-harmony-pretraining
			       :output-path output-path :k k
			       :training-set-size training-set-size)))))

(defun analyse-viewpoint
    (viewpoint dataset pretraining-ids reduce-harmony reduce-harmony-pretraining
     &key (output-path "/home/peter/idyom-output/study-1/")
       (k 10) training-set-size)
  "Analyses a derived viewpoint, identified by symbol <viewpoint>,
on dataset with ID <dataset>, saving the output to a sub-directory
of <output-path>, which will be created if it doesn't exist.
This subdirectory will be identified by the dataset and the viewpoint.
If <reduce-harmony> is true, harmonic reduction is applied to 
the test dataset before analysis.
If <reduce-harmony-pretraining> is true, harmonic reduction is applied to 
the pretraining dataset before analysis.
The analysis uses <k> cross-validation folds.
<pretraining-ids> is a list of datasets to pretrain on.
If <trainining-set-size> is not null, it should be an integer corresponding
to the size that each training set should be downsized to."
  (assert (integerp dataset))
  (assert (listp pretraining-ids))
  (assert (symbolp viewpoint))
  (let* ((output-root-dir (utils:ensure-directory output-path))
	 (output-dir
	  (ensure-directories-exist
	   (merge-pathnames
	    (make-pathname
	     :directory
	     (list :relative
		   (if pretraining-ids
		       (format nil "pretraining-~{~S-~}harmonic-reduction-~A"
			       pretraining-ids
			       (string-downcase (symbol-name
						 reduce-harmony-pretraining)))
		       "pretraining-none")
		   (format nil "test-dataset-~A-harmonic-reduction-~A" dataset
			   (string-downcase (symbol-name reduce-harmony)))
		   (if training-set-size
		       (format nil "resampling-training-set-size-~A"
			       training-set-size)
		       "no-training-set-downsampling")
		   (string-downcase (symbol-name viewpoint))))
	    output-root-dir)))
	 (output-resampling-set-path
	  (namestring (merge-pathnames
		       (make-pathname :name "resampling" :type "lisp")
		       output-dir)))
	 (output-analysis-path
	  (merge-pathnames
	   (make-pathname :directory '(:relative "dat_from_idyom"))
	   output-dir))
	 (viewpoints::*basic-types* (list :h-cpitch)))
    (idyom:idyom dataset '(h-cpitch) (list viewpoint)
		 :k k :texture :harmony :models :ltm
		 :pretraining-ids pretraining-ids
		 :harmonic-reduction (if reduce-harmony
					 :regular-harmonic-rhythm
					 :none)
		 :pretraining-harmonic-reduction (if reduce-harmony-pretraining
						     :regular-harmonic-rhythm
						     :none)
		 :separator #\tab :detail 2.5
		 :use-resampling-set-cache? t
		 :slices-or-chords :chords
		 :resampling-set-cache-path output-resampling-set-path
		 :num-quantiles 10
		 :training-set-size training-set-size
		 :use-ltms-cache? nil
		 :overwrite nil
		 :output-path output-analysis-path)))

		 
