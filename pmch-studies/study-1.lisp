;;; =======================================================================
;;;; File:       study-1.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:37:26 peter>                          
;;;; Time-stamp: <2017-07-05 15:42:48 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides utility functions for Peter's study on harmony representations.

(cl:in-package #:pmch-s1)

;;;; Top-level call

(defun run-study-1 ()
  ;; Set paths
  (defparameter cl-user::*output-dir*
  (cond ((member :os-macosx cl-user::*features*)
	 "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/")
	((member :marcus-pc cl-user::*features*)
	 "/home/pharrison/HarmonyRepresentations/data-6/")
	(t "/home/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data6/data/")))
  ;;;; Analyse test length
  ;; Classical (1022 pieces in corpus, max 987 in training set with 30-fold CV)
  (loop for ts-size in '(987 8 512 256 16 128 32 64 4 2 1)
     do (pmch-s1:analyse-all-viewpoints 
	 1 nil
	 :reduce-harmony t
	 :k 30
	 :training-set-size ts-size
	 :output-path cl-user::*output-dir*
	 :remove-repeated-chords t))
  ;; Pop (739 pieces in corpus, max 714 in training set with 30-fold CV)
  (loop for ts-size in '(714 8 512 256 16 128 32 64 4 2 1)
     do (pmch-s1:analyse-all-viewpoints 
	 2 nil
	 :k 30
	 :training-set-size ts-size
	 :output-path cl-user::*output-dir*
	 :remove-repeated-chords t))
  ;; Jazz (1186 pieces in corpus, max 714 in training set with 30-fold CV)
  (loop for ts-size in '(1024 8 512 256 16 128 32 64 4 2 1)
     do (pmch-s1:analyse-all-viewpoints 
	 3 nil
	 :k 30
	 :training-set-size ts-size
	 :output-path cl-user::*output-dir*
	 :remove-repeated-chords t))
  ;;;; Analyse generalisation
  ;; Train on classical, test on jazz
  (pmch-s1:analyse-all-viewpoints 3 '(1)
				  :reduce-harmony nil
				  :reduce-harmony-pretraining t
				  :k 1
				  :output-path cl-user::*output-dir*
				  :remove-repeated-chords t)
  ;; Train on pop, test on jazz
  (pmch-s1:analyse-all-viewpoints 3 '(2)
				  :reduce-harmony nil
				  :reduce-harmony-pretraining nil
				  :k 1
				  :output-path cl-user::*output-dir*
				  :remove-repeated-chords t)
  ;; Train on classical, test on pop
  (pmch-s1:analyse-all-viewpoints 2 '(1)
				  :reduce-harmony nil
				  :reduce-harmony-pretraining t
				  :k 1
				  :output-path cl-user::*output-dir*
				  :remove-repeated-chords t)
  (utils:message "Study 1 analyses complete!"))

;;;; Utility functions

(defparameter *harmony-viewpoints* '(h-bass-cpc
				     h-bass-cpcint
				     h-bass-csd
				     h-bass-int-from-gct-root
				     h-cpc-int-from-bass
				     h-cpc-int-from-gct-root
				     h-cpc-milne-sd-cont=min
				     h-cpc-vl-dist-p=1
				     h-cpitch
				     h-cpitch-class-set
				     h-csd
				     h-gct-3rd-type
				     h-gct-7th-type
				     h-gct-base
				     h-gct-ext
				     h-gct-meeus-int
				     h-gct-root-5ths-dist
				     h-gct-root-cpc
				     h-gct-root-cpcint
				     h-gct-root-csd
				     h-hedges-chord-type
				     h-hutch-rough
				     (h-csd h-bass-csd)
				     (h-cpc-int-from-bass h-bass-cpcint)
				     (h-cpc-int-from-gct-root h-gct-root-cpcint)))

(defun analyse-all-viewpoints
    (dataset pretraining-ids
     &key reduce-harmony reduce-harmony-pretraining
       (output-path "/home/peter/idyom-output/study-1/")
       (k 10) training-set-size
       (remove-repeated-chords t))
  (let ((viewpoints *harmony-viewpoints*))
    (analyse-viewpoints viewpoints dataset pretraining-ids
			:reduce-harmony reduce-harmony
			:reduce-harmony-pretraining reduce-harmony-pretraining
			:output-path output-path :k k
			:training-set-size training-set-size
			:remove-repeated-chords remove-repeated-chords)))

(defun analyse-viewpoints
    (viewpoints dataset pretraining-ids
     &key reduce-harmony reduce-harmony-pretraining
       (output-path "/home/peter/idyom-output/study-1/")
       (k 10)
       training-set-size
       (remove-repeated-chords t))
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
			       :training-set-size training-set-size
			       :remove-repeated-chords remove-repeated-chords)))))

(defun analyse-viewpoint
    (viewpoint dataset pretraining-ids reduce-harmony reduce-harmony-pretraining
     &key (output-path "/home/peter/idyom-output/study-1/")
       (k 10) training-set-size (remove-repeated-chords t))
  "Analyses a derived viewpoint, identified by symbol/list <viewpoint>,
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
  (assert (or (listp viewpoint) (symbolp viewpoint)))
  (let* ((output-root-dir (utils:ensure-directory output-path))
	 (training-set-size-dir
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
		      "no-training-set-downsampling")))
	   output-root-dir))
	 (output-dir (merge-pathnames
		      (make-pathname :directory
				     (list :relative
					   (string-downcase
					    (if (listp viewpoint)
						(format nil "~{~A~^-x-~}"
							(mapcar #'symbol-name
								viewpoint))
						(symbol-name viewpoint)))))
		      training-set-size-dir)))
    (if (probe-file output-dir)
	(utils:message "Output directory already exists, skipping analysis.")
	(progn
	  (ensure-directories-exist output-dir)
	  (let* ((output-resampling-set-path
		  (namestring (merge-pathnames
			       (make-pathname :name "resampling" :type "lisp")
			       training-set-size-dir)))
		 (output-analysis-path
		  (merge-pathnames
		   (make-pathname :directory '(:relative "dat_from_idyom"))
		   output-dir))
		 (viewpoints::*basic-types* (list :h-cpitch)))
	    (idyom:idyom
	     dataset '(h-cpitch) (list viewpoint)
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
	     :remove-repeated-chords remove-repeated-chords
	     :resampling-set-cache-path output-resampling-set-path
	     :num-quantiles 12
	     :training-set-size training-set-size
	     :use-ltms-cache? nil
	     :overwrite nil
	     :output-path output-analysis-path))))))

		 
