;;; =======================================================================
;;;; File:       study-1.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:37:26 peter>                          
;;;; Time-stamp: <2017-05-15 19:03:18 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides utility functions for Peter's study on harmony representations.

(cl:in-package #:pmch-s1)

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
						  (make-pathname :name "resampling" :type "csv")
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
		 :output-path output-analysis-path)))

		 
