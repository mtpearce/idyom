;;; =======================================================================
;;;; File:       pmch-utils.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2018-01-11 17:56:42 peter>                          
;;;; Time-stamp: <2019-07-14 22:47:32 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Some misc. utility functions.

(cl:in-package #:pmch-utils)

;; Exports selected harmony corpora to a directory
(defun export-harmony-corpora (dir)
  (let ((dir (utils:ensure-directory dir))
	(spec (list (list (cons :file "classical.json")
			  (cons :reduce-harmony t)
			  (cons :dataset-id 1))
		    (list (cons :file "popular.json")
			  (cons :reduce-harmony nil)
			  (cons :dataset-id 2))
		    (list (cons :file "jazz.json")
			  (cons :reduce-harmony nil)
			  (cons :dataset-id 3)))))
    (ensure-directories-exist dir)
    (loop
       for s in spec
       do (let ((path (merge-pathnames dir (cdr (assoc :file s)))))
	    (export-harmony-corpus (cdr (assoc :dataset-id s))
				   path
				   :reduce-harmony (cdr (assoc :reduce-harmony s))
				   :remove-repeated-chords t)))))

(defun export-harmony-corpus
    (dataset-id path
     &key
       reduce-harmony
       (remove-repeated-chords t))
  (let* ((data
	  (md:get-music-objects
	   (list dataset-id) nil
	   :voices nil
	   :texture :harmony
	   :harmonic-reduction (if reduce-harmony
				   :regular-harmonic-rhythm
				   :none)
	   :slices-or-chords :chords
	   :remove-repeated-chords remove-repeated-chords))
	 (data
	  (mapcar
	   #'(lambda (x)
	       (let* ((desc (md:description x))
		     (seq (mapcar #'(lambda (chord)
				       (mapcar #'round
					       (md:get-attribute chord
								 'h-cpitch)))
				  (coerce x 'list)))
		     (first-chord (car (coerce x 'list)))
		     (mode (md:get-attribute first-chord 'mode))
		     (keysig (md:get-attribute first-chord 'keysig)))
		 (list (cons :description desc)
		       (cons :mode mode)
		       (cons :keysig keysig)
		       (cons :chords seq))))
	   data)))
    (with-open-file (stream (pathname path)
			    :direction :output
			    :if-exists :supersede)
      (cl-json:encode-json data stream))))
  
;; (export-harmony-corpora "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyCorpora/data-raw/new-corpora-with-names/")
