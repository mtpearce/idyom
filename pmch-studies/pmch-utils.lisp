;;; =======================================================================
;;;; File:       pmch-utils.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2018-01-11 17:56:42 peter>                          
;;;; Time-stamp: <2018-01-11 20:07:01 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Some misc. utility functions.

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
	 ;; This returns a list of harmonic sequence objects
	 (data
	  (mapcar #'(lambda (x) (coerce x 'list)) data))
	 ;; Now each composition is represented as a list of chords
	 (data
	  (mapcar
	   #'(lambda (seq) (mapcar #'(lambda (chord)
				       (mapcar #'round
					       (md:get-attribute chord
								 'h-cpitch)))
				   seq))
	   data))) ;; Now chords are represented as lists of integers
    (with-open-file (stream (pathname path)
			    :direction :output
			    :if-exists :supersede)
      (cl-json:encode-json data stream))))
  
