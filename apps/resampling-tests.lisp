;;;; ======================================================================
;;;; File:       resampling-tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-09 14:00:34 peter>                             
;;;; Time-stamp: <2017-05-11 11:30:34 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the resampling package.

(cl:in-package #:resampling)
(5am:def-suite resampling)

;;;============================
;;;* Creating resampling sets *
;;;============================

(5am:def-suite create-resampling-sets :in resampling)
(5am:in-suite create-resampling-sets)

;; Check number of test sets
(5am:test create-resampling-sets-ex-1
  (5am:is (eql (length (create-resampling-sets 10 5)) 5)))
(5am:test create-resampling-sets-ex-2
  (5am:is (eql (length (create-resampling-sets 100 7)) 7)))
(5am:test create-resampling-sets-ex-3
  (5am:is (eql (length (create-resampling-sets 99 35)) 35)))
;; Check for repetition in the test sets
(5am:test create-resampling-sets-ex-4
  (5am:is (every #'(lambda (x) (let ((test-set (second (car x))))
				 (not (utils:any-duplicated test-set))))
		 (create-resampling-sets 80 13))))
;; Check for repetition in the training sets
(5am:test create-resampling-sets-ex-5
  (5am:is (every #'(lambda (x) (let ((training-set (second (second x))))
				 (not (utils:any-duplicated training-set))))
		 (create-resampling-sets 80 13))))
;; Check symbols
(5am:test create-resampling-sets-ex-6
  (5am:is (every #'(lambda (x) (eql (caar x) 'test)) (create-resampling-sets 12 2))))
(5am:test create-resampling-sets-ex-7
  (5am:is (every #'(lambda (x) (eql (car (second x)) 'train))
		 (create-resampling-sets 40 3))))
;; Check that no element appears in more than one test set
(5am:test create-resampling-sets-ex-8
  (5am:is (not (utils:any-duplicated (loop for resampling-set
				  in (create-resampling-sets 20 6)
				  append (second (car resampling-set)))))))
;; Check that test sets are all approximately equal sizes
(5am:test create-resampling-sets-ex-9
  (5am:is (eval '(let* ((resampling-sets (create-resampling-sets 70 8))
			(min-size (apply #'min (mapcar #'(lambda (x)
							   (length (second (car x))))
						       resampling-sets)))
			(max-size (apply #'max (mapcar #'(lambda (x)
							   (length (second (car x))))
						       resampling-sets))))
		  (< (- max-size min-size) 2)))))
;; Check that no elements appear in both training and test sets
(5am:test create-resampling-sets-ex-10
  (5am:is (every #'(lambda (x) (let* ((test-set (second (car x)))
				      (training-set (second (second x)))
				      (combined (append test-set training-set)))
				 (not (utils:any-duplicated combined))))
		 (create-resampling-sets 120 14))))

;;;=====================
;;;* Output formatting *
;;;=====================

;; Here we test the new formatting function against the old formatting
;; function and check for discrepancies. Note that the old formatting
;; function produced incorrect outputs when predicting onset,
;; so the top-level IDyOM command used here should only predict cpitch.

;; This test relies on the UNIX diff tool, so probably will fail on Windows.
;; Future work should disable this test depending on the operating system.

(5am:def-suite output-formatting :in resampling)
(5am:in-suite output-formatting)

(5am:test output-format-ex-1
  (5am:is (idyom-compare-format-methods)))

(defparameter *example-dataset-db-id* -999)

;; Files and folders
(defparameter *temp-dir* (merge-pathnames
			  (make-pathname :directory
					 '(:relative "temp" "tests"))
			  cl-user::*idyom-root*))
(ensure-directories-exist *temp-dir*)

(defparameter *ex-composition-dir*
  (merge-pathnames (make-pathname :directory
				  '(:relative "ex-compositions"))
		   *temp-dir*))
(ensure-directories-exist *ex-composition-dir*)

(defparameter *ex-idyom-output-dir-format=old*
  (merge-pathnames (make-pathname :directory
				  '(:relative "ex-idyom-format-old"))
		   *temp-dir*))
(defparameter *ex-idyom-output-dir-format=new*
  (merge-pathnames (make-pathname :directory
				  '(:relative "ex-idyom-format-new"))
		   *temp-dir*))

(defun write-ex-compositions-to-file ()
  (let ((file-1 (merge-pathnames
		 (make-pathname :name "ex-1" :type "krn")
		 *ex-composition-dir*))
	(file-2 (merge-pathnames
		 (make-pathname :name "ex-2" :type "krn")
		 *ex-composition-dir*))
	(file-3 (merge-pathnames
		 (make-pathname :name "ex-3" :type "krn")
		 *ex-composition-dir*))
	(file-4 (merge-pathnames
		 (make-pathname :name "ex-4" :type "krn")
		 *ex-composition-dir*)))
    (with-open-file (stream file-1
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream *ex-composition-1-string*))
    (with-open-file (stream file-2
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream *ex-composition-2-string*))
    (with-open-file (stream file-3
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream *ex-composition-3-string*))
    (with-open-file (stream file-4
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream *ex-composition-4-string*))))

(defun import-ex-compositions ()
  (idyom-db:delete-dataset *example-dataset-db-id*)
  (idyom-db:import-data :krn *ex-composition-dir*
			"Four example **kern files for regression tests"
			*example-dataset-db-id*))

(defun idyom-compare-format-methods ()
  "Computes an IDyOM analysis using the two outputting format modes
and compares the text files that they output. If the two text files
are the same, returns T, otherwise NIL."
  (write-ex-compositions-to-file)
  (import-ex-compositions)
  (flet ((run-idyom (output-path)
	   (idyom:idyom *example-dataset-db-id*
			'(cpitch)
			'(cpint cpcint)
			:k 2
			:use-resampling-set-cache? t
			:use-ltms-cache? t
			:output-path output-path
			:overwrite t :detail 3
			:separator #\tab)))
    (let ((resampling::*use-old-format-method* t))
      (run-idyom *ex-idyom-output-dir-format=old*))
    (let ((resampling::*use-old-format-method* nil))
      (run-idyom *ex-idyom-output-dir-format=new*)))
  (let* ((file-new (uiop:directory-files *ex-idyom-output-dir-format=new*
					 "*.dat"))
	 (file-old (uiop:directory-files *ex-idyom-output-dir-format=old*
					 "*.dat"))
	 (exit-code (sb-ext:process-exit-code
		     (sb-ext:run-program "/usr/bin/diff"
					 (list (namestring (car file-new))
					       (namestring (car file-old)))))))
    (eql exit-code 0)))

(defun format-information-content-detail=3-old
    (stream resampling-predictions dataset-id &key (separator " "))
  (let ((results (make-hash-table :test #'equal))
	(features))  ; accumulates a list of target viewpoints
    (flet ((create-key (feature attribute)
	     (intern (concatenate 'string (symbol-name feature) "."
				  (format nil "~A" attribute)) :keyword))
	   (sort-function (x y) (let ((x1 (car x)) (x2 (cadr x))
				      (y1 (car y)) (y2 (cadr y)))
				  (if (= x1 y1) (< x2 y2) (< x1 y1)))))
      ;; FOR EACH: resampling set prediction 
      (dolist (rsp resampling-predictions)
	;; FOR EACH: feature prediction (e.g. cpitch, onset)
	(dolist (fp rsp)
	  (let ((feature (viewpoints:viewpoint-type
			  (prediction-sets:prediction-viewpoint fp))))
	    (pushnew feature features)
	    ;; FOR EACH: song prediction 
	    (dolist (sp (prediction-sets:prediction-set fp))
	      (let ((composition-id (prediction-sets:prediction-index sp)))
		;; FOR EACH: event 
		(dolist (ep (prediction-sets:prediction-set sp))
		  (let* ((event (prediction-sets:prediction-event ep))
			 (event-id (md:get-event-index (md:get-attribute event 'identifier)))
			 (probability (float (probability ep) 0.0))
			 (distribution (prediction-sets:prediction-set ep))
			 (orders (prediction-sets:prediction-order ep))
			 (weights (prediction-sets:prediction-weights ep))
			 (existing-results (gethash (list composition-id event-id) results))
			 (event-results (if existing-results existing-results (make-hash-table)))
			 (timebase (md:timebase event)))
		    ;; Store event information
		    (unless existing-results
		      (setf (gethash 'dataset.id event-results) dataset-id)
		      (setf (gethash 'melody.id event-results) (1+ composition-id))
		      (setf (gethash 'note.id event-results) (1+ event-id))
		      (setf (gethash 'melody.name event-results)
			    (quote-string (md:get-description
					   dataset-id
					   composition-id)))
		      ;; TODO - this needs to be specific to each type of music-object (music-event, music-slice etc.)
		      (dolist (attribute (viewpoints:get-basic-types event))
			(let ((value (md:get-attribute event attribute)))
			  (when (member attribute '(:dur :bioi :deltast :onset) :test #'eq)
			    (setf value (* value (/ timebase 96))))
			  (setf (gethash attribute event-results) value))))
		    ;; Store feature prediction
		    (dolist (o orders) ; orders
		      (setf (gethash (create-key feature (car o)) event-results) (cadr o)))
		    (when weights
		      (dolist (w weights) ; weights
			(setf (gethash (create-key feature (car w)) event-results) (cadr w))))
		    (setf (gethash (create-key feature 'probability) event-results) probability)
		    (setf (gethash (create-key feature 'information.content) event-results) (- (log probability 2)))
		    (setf (gethash (create-key feature 'entropy) event-results) (float (prediction-sets:shannon-entropy distribution) 0.0))
		    (setf (gethash (create-key feature 'distribution) event-results) distribution)
		    (dolist (p distribution)
		      (setf (gethash (create-key feature (car p)) event-results) (cadr p)))
		    (setf (gethash (list composition-id event-id) results) event-results))))))))
      ;; Combine probabilities from different features
      (maphash #'(lambda (k v)
		   (let* ((event-results v)
			  (probability-keys (mapcar #'(lambda (f) (create-key f 'probability)) features))
			  (probabilities (mapcar #'(lambda (x) (gethash x v)) probability-keys))
			  (probability (apply #'* probabilities))
			  (distribution-keys (mapcar #'(lambda (f) (create-key f 'distribution)) features))
			  (distributions (mapcar #'(lambda (x) (gethash x v)) distribution-keys))
			  (distribution (mapcar #'(lambda (x) (let ((elements (mapcar #'first x))
								    (probabilities (mapcar #'second x)))
								(list elements (apply #'* probabilities))))
						(apply #'utils:cartesian-product distributions))))
		     (setf (gethash 'probability event-results) probability)
		     (setf (gethash 'information.content event-results) (- (log probability 2)))
		     (setf (gethash 'entropy event-results) (prediction-sets:shannon-entropy distribution))
		     ;; TODO elements of combined distribution
		     (mapc #'(lambda (key) (remhash key event-results)) distribution-keys)
		     (setf (gethash k results) event-results)))
	       results)
      ;; Sort values and print
      (let ((sorted-results (utils:hash-table->sorted-alist results #'sort-function))
	    (print-header t))
	(dolist (entry sorted-results)
	  (when print-header
	    (maphash #'(lambda (k v) (declare (ignore v))
			       (format stream "~A~A" (string-downcase
						      (symbol-name k))
				       separator))
		     (cdr entry))
	    (setf print-header nil)
	    (format stream "~&"))
	  (maphash #'(lambda (k v) (declare (ignore k))
			     (format stream "~A~A" (if v v "NA") separator))
		   (cdr entry))
	  (format stream "~&"))))))


;;;=====================
;;;* Generative systems*
;;;=====================

(5am:def-suite generative-systems :in resampling)
(5am:in-suite generative-systems)

(5am:test (filter-and-merge-var-sets :depends-on utils::set-equal)
  (5am:is (set-equal (filter-and-merge-var-sets '((a) (b)))
		     '((a) (b)) :test #'set-equal))
  (5am:is (reduce #'(lambda (a b) (set-equal a b :test #'set-equal))
		  (list (filter-and-merge-var-sets '((a) (a b)))
			(filter-and-merge-var-sets '((a b) (b)))
			'((a b)))))
  (5am:is (reduce #'(lambda (a b) (set-equal a b :test #'set-equal))
		  (list (filter-and-merge-var-sets '((a b) (b c)))
			(filter-and-merge-var-sets '((b c) (a b)))
			'((a b c)))))
  (5am:is (reduce #'(lambda (a b) (set-equal a b :test #'set-equal))
		  (list (filter-and-merge-var-sets '((a b) (b) (a)))
			(filter-and-merge-var-sets '((a) (b) (a b)))
			'((a b)))))
  (5am:is (reduce #'(lambda (a b) (set-equal a b :test #'set-equal))
		  (list (filter-and-merge-var-sets '((a) (b) (b c)))
			(filter-and-merge-var-sets '((b c) (b) (a)))
			'((a) (b c))))))

(5am:test align-variables-with-viewpoints
  (let* ((test-viewpoint-attributes '(metpos abstract-sdeg (metpos style-onset)))
	 (viewpoints (viewpoints:get-viewpoints test-viewpoint-attributes))
	 (test-variable-attributes-1 '(style key metre))
	 (test-variable-attributes-2 '(key))
	 (variables-1 (lv:get-latent-variables test-variable-attributes-1))
	 (variables-2 (lv:get-latent-variables test-variable-attributes-2)))
  (5am:is ((align-variables-with-viewpoints viewpoints variables-1)
  ;; TODO
  nil)

(5am:test (create-generative-systems
	   :depends-on (and . (filter-and-merge-var-sets
			       align-variables-with-viewpoints)))
  ;; TODO
  nil)

;; Example composition 1

(defparameter *ex-composition-1-string*
  "!!!OTL: A Lady Fair, S. 34
!!!id: I0500
**kern
*clefG2
*k[]
*d:
{8a
8g
8e
=1
8.d
16e
8f
8g
8a
8b
=2
8cc
4dd}
=3
{16ee
16ee
16dd
16cc
16a
16g
=4
8.a
16g
8e
8g
8e
8d
=5
4.c}
=6
{8a
8g
8e
=7
8.d
16e
8f
8g
8a
8b
=8
8cc
4dd}
=9
{16ee
16ee
16dd
16cc
16a
16g
=10
4a
8g
16e
16e
8d
8d
=11
4.d}
=12
{8g
8a
8dd
=13
4dd
8a
8b
8cc
8b
=14
8cc
4dd}
=15
{8cc
8a
8a
=16
4a
8g
8e
8d
8d
=17
4.c}
=18
{8a
8g
8e
=19
8.d
16e
8f
8g
8a
8b
=20
8cc
4dd}
=21
{8ee
16dd
16cc
16a
16g
=22
16a
8a
16a
8g
8e
8d
8d
=23
4.d}
==
*-")

;; Example composition 2

(defparameter *ex-composition-2-string*
  "!!!OTL: Aililiu na Gamhna, S.35
!!!id: I0501
**kern
*clefG2
*k[b-]
*a:
{8a
=1
8a
4a
8g
=2yy
8e
=3
4d
8d
8d
=4
4e
8g
8g
=5
4g
8g}
=6
{8g
=7
4a
8dd
8dd
=8
4ff
4ee
=9
4dd
8cc
8a
=10
4g
8g}
=11
{8g
=12
8a
8dd
8dd
8dd
=13
4ff
8ee
8ee
=14
8dd
8cc
8a
8cc
=15
8cc
8cc
8ee}
=16
{8ee
=17
8dd
8cc
8a
8g
=18
4e
8d
8d
=19
8d
8a
8a
8a
=20
4a
4a}
=21
{8a
8b-
8a
8g
=22
4e
8d
8d
=23
4e
4g
=24
4g
4g}
=25
{8a
8a
8dd
8dd
=26
4ff
8ee
8ee
=27
8dd
8cc
8a
8a
=28
4g
4g}
=29
{8a
8a
8dd
8dd
=30
4ff
8ee
8ee
=31
8dd
8cc
8a
8cc
=32
8cc
8dd
8ee}
=33
{8ee
=34
8dd
8cc
8a
8g
=35
4e
8d
8d
=36
8d
8d
8a
8a
=37
4a
4a}
==
*-")

;; Example composition 3

(defparameter *ex-composition-3-string*
  "!!!OTL: A Bhruinnillin Bhéasach, S. 36
!!!id: I0502
**kern
*clefG2
*k[]
*F:
{8f
8g
=1
4a
4ff
8.ee
16cc
=2
4dd
4ee
8.a
16b
=3
4cc
4dd
8.a
16f
=4
4g
4.f
8f
=5
4.f
8r
=6
16ffff
16ffff}
{8.f
16g
=7
4a
4a
8f
8d
=8
4f
4f
8a
8cc
=9
4dd
4dd
8.ff
16ee
=10
8dd
16ee
16dd
4.cc
8cc
=11
4.cc
8r
=12
16gggg
16ffff}
{8.f
16g
=13
4a
4a
8.f
16d
=14
4f
4f
8a
8cc
=15
4dd
4dd
8.ff
16ee
=16
8dd
16ee
16dd
4cc
4cc
=17
4.cc
8r
=18
16aaaa
16ffff}
{8.f
16g
=19
4a
4ff
8.ee
16cc
=20
4dd
4ee
8.a
16b
=21
4cc
4dd
8.a
16f
=22
4g
4.f
8f
=23
4.f}
8r
==
*-")

;; Example composition 4

(defparameter *ex-composition-4-string*
  "!!!OTL: Ban - chnoic éireann &oacute;, S. 37
!!!id: I0503
**kern
*clefG2
*k[f#]
*D:
{8d
8f#
=1
8g
4g
8a
=2
8g
8f#
8e
8d
=3
4d
8f#
8a
=4
4b}
=5
{8dd
8b
=6
8a
8f#
8e
8d
=7
8B
8A
8B
8d
=8
[2d
=9
4d]}
=10
{8d
8f#
=11
8g
4g
8a
=12
8g
8f#
8e
8d
=13
4d
8d
8f#
=14
4b}
=15
{8dd
8b
=16
8a
8f#
8e
8d
=17
8B
8A
8B
8d
=18
[2d
=19
4d]}
=20
{8a
8b
=21
8.cc
16b
8cc
8dd
=22
8cc
8b
8a
8f#
=23
4a
8b
8dd
=24
4dd}
=25
{8a
8b
=26
8.cc
16b
8cc
8dd
=27
4b
8dd
8b
=28
8a
8f#
8e
8d
=29
4B}
=30
{8a
8b
=31
8.cc
16b
8cc
8dd
=32
8cc
8b
8a
8f#
=33
4d
8f#
8a
=34
4b}
=35
{8dd
8b
=36
8a
8f#
8e
8d
=37
8B
8A
8B
8d
=38
[2d
=39
4d]}
==
*-")
