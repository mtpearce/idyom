;;;; ======================================================================
;;;; File:       resampling-tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-09 14:00:34 peter>                             
;;;; Time-stamp: <2017-05-09 15:59:17 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the resampling package.

(cl:in-package #:resampling)

;;;=========
;;;* Tests *
;;;=========

(5am:def-suite resampling)

(5am:def-suite output-formatting :in resampling)
(5am:in-suite output-formatting)

(5am:test output-format-ex-1
  (5am:is (idyom-compare-format-methods)))


;;;===========
;;;* Options *
;;;===========

(defparameter *example-dataset-db-id* -999)

;;;=====================
;;;* Files and folders *
;;;=====================

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
  (flet ((run-idyom (output-path)
	   (idyom:idyom *example-dataset-db-id*
			'(cpitch onset)
			'(cpint ioi)
			:k 2
			:use-resampling-set-cache? t
			:use-ltms-cache? t
			:output-path output-path
			:overwrite t :detail 3)))
    (let ((resampling::*use-new-format-method* t))
      (run-idyom *ex-idyom-output-dir-format=new*))
    (let ((resampling::*use-new-format-method* nil))
      (run-idyom *ex-idyom-output-dir-format=old*)))
  (let* ((file-new (uiop:directory-files *ex-idyom-output-dir-format=new*))
	 (file-old (uiop:directory-files *ex-idyom-output-dir-format=old*))
	 (exit-code (sb-ext:process-exit-code
		     (sb-ext:run-program "/usr/bin/diff"
					 (list (namestring (car file-new))
					       (namestring (car file-old)))))))
    (eql exit-code 0)))


;;;========================
;;;* Example compositions *
;;;========================

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
