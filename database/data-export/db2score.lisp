;;;; ======================================================================
;;;; File:       db2score.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-11-25 17:08:20 marcusp>
;;;; Time-stamp: <2017-02-15 15:03:54 peter>
;;;; ======================================================================

(cl:in-package #:db2lilypond) 

(defvar *lilypond*  "lilypond")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-lilypond-exporter (format backend)
    `(progn 
       (defmethod export-data ((d idyom-db:mtp-dataset) (type (eql ,format)) path &key filename)
	 (declare (ignore pathname))
	 (dolist (c (idyom-db::dataset-compositions d))
	   (export-data c type path)))
       (defmethod export-data ((c idyom-db:mtp-composition) (type (eql ,format)) path &key filename)
	 (declare (ignore pathname))
	 (let* ((title (idyom-db::composition-description c))
		(ly-file (concatenate 'string path "/" title ".ly")))
	   (export-data c :ly path)
	   (let ((current-dir (utils:pwd))
		 (output-format (string-downcase (format nil "--~A" ,format)))
		 (backend (string-downcase (format nil "--backend=~A" ,backend))))
	     (utils:cd path)
	     (utils:shell-command *lilypond* (list backend output-format ly-file))
	     (utils:cd current-dir)))
	 nil)
       (defmethod export-data ((event-list list) (type (eql ,format)) path &key filename)
	 (let* ((title (idyom-db::composition-description (car event-list)))
		(ly-file (concatenate 'string path "/" title ".ly")))
	   (export-data event-list :ly path)
	   (let ((current-dir (utils:pwd))
		 (output-format (string-downcase (format nil "--~A" ,format)))
		 (backend (string-downcase (format nil "--backend=~A" ,backend))))
	     (utils:cd path)
	     (utils:shell-command *lilypond* (list backend output-format ly-file))
	     (utils:cd current-dir)))
	 nil)))
  ) ; eval-when 

(define-lilypond-exporter :tex :tex)
(define-lilypond-exporter :dvi :tex)
(define-lilypond-exporter :ps  :ps)
(define-lilypond-exporter :pdf :ps)
(define-lilypond-exporter :png :ps)
