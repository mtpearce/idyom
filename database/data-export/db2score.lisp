;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       db2ps.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2005-11-25 17:08:20 marcusp>
;;;; Time-stamp: <2008-10-31 17:12:43 marcusp>
;;;; ======================================================================

(cl:in-package #:db2lilypond) 

(defvar *lilypond* 
  #+linux "/usr/bin/lilypond"
  #+darwin "/usr/local/bin/lilypond")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro define-lilypond-exporter (format backend)
  `(progn 
    (defmethod export-data ((d mtp-admin:mtp-dataset) (type (eql ,format)) path)
      (dolist (c (mtp-admin::dataset-compositions d))
        (export-data c type path)))
    (defmethod export-data ((c mtp-admin:mtp-composition) (type (eql ,format)) path)
      (let* ((title (mtp-admin::composition-description c))
             (ly-file (concatenate 'string path "/" title ".ly")))
        (export-data c :ly path)
        (let ((current-dir (pwd))
              (output-format (string-downcase (format nil "--~A" ,format)))
              (backend (string-downcase (format nil "--backend=~A" ,backend))))
          (cd path)
          (shell-command *lilypond* (list backend output-format ly-file))
          (cd current-dir)))
      nil)
    (defmethod export-data ((event-list list) (type (eql ,format)) path)
      (let* ((title (mtp-admin::composition-description (car event-list)))
             (ly-file (concatenate 'string path "/" title ".ly")))
        (export-data event-list :ly path)
        (let ((current-dir (pwd))
              (output-format (string-downcase (format nil "--~A" ,format)))
              (backend (string-downcase (format nil "--backend=~A" ,backend))))
          (cd path)
          (shell-command *lilypond* (list backend output-format ly-file))
          (cd current-dir)))
      nil)))
) ; eval-when 

(define-lilypond-exporter :tex :tex)
(define-lilypond-exporter :dvi :tex)
(define-lilypond-exporter :ps  :ps)
(define-lilypond-exporter :pdf :ps)
(define-lilypond-exporter :png :ps)
