;;;; ======================================================================
;;;; File:       apps.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-11-27 16:27:35 marcusp>
;;;; Time-stamp: <2014-06-04 16:04:03 marcusp>
;;;; ======================================================================

(cl:in-package #:apps) 

;;; Paths 
;;;
;;; All users should define the path *idyom-root* as a 'working
;;; directory' for IDyOM, e.g. using (setq "path") in their .sbclrc
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; *root-dir* provides a location for IDyOM's caches etc.
  (defvar *root-dir* 
    (if (boundp 'common-lisp-user::*idyom-root*)
	;; Use *idyom-root* if defined
	common-lisp-user::*idyom-root*
	;; Else use home directory
	(concatenate 'string
		     #+cmu (namestring (car (ext:search-list "home:")))
		     #+sbcl (sb-ext:posix-getenv "HOME")
		     "/idyom/")))
  ;; *ep-cache-dir* 
  (setf mvs:*ep-cache-dir* (string-append *root-dir* "data/cache/")))

;;; A way of generating filenames to store results, cached data etc.
(defun dataset-modelling-filename (dataset-id basic-attributes attributes
                                   &key (extension "")
                                   pretraining-ids (k 10) (models :both+)
                                   resampling-indices
				   (ltmo mvs::*ltm-params*) (stmo mvs::*stm-params*))
  (labels ((format-list (list token)
             (when list
               (let ((flist (format nil (format nil "~~{~~A~A~~}" token) (flatten-links list))))
                 (subseq flist 0 (1- (length flist))))))
           (flatten-links (list)
             (mapcar #'(lambda (x) (if (atom x) x (format-list x "%"))) list)))
    (let* ((resampling-indices (if (and (numberp k) (= (length resampling-indices) k)) nil resampling-indices))
           (string (format nil "~(~{~A-~}~)" 
                           (list dataset-id 
                                 (format-list basic-attributes "_")
                                 (format-list attributes "_")
                                 (format-list pretraining-ids "_")
                                 (format-list resampling-indices "_")
                                 k models
				 (getf ltmo :order-bound) (getf ltmo :mixtures)
				 (getf ltmo :update-exclusion) (getf ltmo :escape)
				 (getf stmo :order-bound) (getf stmo :mixtures)
				 (getf stmo :update-exclusion) (getf stmo :escape)))))
      (concatenate 'string (subseq string 0 (1- (length string))) extension))))
