;;;; ======================================================================
;;;; File:       r.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-06-19 14:31:19 peter>                          
;;;; Time-stamp: <2017-06-20 11:38:15 peter>                           
;;;; ======================================================================

;;;; This page contains code for interfacing with R.

(in-package :interfaces)

;; =================
;; Top-level command
;; =================

(defun call-r (command inputs outputs)
  "Calls R. <command> should be a string corresponding to the command
that R should run. This command may span multiple lines. <inputs> should 
be a list of elements each corresponding to an R object that should 
be pre-assigned before <command> is run. The car of each element should
be a string corresponding to the name of the R object, and the cdr 
of each element should be a Lisp object that should be translated 
to the corresponding R object. If only one input is desired, then 
<inputs> may alternatively just be a single cons pair.
<outputs> should be a list of strings, each naming R objects
that should be read back into Lisp once <command> is completed."
  (assert (stringp command))
  (let* ((inputs (cond ((null inputs) inputs)
		       ((consp (car inputs)) inputs)
		       (t (list inputs))))
	 (outputs (if (listp outputs)
		      outputs (list outputs)))
	 (output-files
	  (loop for output in outputs
	     collect (uiop:with-temporary-file
			 (:stream s :direction :output :keep t)
		       (pathname s)))))
    (assert (listp inputs))
    (assert (every #'consp inputs))
    (assert (every #'(lambda (elt) (stringp (car elt))) inputs))
    (assert (listp outputs))
    (assert (every #'stringp outputs))
    (run-r-command
     (with-output-to-string (str)
       ;; Set inputs
       (loop for input in inputs
	  do (let* ((r-name (car input))
		    (lisp-obj (cdr input))
		    (r-obj (lisp->r-object lisp-obj)))
	       (format str "~A <- ~A~%" r-name r-obj)))
       ;; Run command
       (format str "~A~%" command)
       ;; Export outputs
       (loop
	  for output in outputs
	  for output-file in output-files
	  do (format str "~A~%" (r-export-command output
						  output-file)))))
    ;; Read back outputs
    (loop for output-file in output-files
       collect (with-open-file (s output-file)
		 (eval (read s))))))

;; ====================
;; Supporting functions
;; ====================

(defun run-r-script (path &key (wait t) error-stream)
  "Runs an R script located at pathname <path>.
If <wait> is nil, then the command is run asynchronously."
  (sb-ext:run-program cl-user::*rscript-path*
		      (list (namestring path))
		      :wait wait :error error-stream))

(defun run-r-command (command &optional (wait t))
  "Runs <command>, which should be a string, in R.
If <wait> is nil, then the command is run asynchronously."
  (let ((temp-file nil)
	(error-string nil)
	(exit-code nil))
    (setf error-string
	  (with-output-to-string (error-stream)
	    (uiop:with-temporary-file (:stream s :direction :output :keep t)
	      (princ command s)
	      (setf temp-file (pathname s)))
	    (setf exit-code (sb-ext:process-exit-code
			     (run-r-script temp-file
					   :wait wait
					   :error-stream error-stream)))))
    (delete-file temp-file)
    (when (not (eql exit-code 0))
      (error (format nil "An error occurred in R:~%***~%~A***~%" error-string)))))
;; (format t "~%Ran R command:~%~A at temp file ~A~%" command temp-file)))

(defun r-export-command (r-obj temp-file)
  (format nil 
	  "if(is.vector(~A)) {
  write(sprintf(\"(vector %s)\", paste(~A, collapse = \" \")),
        file = \"~A\")
} else {
  stop(\"Don't know how to export outputs of class \", class(~A))
}" r-obj r-obj (namestring temp-file) r-obj))

;; ====================================
;; Converting Lisp objects to R objects
;; ====================================

(defgeneric lisp->r-object (obj)
  (:documentation "Converts a lisp object to a string corresponding
to an equivalent R object."))

(defmethod lisp->r-object ((obj list))
  (with-output-to-string (str)
    (format str "list(")
    (loop for elt in obj 
	  for i from 1
	  if (> i 1) do (format str ",")
	  do (format str "~A" (lisp->r-object elt)))
    (format str ")")))

(defmethod lisp->r-object ((obj vector))
  (with-output-to-string (str)
    (format str "c(")
    (loop for elt across obj 
	  for i from 1
	  if (> i 1) do (format str ",")
	  do (format str "~A" (lisp->r-object elt)))
    (format str ")")))

(defmethod lisp->r-object ((obj number))
  (let ((obj (if (integerp obj)
		 obj
		 (coerce obj 'float))))
    (format nil "~A" obj)))


