;;; =======================================================================
;;;; File:       mcgill2db.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-02-16 15:38:15 peter>                           
;;;; Time-stamp: <2017-02-16 20:26:26 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides methods for importing data from the McGill Billboard Corpus.
;;;;
;;;; Todo (features) ======================================================
;;;; ======================================================================

;;;; Todo (coding) ========================================================
;;;; ======================================================================

(cl:in-package #:mcgill2db)

;;;==================
;;;* User parameters *
;;;==================

;; This parameter determines whether to stop when an unrecognised
;; token is encountered. If true, unrecognised tokens throw an error.
;; If nil, the import process continues, but a warning is given
;; to the user afterwards.
(defparameter *stop-on-unrecognised-tokens* nil)

(defparameter *default-timebase* 96)    ;basic time units in a semibreve 
(defparameter *middle-c* '(60 35))      ;pitch mapping for middle c
(defparameter *default-onset* 0)        ;initial onset

(defparameter *default-timesig* '(nil nil))   ;default time signature
(defparameter *default-keysig* nil)           ;no. of sharps in keysig
(defparameter *default-tonic* nil)            ;default tonic
(defparameter *default-mode* nil)             ;0 major - 9 minor
(defparameter *default-tempo* nil)            ;default tempo/bpm
(defparameter *default-bioi* 0)               ;default inter-onset interval
(defparameter *default-instrument* nil)       ;default instrument
(defparameter *default-instrument-class* nil) ;default instrument class
(defparameter *default-instrument-group* nil) ;default instrument group
(defparameter *default-voice* 1)            ;default instrument group
(defparameter *default-subvoice* '(1))         ;default subvoice
(defparameter *default-pulses* nil)           ;default pulses
(defparameter *default-barlength* nil)        ;default barlength

;;;======================
;;;* Global constants *
;;;======================

(defvar *eof* (list 'eof))
(defvar *input-file-extensions* (list "txt"))

(defparameter *keysig-dictionary*
  ;; This is a hash-table where keys are strings corresponding to musical keys
  ;; and values are integers corresponding to the number of sharps (positive
  ;; integers) or flats (negative integers) for that key. 
  (utils:csv->hash-table (merge-pathnames
			  (make-pathname :directory
					 '(:relative "database"
					   "data-import" "dictionary")
					 :name "key-signatures" :type "csv")
			  cl-user::*idyom-code-root*)
			 :value-fun #'parse-integer))

(defparameter *chord-quality-dictionary* nil)

(defun load-chord-quality-dictionary ()
  "Loads a hash-table where keys are strings representing notated
   musical chords and values are lists of integers corresponding
   to the pitch-class sets implied by these chords, expressed
   relative to the root (so 0 is the root, 7 is a perfect fifth
   above the root, and so on)."
  (setf *chord-quality-dictionary* 
	(utils:csv->hash-table (merge-pathnames
				(make-pathname :directory
					       '(:relative "database"
						 "data-import" "dictionary")
					       :name "chord-qualities" :type "csv")
				cl-user::*idyom-code-root*)
			       :value-fun #'(lambda (x)
					      (mapcar #'parse-integer
						      (utils:split-string x " "))))))
(load-chord-quality-dictionary)

;;;======================
;;;* Global variables *
;;;======================

(defvar *unrecognised-tokens* '())         ;list of unrecognised tokens
(defvar *file-number* 0)
(defvar *file-name* nil)
(defvar *line-number* 0)                   ;current line being parsed
(defvar *line* nil)
(defvar *lines* '())                       ;list of lines in the file being parsed

;;;==================
;;;* Top level call *
;;;==================

(defmethod import-data ((type (eql :mcgill)) path description id)
  (idyom-db:insert-dataset (mcgill2db path description) id))

(defun mcgill2db (file-or-dir-name description
                &key (timesig *default-timesig*)
                  (keysig *default-keysig*)
		  (tonic *default-tonic*)
                  (mode *default-mode*)
                  (timebase *default-timebase*)
                  (onset *default-onset*)
                  (tempo *default-tempo*)
                  (bioi *default-bioi*)
                  (middle-c *middle-c*))
  "A top level call to convert a kern file or a directory of kern files
   <file-or-dir-name> to CHARM readable format. The keyword parameters
   allow the user to change the default parameters for the conversion."
  (setq *default-timesig* timesig
        *default-keysig* keysig
	*default-tonic* tonic
        *default-mode* mode
        *default-timebase* timebase
        *default-onset* onset
        *default-tempo* tempo
        *default-bioi* bioi
        *middle-c* middle-c
        *unrecognised-tokens* '())
  (let ((directory (not (pathname-name file-or-dir-name))))
    (prog1 
        (append (list description *default-timebase* (car *middle-c*))
                (process-files file-or-dir-name directory))
      (print-status))))

(defun process-files (file-or-dir directory)
  "If <file-or-dir> is a directory all the files in that directory
   are converted -- if it is a filename that file is processed."
  (load-chord-quality-dictionary)
  (setf *file-number* 0)
  (setf *file-name* nil)
  (if directory
      (let* ((files (utils:recursively-list-files
		     file-or-dir
		     :extensions *input-file-extensions*))
	     (num-files (length files))
	     (converted-files nil))
	(utils:message (format nil "Converting ~A files..." num-files)
		       :detail 1)
	(utils:dolist-pb (file files (reverse converted-files))
	  (incf *file-number*)
	  (setf *file-name* file)
	  (utils:message (format nil "Converting file ~A out of ~A: ~A"
				 *file-number* num-files *file-name*)
			 :detail 3)
	  (push (process-file file) converted-files)))
      (progn
	(setf *file-number* 1)
	(setf *file-name* file-or-dir)
	(utils:message (format nil "Converting file: ~A" *file-name*)
		       :detail 1)
	(list (process-file file-or-dir)))))

(defun process-file (file-name)
  "Top level call to convert the file <file-name> to CHARM readable
   format using the default parameters."
  (let* ((raw-data (read-data file-name))
         (processed-data (process-data raw-data)))
    (cons (pathname-name file-name) processed-data)))

(defun print-status ()
  "Print message warning about unrecognised tokens."
  (unless (null *unrecognised-tokens*)
    (utils:message
     (format nil "~%The following tokens were unrecognised: ~S"
	     *unrecognised-tokens*)
     :detail 1)))

;;;================================
;;;* Reading data from file. * 
;;;================================

;; Much of this code is similar to kern2db.lisp, and could be merged.

(defun read-data (file-name)
  "Reads data from a text file, returning a numbered
   list of records, where each record has been split
   into a list of its component tokens."
  (setf *lines* (reverse (get-lines file-name)))
  (remove-empty-strings *lines*))

(defun get-lines (file-name)
  "Returns a numbered list of strings corresponding to the lines in <file-name>.
   Each line in a kern file is a distinct record of musical events."
  (let ((lines '())
	(line-count 0))
    (map-file-objects
     #'(lambda (line) (setf lines (cons (list (incf line-count) line)
					lines)))
     file-name)
    lines))

(defun map-stream-objects (function stream)
  "Applies <function> to lines read from <stream>."
  (loop (let ((input (read-line stream nil *eof*)))
          (if (eql input *eof*)
              (return nil)
              (funcall function input)))))

(defun map-file-objects (function file-name)
  "Creates a stream associated with <file-name> which it
   passess to map-stream-objects."
  (with-open-stream (s (open file-name :direction :input 
                             :external-format :iso-8859-1))
    (map-stream-objects function s)))

(defun get-line (line-number)
  "Finds the line with number <line-number> from the list of lines <*lines*> 
   originally returned by the function <get-lines>."
  (second (nth (1- line-number) *lines*)))

(defun remove-empty-strings (list)
  "Removes empty strings from a numbered list of strings."
  (remove-if #'(lambda (x) (string= (second x)  "")) list))

(defun split-string (string separator)
  "Takes a string object and returns a list of strings corresponding to each
   <separator> delimited sequence of characters in that string."
  (labels ((find-words (char-list word result)
             (cond ((null char-list) (reverse (cons word result)))
                   ((not (string= (car char-list) separator))
                    (find-words (cdr char-list)
                                (concatenate 'string word (list (car char-list)))
                                result))
                   (t (find-words (cdr char-list) "" (cons word result))))))
    (find-words (coerce string 'list) "" '())))

;;;====================
;;;* Processing data. * 
;;;====================

(define-condition line-read-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream
		     "Error parsing line ~A of file ~A.~%~A~%~%The line reads:~%~%~S"
		     *line-number*
		     *file-name*
		     (text condition)
		     *line*))))

(defun process-data (raw-data)
  (labels ((fun (remaining-lines reader)
	     (if (null remaining-lines)
		 (reverse (reader-output reader))
		 (fun (cdr remaining-lines)
		      (process-line (car remaining-lines) reader)))))
    (fun raw-data (initialise-reader))))

(defstruct reader title artist metre envir output)

(defun initialise-envir ()
  (list (list :onset *default-onset*)
	(list :bioi *default-bioi*)
	(list :keysig *default-keysig*)
	(list :tonic *default-tonic*)
	(list :mode *default-mode*)
	(list :pulses *default-pulses*)
	(list :barlength *default-barlength*)
	(list :timebase *default-timebase*)
	(list :tempo *default-tempo*)
	(list :deltast *default-onset*)
	(list :phrase 0)
	(list :instrument *default-instrument*)
	(list :instrument-class *default-instrument-class*)
	(list :instrument-group *default-instrument-group*)
	(list :voice *default-voice*)
	(list :subvoice *default-subvoice*)))

(defun initialise-reader ()
  (make-reader :envir (initialise-envir)))

(defun process-line (line reader)
  (setf *line-number* (first line)
	*line* (second line))
  (if (char-equal (char *line* 0) #\#)
      (process-metadata *line* reader)
      (process-body *line* reader)))

(defun process-metadata (line reader)
  (if (not (cl-ppcre:scan-to-strings
	    "^# .+: " line))
      (error 'line-read-error :text "Found an unusual comment line."))
  (let* ((no-comment (cl-ppcre:regex-replace-all "^# " line "" :preserve-case t))
	 (type (cl-ppcre:regex-replace-all ":.*" no-comment "" :preserve-case t))
	 (value (cl-ppcre:regex-replace-all "^.*: " no-comment "" :preserve-case t)))
    (cond ((string= type "title") (process-title value reader))
	  ((string= type "artist") (process-artist value reader))
	  ((string= type "metre") (process-metre value reader))
	  ((string= type "tonic") (process-tonic value reader))
	  (t (progn (error 'line-read-error
			   :text (format nil "Found unrecognised metadata type ~A." type))
		    reader)))))

(defun process-title (value reader)
  (setf (reader-title reader) value)
  reader)

(defun process-artist (value reader)
  (setf (reader-artist reader) value)
  reader)

(defun process-tonic (value reader)
  (setf (reader-tonic reader) value)
  reader)

(defun process-metre (value reader)
  (if (not (cl-ppcre:scan-to-strings
	    "^[0-9]+/[0-9]+$" value))
      (error 'line-read-error :text "Found an unusual metre."))
  (let* ((numerator (parse-integer
		     (cl-ppcre:regex-replace-all "/[0-9]+$" value "")))
	 (denominator (parse-integer
		       (cl-ppcre:regex-replace-all "^[0-9]+/" value "")))
	 (barlength (* (/ *default-timebase* denominator)
		       numerator)))
    (setf (reader-envir reader)
	  (utils:update-alist (reader-envir reader)
			      (list :pulses numerator)
			      (list :barlength barlength)))
    reader))
