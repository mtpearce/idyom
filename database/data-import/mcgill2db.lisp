;;; =======================================================================
;;;; File:       mcgill2db.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-02-16 15:38:15 peter>                           
;;;; Time-stamp: <2017-02-17 17:54:26 peter>                           
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

;; Whether or not to expand repeated lines.
(defparameter *expand-repeated-lines* t)

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
(defparameter *major-scale-degree-dictionary* nil)
(defparameter *minor-scale-degree-dictionary* nil)
(defparameter *num-beats-in-bar-dictionary* nil)

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

(defun load-major-scale-degree-dictionary ()
  "Loads a hash-table where keys are strings representing notated
   scale degrees, asssuming a major tonality, and values are integers
   corresponding to the pitch class of the scale degree, expressed
   relative to the root (so 0 is the root, 7 is a perfect fifth
   above the root, and so on)."
  (setf *major-scale-degree-dictionary* 
	(utils:csv->hash-table (merge-pathnames
				(make-pathname :directory
					       '(:relative "database"
						 "data-import" "dictionary")
					       :name "major-scale-degrees" :type "csv")
				cl-user::*idyom-code-root*)
			       :value-fun #'parse-integer)))

(defun load-minor-scale-degree-dictionary ()
  "Loads a hash-table where keys are strings representing notated
   scale degrees, asssuming a minor tonality, and values are integers
   corresponding to the pitch class of the scale degree, expressed
   relative to the root (so 0 is the root, 7 is a perfect fifth
   above the root, and so on)."
  (setf *minor-scale-degree-dictionary* 
	(utils:csv->hash-table (merge-pathnames
				(make-pathname :directory
					       '(:relative "database"
						 "data-import" "dictionary")
					       :name "minor-scale-degrees" :type "csv")
				cl-user::*idyom-code-root*)
			       :value-fun #'parse-integer)))

(defun load-num-beats-in-bar-dictionary ()
  "Loads a hash-table where keys are strings representing notated
   time signatures and values are integers corresponding to the number
   of beats in the bar."
  (setf *num-beats-in-bar-dictionary* 
	(utils:csv->hash-table (merge-pathnames
				(make-pathname :directory
					       '(:relative "database"
						 "data-import" "dictionary")
					       :name "num-beats-in-bar" :type "csv")
				cl-user::*idyom-code-root*)
			       :value-fun #'parse-integer)))

(defun load-dictionaries ()
  (load-chord-quality-dictionary)
  (load-major-scale-degree-dictionary)
  (load-minor-scale-degree-dictionary)
  (load-num-beats-in-bar-dictionary))

(load-dictionaries)

(defparameter *token-regex-alist*
  (mapcar #'(lambda (x) 
              (list (cl-ppcre:create-scanner (car x) :single-line-mode t) 
                    (cadr x)))
          '(("^[A-G][#b]*:" chord-token)               
            ("^\\(?[0-9]+/[0-9]+\\)?$" metre-token)
	    (".*" unrecognised-token))))

(defun get-token-type (string)
  "Returns the token type of <string> by searching in *token-regex-alist*.
   Note: only the first match is returned." 
  (second (assoc string *token-regex-alist*
		 :test #'(lambda (item patt) 
			   (cl-ppcre:scan-to-strings patt item)))))

;;;======================
;;;* Global variables *
;;;======================

(defvar *unrecognised-tokens* '())         ;list of unrecognised tokens
(defvar *file-number* 0)
(defvar *file-name* nil)
(defvar *line-number* 0)                   ;current line being parsed
(defvar *line* nil)
(defvar *lines* '())                       ;list of lines in the file being parsed

;;;======================
;;;* Objects etc. *
;;;======================

(defstruct reader title artist metre num-beats-in-bar envir output)

(defclass token ()
  ((text :initarg :text :accessor text)))

(defclass chord-token (token)
  ((cpitch
    :accessor cpitch
    :documentation "List of cpitch values that represent the pitch
   content of the chord. These cpitch values span from the C one
   octave below middle C to the B just under one octave above
   middle C. Exactly one cpitch value is below middle middle C,
   and corresponds to the bass note of the chord. The bass note
   is derived from the <bass-token> if one is provided; if the bass token
   is nil, then the bass note is assumed to be the root note. The remaining
   notes in the chord are mapped to pitches in the octave above middle C.
   If a pitch class appears in the bass, it is not repeated in the octave
   above middle C. This should be consistent across IDyOM import methods
   for chord sequences.")))

(defclass metre-token (token)
  ((numerator :accessor :numerator)
   (denominator :accessor :denominator)
   (num-beats-in-bar :accessor :num-beats-in-bar)
   (pulses :accessor pulses)         ; numerator of time signature
   (barlength :accessor barlength)))
						 
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
  (load-dictionaries)
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
         (processed-data (process-data raw-data))
	 (title (reader-title processed-data))
	 (artist (reader-artist processed-data))
	 (description (format nil "~A: ~A"
			      (if artist artist "Unknown artist")
			      (if title title "Unknown title")))
	 (events (reader-output processed-data)))
    (cons description events)))

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

;; This is already defined in utils?
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
		 (progn (setf (reader-output reader)
			      (reverse (reader-output reader)))
			reader)
		 ;;(reverse (reader-output reader))
		 (fun (cdr remaining-lines)
		      (process-line (car remaining-lines) reader)))))
    (fun raw-data (initialise-reader))))



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
  "Processes one <line> of metadata, returning an updated <reader>."
  ;; A typical line looks like this:
  ;; # title: Last Child
  (if (not (cl-ppcre:scan-to-strings
	    "^# .+: " line))
      (error 'line-read-error :text "Found an unusual comment line."))
  (let* ((no-comment (cl-ppcre:regex-replace-all "^# " line ""
						 :preserve-case t))
	 (type (cl-ppcre:regex-replace-all ":.*" no-comment ""
					   :preserve-case t))
	 (value (cl-ppcre:regex-replace-all "^[^:.]*: " no-comment ""
					    :preserve-case t)))
    (cond ((string= type "title") (process-title value reader))
	  ((string= type "artist") (process-artist value reader))
	  ((string= type "metre") (process-metre value reader))
	  ((string= type "tonic") (process-tonic value reader))
	  (t  (error 'line-read-error
		     :text (format nil
				   "Found unrecognised metadata type ~A."
				   type))))))

(defun process-title (value reader)
  (setf (reader-title reader) value)
  reader)

(defun process-artist (value reader)
  (setf (reader-artist reader) value)
  reader)

(defun process-tonic (value reader)
  (setf (reader-envir reader)
	(utils:update-alist (reader-envir reader)
			    (list :tonic (process-pitch-class value))))
  reader)

(defun process-metre (value reader)
  "Processes a metre string in a metadata line where the comment has been removed."
  (let ((metre-token (make-instance 'metre-token :text value)))
    (process-token metre-token reader)))

(defun process-pitch-class (string)
  (if (not (cl-ppcre:scan-to-strings
	    "^[A-G][#b]*$" string))
      (error 'line-read-error
	     :text (format nil "Found an unusual pitch string: ~A." string)))
  (let* ((num-sharps (length (cl-ppcre:scan-to-strings "[#]+" string)))
         (num-flats (length (cl-ppcre:scan-to-strings "[b]+" string)))
	 (letter (char string 0))
	 (letter-pc (case letter
		      (#\C 0)
		      (#\D 2)
		      (#\E 4)
		      (#\F 5)
		      (#\G 7)
		      (#\A 9)
		      (#\B 11)
		      (otherwise -99)))
	 (pc (- (+ letter-pc num-sharps) num-flats)))
    pc))

(defun process-body (line reader)
  "Processes one <line> of body, returning an updated <reader>."
  ;; A body line typically looks something like this:
  ;; 16.562811791	| D:maj/9 E:min | E:min C:maj |, (keyboard
  ;; Note other possibilities:
  ;; 0.000000000	silence
  ;; 8.753378684	| N | N | N | N |, (drums)
  (let* (;; Remove everything before and after the first
	 ;; and last | symbols
	 (bars-regex "\\|.*\\|")
	 (bars (cl-ppcre:scan-to-strings bars-regex line)))
    ;; If there are no bars, skip the line
    (if (null bars) (return-from process-body reader))
    ;; Check that bars looks something like this:
    ;; "| Eb:7 | Eb:7 | Ab:maj | Ab:maj |"
    (if (not (cl-ppcre:scan-to-strings "(^\\| .* )+\\|$" bars))
	(error 'line-read-error "Found an unusual body line."))
    (let* (;; Get everything after the end of the last bar
	   (line-suffix (cl-ppcre:regex-replace ".*\\|" line ""
						:preserve-case t))
	   ;; Find the number of marked repetitions, if any
	   ;; Repetition is defined inclusively, so two repetitions
	   ;; is equivalent to x2, i.e. play twice.
	   (rep-token (cl-ppcre:scan-to-strings "x[0-9]+"
						line-suffix))
	   (num-reps (if rep-token
			 (parse-integer (cl-ppcre:scan-to-strings "[0-9]+"
								  rep-token))
			 1))
	   ;; Split into bars
	   (bar-list (cl-ppcre:all-matches-as-strings "\\| [^\\|]*" bars))
	   ;; Trim bars
	   (trim-bar-list (mapcar #'(lambda (x) (cl-ppcre:regex-replace-all
						 "(^\\| )|( $)" x ""))
				  bar-list))
	   ;; Split each bar into a list of chords
	   (chord-list (mapcar #'(lambda (x) (utils:split-string x " "))
			       trim-bar-list)))
      
      
      (values chord-list num-reps reader))))

;; (defclass ())

;; (defun classify-token (token)
  
;;   )

;; (defun parse-token (token)
	       

      
;;;======================
;;;* Token matching  *
;;;======================

(defgeneric process-token (token reader)
  (:documentation "Processes a <token> and returns an updated <reader>."))

;;; Chord tokens

(defmethod initialize-instance :after ((token chord-token) &key)
  "Finds the cpitch representation for a chord from its textual representation."
  (let ((text (text token)))
    (if (not (eql (get-token-type text) 'chord-token))
	(error 'line-read-error
	       :text "Tried to parse an incorrectly formatted chord token."))
    (setf (slot-value token 'cpitch)
	  (multiple-value-bind (root-token quality-token bass-token)
	      (parse-chord-text text)
	    (parsed-chord->cpitch root-token quality-token bass-token)))))


(defun parse-chord-text (text)
  "Parses the <text> for a chord token (note: assumes correct syntax)."
  (let* ((root-regex "^[A-G][#b]*")
	 (root-token  (cl-ppcre:scan-to-strings
		       root-regex text))                         ; e.g. Db
	 (root-regex-2 "^[A-G][#b]*:")
	 (remaining-token (cl-ppcre:regex-replace-all            ; e.g. maj/b7
			   root-regex-2 text "" :preserve-case t))
	 (slash-regex "/[^/]+$")
	 (slash-token (cl-ppcre:scan-to-strings                  ; e.g. /b7
		       slash-regex remaining-token))             ; e.g. nil
	 (bass-token (if slash-token (subseq slash-token 1) nil)); e.g. b7
	 (quality-token (cl-ppcre:regex-replace-all              ; e.g. maj
			 slash-regex remaining-token
			 "" :preserve-case t)))
    (values root-token quality-token bass-token)))

;; This function is similar to one in kern2db.lisp.
(defun parsed-chord->cpitch (root-token quality-token bass-token)
  (let* ((cpitch-middle-c (nth 0 *middle-c*))
	 (root-pc (process-pitch-class root-token))
	 (bass-relative-pc (if bass-token
			       (bass-token->relative-pc
				bass-token quality-token)
			       0))
	 (bass-pc (mod (+ root-pc bass-relative-pc) 12))
	 (quality-relative-pc
	  (multiple-value-bind (result result-found?)
	      (gethash quality-token *chord-quality-dictionary*)
	    (if result-found?
		result
		(error 'line-read-error
		       :text (format nil "Unrecognised chord quality: ~A"
				     quality-token)))))
	 (quality-absolute-pc (mapcar #'(lambda (x)
					  (mod (+ x root-pc) 12))
				      quality-relative-pc))
	 (non-bass-pc (remove-duplicates 
		       (remove-if #'(lambda (x) (eql x bass-pc))
				  (cons root-pc quality-absolute-pc))))
	 (non-bass-cpitch (mapcar #'(lambda (x) (+ x cpitch-middle-c))
				  non-bass-pc))
	 (bass-cpitch (- (+ bass-pc cpitch-middle-c) 12))
	 (cpitch (cons bass-cpitch non-bass-cpitch))
	 (cpitch (sort cpitch #'<)))
    cpitch))

(defun bass-token->relative-pc (bass-token quality-token)
  (let* ((minor-chord? (cl-ppcre:scan-to-strings "^min" quality-token))
	 (dictionary (if minor-chord?
			 *minor-scale-degree-dictionary*
			 *major-scale-degree-dictionary*)))
    (multiple-value-bind (result result-found?)
	      (gethash bass-token dictionary)
	    (if result-found?
		result
		(error 'line-read-error
		       :text (format nil "Unrecognised bass token: ~A"
				     bass-token))))))
    
;;; Metre tokens

(defmethod initialize-instance :after ((token metre-token) &key)
  "Abstracts information from a metre token.
   Note: there is an independent function, process-metre, that gets
   metre information from a metadata line."
  (let ((text (text token)))
    (if (not (eql (get-token-type text) 'metre-token))
	(error 'line-read-error
	       :text "Tried to parse an incorrectly formatted metre token."))
    (let* ((no-brackets (cl-ppcre:regex-replace-all "[\\(\\)]" text ""))
	   (numerator (parse-integer
		       (cl-ppcre:regex-replace-all "/[0-9]+$" no-brackets "")))
	   (denominator (parse-integer
			 (cl-ppcre:regex-replace-all "^[0-9]+/" no-brackets "")))
	   (barlength (* (/ *default-timebase* denominator)
			 numerator))
	   (num-beats-in-bar (multiple-value-bind (result result-found?)
				 (gethash no-brackets *num-beats-in-bar-dictionary*)
			       (if result-found?
				   result
				   (error 'line-read-error
					  :text (format nil "Didn't know how many beats in the bar for ~A."
							no-brackets))))))
      (setf (slot-value token 'numerator) numerator
	    (slot-value token 'denominator) denominator
	    (slot-value token 'pulses) numerator
	    (slot-value token 'barlength) barlength
	    (slot-value token 'num-beats-in-bar) num-beats-in-bar))))

(defmethod process-token ((token metre-token) reader)
  (setf (reader-envir reader)
	(utils:update-alist (reader-envir reader)
			    (list :pulses (pulses token))
			    (list :barlength (barlength token))))
  (setf (reader-metre reader) value)
  (setf (reader-num-beats-in-bar reader) (num-beats-in-bar token))
  token)
