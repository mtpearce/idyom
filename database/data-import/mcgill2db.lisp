;;; =======================================================================
;;;; File:       mcgill2db.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-02-16 15:38:15 peter>                           
;;;; Time-stamp: <2017-05-23 21:31:38 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides methods for importing data from the McGill Billboard Corpus.
;;;;
;;;; One chord is produced for each beat in the bar.
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
;; line/token is encountered. If true, unrecognised lines/tokens throw an error.
;; If nil, the import process continues, but a warning is given
;; to the user afterwards.
(defparameter *stop-on-unrecognised-lines* t)
(defparameter *stop-on-unrecognised-tokens* t)

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

;;;======================
;;;* Global variables *
;;;======================

(defvar *unrecognised-lines* '())
(defvar *unrecognised-tokens* '())         ;list of unrecognised tokens
(defvar *file-number* 0)
(defvar *file-name* nil)
(defvar *line-number* 0)                   ;current line being parsed
(defvar *line* nil)
(defvar *lines* '())                       ;list of lines in the file being parsed

;;;======================
;;;* Dictionaries *
;;;======================

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
						      (utils:split-string
						       x " "))))))

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
					       :name "major-scale-degrees"
					       :type "csv")
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
					       :name "minor-scale-degrees"
					       :type "csv")
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

;;;======================
;;;* Regex matching *
;;;======================

(defparameter *token-regex-alist*
  (mapcar #'(lambda (x) 
              (list (cl-ppcre:create-scanner (car x) :single-line-mode t) 
                    (cadr x)))
          '(("^[A-G][#b]*:" chord-token)               
            ("^\\(?[0-9]+/[0-9]+\\)?$" metre-token)
	    ("^\\.$" repeat-chord-token)
	    ("^&pause$" pause-token)
	    ("^\\*$" complex-token)
	    ("^N$" null-token)
	    (".*" unrecognised-token))))

(defparameter *line-regex-alist*
  (mapcar #'(lambda (x) 
              (list (cl-ppcre:create-scanner (car x) :single-line-mode t) 
                    (cadr x)))
	  '(("^# title: " title-line)
	    ("^# artist: " artist-line)
	    ("^# tonic: " tonic-line)
	    ("^# metre: " metre-line)
	    ("^$" blank-line)
	    ("^[0-9e\\.-]+\\t" body-line)
	    (".*" unrecognised-line))))

(defun get-regexp-in-alist (string alist)
  "Returns the first entry in <alist> (whose keys are regular
   expressions) which matches <string>." 
  (second (assoc string alist :test #'(lambda (item patt) 
					(cl-ppcre:scan-to-strings patt item)))))
(defun get-token-type (string)
  "Returns the token type of <string> by searching in *token-regex-alist*.
   Note: only the first match is returned."
  (get-regexp-in-alist string *token-regex-alist*))

(defun get-line-type (string)
  "Returns the line type of <string> by searching in *line-regex-alist*.
   Note: only the first match is returned."
  (get-regexp-in-alist string *line-regex-alist*))

(defun string->token (string)
  "Takes a string and converts it to a token. Returns an error if the
   token is unrecognised."
  (let ((token-type (get-token-type string)))
    (make-instance token-type :text string)))

;;;======================
;;;* Classes *
;;;======================

(defclass reader ()
  ((title :accessor title :documentation "Title of the song")
   (artist :accessor artist :documentation "Artist of the song")
   (global-metre
    :initform nil :accessor global-metre
    :documentation "Persistent metre; appears in line comments")
   (local-metre
    :initform nil :accessor local-metre
    :documentation "Local metre; appears in individual bars")
   (envir
    :initarg :envir
    :accessor envir
    :documentation "List of current CHARM properties")
   (output
    :initform nil :accessor output
    :documentation "List of processed events accrued in reverse order")))

(defclass line ()
  ((text :initarg :text :accessor text)))

(defclass title-line (line)
  ((title :initarg :title :accessor title)))

(defclass artist-line (line)
  ((artist :initarg :artist :accessor artist)))

(defclass tonic-line (line)
  ((tonic :initarg :tonic :accessor tonic)))

(defclass metre-line (line)
  ((metre :initarg :metre :accessor metre)))

(defclass blank-line (line) ())

(defclass body-line (line)
  ((bars :initarg :bars :accessor bars :initform nil
	 :documentation "Ordered list of bars present in the line.")
   (num-reps :initarg :num-reps :accessor num-reps
	     :initform 1
	     :documentation "Number of times the line is to be played.")))

(defclass unrecognised-line (line) ())

(defclass bar ()
  ((text :initarg :text :accessor text)
   (metre :accessor metre :initform nil
	  :documentation "String describing metre, e.g. 4/4")
   (tokens :accessor tokens
	   :documentation "List of tokens in bar, without expansion.")))
  
(defclass token ()
  ((text :initarg :text :accessor text)))

;; Event tokens are tokens which correspond to finite periods of musical time.
;; The most common event token is the chord token. Chord tokens are transcribed
;; as CHARM events. The other main class of event token is the empty token, which
;; constitutes a family of event tokens that are transcribed as musical rests.
(defclass event-token (token) ())

;; Environment tokens are the complement of the set of event tokens within the
;; family of tokens. They correspond to tokens which do not correspond themselves
;; to finite periods of musical time. The main example of an environment token
;; is the metre token, which changes how event tokens are transcribed, but
;; does not itselt correspond to any time interval.
(defclass envir-token (token) ())

;; Unrecognised tokens are tokens not recognised by the parser.
(defclass unrecognised-token (token) ())

(defclass chord-token (event-token)
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

(defclass repeat-chord-token (event-token) ())

;; Empty tokens are tokens that are transcribed as musical rests,
;; even if they do not necessarily correspond directly to rests
;; in the original syntax definition.
(defclass empty-token (event-token) ())

(defclass complex-token (empty-token) ())
(defclass null-token (empty-token) ())
(defclass pause-token (empty-token) ())

(defclass metre-token (envir-token)
  ((numerator :initform nil)
   (denominator :initform nil)
   (num-beats-in-bar :accessor num-beats-in-bar :initform nil)
   (pulses :accessor pulses :initform nil)        
   (barlength :accessor barlength :initform nil)))


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

(defun process-files (file-or-dir directory &key (remove-duplicates t))
  "If <file-or-dir> is a directory all the files in that directory
   are converted. The file search is recursive, meaning that 
   subdirectories and subdirectories of subdirectories etc
   are searched. If <file-or-dir> is a filename that file is processed."
  (load-dictionaries)
  (setf *file-number* 0)
  (setf *file-name* nil)
  (setf *unrecognised-tokens* nil)
  (setf *unrecognised-lines* nil)
  (if directory
      (let* ((files (utils:recursively-list-files
		     file-or-dir
		     :extensions *input-file-extensions*))
	     (num-files (length files))
	     (converted-files nil)
	     (song-table (make-hash-table :test 'equal)))
	(utils:message (format nil "Converting ~A files..." num-files)
		       :detail 1)
	(utils:dolist-pb
	    (file files
		  (progn
		    (when remove-duplicates
		      (utils:message
		       (format nil "Retained ~A songs after removing duplicates."
			       (length converted-files))))
		    (reverse converted-files)))
	  (incf *file-number*)
	  (setf *file-name* file)
	  (utils:message (format nil "Converting file ~A out of ~A: ~A"
				 *file-number* num-files *file-name*)
			 :detail 3)
	  (let* ((result (process-file file))
		 (key (cons (string-downcase (cdr (assoc :title result)))
			    (string-downcase (cdr (assoc :artist result))))))
	    (when (or (not remove-duplicates)
		      (null (nth-value 1 (gethash key song-table))))
	      (push (cons (cdr (assoc :description result))
			  (cdr (assoc :events result)))
		    converted-files)
	      (setf (gethash key song-table) t)))))
      (progn
	(setf *file-number* 1)
	(setf *file-name* file-or-dir)
	(utils:message (format nil "Converting file: ~A" *file-name*)
		       :detail 1)
	(let* ((result (process-file file-or-dir)))
	  (cons (cdr (assoc :description result))
		(cdr (assoc :events result)))))))

(defun process-file (file-name)
  "Top level call to convert the file <file-name> to CHARM readable
   format using the default parameters."
  (let* ((raw-data (read-data file-name))
         (processed-data (process-data raw-data))
	 (title (title processed-data))
	 (artist (artist processed-data))
	 (description (format nil "~A: ~A"
			      (if artist artist "Unknown artist")
			      (if title title "Unknown title")))
	 (events (output processed-data)))
    (list (cons :description description)
	  (cons :events events)
	  (cons :title title)
	  (cons :artist artist))))

(defun process-data (raw-data)
  (labels ((fun (remaining-lines reader)
	     (if (null remaining-lines)
		 (progn
		   (setf (output reader)
			 (reverse (output reader)))
		   reader)
		 (progn
		   (setf *line-number* (first (car remaining-lines))
			 *line* (second (car remaining-lines)))
		   (fun (cdr remaining-lines)
			(process (interpret-line (second (car remaining-lines)))
				 reader))))))
    (fun raw-data (initialise-reader))))

(defun interpret-line (line)
  (let ((line-type (get-line-type line)))
    (make-instance line-type :text line)))

;;;================================
;;;* Reading data from file. * 
;;;================================

;; Much of this code is similar to kern2db.lisp,
;; and could be merged with it at some point.

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

;; This is already defined in utils? But without returning nil for a nil input.
(defun split-string (string separator)
  "Takes a <string> as input and returns a list of strings corresponding to each
   <separator> delimited sequence of characters in that string. If the input
   is null, then returns null."
  (if string
      (labels ((find-words (char-list word result)
		 (cond ((null char-list) (reverse (cons word result)))
		       ((not (string= (car char-list) separator))
			(find-words (cdr char-list)
				    (concatenate 'string word (list (car char-list)))
				    result))
		       (t (find-words (cdr char-list) "" (cons word result))))))
	(find-words (coerce string 'list) "" '()))))

;;;=======================
;;;* Errors and warnings * 
;;;=======================

(define-condition line-read-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format
	      stream
	      "Error parsing line ~A of file ~A.~%~A~%~%The line reads:~%~%~S"
	      *line-number*
	      *file-name*
	      (text condition)
	      *line*))))

(defun print-status ()
  "Print message warning about unrecognised tokens/lines."
  (unless (null *unrecognised-tokens*)
    (utils:message
     (format nil "~%The following tokens were unrecognised: ~S"
	     *unrecognised-tokens*)
     :detail 1))
  (unless (null *unrecognised-lines*)
    (utils:message
     (format nil "~%The following lines were unrecognised: ~S"
	     (mapcar #'text *unrecognised-lines*))
     :detail 1)))

;;;===========================
;;;* Initialising instances *
;;;===========================

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
  (make-instance 'reader :envir (initialise-envir)))

;;;========================
;;;* Initialising lines *
;;;========================

(defmethod initialize-instance :after ((line title-line) &key)
  (let* ((text (text line))
	 (title (cl-ppcre:regex-replace "^# title: " text
					""
					:preserve-case t)))
    (setf (title line) title)))

(defmethod initialize-instance :after ((line artist-line) &key)
  (let* ((text (text line))
	 (artist (cl-ppcre:regex-replace "^# artist: " text
					 ""
					 :preserve-case t)))
    (setf (artist line) artist)))

(defmethod initialize-instance :after ((line tonic-line) &key)
  (let* ((text (text line))
	 (tonic (cl-ppcre:regex-replace "^# tonic: " text
					""
					:preserve-case t)))
    (setf (tonic line) tonic)))

(defmethod initialize-instance :after ((line metre-line) &key)
  (let* ((text (text line))
	 (metre (cl-ppcre:regex-replace "^# metre: " text
					""
					:preserve-case t))
	 (metre-token (make-instance 'metre-token :text metre)))
    (setf (metre line) metre-token)))

(defmethod initialize-instance :after ((line body-line) &key)
  ;; A body line typically looks something like this:
  ;; 16.562811791	| D:maj/9 E:min | E:min C:maj |, (keyboard
  ;; Note other possibilities:
  ;; 0.000000000	silence
  ;; 8.753378684	| N | N | N | N |, (drums)
  (let* ((text (text line))
	 ;; Remove everything before and after the first
	 ;; and last | symbols
	 (bars-regex "\\|.*\\|")
	 (bars (cl-ppcre:scan-to-strings bars-regex text)))
    ;; If there are no bars, skip the line
    (if (null bars) (return-from initialize-instance))
    ;; Check that bars looks something like this:
    ;; "| Eb:7 | Eb:7 | Ab:maj | Ab:maj |"
    (if (not (cl-ppcre:scan-to-strings "(^\\| .* )+\\|$" bars))
	(error 'line-read-error "Found an unusual body line."))
    (let* (;; Get everything after the end of the last bar
	   (line-suffix (cl-ppcre:regex-replace ".*\\|" text ""
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
	   ;; Split the rest into bars
	   (bar-list (cl-ppcre:all-matches-as-strings "\\| [^\\|]*" bars))
	   ;; Trim bars
	   (bar-list (mapcar #'(lambda (x) (cl-ppcre:regex-replace-all
					    "(^\\| )|( $)" x ""))
			     bar-list))
	   ;; Initialize bar objects
	   (bar-list (mapcar #'(lambda (x) (make-instance 'bar :text x))
			     bar-list)))
      (setf (bars line) bar-list
	    (num-reps line) num-reps))))

;;;=======================
;;;* Initialising bars *
;;;=======================

(defmethod initialize-instance :after ((bar bar) &key)
  "Splits bar into tokens. Assumes that the | delimiters
   and surrounding whitespace have already been removed."
  (let* ((text (text bar))
	 (token-strings (utils:split-string text " "))
	 (tokens (mapcar #'string->token token-strings))
	 (token-types (mapcar #'type-of tokens))
	 (which-metre-tokens
	  (utils:all-positions-if
	   #'(lambda (x) (eql x 'metre-token))
	   token-types)))
    (if (some #'(lambda (x) (> x 0)) which-metre-tokens)
	(error 'line-read-error
	       :text "Encountered a metre token partway through a bar."))
    (if (eql (car token-types) 'metre-token)
	(setf (metre bar) (car tokens)))
    (setf (tokens bar) tokens)))

;;;========================
;;;* Initialising tokens *
;;;========================

(defmethod initialize-instance :after ((token metre-token) &key)
  "Abstracts information from a metre token.
   Note: there is an independent function, process-metre, that gets
   metre information from a metadata line."
  (let ((text (text token)))
    (if (not (eql (get-token-type text) 'metre-token))
	(error 'line-read-error
	       :text (format
		      nil
		      "Tried to parse an incorrectly formatted metre token: (~A)"
		      text)))
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

(defmethod initialize-instance :after ((token chord-token) &key)
  "Finds the cpitch representation for a chord from its textual representation."
  (let ((text (text token)))
    (setf (slot-value token 'cpitch)
	  (multiple-value-bind (root-token quality-base-token
					   quality-added-tokens bass-token)
	      (parse-chord-text text)
	    (parsed-chord->cpitch root-token quality-base-token
				  quality-added-tokens bass-token)))))

;;;=========================
;;;* Processing objects *
;;;=========================

(defgeneric process (object reader)
  (:documentation "Processes <object> and returns an updated <reader>."))

;;;=========================
;;;* Processing lines *
;;;=========================

(defmethod process ((line title-line) reader)
  (setf (title reader) (title line))
  reader)

(defmethod process ((line artist-line) reader)
  (setf (artist reader) (artist line))
  reader)

(defmethod process ((line tonic-line) reader)
  (setf (envir reader)
	(utils:update-alist (envir reader)
			    (list :tonic (process-pitch-class (tonic line)))))
  reader)

(defmethod process ((line metre-line) reader)
  (setf (global-metre reader) (metre line))
  reader)

(defmethod process ((line blank-line) reader)
  reader)

(defmethod process ((line unrecognised-line) reader)
  (if *stop-on-unrecognised-lines*
      (error 'line-read-error
	     :text "Did not recognise line.")
      (progn (pushnew line *unrecognised-lines*)
	     reader)))

(defmethod process ((line body-line) reader)
  (let ((num-reps (num-reps line))
	(bars (bars line)))
    (dotimes (i num-reps reader)
      (dolist (bar bars reader)
	(setf reader (process bar reader))))))

;;;=====================
;;;* Processing bars *
;;;=====================

(defmethod process ((bar bar) reader)
  (let ((local-metre (metre bar))
	(global-metre (global-metre reader)))
    (if (null global-metre)
	(error 'line-read-error
	       :text "No metre was defined before the first bar."))
    (setf (local-metre reader) local-metre)
    (setf reader (if local-metre
		     (process local-metre reader)
		     (process global-metre reader)))
    (let* ((num-beats-in-bar (num-beats-in-bar (if local-metre
						   local-metre
						   global-metre)))
	   (event-tokens (remove-if-not
			  #'(lambda (x) (typep x 'event-token))
			  (tokens bar)))
	   (event-tokens (expand-repeat-chord-tokens event-tokens))
	   (num-event-tokens (length event-tokens))
	   (event-tokens (cond
			   ((eql num-event-tokens 1)
			    (make-list num-beats-in-bar
				       :initial-element (car event-tokens)))
			   ((and (eql num-beats-in-bar 4)
				 (eql num-event-tokens 2))
			    (append
			     (make-list 2 :initial-element (first event-tokens))
			     (make-list 2 :initial-element (second event-tokens))))
			   (t event-tokens)))
	   (num-event-tokens (length event-tokens)))
      (if (not (eql num-beats-in-bar num-event-tokens))
	  (error 'line-read-error
		 :text "Metre incompatible with number of event tokens provided."))
      (dolist (event-token event-tokens reader)
	(setf reader (process event-token reader))))))

;;;========================
;;;* Processing tokens *
;;;========================
			   
(defmethod process ((token metre-token) reader)
  "Updates the envir slot of <reader> according to <token>.
   Does not change the global-metre or local-metre slots,
   of <reader>, which should instead be updated by the process-line
   and process-bar methods."
  (setf (envir reader)
	(utils:update-alist (envir reader)
			    (list :pulses (pulses token))
			    (list :barlength (barlength token))))
  reader)

(defmethod process ((token chord-token) reader)
  "Processes <token> and updates <reader>. Assumes that any implicit
   chord repeats have been expanded already, meaning that <chord>
   corresponds to exactly one beat in duration."
  (let* ((envir (envir reader))
	 (num-beats-in-bar (num-beats-in-bar reader))
	 (barlength (second (assoc :barlength envir)))
	 (dur (/ barlength num-beats-in-bar))
	 (cpitch (cpitch token))
	 (new-events
	  (mapcar #'(lambda (x) (append (list (list :dur dur)
					      (list :cpitch x))
					envir))
		  cpitch))
	 (old-onset (second (assoc :onset envir)))
	 (new-onset (+ old-onset dur))
	 (new-envir (utils:update-alist envir
					(list :onset new-onset)
					(list :bioi dur))))
    (setf (output reader) (append (reverse new-events)
				  (output reader))
	  (envir reader) new-envir)
    reader))

(defmethod process ((token empty-token) reader)
  "Processes an <empty-token> and updates <reader>. 
   Assumes that the duration of <empty-token> follows
   the same rules as durations of chord-tokens, though
   according to the McGill corpus specification this 
   is not always the case."
  (let* ((envir (envir reader))
	 (num-beats-in-bar (num-beats-in-bar reader))
	 (barlength (second (assoc :barlength envir)))
	 (dur (/ barlength num-beats-in-bar))
	 (old-deltast (second (assoc :deltast envir)))
	 (old-bioi (second (assoc :bioi envir)))
	 (old-onset (second (assoc :onset envir)))
	 (new-deltast (+ old-deltast dur))
	 (new-bioi (+ old-bioi dur))
	 (new-onset (+ old-onset dur))
	 (new-envir (utils:update-alist envir
					(list :onset new-onset)
					(list :bioi new-bioi)
					(list :deltast new-deltast))))
    (setf (envir reader) new-envir)
    reader))

(defmethod process ((token unrecognised-token) reader)
  (if *stop-on-unrecognised-tokens* 
      (error 'line-read-error
	     :text (format nil "Unrecognised token: ~A" token))
      (progn (pushnew token *unrecognised-tokens*)
	     reader)))

;;;=============================
;;;* Getting reader properties *
;;;=============================

(defgeneric num-beats-in-bar (object)
  (:documentation
   "Processes <object> and returns the current number of beats in a bar.
    Gets updated every time the reader begins processing a new bar,
    at the beginning of the process-bar method."))

(defmethod num-beats-in-bar ((reader reader))
  (let ((local-metre (local-metre reader))
	(global-metre (global-metre reader)))
    (if (null global-metre)
	(error 'line-read-error
	       :text "Global metre undefined."))
    (num-beats-in-bar (if local-metre
			  local-metre
			  global-metre))))
		      

;;;=========================
;;;* Supporting functions *
;;;=========================

(defun expand-repeat-chord-tokens (token-list)
  "Takes a list of tokens and replaces any repeat-chord-tokens
   with a copy of the most recent token that was not a 
   repeat-chord-token."
  (labels ((repeat-chord-token-p (token)
	     (typep token 'repeat-chord-token))
	   (fun (input accumulator)
	     (if (null input)
		 (reverse accumulator)
		 (if (repeat-chord-token-p (car input))
		     (if (null accumulator)
			 (error 'line-read-error
				:text "Bar cannot begin with \".\" symbol.")
			 (fun (cdr input)
			      (cons (car accumulator)
				    accumulator)))
		     (fun (cdr input)
			  (cons (car input) accumulator))))))
    (fun token-list nil)))

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

(defun parse-chord-text (text)
  "Parses the <text> for a chord token (note: assumes correct syntax).
   Based on the syntax used in the Harte et al. 2005 ISMIR paper."
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
	 (quality-token (cl-ppcre:regex-replace-all              ; e.g. maj(b5,b7)
			 slash-regex remaining-token
			 "" :preserve-case t))
	 (added-notes-regex "\\(.+\\)$")
	 (quality-base-token (cl-ppcre:regex-replace-all               ; e.g. maj
			      added-notes-regex quality-token
			      "" :preserve-case t))
	 (remaining-token (cl-ppcre:scan-to-strings              ; e.g. (b5,b7)
			   added-notes-regex quality-token))
	 (quality-added-token (cl-ppcre:regex-replace-all
			       "[\\(\\)]" remaining-token
			       "" :preserve-case t))
	 (quality-added-tokens (split-string quality-added-token ",")))
    (values root-token quality-base-token quality-added-tokens bass-token)))

(defun parsed-chord->cpitch (root-token quality-base-token quality-added-tokens bass-token)
  (let* ((cpitch-middle-c (nth 0 *middle-c*))
	 (root-pc (process-pitch-class root-token))
	 (bass-relative-pc (if bass-token
			       (bass-token->relative-pc
				bass-token quality-base-token)
			       0))
	 (bass-pc (mod (+ root-pc bass-relative-pc) 12))
	 (quality-base-relative-pc (quality-token->relative-pc quality-base-token))
	 (quality-added-relative-pc (mapcar #'scale-degree->relative-pc
					    quality-added-tokens))
	 (quality-relative-pc (remove-duplicates (append quality-base-relative-pc
							 quality-added-relative-pc)))
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

(defun bass-token->relative-pc (bass-token quality-base-token)
  "Finds the relative pitch class of a <bass-token>.
   Refers to <quality-token> to make a guess at 
   the accidental when none is provided (e.g. 
   min/3 is assumed to have a minor third as the root,
   whereas maj/3 is assumed to have a major third as 
   the root."
  (let* ((minor-chord? (cl-ppcre:scan-to-strings "^min" quality-base-token))
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

(defun scale-degree->relative-pc (string)
  "Maps a scale degree (as a string) to a relative pitch class
   (as an integer)."
  (multiple-value-bind (result result-found?)
      (gethash string *major-scale-degree-dictionary*)
    (if result-found?
	result
	(error 'line-read-error
	       :text (format nil "Unrecognised scale degree: ~A"
			     string)))))

(defun quality-token->relative-pc (quality-token)
  "Maps a <quality-token> to a list of relative pitch classes."
  (multiple-value-bind (result result-found?)
      (gethash quality-token *chord-quality-dictionary*)
    (if result-found?
	result
	(error 'line-read-error
	       :text (format nil "Unrecognised chord quality: ~A"
			     quality-token)))))
