;;;; =======================================================================
;;;; File:       kern2db.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2002-05-03 18:54:17 marcusp>                           
;;;; Time-stamp: <2017-02-07 14:10:05 peter>                           
;;;; =======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; kern2db converts kern data into a format suitable for reading
;;;; into the database. Each file is converted to a list of events where
;;;; each event is a list of attribute values:
;;;;
;;;;    (<onset> <cpitch> <mpitch> <dur> <keysig> <mode> <barlength>
;;;;     <phrase> <voice>)
;;;; 
;;;; where: <onset>      is an integer using *default-onset* as the onset of
;;;;                     of the start of the piece and *default-timebase* as
;;;;                     the unit granularity: 8=crotchets; 16=semiquavers
;;;;                     etc.
;;;;        <cpitch>    is an integer representing chromatic pitch using
;;;;                     (nth 0 *middle-c) as middle c.
;;;;        <mpitch>    is an integer representing morphetic (diatonic)
;;;;                     pitch using (nth 1 *middle-c*) as middle-c. 
;;;;        <dur>        is an integer representing the number of time-units
;;;;                     that the event lasts. 
;;;;        <keysig>     the key signature is represented by a number of
;;;;                     sharps (positive integers) or flats (negative
;;;;                     integers). 
;;;;        <mode>       the mode is represented by an integer -- 0 for
;;;;                     major and 9 for minor. 
;;;;        <barlength>    is an integer specifying the number of time units
;;;;                     in a bar.
;;;;        <pulses>     is an integer representing the number of metric
;;;;                     pulses in a bar (the numerator of the time
;;;;                     signature). 
;;;;        <phrase>     is 1 if an event is the first in a phrase, -1 if
;;;;                     it is the last in the phrase and 0 otherwise.
;;;;        <voice>      is an integer representing the voice in which the
;;;;                     event occurs -- voices are represented by the
;;;;                     positive integers upwards of 1 which represents
;;;;                     the left-most spine in the kern file.
;;;;
;;;; Note that all these values are derived directly from a score-like
;;;; representation (not a performance), rests are not explicitly encoded
;;;; and repeated sections are not explicitly expanded. 
;;;;
;;;; Todo =================================================================
;;;; ======================================================================
;;;;
;;;; 1. take account of spine path tokens (*+ *- *^ *v *x) when
;;;;    converting records to spines.
;;;; 2. process representations other than **kern (e.g, **dyn, **dur,
;;;;    **harm etc.)
;;;; 3. deal with other event tokens
;;;; 4. deal with nested or overlapping phrases.
;;;; 5. explicitly expand repeated sections
;;;;
;;;; =======================================================================

(cl:in-package #:kern2db)

(defvar *eof* (list 'eof))
(defvar *input-file-extension* ".krn")
(defvar *kern-spine-separator* #\Tab)

(defvar *kern-token-alist*
  (mapcar #'(lambda (x) 
              (list (cl-ppcre:create-scanner (car x) :single-line-mode t) 
                    (cadr x)))
          '(("^\\.$" ignore-token)                 ;null tokens
            ("^!" ignore-token)                    ;in-line comments 
            ("^\\*$" ignore-token)                 ;null interpretations
            ("^[^!*=].*[qQ]" ignore-token)         ;grace notes/groupettos 
            ("^\\*\\|\\." ignore-token)            ;staff lining  
            ("^\\*staff" ignore-token)             ;staff position 
            ("^\\*clef" ignore-token)              ;clef info
	    ("^\\*ITr" ignore-token)               ;transposed instruments
	    ("^\\*8|^\\*Tr]" ignore-token)         ;transposition token   
	    ("^\\*>.*\\[" ignore-token)            ;section expansion lists
	    ("^\\*>[^\\[]+" ignore-token)          ;section labels
	    ("^\\*tb" ignore-token)                ;timebase token
	    ("^\\*[+^vx-]" spine-path-token)       ;spine path tokens
	    ("^\\*\\*" excl-interpret-token)       ;exclusive interpretation
	    ("^\\*MM[0-9]" tempo)                  ;tempo token
            ("^\\*MM\\[" tempo)                    ;tempo token
	    ("^\\*I[^TGC]" instrument)             ;instrument token
	    ("^\\*IC" instrument-class)            ;instrument class
            ("^\\*IG" instrument-group)            ;instrument group
            ("^\\*k\\[" keysig)                    ;keysig token
            ("^\\*[a-gA-G?X]" mode)                ;mode token
            ("^\\*M(FREI)?[0-9?XZ]" timesig)       ;timesig token
	     ("^=[a-z;=|!'`:-]*1[a-z;=|!'`:-]*$"   ;first barline
	      first-barline)     
            ("^=" ignore-token)                    ;ignore the other barlines 
            ("^[^!=.*].*r" musical-rest)           ;rests 
            ("^[^!=.*].* .+" chord)                ;chords
            ("^[^!=.*]" kern-event))))             ;events   

(defvar *unrecognised-representations* '())
(defvar *unrecognised-tokens* '())
(defvar *voices* '())
(defvar *voice-counter* 0)
(defvar *line-number* 0)
(defvar *lines* '())
(defvar *first-barline-reached* nil)
(defvar *onset-correction* 0)
(defvar *ties* nil)
(defvar *record-onsets* nil)

(defparameter *default-timebase* 96)    ;basic time units in a semibreve 
(defparameter *middle-c* '(60 35))      ;pitch mapping for middle c
(defparameter *spines* '(1))            ;spines to convert (nil = all spines)
(defparameter *default-onset* 0)        ;initial onset  
(defparameter *default-pause* 1)        ;initial pause off

(defparameter *default-timesig* '(nil nil))   ;default time signature
(defparameter *default-keysig* nil)           ;no. of sharps in keysig
(defparameter *default-mode* nil)             ;0 major - 9 minor
(defparameter *default-tempo* nil)            ;default tempo/bpm
(defparameter *default-bioi* 0)               ;default inter-onset interval
(defparameter *default-instrument* nil)       ;default instrument
(defparameter *default-instrument-class* nil) ;default instrument class
(defparameter *default-instrument-group* nil) ;default instrument group

;; This parameter determines whether onsets should be corrected so that
;; an onset of 0 corresponds to the beginning of the first complete bar
;; in the piece, with anacruses being imaginarily extended to the
;; length of a full bar (see Pearce, 2005 for more details).
(defparameter *correct-onsets-to-first-barline* t)

;; This parameter determines whether ties are allowed to cross voices.
(defparameter *ties-may-cross-voices* t)

;; This parameter determines whether ties are allowed to cross subvoices
;; (ignored if *ties-may-cross-voices* is true).
(defparameter *ties-may-cross-subvoices* t)




;;;==================
;;;* Structures *
;;;==================

(defstruct humdrum-state
  excl-interpret    ;; exclusive interpretation, e.g. **kern, **dyn, ...
  environment       ;; e.g. time signature, mode, ...
  cued-for-join     ;; boolean, concerns spine paths
  cued-for-exchange ;; boolean, concerns spine paths
  tied-events)      ;; list of "tie-markers", each corresponding to an unclosed tie

(defstruct tie-marker
  cpitch               ;; chromatic pitch of the tied note
  closed               ;; boolean: whether an end has been found for this tie
  voice
  subvoice
  tokens               ;; tokens that formed part of the tie
  attach-onsets        ;; list of onsets at which future notes can join the tie
  position)            ;; ordinal position of the processed event corresponding
;; to this tied note in <processed-events>, 1-indexed once <processed-events>
;; has been reversed so that earlier elements of the list correspond to earlier
;; temporal positions.


;;;==================
;;;* Top level call *
;;;==================

(defmethod import-data ((type (eql :krn)) path description id)
  (idyom-db:insert-dataset (kern2db path description) id))

(defun kern2db (file-or-dir-name description
                &key (timesig *default-timesig*)
                  (keysig *default-keysig*)
                  (mode *default-mode*)
                  (timebase *default-timebase*)
                  (onset *default-onset*)
                  (pause *default-pause*)
                  (tempo *default-tempo*)
                  (bioi *default-bioi*)
                  (middle-c *middle-c*))
  "A top level call to convert a kern file or a directory of kern files
   <file-or-dir-name> to CHARM readable format. The keyword parameters
   allow the user to change the default parameters for the conversion."
  (setq *default-timesig* timesig
        *default-keysig* keysig
        *default-mode* mode
        *default-timebase* timebase
        *default-onset* onset
        *default-pause* pause
        *default-tempo* tempo
        *default-bioi* bioi
        *middle-c* middle-c
        *unrecognised-representations* '()
        *unrecognised-tokens* '())
  (let ((directory (not (pathname-name file-or-dir-name))))
    (prog1 
        (append (list description *default-timebase* (car *middle-c*))
                (process-data file-or-dir-name directory))
      (print-status))))

(defun process-data (file-or-dir directory)
  "If <file-or-dir-name> is a directory all the files in that directory
   are converted -- if it is a filename that file is processed."
  (if directory
      (mapcar #'(lambda (file-name) (convert-kern-file file-name))
              (directory (concatenate 'string
                                      (directory-namestring file-or-dir)
                                      "*" *input-file-extension*)))
      (list (convert-kern-file file-or-dir))))

(defun convert-kern-file (file-name)
  "Top level call to convert the kern file <file-name> to CHARM readable
   format using the default parameters."
  (let* ((kern-data (read-kern-data file-name))
         (processed-data (process-kern-data kern-data)))
    (cons (pathname-name file-name) processed-data)))

(defun print-status ()
  "Print message warning about unrecognised representations or tokens."
  (unless (null *unrecognised-representations*)
    (format t "~%The following representations were unrecognised: ~S"
            *unrecognised-representations*))
  (unless (null *unrecognised-tokens*)
    (format t "~%The following tokens were unrecognised: ~S"
            *unrecognised-tokens*)))


;;;===========================
;;;* Recognising kern tokens *
;;;===========================

(defun kern-p (x)
  (string= x "**kern"))

(defun pause-p (token)
  (cl-ppcre:scan-to-strings ";" token))

(defun end-of-phrase-p (token)
  (cl-ppcre:scan-to-strings "}" token))

(defun start-of-phrase-p (token)
  (cl-ppcre:scan-to-strings "{" token))

(defun line-comment-p (string)
  (cl-ppcre:scan-to-strings "^!!" string))

(defun open-tie-p (kern-event)
  "Returns whether a given <kern-event> token opens a tie.
   Assumes that it has already been determined that the 
   token is a kern-event and not, say, a key signature."
  (cl-ppcre:scan-to-strings "\\[" kern-event))

(defun middle-tie-p (kern-event)
  "Returns whether a given <kern-event> token opens a tie.
   Assumes that it has already been determined that the 
   token is a kern-event and not, say, a key signature."
  (cl-ppcre:scan-to-strings "\\_" kern-event))

(defun close-tie-p (kern-event)
  "Returns whether a given <kern-event> token opens a tie.
   Assumes that it has already been determined that the 
   token is a kern-event and not, say, a key signature."
  (cl-ppcre:scan-to-strings "\\]" kern-event))

(defun chord-open-tie-p (chord)
  "Returns whether a given <chord> token opens a tie.
   Assumes that it has already been determined that the 
   token is a chord and not, say, a key signature."
  (mapcar #'open-tie-p (split-string chord " ")))

(defun chord-close-tie-p (chord)
  "Returns whether a given <chord> token closes a tie.
   Assumes that it has already been determined that the 
   token is a chord and not, say, a key signature."
  (mapcar #'close-tie-p (split-string chord " ")))


;;;================================
;;;* Reading kern data from file. * 
;;;================================


(defun read-kern-data (file-name)
  "Converts a kern file into a list of lists corresponding to each spine in
   the file. Elements in the spine lists are strings corresponding to each
   element in the spine."
  (setf *lines* (reverse (get-lines file-name)))
  (let* ((records (remove-empty-strings *lines*))
	 (records (remove-line-comments records)))
    (mapcar #'(lambda (record)
		(setf (second record) (split-string
				       (second record)
				       *kern-spine-separator*))
		record)
	    records)))

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

(defun get-lines (file-name)
  "Returns a list of strings corresponding to the lines in <file-name>. Each
   line in a kern file is a distinct record of musical events."
  (let ((lines '())
	(line-count 0))
    (map-file-objects
     #'(lambda (line) (setf lines (cons (list (incf line-count) line)
					lines)))
     file-name)
    lines))

(defun get-line (line-number)
  "Finds the line with number <line-number> from the list of lines <*lines*> 
   originally returned by the function <get-lines>."
  (second (nth (1- line-number) *lines*)))

(defun remove-empty-strings (list)
  "Removes empty strings from a numbered list of strings."
  (remove-if #'(lambda (x) (string= (second x)  "")) list))

(defun remove-line-comments (list)
  "Removes line comments from a list of strings."
  (remove-if #'(lambda (x) (line-comment-p (second x))) list))

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


;;;=====================================
;;;* Processing the recorded kern data *
;;;=====================================


(defun process-kern-data (records)
  "Converts the recorded kern data into a CHARM readable format."
  (let ((interpretations (car records))
	(records-to-parse (cdr records))
	(processed-events nil))
    (check-interpretations interpretations)
    (setf *first-barline-reached* nil)
    (setf *ties* nil)
    (let ((humdrum-states (initialise-humdrum-states
			   interpretations))
	  (next-humdrum-states nil))
      ;; Iterate over records
      (dolist (numbered-record records-to-parse)
	(let ((record (second numbered-record)))
	  (setf *line-number* (first numbered-record))
	  (setf *record-onsets* nil)
	  ;;(format t "~%Line number: ~A~%" *line-number*)
	  ;;(format t "Record: ~A~%" record)
	  ;;(format t "Ties present at beginning of record: ~A~%" *ties*)
	  ;;(format t "Processed events at beginning of record: ~A~%" processed-events)
	  (check-num-tokens humdrum-states record)
	  ;; Iterate over states/tokens	
	  (dotimes (i (length humdrum-states))
	    (let* ((humdrum-state (nth i humdrum-states))
		   (kern-token (nth i record))
		   (regexp-match (get-regexp-in-alist kern-token
						      *kern-token-alist*))
		   (kern-token-type (if (null regexp-match)
					nil
					(cadr regexp-match)))
		   (excl-interpret (humdrum-state-excl-interpret
				    humdrum-state)))
	      (check-token-type humdrum-state kern-token-type)
	      (multiple-value-bind (new-humdrum-states new-processed-events)
		  (case kern-token-type
		    (spine-path (process-spine-path
				 kern-token humdrum-state processed-events))

		    (otherwise
		     (cond
		       ((string= excl-interpret "**kern")
			(process-kern-token humdrum-state processed-events
					    kern-token kern-token-type))
		       (t (values humdrum-state processed-events)))))
		(setf next-humdrum-states (append new-humdrum-states
						  next-humdrum-states))
		;;(format t "New processed events after state ~A: ~A~%"
		;;	i new-processed-events)
		(setf processed-events new-processed-events))))
	  ;;(format t "Processed events: ~A~%" processed-events)
	  ;;(format t "Record onsets: ~A~%" *record-onsets*)
	  (check-record-onsets *record-onsets*)
	  (setf *ties* (check-ties *ties* *record-onsets* processed-events))
	  ;;(format t "Processed events: ~A~%" processed-events)
	  
	  (setf next-humdrum-states (join-states next-humdrum-states))
	  (setf next-humdrum-states (exchange-states next-humdrum-states))
	  (setf humdrum-states (reverse next-humdrum-states)
		next-humdrum-states nil))))
    (if *correct-onsets-to-first-barline*
	(setf processed-events (correct-onsets processed-events
					       *onset-correction*)))
    (reverse processed-events)))

(defun process-kern-token
    (humdrum-state processed-events kern-token kern-token-type)
  (case kern-token-type
    (ignore-token (process-ignore-token
		   humdrum-state processed-events))
    (excl-interpret-token (process-excl-interpret-token
			   kern-token humdrum-state
			   processed-events))
    (spine-path-token (process-spine-path
		       kern-token humdrum-state processed-events))
    (kern-event (process-kern-event
		 kern-token humdrum-state processed-events))
    (chord (process-chord
	    kern-token humdrum-state processed-events))	  
    (first-barline (process-first-barline
		    humdrum-state processed-events))
    (musical-rest (process-musical-rest
		   kern-token humdrum-state processed-events))
    (otherwise (process-other-tokens
		kern-token kern-token-type
		humdrum-state processed-events))))

(defun join-states (humdrum-states)
  "This function takes a list of humdrum states, <humdrum-states>,
   and joins any adjacent states that all have :cued-for-join
   as true. Joining is only permitted if the states to be joined
   have the same exclusive interpretation. The join preserves only
   the properties of the first state in the collection to be joined.
   Note 1: joining just one state to itself is not an error.
   Note 2: joining more than two states to one state is not an error."
  (if (null humdrum-states)
      nil
      (labels ((fun (input joining-stage accumulator)
		 ;; <input> is a list of humdrum-states yet to be processed
		 ;; <joining-stage> is a list of states cued to be joined
		 ;; <accumulator> is the output list of states in reverse order
		 (cond ((null input)
			(if (null joining-stage)
			    (reverse accumulator)
			    (if (utils:all-eql (mapcar #'humdrum-state-excl-interpret
						       joining-stage)
					       :predicate #'string=)
				;; Apply all staged joins
				(fun nil nil (cons (combine-specific-states
						    joining-stage)
						   accumulator))
				(error 'kern-line-read-error
				       :text "Tried to join spines without matching exclusive interpretations."))))
		       ;; If the first element is cued for joining, stage it
		       ((humdrum-state-cued-for-join (car input))
			(fun (cdr input)
			     (cons (car input) joining-stage)
			     accumulator))
		       ;; Otherwise, join the staged elements if appropriate
		       (t (if (null joining-stage)
			      (fun (cdr input) nil
				   (cons (car input) accumulator))
			      (if (utils:all-eql
				   (mapcar #'humdrum-state-excl-interpret
					   joining-stage) :predicate #'string=)
				  (fun input nil (cons (combine-specific-states
							joining-stage)
						       accumulator))
				  (error 'kern-line-read-error
					 :text "Tried to join spines without matching exclusive interpretations."))))))
	       (combine-specific-states (state-list)
		 "Combines a list of states specifically identified
                  for combination into one state."
		 (let ((out-state (car (last state-list))))
		   (setf (humdrum-state-cued-for-join out-state)
			 nil)
		   out-state)))
	(fun humdrum-states nil nil))))


(defun exchange-states (humdrum-states)
  (if (null humdrum-states)
      nil
      (labels ((fun (input accumulator)
		 (cond ((null input) (reverse accumulator))
		       ((eql (length input) 1)
			(if (humdrum-state-cued-for-exchange (car input))
			    (error 'kern-line-read-error
				   :text "Unmatched spine exchange token found.")
			    (fun nil (cons (car input) accumulator))))
		       (t (if (humdrum-state-cued-for-exchange (car input))
			      (if (humdrum-state-cued-for-exchange (second input))
				  ;; Exchange spines
				  (fun (cddr input)
				       (cons (first input)
					     (cons (second input)
						   accumulator)))
				  (error 'kern-line-read-error
					 :text "Unmatched spine exchange token found."
					 ))
			      (fun (cdr input) (cons (car input) accumulator)))))))
	(fun humdrum-states nil))))



(defun check-matching-excl-interpret (state1 state2)
  "Checks whether two humdrum states (<state1> and <state2>
   have the same exclusive interpretation."
  (eql (humdrum-state-excl-interpret state1)
       (humdrum-state-excl-interpret state2)))
    
(define-condition kern-line-read-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
	     (format stream
		     "Error parsing line ~A.
~A
The line reads:
~S"
		     *line-number*
		     (text condition)
		     (get-line *line-number*)))))

(defun check-interpretations (interpretations)
  (let ((*line-number* (first interpretations)))
    (if (not (every #'(lambda (x) (>= (length x) 2))
		    (second interpretations)))
	(error 'kern-line-read-error
	       :text "First line of kern file (excluding comments and blank lines) contained elements with length fewer than two characters."))
    (if (not (every #'(lambda (x) (string= (subseq x 0 2) "**"))
		    (second interpretations)))
	(error 'kern-line-read-error
	       :text "First line of kern file (excluding comments and blank lines) contained tokens which did not begin with two asterisks, which is required by the Humdrum specification (these tokens should correspond to the spines' exclusive interpretations."))))

(defun check-num-tokens
    (humdrum-states record)
  (if (not (eql (length humdrum-states) (length record)))
      (error 'kern-line-read-error
	     :text (format nil "Expected ~A tokens, but only found ~A."
			   (length humdrum-states) (length record)))))

(defun initialise-environment (voice &optional subvoice)
  (let ((assigned-subvoice (if subvoice 
			       subvoice
			       (list voice)))) ; subvoice defaults to voice
    ;; When we initialise this list, we need to copy any elements
    ;; which themselves are lists.
    (list (list 'onset *default-onset*)
	  (list 'bioi *default-bioi*)
	  (list 'keysig *default-keysig*)
	  (list 'mode *default-mode*)
	  (list 'timesig (copy-list *default-timesig*))
	  (list 'timebase *default-timebase*)
	  (list 'pause *default-pause*)
	  (list 'tempo *default-tempo*)
	  (list 'correct-onsets *default-onset*)
	  (list 'deltast *default-onset*)
	  (list 'phrase 0)
	  (list 'instrument *default-instrument*)
	  (list 'instrument-class *default-instrument-class*)
	  (list 'instrument-group *default-instrument-group*)
	  (list 'voice voice)
	  (list 'subvoice assigned-subvoice))))

(defun initialise-humdrum-states (interpretations)
  (reset-voice-counter)
  (mapcar #'(lambda (x)
	      (let ((environment
		     (if (kern-p x)
			 (initialise-environment (get-new-voice))
			 nil)))
		(make-humdrum-state
		 :excl-interpret x
		 :environment environment)))
	  (second interpretations)))

(defun get-tie-position-in-processed-events
    (tie-marker processed-events)
  "Gets the position of a <tie-marker> in <processed-events>.
   The :position slot of <tie-marker> gives the 1-indexed position 
   of the tied note in <processed-events> if <processed-events>
   were to be reversed. However, normally we want to know 
   the 0-indexed position without reversing."
  (- (length processed-events)
     (tie-marker-position tie-marker)))


(defun deep-copy-humdrum-state
    (state &key
	     (voice nil voice-supplied-p)
	     (subvoice nil subvoice-supplied-p)
	     (excl-interpret nil excl-interpret-supplied-p))
  "Makes a deep copy of every aspect of <state>.
   If <voice>, <subvoice>, or <excl-interpret> are supplied, 
   these are used to update the corresponding slots in the new 
   object."
  (let ((new-state (deep-copy-via-prin1 state)))
    (if voice-supplied-p
	(setf-in-humdrum-state-envir 'voice new-state voice))
    (if subvoice-supplied-p
	;; We copy <subvoice> because it's a list and we may
	;; want to alter the new state's subvoice in the future
	;; without altering the parent state's subvoice.
	(setf-in-humdrum-state-envir 'subvoice new-state
				     (copy-list subvoice)))
    (if excl-interpret-supplied-p
	(setf (humdrum-state-excl-interpret new-state)
	      excl-interpret))
    new-state))

(defun get-from-humdrum-state-envir (key state)
  "Gets the value associated with <key> from the 
   :environment slot of the humdrum-state object
   <state>.
   e.g. (get-from-humdrum-state-envir 'onset my-state)."
  (cadr (assoc key (humdrum-state-environment state))))

(defun setf-in-humdrum-state-envir (key state value)
  "Sets the value associated with <key> in the 
   :environment slot of the humdrum-state object
   <state> to <value>.
   e.g. (setf-in-humdrum-state-envir 'onset my-state 50)."
  (setf (cadr (assoc key (humdrum-state-environment state)))
	value))

(defun deep-copy-via-prin1 (obj)
  "Makes a deep copy of an object by reading its printed representation.
   Only works on simple types of objects, e.g. nested lists and
   simple structures."
  (values (read-from-string (prin1-to-string obj))))

(defun copy-environment (environment)
  "Copies an environment."
  (deep-copy-via-prin1 environment))

(defun check-record-onsets (record-onsets)
  "Checks that all onsets recorded for the current record
   are equal."
  (if (not (utils:all-eql record-onsets))
      (error
	 'kern-line-read-error
	 :text "Note timings failed to match up between spines.")))

(defun get-onset-from-humdrum-states (state-list)
  (let ((onsets nil))
    (dolist (state state-list)
      (if (string= (humdrum-state-excl-interpret state)
		   "**kern")
	  (push (get-from-humdrum-state-envir 'onset state)
		onsets)))
    (if (utils:all-eql onsets)
	(car onsets)
	(error
	 'kern-line-read-error
	 :text "Note timings failed to match up between spines."))))

(defun check-ties (ties record-onsets processed-events)
  "Checks that no tied notes have been left unclosed. 
   Also removes old tie-markers from ties."
  (if (null record-onsets)
      ties
      (let ((current-onset (car record-onsets))
	    (new-ties nil))
	(dolist (tie ties)
	  (let* ((tie-position (get-tie-position-in-processed-events
				tie processed-events))
		 (tie-event (nth tie-position processed-events))
		 (tie-offset (+ (second (assoc :onset tie-event))
				(second (assoc :dur tie-event))))
		 (tie-closed (tie-marker-closed tie))
		 (tie-tokens (tie-marker-tokens tie)))
	    ;;(format t "Tie-position: ~A~%" tie-position)
	    ;;(format t "Tie-event: ~A~%" tie-event)
	    ;;(format t "Tie-offset: ~A~%" tie-offset)
	    ;;(format t "Tie-closed: ~A~%" tie-closed)
	    (if (>= tie-offset current-onset)
		(push tie new-ties)
		(if (not tie-closed)
		    (error 'kern-line-read-error
			   :text (format nil "Unclosed tie found.~%
Tie position: ~A
Tie event: ~A
Tie offset: ~A
Tie closed: ~A
Tie **kern tokens: ~A~%" tie-position tie-event
tie-offset tie-closed (reverse tie-tokens)))))))
	new-ties)))

(defun check-token-type (state token-type)
  "Checks that the type of the observed token is consistent
   with the the current state."
  (if (null (humdrum-state-excl-interpret state))
      ;; If the exclusive interpretation slot is NIL, then 
      ;; we must see an exclusive interpretation on the
      ;; upcoming token.
      (if (not (eql token-type 'excl-interpret-token))
	  (error 'kern-line-read-error
		 :text "Missing exclusive interpretation token."))
      ;; Otherwise, we do not expect to see an exclusive
      ;; interpretation token.
      (if (eql token-type 'excl-interpret-token)
	  (error 'kern-line-read-error
		 :text "Unexpected new exclusive interpretation token."))))

(defun reset-voice-counter () (setf *voice-counter* 0))
(defun get-new-voice () (incf *voice-counter*))

(defun make-subvoice (voice)
  "Makes a new subvoice equivalent to <voice>."
  (list voice))

(defun split-subvoice (subvoice)
  "Split a subvoice into two subvoices."
  (let ((new-subvoice-1 (append subvoice
				(list 1)))
	(new-subvoice-2 (append subvoice
				(list 2))))
    (values new-subvoice-1
	    new-subvoice-2)))

(defun join-subvoices (subvoice-1 subvoice-2)
  "Joins two subvoices.
   If the two subvoices came from splitting the same subvoice,
   then the original subvoice is returned. Otherwise
   the two subvoices are merged as follows:
   ((<subvoice-1> <subvoice-2>))."
  (let* ((subvoice-1-head (butlast subvoice-1))
	 (subvoice-2-head (butlast subvoice-2))
	 (subvoice-1-tail (last subvoice-1))
	 (subvoice-2-tail (last subvoice-2))
	 (recognised-subvoice-tokens '((1) (2))))
    ;; If the heads match, then we can just remove the tails,
    ;; as long as both have valid tails to remove.
    (if (and (equal subvoice-1-head subvoice-2-head)
	     (not (null subvoice-1-head))
	     (member subvoice-1-tail
		     recognised-subvoice-tokens :test #'equal)
	     (member subvoice-2-tail
		     recognised-subvoice-tokens :test #'equal))
	subvoice-1-head
	(list (list subvoice-1 subvoice-2)))))

(defun get-regexp-in-alist (string alist)
  "Returns the first entry in <alist> (whose keys are regular
   expressions) which matches <string>." 
  (assoc string alist :test #'(lambda (item patt) 
                                (cl-ppcre:scan-to-strings patt item))))

(defun correct-onsets-in-first-bar (converted-spine first-onset environment
				    &optional (offset 0))
  "Corrects the onsets of events in the first bar in cases where the first
   event in the piece is not the first event in the first bar."
  (if (null (car (cadr (assoc 'timesig environment))))
      converted-spine 
      (let* ((current-event (car converted-spine)) ;; last note of first bar
             (current-onset (cadr (assoc :onset current-event))) ;; last note onset
             (current-dur (cadr (assoc :dur current-event))) ;; last note duration
             (bar-length (calculate-bar-length environment)) ;; duration of first bar
             (next-offset (if (not (null current-event))
                              (+ offset current-dur)
                              offset))
             (new-onset (if (not (null current-event))
                            (- (+ first-onset bar-length) next-offset))))
        (cond ((null converted-spine) '())
              ((= current-onset first-onset)
               (cons (update-alist current-event
                                   (list :onset new-onset)
                                   (list :bioi new-onset)
					;(list :deltast new-onset))
                                   (list :deltast 0))
                     (cdr converted-spine)))
              (t (cons (update-alist current-event (list :onset new-onset
							 :bioi new-onset))
                       (correct-onsets-in-first-bar (cdr converted-spine)
                                                    first-onset
                                                    environment
                                                    next-offset)))))))

(defun calculate-bar-length (environment)
  "Calculates the number of time-units in a bar from the values of the
   timesig and timebase keys in the environment."
  (let* ((timesig (cadr (assoc 'timesig environment)))
         (timebase (cadr (assoc 'timebase environment)))
         (numerator (car timesig))
         (denominator (cadr timesig)))
    (if (null denominator) nil (* (/ timebase denominator) numerator))))

(defun merge-tied-notes (note1 note2)
  "Returns a replacement note for two tied notes by extending the 
   offset of the first note to match the offset of the second note.
   If the offset of the first note is already greater or equal to that 
   of the second note (e.g. because of another tie to that first
   note) then the original note is left unchanged."
  (let* ((onset1 (cadr (assoc :onset note1)))
	 (onset2 (cadr (assoc :onset note2)))
	 (dur1 (cadr (assoc :dur note1)))
	 (dur2 (cadr (assoc :dur note2)))
	 (offset1 (+ onset1 dur1))
	 (offset2 (+ onset2 dur2))
	 (new-dur (- (max offset1 offset2) onset1))
	 (cpitch1 (cadr (assoc :cpitch note1)))
	 (cpitch2 (cadr (assoc :cpitch note2))))
    (cond ((< offset1 onset2)
	   (error 'kern-line-read-error
		  :text "Tried to merge two non-adjacent notes."))
	  ((not (eql cpitch1 cpitch2))
	   (error 'kern-line-read-error
		  :text "Tried to merge two notes with different pitches."))
	  ((> offset1 offset2) note1)
	  (t (update-alist note1 
			   (list :dur new-dur)
			   (list :phrase 
				 (let ((p1 (cadr (assoc :phrase note1)))
				       (p2 (cadr (assoc :phrase note2))))
				   (cond ((and (= p1 1) (= p2 0))
					  1)
					 ((and (= p1 0) (= p2 -1))
					  -1)
					 ((and (= p1 0) (= p2 0))
					  0)
					 (t 
					  (print "Warning: unexpected phrase token within tied note.")
					  -1)))))))))

(defun update-alist (alist &rest new-entries)
  "Returns a version of <alist> updated with <new-entries> which must be 
   key-value pairs. If the value is nil then the pair is not added to the 
   alist unless the key is 'correct-onsets which is the only key in the
   environment allowed to have null values. Does not modify original list."
  (flet ((insert-entry (alist force new-entry)                           
           (cond ((and (null force) (null (cadr new-entry)))              
                  alist)
                 ((assoc (car new-entry) alist)
                  (substitute-if new-entry #'(lambda (key)
                                               (eql key (car new-entry)))
                                 alist :key #'car))
                 (t
                  (cons new-entry alist)))))
    (let* ((entry (car new-entries))
           (force (if (eql (car entry) 'correct-onsets) t nil)))
      (if (null entry)
          alist
          (apply #'update-alist (insert-entry alist force entry)
                 (cdr new-entries))))))

(defun ignore-token (&optional token environment)
  "Return nil for ignored tokens."
  (declare (ignore token environment)))

(defun first-barline (token environment)
  "Returns nil."
  (declare (ignore token environment)))

(defun instrument (token environment)
  "Adds the instrument to the list of instruments already associated 
   with this spine. Note: **kern does not provide for changing 
   instruments within a spine, only adding instruments."
  (let ((current-instruments (cadr (assoc 'instrument environment)))
	(new-instrument (subseq token 2)))
    (pushnew new-instrument current-instruments :test #'string=)))

(defun instrument-class (token environment)
  "Adds the instrument class to the list of instrument classes already associated 
   with this spine. Note: **kern does not provide for changing 
   instrument classes within a spine, only adding instrument classes."
  (let ((current-instrument-classes (cadr (assoc 'instrument-class environment)))
	(new-instrument-class (subseq token 3)))
    (pushnew new-instrument-class current-instrument-classes :test #'string=)))

(defun instrument-group (token environment)
  "Adds the instrument group to the list of instrument groups already associated 
   with this spine. Note: **kern does not provide for changing 
   instrument groups within a spine, only adding instrument groups."
  (let ((current-instrument-groups (cadr (assoc 'instrument-group environment)))
	(new-instrument-group (subseq token 3)))
    (pushnew new-instrument-group current-instrument-groups :test #'string=)))

(defun keysig (keysig-token &optional environment)
  "Process a key signature token."
  (declare (ignore environment))
  (let* ((keysig-string (cl-ppcre:scan-to-strings "[a-g A-G n # -]+" keysig-token))
	 (keysig-char (coerce keysig-string 'list)))
    (labels ((find-sharps (char-list num-sharps)
               (cond ((null char-list) num-sharps)
                     ((char= #\# (car char-list))
                      (find-sharps (cdr char-list) (+ num-sharps 1)))
                     ((char= #\- (car char-list))
                      (find-sharps (cdr char-list) (- num-sharps 1)))
                     (t (find-sharps (cdr char-list) num-sharps)))))
      (find-sharps keysig-char 0))))

(defun mode (mode-token &optional environment)
  "Process a key token."
  (declare (ignore environment))
  (let* ((mode-string (cl-ppcre:scan-to-strings "[a-g A-G ? X]" mode-token))
         (mode-char (car (coerce mode-string 'list))))
    (cond ((upper-case-p mode-char) 0)
          ((lower-case-p mode-char) 9)
          (t 0))))

(defun timesig (timesig-token &optional environment)
  "Process a time signature token."
  (declare (ignore environment))
  (if (cl-ppcre:scan-to-strings "[X|Z|?]" timesig-token)
      (list nil nil)
      (let* ((timesig-string 
              (cl-ppcre:scan-to-strings "[0-9]+/[0-9]+" timesig-token))
             (split-timesig-string (split-string timesig-string "/"))
             (num (parse-integer (car split-timesig-string)))
             (den (parse-integer (cadr split-timesig-string))))
        (list num den))))

(defun timebase (timebase-token &optional environment)
  "Process a timebase token."
  (declare (ignore environment))
  (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" timebase-token)))

(defun tempo (tempo-token &optional environment)
  "Process a tempo token."
  (declare (ignore environment))
  (if (cl-ppcre:scan-to-strings "^\\*MM\\[" tempo-token) ; verbal tempo indication
      nil
      (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" tempo-token))))

(defun musical-rest (musical-rest environment)
  "Extracts and converts the duration of a rest event."
  (list `(dur ,(process-dur (cl-ppcre:scan-to-strings "[0-9]+[.]*" musical-rest) 
                            environment))))

(defun chord (chord environment)
  "Processes a chord (two or more space delimited events in one spine
   record) and returns a list of event alists corresponding to each note 
   in the chord."
  (labels ((make-event-list (note-list event-list)
             (if (null note-list) event-list
                 (make-event-list (cdr note-list)
                                  (cons (kern-event (car note-list) environment)
                                        event-list)))))
    (make-event-list (split-string chord " ") '())))

(defun kern-event (event environment)
  "Extracts and converts the pitch, duration and phrasing from event
   tokens." 
  (let* (;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; deal with other event tokens here ;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;(stem-direction (cl-ppcre:scan-to-strings "[\\\\\ /]" event))
					;(beaming (cl-ppcre:scan-to-strings "[L J k K]+" event))
					;(ornaments (cl-ppcre:scan-to-strings "[T t M m W w S $ R o O]" event))
					;(articulation (cl-ppcre:scan-to-strings "[` ' \\" ~ ^ , s z I]" event))
					;(slur (cl-ppcre:scan-to-strings "&?[)(]" event))
					;(editorial (cl-ppcre:scan-to-strings "[Y y x X \\?]+" event))
					;(bowing (cl-ppcre:scan-to-strings "[v u]" event)) 
					;(dynamic (list :dynamic *default-dynamic*))
         (tempo (list :tempo (cadr (assoc 'tempo environment))))
         (deltast (list :deltast (cadr (assoc 'deltast environment))))
         (onset (list :onset (cadr (assoc 'onset environment))))
         (bioi (list :bioi (cadr (assoc 'bioi environment))))
         (pitch (process-pitch event))
         (cpitch (list :cpitch (nth 0 pitch)))
         (mpitch (list :mpitch (nth 1 pitch)))
         (accidental (list :accidental (nth 2 pitch)))
         (dur (list :dur (process-dur event environment)))
         (keysig (list :keysig (cadr (assoc 'keysig environment))))
         (mode (list :mode (cadr (assoc 'mode environment))))
         (barlength (list :barlength (calculate-bar-length environment)))
         (pulses (list :pulses (car (cadr (assoc 'timesig environment)))))
         (phrase (list :phrase (process-phrase event environment)))
         (voice (list :voice (cadr (assoc 'voice environment)))))
    (list onset dur deltast bioi cpitch mpitch accidental keysig mode barlength
          pulses phrase voice tempo)))

(defun process-pitch (event-token)    
  "Convert a kern pitch token <pitch-token> into a chromatic pitch
   and morphetic pitch given *middle-c*." 
  (let* ((c-middle-c (nth 0 *middle-c*))
         (m-middle-c (nth 1 *middle-c*))
         (pitch-token (cl-ppcre:scan-to-strings "[a-g A-G]+[- # n]*" event-token))
         (pitch (cl-ppcre:scan-to-strings "[a-g A-G]+" pitch-token))
         (num-octaves (- (length pitch) 1))
         (num-sharps (length (cl-ppcre:scan-to-strings "[#]+" pitch-token)))
         (num-flats (length (cl-ppcre:scan-to-strings "[-]+" pitch-token)))
         (c-interval (case (char-downcase (char pitch 0))
                       (#\c 0)
                       (#\d 2)
                       (#\e 4)
                       (#\f 5)
                       (#\g 7)
                       (#\a 9)
                       (#\b 11)))
         (m-interval (case (char-downcase (char pitch 0))
                       (#\c 0)
                       (#\d 1)
                       (#\e 2)
                       (#\f 3)
                       (#\g 4)
                       (#\a 5)
                       (#\b 6))))
    (if (lower-case-p (char pitch 0))
        (list
         (+ c-middle-c c-interval (* num-octaves 12) (- num-flats) num-sharps)
         (+ m-middle-c m-interval (* num-octaves 7))
         (- num-sharps num-flats))
        (list 
         (- c-middle-c (- 12 c-interval) (* num-octaves 12) num-flats
            (- num-sharps))
         (- m-middle-c (- 7 m-interval) (* num-octaves 7))
         (- num-sharps num-flats)))))

(defun process-dur (event-token environment)
  "Convert a kern duration token <event-token> into a duration value in
   terms of basic time units specified by the timebase found in the
   current environment."
  (labels ((get-dotted-dur (dur num-dots)
             (if (< num-dots 0) 0
                 (+ dur (get-dotted-dur (/ dur 2) (- num-dots 1))))))
    (let* ((dur-token (cl-ppcre:scan-to-strings "[0-9]+[.]*" event-token))
           (dur (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" dur-token)))
           (dur (if (zerop dur) (/ 1 2) dur))
           (dur (* (/ 1 dur) (cadr (assoc 'timebase environment))))
           (num-dots (length (cl-ppcre:scan-to-strings "[.]+" dur-token))))
      (get-dotted-dur dur num-dots))))

(defun process-phrase (event environment)
  "Returns 1 if <event> is the first in a phrase, -1 if it is the last
in a phrase, and 0 otherwise."
  (cond ((= 1 (cadr (assoc 'pause environment))) 1)
        ((= 1 (cadr (assoc 'phrase environment))) 1)
        ((cl-ppcre:scan-to-strings "^[^ &\\{]*\\{" event) 1)
        ((cl-ppcre:scan-to-strings "^[^ &\\}]*\\}" event) -1)
        ((cl-ppcre:scan-to-strings ";" event) -1)
        (t 0)))

(defun process-excl-interpret-token (kern-token state processed-events)
  (setf (humdrum-state-excl-interpret state)
	kern-token)
  (values (list state) processed-events))

(defun process-ignore-token (state processed-events)
  (values (list state) processed-events))

(defun process-spine-path (kern-token humdrum-state processed-events)
  (let ((next-humdrum-states nil))
    (cond
      ;; Adding a spine path
      ((string= kern-token "*+")
       (push humdrum-state next-humdrum-states)
       (let* ((new-voice (get-new-voice))
	      (new-subvoice (list new-voice))
	      (new-state (deep-copy-humdrum-state
			  humdrum-state
			  :voice new-voice
			  :subvoice new-subvoice
			  :excl-interpret nil)))
	 (push new-state next-humdrum-states)))
      ;; Terminating a spine path
      ((string= kern-token "*-") nil)
      ;; Splitting a spine path
      ((string= kern-token "*^")
       (multiple-value-bind (subvoice-1 subvoice-2)
	   (split-subvoice (get-from-humdrum-state-envir 'subvoice
							 humdrum-state))
	 (let ((new-state-1 (deep-copy-humdrum-state
			     humdrum-state
			     :subvoice subvoice-1))
	       (new-state-2 (deep-copy-humdrum-state
			     humdrum-state
			     :subvoice subvoice-2)))
	   (push new-state-1 next-humdrum-states)
	   (push new-state-2 next-humdrum-states))))
      ;; Joining two spine paths
      ((string= kern-token "*v")
       (setf (humdrum-state-cued-for-join humdrum-state) t)
       (push humdrum-state next-humdrum-states))
      ;; Exchanging two spine paths
      ((string= kern-token "*x")
       (setf (humdrum-state-cued-for-exchange humdrum-state) t)
       (push humdrum-state next-humdrum-states))
      (t (error 'kern-line-read-error
		:text (format 
		       nil "Unrecognised spine-path token \"~A\"."
		       kern-token))))
    (values next-humdrum-states processed-events)))

(defun process-kern-event
    (kern-token humdrum-state processed-events)
  (let ((new-processed-events (process-kern-event->processed-event
			       kern-token
			       humdrum-state
			       processed-events))
	(new-humdrum-state (process-kern-event->environment
			    kern-token humdrum-state)))
    (values (list new-humdrum-state)
	    new-processed-events)))

(defun process-kern-event->processed-event
    (kern-token humdrum-state processed-events)
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (new-event (funcall 'kern-event kern-token
			     environment))
	 (new-onset (second (assoc :onset new-event)))
	 (new-processed-events
	  (cond ((open-tie-p kern-token)
		 (process-open-tie->processed-event
		  kern-token humdrum-state processed-events))
		((or (middle-tie-p kern-token)
		     (close-tie-p kern-token))
		 (process-continue-tie->processed-event
		  kern-token humdrum-state processed-events))
		(t (process-no-tie->processed-event
		    kern-token humdrum-state processed-events)))))
    (push new-onset *record-onsets*)
    new-processed-events))

(defun process-open-tie->processed-event
    (kern-token humdrum-state processed-events)
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (voice (get-from-humdrum-state-envir
		 'voice humdrum-state))
	 (subvoice (get-from-humdrum-state-envir
		    'subvoice humdrum-state))
	 (new-event (funcall 'kern-event kern-token
			     environment))
	 (new-cpitch (second (assoc :cpitch new-event)))
	 (new-onset (second (assoc :onset new-event)))
	 (new-dur (second (assoc :dur new-event)))
	 (new-offset (+ new-onset new-dur)))
    (push new-event processed-events)
    (push (make-tie-marker :cpitch new-cpitch
			   :closed nil
			   :voice voice :subvoice subvoice
			   :tokens (list kern-token)
			   :attach-onsets (list new-offset)
			   :position (length processed-events))
	  *ties*)
    processed-events))  

(defun process-continue-tie->processed-event
    (kern-token humdrum-state processed-events)
  "Takes a <kern-token>, processes it, and returns
   an updated <humdrum-state> and <processed-events>.
   Specifically, <processed-events> receives updates
   to tied notes (if applicable) as well as additional
   note events (if applicable), and the :tied-events
   slot of <humdrum-state> updated. Note that the 
   :environment slot of <humdrum-state> is not
   affected by this function; this functionality
   is achieved by process-kern-event->environment."
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (voice (get-from-humdrum-state-envir
		 'voice humdrum-state))
	 (subvoice (get-from-humdrum-state-envir
		    'subvoice humdrum-state))
	 (new-event (funcall 'kern-event kern-token
			     environment))
	 (new-onset (second (assoc :onset new-event)))
	 (new-dur (second (assoc :dur new-event)))
	 (new-offset (+ new-onset new-dur))
	 (new-cpitch (second (assoc :cpitch new-event)))
	 (index-matching-ties (loop for n below
				   (length *ties*)
				 collect n))
	 ;;; Find the initial tied event
	 ;; Check subvoice
	 (index-matching-ties (if *ties-may-cross-subvoices*
				  index-matching-ties
				  (remove-if-not
				   #'(lambda (x)
				       (eql (tie-marker-subvoice (nth x *ties*))
					    subvoice))
				   index-matching-ties)))
	 ;; Check voice
	 (index-matching-ties (if *ties-may-cross-voices*
				  index-matching-ties
				  (remove-if-not
				   #'(lambda (x)
				       (eql (tie-marker-voice (nth x *ties*))
					    voice))
				   index-matching-ties)))
	 ;; Check pitch
	 (index-matching-ties (remove-if-not
			       #'(lambda (x)
				   (eql (tie-marker-cpitch (nth x *ties*))
					new-cpitch))
			       index-matching-ties))
	 ;; Check onset
	 (index-matching-ties (remove-if-not
			       #'(lambda (x)
				   (member new-onset
					   (tie-marker-attach-onsets (nth x *ties*))))
			       index-matching-ties)))
    ;;(format t "New event: ~A~%" new-event)
    ;;(format t "Index matching ties: ~A~%" index-matching-ties)
    ;;(format t "Ties before update: ~A~%" *ties*)
    (if (null index-matching-ties)
	;; No matching ties found
	(error 'kern-line-read-error
	       :text (format nil
			     "Tie continuation indicated (~S) but could not find any ties to continue."
			     kern-token)))
    (dolist (i index-matching-ties)
      (let ((tied-note-position (get-tie-position-in-processed-events
				 (nth i *ties*) processed-events)))
	(setf (nth tied-note-position processed-events)
	      (merge-tied-notes (nth tied-note-position processed-events)
				new-event))
	(push kern-token (tie-marker-tokens (nth i *ties*)))
	(if (middle-tie-p kern-token)
	    ;; Allow the tie to be continued by future events
	    (push new-offset (tie-marker-attach-onsets (nth i *ties*)))
	    (if (close-tie-p kern-token)
		;; Close the tie marker
		(setf (tie-marker-closed (nth i *ties*)) t)
		(error "Ties should be only able to continue as <middle> or <close>.")))))
    ;; (format t "Ties after update: ~A~%" *ties*)
    ;; (if (string= kern-token "8B\\L]") (break)) 
    processed-events))

(defun process-no-tie->processed-event
    (kern-token humdrum-state processed-events)
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (new-event (funcall 'kern-event kern-token
			     environment)))
    (push new-event processed-events)
    processed-events))

(defun process-kern-event->environment
    (kern-token humdrum-state)
  (let* ((environment (humdrum-state-environment humdrum-state))
	 (current-envir-onset (cadr (assoc 'onset environment)))
	 (current-envir-bioi (cadr (assoc 'bioi environment)))
	 (current-event (kern-event kern-token environment))
	 (event-dur (cadr (assoc :dur current-event)))
	 (new-envir-onset (list 'onset (+ current-envir-onset
					  event-dur)))
	 (new-envir-bioi (list 'bioi (+ event-dur 
					(if (or (middle-tie-p kern-token)
						(close-tie-p kern-token))
					    current-envir-bioi
					    0))))
	 (new-envir-deltast (list 'deltast 0))
	 ;; (phrase (list 'phrase 0))
	 (new-envir-pause (if (pause-p kern-token)
			      (list 'pause 1) (list 'pause 0)))
	 (new-environment (update-alist environment
					new-envir-onset new-envir-pause
					new-envir-deltast new-envir-bioi)))
    (setf (humdrum-state-environment humdrum-state)
	  new-environment)
    humdrum-state))

(defun process-chord
    (kern-token humdrum-state processed-events)
  (multiple-value-bind (new-processed-events any-untied)
      (process-chord->processed-events kern-token
				       humdrum-state
				       processed-events)
    (values (list (process-chord->environment kern-token
					      humdrum-state
					      any-untied))
	    new-processed-events)))

(defun process-chord->processed-events
    (kern-token humdrum-state processed-events)
  "Takes a <kern-token> corresponding to a chord,
   processes it, and returns an updated <humdrum-state>
   and <processed-events>. Specifically, <processed-events>
   receives updates to tied notes (if applicable) as well as 
   additional note events (if applicable), and the :tied-events
   slot of <humdrum-state> updated. Note that the 
   :environment slot of <humdrum-state> is not
   affected by this function; this functionality
   is achieved by process-chord->environment."
  (let ((chord-split (split-string kern-token " "))
	(any-untied nil))
    (labels ((fun (remaining-tokens humdrum-state processed-events)
	       (if (null remaining-tokens)
		   processed-events
		   (let* ((token (car remaining-tokens))
			  (new-remaining-tokens (cdr remaining-tokens))
			  (tied-to-prev-event (or (middle-tie-p token)
						  (close-tie-p token)))
			  (new-processed-events
			   (process-kern-event->processed-event
			    token humdrum-state processed-events)))
		     (if (not tied-to-prev-event)
			 (setf any-untied t))
		     (fun new-remaining-tokens humdrum-state
			  new-processed-events)))))
      (let ((new-processed-events (fun chord-split humdrum-state
				       processed-events)))
	(values new-processed-events any-untied)))))

(defun process-chord->environment
    (kern-token humdrum-state any-untied)
  (let* ((environment (humdrum-state-environment humdrum-state))
	 (current-envir-onset (cadr (assoc 'onset environment)))
	 (current-envir-bioi (cadr (assoc 'bioi environment)))
	 (current-event (chord kern-token environment))
	 (event-durs (mapcar #'(lambda (x) (cadr (assoc :dur x)))
			     current-event))
	 (new-envir-onset (list 'onset (+ current-envir-onset
					  (car event-durs))))
	 (new-envir-bioi (list 'bioi (+ (car event-durs) 
					(if any-untied
					    0
					    current-envir-bioi))))
	 (new-envir-deltast (list 'deltast 0))
	 ;; (phrase (list 'phrase 0))
	 (new-envir-pause (if (pause-p kern-token)
			      (list 'pause 1) (list 'pause 0)))
	 (new-environment (update-alist environment
					new-envir-onset new-envir-pause
					new-envir-deltast new-envir-bioi)))
    (if (not (utils:all-eql event-durs))
	(error 'kern-line-read-error
	       :text "Chord did not have equal durations for all notes.")
	(setf (humdrum-state-environment humdrum-state)
	      new-environment))
    humdrum-state))

(defun process-first-barline
    (humdrum-state processed-events)
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (current-envir-onset (cadr (assoc 'onset environment)))
	 (new-processed-events processed-events)
	 (bar-length (calculate-bar-length environment)))
    (if (and (not *first-barline-reached*)
	     (not (null bar-length)) t)
	(let* ((first-bar-onset (* (ceiling (/ current-envir-onset
					       bar-length))
				   bar-length)))
	  (setf *onset-correction* (- first-bar-onset
				      current-envir-onset))
	  (setf *first-barline-reached* t)))
    (values (list humdrum-state)
	    new-processed-events)))

(defun correct-onsets (events correction)
  (mapcar #'(lambda (event)
	      (let ((current-onset (cadr (assoc :onset event))))
		(update-alist event
			      (list :onset (+ current-onset
					      correction)))))
	  events))

(defun process-musical-rest
    (kern-token humdrum-state processed-events)
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (previous-event (car processed-events))
	 (current-onset (cadr (assoc 'onset environment)))
	 (current-event (musical-rest kern-token environment))
	 (new-processed-events
	  (if (or (pause-p kern-token)
		  (end-of-phrase-p kern-token))
	      (cons (update-alist previous-event (list :phrase -1))
		    (cdr processed-events))
	      processed-events))
	 (rest-duration (cadr (assoc 'dur current-event)))
	 (onset (+ current-onset rest-duration))
	 (onset (list 'onset onset))
	 (deltast (+ (cadr (assoc 'deltast environment)) rest-duration))
	 (deltast (list 'deltast deltast))
	 (bioi (+ (cadr (assoc 'bioi environment)) rest-duration))
	 (bioi (list 'bioi bioi))
	 (pause (list 'pause (cond ((pause-p kern-token) 1)
				   ((= (nth 1 (assoc 'pause environment)) 1)
				    1)
				   (t 0))))
	 ;; (phrase (list
	 ;;	  'phrase (cond
	 ;;		    ((start-of-phrase-p token) 1)
	 ;;		    ((= (nth 1 (assoc 'phrase environment)) 1)
	 ;;		     1)
	 ;;		    (t 0))))
	 (new-environment
	  (update-alist environment onset pause deltast bioi)))
    (setf (humdrum-state-environment humdrum-state)
	  new-environment)
    (values (list humdrum-state) new-processed-events)))

(defun process-other-tokens
    (kern-token kern-token-type
     humdrum-state processed-events)
  (let* ((environment (humdrum-state-environment
		       humdrum-state))
	 (current-event (funcall kern-token-type kern-token environment))
	 (new-environment
	  (update-alist environment
			(list kern-token-type current-event))))
    (setf (humdrum-state-environment humdrum-state)
	  new-environment)
    (values (list humdrum-state) processed-events)))
