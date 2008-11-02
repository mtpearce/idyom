;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; =======================================================================
;;;; File:       kern2db.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2002-05-03 18:54:17 marcusp>                           
;;;; Time-stamp: <2008-10-31 16:47:05 marcusp>                           
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
          '(("^\\.$" ignore-token)                  ;ignore null tokens
            ("^!" ignore-token)                     ;ignore in-line comments 
            ("^\\*$" ignore-token)                  ;ignore null tandem tokens
            ("^[^ ! * =].*[q Q]" ignore-token)      ;ignore grace notes/groupettos 
            ("^\\*\\|\\." ignore-token)             ;ignore staff lining  
            ("^\\*staff" ignore-token)              ;ignore staff position 
            ("^\\*clef" ignore-token)               ;ignore clef info
            ("^\\*IC" ignore-token)                 ;ignore instrument class
            ("^\\*IG" ignore-token)                 ;ignore instrument group
            ("^\\*ITr" ignore-token)                ;ignore transposed instruments
            ("^\\*[8 Tr]" ignore-token)             ;ignore transposition token
            ("^\\*>.*\\[" ignore-token)             ;ignore section expansions 
            ("^\\*>[^ \\[]+" ignore-token)          ;ignore section labels 
            ("^\\*[+ ^ v x -]" ignore-token)        ;ignore spine path tokens
            ("^\\*MM[0-9]" tempo)                   ;process tempo token
            ("^=1[^ 0-9]*$" first-barline)          ;adjust onsets at 1st barline
            ("^=" ignore-token)                     ;ignore barlines 
            ("^[^ ! = . *].*[r]" musical-rest)      ;process rests 
            (".+ .+" chord)                         ;process chords
            ("^[^ ! = . *][^ r q Q]+" kern-event)   ;process events   
            ("^\\*I[^ T G C]" voice)                ;process instrument token
            ("^\\*k\\[[a-g]?" keysig)               ;process keysig token
            ("^\\*[a-g A-G ? X]" mode)              ;process mode token
            ("^\\*M(FREI)?[0-9 ? X Z]" timesig)     ;process timesig token
            ("^\\*tb" ignore-token))))              ;ignore timebase token
  
(defvar *voice-alist* '())
(defvar *unrecognised-representations* '())
(defvar *unrecognised-tokens* '())

(defparameter *default-timebase* 96)    ;basic time units in a semibreve 
(defparameter *middle-c* '(60 35))      ;pitch mapping for middle c
(defparameter *voices* '(1))            ;voices we want to convert 
(defparameter *default-onset* 0)        ;initial onset  
(defparameter *default-pause* 1)        ;initial pause off

(defparameter *default-timesig* '(nil nil)) ;default time signature
(defparameter *default-keysig* nil)         ;no. of sharps in keysig
(defparameter *default-mode* nil)           ;0 major - 9 minor
(defparameter *default-tempo* nil)          ;default tempo/bpm


;;;==================
;;;* Top level call *
;;;==================

(defmethod import-data ((type (eql :krn)) path description id)
  (mtp-admin:insert-dataset (kern2db path description) id))

(defun kern2db (file-or-dir-name description
                                    &key 
                                    (timesig *default-timesig*)
                                    (keysig *default-keysig*)
                                    (mode *default-mode*)
                                    (timebase *default-timebase*)
                                    (onset *default-onset*)
                                    (pause *default-pause*)
                                    (tempo *default-tempo*)
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
  (initialise-voice-alist)
  (let* ((kern-data (read-kern-data file-name))
         (processed-data (process-kern-data kern-data)))
    (cons (pathname-name file-name) processed-data)))

(defun initialise-voice-alist ()
  "Sets *voice-alist* to '()."
  (setq *voice-alist* '()))

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


(defun kern-spine-p (spine)
  (string= (car spine) "**kern"))

(defun end-of-spine-p (token)
  (cl-ppcre:scan-to-strings "^\\*\\-" token))

(defun pause-p (token)
  (cl-ppcre:scan-to-strings ";" token))

(defun end-of-phrase-p (token)
  (cl-ppcre:scan-to-strings "}" token))

(defun start-of-phrase-p (token)
  (cl-ppcre:scan-to-strings "{" token))

(defun line-comment-p (string)
  (cl-ppcre:scan-to-strings "^!!" string))

(defun open-tie-p (event-token)
  (cl-ppcre:scan-to-strings "(^\\[.*)|(^[^ ! * =]+\\[)" event-token))

(defun close-tie-p (event-token)
  (cl-ppcre:scan-to-strings "^[^ ! * =]+\\]" event-token))


;;;================================
;;;* Reading kern data from file. * 
;;;================================


(defun read-kern-data (file-name)
  "Converts a kern file into a list of lists corresponding to each spine in
   the file. Elements in the spine lists are strings corresponding to each
   element in the spine." 
  (let* ((record-list (remove-empty-strings (get-lines file-name)))
         (record-list (remove-line-comments record-list))
         (spine-list (records->spines record-list)))
    spine-list))

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
  (let ((lines '()))
    (map-file-objects #'(lambda (line) (setf lines (cons line lines)))
                       file-name)
    lines))

(defun remove-empty-strings (list)
  "Removes empty strings from a list of strings."
  (remove-if #'(lambda (string) (string= string "")) list))

(defun remove-line-comments (list)
  "Removes line comments from a list of strings."
  (remove-if #'line-comment-p list))

(defun records->spines (record-list)
  "Takes a list of strings corresponding to records (lines) and
   returns a list of strings corresponding to spines (represented in
   columns separated by *kern-spine-separator*)."
  (let ((spined-records 
         (mapcar #'(lambda (record)
                     (split-string record *kern-spine-separator*))
                 (reverse record-list))))
    (extract-spines spined-records '() '() '())))

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

(defun extract-spines (records new-records current-spine spine-list)
  "Given <records>, a list containing lists of strings corresponding to
   each spine in a record, returns a list of spines taking account of
   terminated spines and splitting and joining spines. Does not currently
   take acount of added or exchanged spines." 
  (let* ((current-record (car records))
         (current-token (car current-record))
         (remaining-tokens (cdr current-record)))
    (cond ((null current-record)
           (reverse (if current-spine (cons current-spine spine-list)
                        spine-list)))
          ((end-of-spine-p current-token)
           (extract-spines (append (reverse (cons remaining-tokens
                                                  new-records))
                                   (cdr records))
                           '()
                           '()
                           (cons (reverse current-spine) spine-list)))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; deal with spaghetti junction spine paths here ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;((cl-ppcre:scan-to-strings "^\\*\\^" current-token))
          ;((cl-ppcre:scan-to-strings "^\\*v" current-token))
          ;((cl-ppcre:scan-to-strings "^\\*\\+" current-token))   
          ;((cl-ppcre:scan-to-strings "^\\*x" current-token))
          (t (extract-spines (cdr records)
                             (cons remaining-tokens new-records)
                             (cons current-token current-spine)
                             spine-list)))))


;;;=====================================
;;;* Processing the recorded kern data *
;;;=====================================


(defun process-kern-data (spine-list)
  "Converts the recorded kern data into a CHARM readable format." 
  (merge-spines (process-spines-according-to-type spine-list)))

(defun merge-spines (spine-list &optional (sort-type :onset))
  "Merges all the spines in spine-list into one dataset, sorting them
   according to <sort-type> which must be a key in the event alist (e.g,
   pitch, onset, duration etc.). "
  (labels ((sort-predicate (event1 event2)
             (let ((attribute1 (cadr (assoc sort-type event1)))
                   (attribute2 (cadr (assoc sort-type event2))))
               (< attribute1 attribute2)))
           (sort-events (spine-list sorted-list)
             (if (null spine-list) sorted-list
                 (sort-events (cdr spine-list)
                              (merge 'list (car spine-list) sorted-list 
                                     #'sort-predicate)))))
    (sort-events (reverse spine-list) '())))

(defun process-spines-according-to-type (spines &optional (voices *voices*)
                                                (count 1))
  "Sends each spine in a list to be processed individually according to its
   type and conses the results together." 
  (let ((spine (car spines)))
    (cond ((null spines) '())
          ((member count voices)
           (cond ((kern-spine-p spine)
                  (cons (process-kern-spine (cdr spine))
                        (process-spines-according-to-type (cdr spines)
                                                          voices
                                                          (1+ count))))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
             ;; deal with different humdrum representations here ;; 
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
             ;((string= (car spine) "**dyn")
             ; (cons (process-dyn-spine (cdr spine))
             ;  (process-spines-according-to-type (cdr spines))))     
                 (t (add-to-unrecognised-representations (car spine))
                    (process-spines-according-to-type (cdr spines)
                                                      voices
                                                      count))))
          (t (process-spines-according-to-type (cdr spines)
                                                      voices
                                                      (1+ count))))))

(defun add-to-unrecognised-representations (representation)
  "Adds <representation> to the list of unrecognised representations." 
  (if (not (member representation *unrecognised-representations*
                   :test #'string=))
      (push representation *unrecognised-representations*)))

(defun process-kern-spine (spine)
  "Converts kern spines into CHARM readable format."
  (let ((environment-alist
         (list (list 'onset *default-onset*)
               (list 'bioi 1)
               (list 'keysig *default-keysig*)
               (list 'mode *default-mode*)
               (list 'timesig *default-timesig*)
               (list 'timebase *default-timebase*)
               (list 'pause *default-pause*)
               (list 'tempo *default-tempo*)
               (list 'correct-onsets *default-onset*)
               (list 'deltast *default-onset*)
               (list 'phrase 0)
               (list 'voice 1))))
    (process-kern-tokens spine '() environment-alist)))
               
(defun process-kern-tokens (spine converted-spine environment &key tied)
  "Converts all tokens in <spine> into CHARM readable format in
   <converted-spine>. The <environment> is an initially empty list which
   is used to store global parameters such as onset time, voice,
   key and so on. <tied> is a boolean parameter which signifies whether
   the event token currently being processed is tied to a previous event."
  (let* ((current-token (car spine))
         (remaining-tokens (cdr spine))
         (regexp-match (get-regexp-in-alist current-token *kern-token-alist*))
         (token-type (if (null regexp-match) nil (cadr regexp-match))))
    (cond ((null spine) (reverse converted-spine))
          ((null regexp-match)
           (add-to-unrecognised-tokens current-token)
           (process-kern-tokens remaining-tokens converted-spine environment))
          (t (cond ((open-tie-p current-token)
                    (process-kern-tokens remaining-tokens 
                                         (update-converted-spine
                                          converted-spine current-token
                                          token-type environment)
                                         (update-environment
                                          environment current-token
                                          token-type) :tied t))
                   ((close-tie-p current-token)
                    (process-kern-tokens remaining-tokens 
                                         (update-converted-spine
                                          converted-spine current-token
                                          token-type environment
                                          :tied t)
                                         (update-environment
                                          environment current-token
                                          token-type)))
                   (tied
                    (process-kern-tokens remaining-tokens 
                                         (update-converted-spine
                                          converted-spine current-token
                                          token-type environment
                                          :tied t)
                                         (update-environment
                                          environment current-token
                                          token-type) :tied t))
                   (t (process-kern-tokens remaining-tokens 
                                           (update-converted-spine
                                            converted-spine current-token
                                            token-type environment)
                                           (update-environment
                                            environment current-token
                                            token-type))))))))

(defun add-to-unrecognised-tokens (token)
  "Adds <token> to the list of unrecognised tokens." 
  (if (not (member token *unrecognised-tokens* :test #'string=))
      (push token *unrecognised-tokens*)))

(defun get-regexp-in-alist (string alist)
  "Returns the first entry in <alist> (whose keys are regular
   expressions) which matches <string>." 
  (assoc string alist :test #'(lambda (item patt) 
                                (cl-ppcre:scan-to-strings patt item))))

(defun update-converted-spine (converted-spine token type environment
                                               &key tied)
  "Updates <converted-spine> with a new <token> according to its <type>
   and the current <environment>."
  (let ((current-event (funcall type token environment))
        (previous-event (car converted-spine)))
    ;(format t "~&token: ~A; type: ~A~%" token type)
    (case type
      (kern-event
       (if tied
           (cons (merge-tied-notes previous-event current-event)
                 (cdr converted-spine))
           (cons current-event converted-spine)))
      (chord
       (if tied
           (append (mapcar #'merge-tied-notes converted-spine current-event)
                   (nthcdr (length current-event) converted-spine))
           (append current-event converted-spine)))
      (first-barline
       (if current-event
           (correct-onsets-in-first-bar converted-spine current-event
                                        environment)
           converted-spine))
      (musical-rest 
       (if (or (pause-p token) (end-of-phrase-p token))
           (progn 
             ;(format t "~&Musical-rest: ~A; previous-event: ~A; end-of-phrase-p: ~A~%" 
             ;token previous-event (end-of-phrase-p token))
             ;(format t "~&Updated event: ~A~%" (update-alist previous-event (list :phrase -1)))
             (cons (update-alist previous-event (list :phrase -1))
                   (cdr converted-spine))
             )
           converted-spine))
      (otherwise converted-spine))))

(defun correct-onsets-in-first-bar (converted-spine first-onset environment
                                                    &optional (offset 0))
  "Corrects the onsets of events in the first bar in cases where the first
   event in the piece is not the first event in the first bar."
  (if (null (car (cadr (assoc 'timesig environment))))
      converted-spine 
      (let* ((current-event (car converted-spine))
             (current-onset (cadr (assoc :onset current-event)))
             (current-dur (cadr (assoc :dur current-event)))
             (bar-length (calculate-bar-length environment))
             (next-offset (if (not (null current-event))
                              (+ offset current-dur)
                              offset))
             (new-onset (if (not (null current-event))
                            (- (+ first-onset bar-length) next-offset))))
        (cond ((null converted-spine) '())
              ((= current-onset first-onset)
               (cons (update-alist current-event
                                   (list :onset new-onset)
                                   ;(list :deltast new-onset))
                                   (list :deltast 0))
                     (cdr converted-spine)))
              (t (cons (update-alist current-event (list :onset new-onset))
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
  "Returns a replacement note for two tied notes by summing their durations."
  (update-alist note1 
                (list :dur (+ (cadr (assoc :dur note1))
                              (cadr (assoc :dur note2))))
                (list :phrase (cadr (assoc :phrase note2)))))
                
(defun update-environment (environment token type)
  "Updates and returns <environment> given a <token> of type <type>."
  (let ((current-onset (cadr (assoc 'onset environment)))
        (current-event (funcall type token environment)))
    (case type
      (kern-event
       (let* ((dur (cadr (assoc :dur current-event)))
              (onset (list 'onset (+ current-onset dur)))
              (bioi (list 'bioi dur))
              (deltast (list 'deltast 0))
              ;(phrase (list 'phrase 0))
              (pause (if (pause-p token) (list 'pause 1) (list 'pause 0))))
         (update-alist environment onset pause deltast bioi)))
      (musical-rest
       (let* ((rest-duration (cadr (assoc 'dur current-event)))
              (onset (+ current-onset rest-duration))
              (onset (list 'onset onset))
              (deltast (+ (cadr (assoc 'deltast environment)) rest-duration))
              (deltast (list 'deltast deltast))
              (bioi (+ (cadr (assoc 'bioi environment)) rest-duration))
              (bioi (list 'bioi bioi))
              ;(phrase (list 'phrase (cond ((start-of-phrase-p token) 1)
              ;                            ((= (nth 1 (assoc 'phrase environment)) 1)
              ;                             1)
              ;                            (t 0))))
              (pause (list 'pause (cond ((pause-p token) 1)
                                        ((= (nth 1 (assoc 'pause environment)) 1)
                                         1)
                                        (t 0)))))
         (update-alist environment onset pause deltast bioi)))
      (chord
       (let* ((onset (+ current-onset (cadr (assoc :dur (car current-event)))))
              (onset (list 'onset onset))
              (deltast (list 'deltast 0)))
         (update-alist environment onset deltast)))
      (first-barline
       (let ((bar-length (calculate-bar-length environment)))
         (if (and (cadr (assoc 'correct-onsets environment))
                  (not (null bar-length)))
             (let* ((onset (+ current-event bar-length))
                    (onset (if (zerop current-onset) 0 onset))
                    (onset (list 'onset onset))
                    (correct-onsets (list 'correct-onsets nil)))
               (update-alist environment onset correct-onsets))
             environment)))
      (otherwise
       (update-alist environment (list type current-event))))))

(defun update-alist (alist &rest new-entries)
  "Updates <alist> with <new-entries> which must be key-value pairs.
   If the value in nil then the pair is not added to the alist unless
   the key is 'correct-onsets which is the only key in the environment
   allowed to have null values."
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
  "Returns a modified onset value to take account of situations where
   the first event in the piece is not the first event in the first bar."
  (declare (ignore token))
  (cadr (assoc 'correct-onsets environment)))

(defun correct-onsets (token environment)
  "Returns the onset value in the current environment."
  (declare (ignore token))
  (cadr (assoc 'onset environment)))

(defun voice (voice-token &optional environment)
  "Process an voice token."
  (declare (ignore environment))
  (let ((voice-entry (assoc voice-token *voice-alist*
                                  :test #'string=)))
    (if (null voice-entry)
        (setf *voice-alist*
              (update-alist *voice-alist* 
                            (list voice-token
                                  (+ (length *voice-alist*) 1)))))
    (cadr (assoc voice-token *voice-alist* :test #'string=))))

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
  (if (cl-ppcre:scan-to-strings "[X|Z|\?]" timesig-token)
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
  (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" tempo-token)))

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


