;;;; ======================================================================
;;;; File:       midi2db.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2007-03-21 09:47:26 marcusp>
;;;; Time-stamp: <2014-05-09 18:43:27 marcusp>
;;;; ======================================================================

(cl:in-package #:midi2db) 

(defvar *timebase* 96 "Basic time units per semibreve")
(defvar *middle-c* (list 60 35) "Chromatic and diatonic integer mappings for c_4")
(defvar *default-midifile-extension* "mid")

(defmethod import-data ((type (eql :mid)) path description id)
  (mtp-admin:insert-dataset (midi2db path description) id))

(defun midi2db (path description) 
  (let ((data (get-data path)))
    (append (list description *timebase* (car *middle-c*)) data)))

(defun get-data (path)
  (if (pathname-name path)
      (list (cons (pathname-name path)
                  (convert-midi-file (midi:read-midi-file path))))
      (mapcar #'(lambda (f) 
                  ;; (format t "~&Convert-midi-file: ~A~%" f)
                  (cons (pathname-name f)
                        (convert-midi-file (midi:read-midi-file f))))
              (directory 
               (concatenate 'string (directory-namestring path)
                            "*" "." *default-midifile-extension*)))))


;; JG - useful for debugging MIDI import code
(defun print-midi-messages (midifile)
  "Display messages for the given MIDI file"
  (let* ((ppqn (midi:midifile-division midifile)))
    (format t "PPQN: ~A~%" ppqn)
    (dolist (track (midi:midifile-tracks midifile))
    (dolist (msg track)
      (typecase msg
	(midi:time-signature-message (format t "Time sig. ~A / ~A ~%" (midi:message-numerator msg) (midi:message-denominator msg)))
	(midi:key-signature-message (format t "Key~%"))
	(midi:tempo-message (format t "Tempo ~A~%" (midi:message-tempo msg)))
	(midi:sequence/track-name-message (format t "~A Name~%" (midi:message-time msg)))
	(midi:program-change-message (format t "Program change~%"))
	(midi:note-on-message (format t "~A Note on, pitch ~A [velocity ~A]~%" (midi:message-time msg) (midi:message-key msg) (midi:message-velocity msg)))
	(midi:note-off-message (format t "      ~A Note off~%" (midi:message-time msg)))
	(midi:pitch-bend-message (format t "      ~A Pitch bend ~A~%" (midi:message-time msg) (midi:message-value msg)))
	(midi:smpte-offset-message (format t "SMPTE message~%"))
	(t (format t "UNKNOWN MESSAGE~%")))))))

	 
(defun midi-to-list (midifile)
  "List messages for the given MIDI file"
  (mapcar #'(lambda (x) (mapcar 'midi-item x)) (midi:midifile-tracks midifile)))

(defun midi-item (msg) 
  (typecase msg
    (midi:note-on-message `(:on ,(midi:message-time msg) ,(midi:message-key msg)))
    (midi:note-off-message `(:off ,(midi:message-time msg)))
    (midi:pitch-bend-message `(:bend ,(midi:message-time msg) ,(midi:message-value msg)))))



;;; Convert MIDI into
;;; i) a property list of note
;;; ii) an association list of (note onset . pitch bend) pairs
;;;
(defun extract-midi-notes (midifile)
  (let* ((notes)
	 (bends)
         (tracknum 0)
	 ;; Initial list of note properties
	 (props '(:pulses 4 :barlength 96 :tempo 120 :phrase 0)))
    ; Iterate over MIDI tracks
    (dolist (track (midi:midifile-tracks midifile)
	     (values (nreverse notes) (nreverse bends)))
      (let ((sorted-track (sort track #'< :key #'midi:message-time))
	    (track-notes)
	    (track-bends))
	; Iterate over MIDI messages
        (do* ((st sorted-track (cdr st))
              (message (car st) (car st)))
             ((null st))
          (typecase message
	    ;;
	    ;; Meta-messages (alter subsequent note properties)
	    ;;
	    ;; Time signature 
	    (midi:time-signature-message
	     (let* ((nn (midi:message-numerator message))
		    (dd (midi:message-denominator message))
		    (bl (* nn (* *timebase* (expt 2 (- dd))))))
	       (setf (getf props :pulses) nn 
		     (getf props :barlength) bl)))
	    ;; Key signature
	    (midi:key-signature-message
	     (let ((mi (midi:message-mi message))
		   (sf (midi:message-sf message)))
	       (setf (getf props :keysig) sf
		     (getf props :mode) (if (zerop mi) 0 9))))
	    ;; Tempo (in microseconds per quarter note)
	    (midi:tempo-message
	     (setf (getf props :tempo) (midi:message-tempo message)))
	    ;;
	    ;; Note messages (refer to specific notes)
	    ;;
	    ;; Note on
            (midi:note-on-message
             (let ((velocity (midi:message-velocity message)))
	       (unless (= velocity 0) 		   
		 (let* ((onset (midi:message-time message))
			(pitch (midi:message-key message))
		        (off (find-matching-note-off message st))
                        (offset (midi:message-time off)))
		   ;; Add to note list
		   (push (append `(:onset ,onset :pitch ,pitch
					  :offset ,offset :voice ,tracknum)
				 props) track-notes)))))
	    ;; Pitch-bend
	    (midi:pitch-bend-message 
	     (let* ((time (midi:message-time message)))
	       (if (null (assoc time track-bends))
		   (push (cons time (midi:message-value message)) track-bends))))
	    ;; Ignore other message types
            (midi:note-off-message)
            (midi:sequence/track-name-message)
            (midi:program-change-message)))
	    ;;(t (format t "Warning: ignoring MIDI message time ~a~%"
	    ;;(midi:message-time message)))
	(push (nreverse track-notes) notes)
	(push (nreverse track-bends) bends))
      ;; Next track
      (incf tracknum))))

(defun convert-midi-file (midifile)
  (multiple-value-bind (notes bends) (extract-midi-notes midifile)
    (let* ((ppqn (midi:midifile-division midifile))
	   (dbevents))
      (dotimes (i (length notes) (nreverse dbevents))
	(let* ((prev-onset) (prev-offset)
               (ponset 0) (pdur 0) (pdeltast 0) (pbioi 0))
	  (dolist (note (nth i notes))
	    (let* (;; Timing
		   (midi-onset (getf note :onset))
		   (onset (midi-time->time midi-onset ppqn))
		   (midi-offset (getf note :offset))
		   (offset (midi-time->time midi-offset ppqn))
		   ;; Pitch
		   (midi-pitch (midi-pitch->pitch (getf note :pitch)))
		   (bend (cdr (assoc midi-onset (nth i bends))))
		   (cpitch (+ midi-pitch
			      (if (null bend) 0
				  (pitch-bend->cents bend))))
		   ;; Duration
		   (dur (- offset onset))
		   ;; IOI
		   (midi-bioi (if (null prev-onset) 0
				  (- midi-onset prev-onset)))
		   (bioi (cond ((null prev-onset) 0)
                               ((zerop midi-bioi) 1)
                               (t (midi-time->time midi-bioi ppqn))))
		   ;; deltast
		   (midi-deltast (if (null prev-offset) 0 
				     (- midi-onset prev-offset)))
		   (deltast (midi-time->time midi-deltast ppqn)))
              ;; (format t "~&A: ~A ~A ~A ~A~%" (float onset) (float bioi) (float deltast) (float dur))
              (setf bioi (round bioi)
                    onset (+ ponset bioi)
                    dur (round dur)
                    deltast (- bioi pdur))
              ;; (format t "~&B: ~A ~A ~A ~A~%" onset bioi deltast dur)
	      (push (make-dbevent onset dur deltast bioi cpitch note) dbevents)
	      (setf ponset onset
                    pdur dur
                    pdeltast deltast
                    pbioi bioi
                    prev-onset midi-onset
		    prev-offset midi-offset))))))))

(defun make-dbevent (onset dur deltast bioi cpitch note)
  (list (list :onset onset)
        (list :dur dur)
        (list :deltast deltast)
        (list :bioi bioi)
        (list :cpitch cpitch)
        (list :keysig (getf note :keysig))
        (list :mode (getf note :mode))
        (list :barlength (getf note :barlength))
        (list :pulses (getf note :pulses))
        (list :tempo (getf note :tempo))
        (list :phrase (getf note :phrase))
        (list :voice (getf note :voice))
        (list :articulation 0)
        (list :comma 0)
        (list :ornament 0)
        (list :dyn (getf note :dyn))))


;;; Convert MIDI into database events
;;;
;;; Interprets pitch bend messages as adjustments to the pitch of the
;;; entire note.
(defun convert-midi-file-old (midifile)
  (let* ((ppqn (midi:midifile-division midifile))
         (notes)
	 (bends)
         (tracknum 0)
         (pulses 4)     ;; MIDI default 
         (barlength 96) ;; MIDI default 
         (tempo 120)    ;; MIDI default 
         (keysig nil)   
         (mode nil)
         (phrase 0)
         (previous-note-off-time nil)
         (previous-note-on-time nil))
    ; Iterate over MIDI tracks
    (dolist (track (midi:midifile-tracks midifile)
	     (mapcar #'(lambda (n) (apply-pitch-bend n bends))
		     (nreverse notes)))
      (let ((sorted-track (sort track #'< :key #'midi:message-time)))
	; Iterate over MIDI messages
        (do* ((st sorted-track (cdr st))
              (message (car st) (car st)))
             ((null st))
          (typecase message
	    ; Time signature 
            (midi:time-signature-message
             (let ((nn (midi:message-numerator message))
                   (dd (midi:message-denominator message)))
               (setf pulses nn 
                     barlength (* nn (* *timebase* (expt 2 (- dd)))))))
	    ; Key signature
            (midi:key-signature-message
             (let ((mi (midi:message-mi message))
                   (sf (midi:message-sf message)))
               (setf keysig sf mode (if (zerop mi) 0 9))))
            (midi:tempo-message
	     (let ((tt (midi:message-tempo message)))
               (setf tempo (usecs-per-quarter-note->bpm tt))))
	    ; Note on
            (midi:note-on-message
             (let ((velocity (midi:message-velocity message)))
               (unless (= velocity 0) 
                 (let* ((time (midi:message-time message))
			;; Note pitch
			(pitch (midi-pitch->pitch (midi:message-key message)))
			;; Note onset
                        (onset (midi-time->time time ppqn))
			;; Note duration
                        (midi-offset-time
                         (midi:message-time (find-matching-note-off message st)))
                        (offset-time (midi-time->time midi-offset-time ppqn))
                        (dur (- offset-time onset))
			;; IOI
                        (midi-bioi (if (null previous-note-on-time) 
                                       0
                                       (- time previous-note-on-time)))
                        (bioi (if (zerop midi-bioi) 1
                                  (midi-time->time midi-bioi ppqn)))
			;; deltast
                        (midi-deltast (if (null previous-note-off-time) 
                                          0 
                                          (- time previous-note-off-time)))
                        (deltast (midi-time->time midi-deltast ppqn)))
		   ;; Remember time and offset
                   (setf previous-note-off-time midi-offset-time
                         previous-note-on-time time)
		   ;; Add to note list
		   (push (make-event-alist onset dur deltast bioi pitch keysig 
                                           mode barlength pulses phrase tracknum 
                                           velocity)
                         notes)))))
	    ; Pitch-bend
	    (midi:pitch-bend-message 
	     (let* ((time (midi-time->time (midi:message-time message) ppqn))
		    (rtime (car (multiple-value-list (round time)))))
	     ;(let ((time (midi:message-time message)))
	       (if (null (assoc rtime bends))
		   (push (cons rtime
			       (pitch-bend->cents (midi:message-value message)))
		   bends))))
	    ; Ignore other message types
            (midi:note-off-message)
            (midi:sequence/track-name-message)
            (midi:program-change-message)
	    ;;(t (format t "Warning: ignoring MIDI message time ~a~%"
	    ;;(midi:message-time message)))
	    )))
      ;; Next track
      (incf tracknum))))


(defun apply-pitch-bend (note bends)
  (let* ((time (cadr (assoc :onset note)))
	 (straight (cadr (assoc :cpitch note)))
	 (bend (assoc time bends))
	 (bent (if (null bend) straight
		   (+ straight (cdr bend)))))
    (append (list (list :cpitch bent)) note)))

; Convert pitch adjustment encoded as MIDI pitch bend value to cents / 100
(defun pitch-bend->cents (bend)
  (let* ((bend-cents (/ (- bend 8192) 40.96)))
    (/ (car (multiple-value-list (round bend-cents))) 100)))

(defun make-event-alist (onset dur deltast bioi cpitch keysig mode barlength 
                         pulses phrase voice dyn)
  (list (list :onset (round onset))
        (list :dur (round dur))
        (list :deltast (round deltast))
        (list :bioi (round bioi))
        (list :cpitch cpitch)
        (list :keysig keysig)
        (list :mode mode)
        (list :barlength barlength)
        (list :pulses pulses)
        (list :phrase phrase)
        (list :voice voice)
        (list :articulation 0)
        (list :comma 0)
        (list :ornament 0)
        (list :dyn dyn)))

(defun find-matching-note-off (note-on-message track)
  (find (midi:message-key note-on-message)
        track 
        :start 1 
        :key #'(lambda (x) (typecase x
                             ((or note-on-message note-off-message)
                              (midi:message-key x))
                             (t -50000)))
        :test #'=))

(defun usecs-per-quarter-note->bpm (usecs-per-quarter-note)
  (/ 60000000 usecs-per-quarter-note))

(defun midi-time->time (midi-time ppqn &optional (timebase *timebase*))
  (* (/ timebase 4) (/ midi-time ppqn)))

(defun midi-pitch->pitch (midi-pitch)
  midi-pitch)


;;; Quantisation 

(defgeneric quantise (midifile quantum  &key message-kind))
(defmethod quantise ((m midifile) (quantum integer) &key (message-kind :both))
  "QUANTUM is number of even subdivisions of a crotchet: ie 1 =
semibreve, 2 = minim, 4 = crotchet, 8 = quaver etc. MESSAGE-KIND can
be :note-on, :note-off or :both."
  (let ((smallest-interval (/ (* (midi:midifile-division m) 4) quantum)))
    (dolist (track (midi:midifile-tracks m))
      (dolist (message track)
        (typecase message
          (midi:note-on-message
           (when (or (eq message-kind :note-on) (eq message-kind :both))
             (setf (midi:message-time message) 
                   (snap-to-grid (midi:message-time message) 
                                 smallest-interval))))
          (midi:note-off-message
           (when (or (eq message-kind :note-off) (eq message-kind :both))
             (setf (midi:message-time message) 
                   (snap-to-grid (midi:message-time message) 
                                 smallest-interval)))))))))

(defun snap-to-grid (time smallest-interval)
  (let* ((divisor (floor time smallest-interval))
         (t1 (* smallest-interval divisor))
         (t2 (* smallest-interval (1+ divisor)))
         (d1 (- time t1))
         (d2 (- t2 time)))
    ;;(format t "~&time = ~A; t1 = ~A; t2 = ~A; d1 = ~A; d2 = ~A~%" 
    ;;        time t1 t2 d1 d2)
    (cond ((= d1 d2) time) ; if its exactly equal it keeps its time 
          ((> d1 d2) t2)
          ((> d2 d1) t1))))


;;; Christophe's note-on note-off macro 

(defun tempos (midifile)
  (remove-if-not (lambda (x) (typep x 'midi:tempo-message))
                 (first (midi:midifile-tracks midifile))))
  
(defun note-on-events (midifile)
  (assert (= (midi:midifile-format midifile) 1))
  (remove-if-not (lambda (x) (typep x 'midi:note-on-message))
                 (second (midi:midifile-tracks midifile))))

(defun s/time (midifile tempo)
  (/ (/ (midi:message-tempo tempo) 1000000) (midi:midifile-division midifile)))

(defmacro do-paired-events (((on off) events &optional result) &body body)
  `(do ((events ,events))
       ((null events) ,result)
    (let* ((event (car events))
           (key (midi:message-key event))
           (channel (midi:message-channel event)))
      (assert (> (midi:message-velocity event) 0))
      (let* ((matchings (do ((events events (cdr events)))
                            ((null (cdr events)) 
                             (error "Found no match for ~S" event))
                          (let ((m (cadr events)))
                            (when (and (= (midi:message-channel m) channel)
                                       (= (midi:message-key m) key)
                                       (= (midi:message-velocity m) 0))
                              (return events)))))
             (matching (cadr matchings)))
        (let ((,on event)
              (,off matching))
          ,@body
          (setf (cdr matchings) (cddr matchings)
                events (cdr events)))))))

;; (defun table (filename hop frame)
;;   (let ((midifile (midi:read-midi-file filename)))
;;     (let ((tempos (tempos midifile)))
;;       (assert (null (cdr tempos)))
;;       (let ((s/time (s/time midifile (car tempos))))
;;         (let ((events (note-on-events midifile)))
;;           ;; could save a little time by using an array.
;;           (let ((result (make-hash-table)))
;;             (do-paired-events ((on off) events result)
;;               (let ((start (ceiling (/ (- (* s/time (midi:message-time on)) 
;;                                           frame) 
;;                                        hop)))
;;                     (end (floor (/ (* s/time (midi:message-time off)) hop))))
;;                 (assert (>= end start))
;;                 (do ((i start (1+ i))) 
;;                     ((> i end))
;;                   (push event (gethash i result)))))))))))
;; 
;; (with-open-file (s "/tmp/k009CR.txt" :direction :output :if-exists :supersede)
;;   (let ((*print-pretty* nil))
;;     (let ((table (table "/isms/tmp/TC_testfiles/midi/K009_2.MID" 0.1 0.372))
;;           (chroma (make-array 12 :element-type 'single-float)))
;;       (dotimes (i 920)
;;         (fill chroma 0.0f0)
;;         (let* ((notes (gethash i table))
;;                (factor (if (null notes) 0f0 (/ 1f0 (length notes)))))
;;           (dolist (note notes)
;;             (incf (aref chroma (mod (midi:message-key note) 12)) factor)))
;;         (format s "~&~D" i)
;;         (dotimes (i 12 (format s "~%"))
;;           (format s " ~,4F" (aref chroma i)))))))

;;; Removing legato introduced by Finale

(defun fix-daniels-midi-files (directory &key (quantum 32))
  (mapcar #'(lambda (f) 
              (fix-daniels-midi-file (namestring f) :quantum quantum))
          (directory 
           (concatenate 'string (directory-namestring directory)
                        "*" "." "MID"))))

(defun fix-daniels-midi-file (filename &key (quantum 32))
  (let ((midifile (midi:read-midi-file filename))
        (outfile (concatenate 'string (subseq filename 0 (- (length filename) 3))
                              "mid")))
    (remove-legato midifile)
    (quantise midifile quantum :message-kind :note-off)
    (midi:write-midi-file midifile outfile)))

(defgeneric remove-legato (midifile))
(defmethod remove-legato ((m midifile))
  (let ((events (note-on-and-note-off-events m))
        (paired-events '()))
    (do-paired-events ((on off) events nil)
      (push (list on off) paired-events))
    (do* ((pe paired-events (cdr pe))
          (e2 (car pe) (car pe))
          (e1 (cadr pe) (cadr pe)))
         ((null e1))
      (let ((e2-noteon (car e2))
            (e1-noteoff (cadr e1)))
        (when (> (midi:message-time e1-noteoff) (midi:message-time e2-noteon))
          (setf (midi:message-time e1-noteoff) 
                (midi:message-time e2-noteon)))))))

(defun note-on-and-note-off-events (midifile)
  (assert (= (midi:midifile-format midifile) 1))
  (remove-if-not (lambda (x) (or (typep x 'midi:note-on-message)
                                 (typep x 'midi:note-off-message)))
                 (second (midi:midifile-tracks midifile))))

