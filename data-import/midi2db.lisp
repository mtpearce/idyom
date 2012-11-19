;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       midi2db.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2007-03-21 09:47:26 marcusp>
;;;; Time-stamp: <2012-04-27 10:20:04 marcusp>
;;;; ======================================================================

(cl:in-package #:midi2db) 

(defvar *timebase* 96 "Basic time units per semibreve")
(defvar *middle-c* (list 60 35) 
  "Chromatic and diatonic integer mappings for c_4")
(defvar *default-midifile-extension* "mid")

(defmethod import-data ((type (eql :mid)) path description id)
  (mtp-admin:insert-dataset (midi2db path description) id))

(defmethod import-data ((type (eql :mid-raw)) path description id)
  (mtp-admin:insert-dataset (midi2db path description NIL) id))

(defun midi2db (path description &optional (cents t)) 
  (let ((data (get-data path cents)))
    (append (list description *timebase* (car *middle-c*)) data)))

(defun get-data (path &optional (cents t))
  (if (pathname-name path)
      (list (cons (pathname-name path)
                  (convert-midi-file (midi:read-midi-file path) :cents cents)))
      (mapcar #'(lambda (f) 
                  (cons (pathname-name f)
                        (convert-midi-file (midi:read-midi-file f) :cents cents)))
              (directory 
               (concatenate 'string (directory-namestring path)
                            "*" "." *default-midifile-extension*)))))

; Convert MIDI into database events
;
; MIDI pitch values are multipled by 100 ("cents above C-1"), unless :cents is false.
;
; If :bend is true, this interprets any initial pitch bend messages as
; adjustments to the pitch of the entire note.  Any other bends are
; ignored.
(defun convert-midi-file (midifile &key (cents t) (bend t))
  (let* ((ppqn (midi:midifile-division midifile))
         (data '())
         (tracknum 0)
         (pulses 4)     ;; MIDI default 
         (barlength 96) ;; MIDI default 
         (tempo 120)    ;; MIDI default 
         (keysig nil)   
         (mode nil)
         (phrase 0)
	 (true-pitch t)
         (previous-note-off-time nil)
         (previous-note-on-time nil))
    ; Iterate over MIDI tracks
    (dolist (track (midi:midifile-tracks midifile) (nreverse data))
      (let ((sorted-track (sort track #'< :key #'midi:message-time))
	    (bend-time 0)
	    (bend-value 0))
	; Iterate over MIDI messages
        (do* ((st sorted-track (cdr st))
              (message (car st) (car st)))
             ((null st))
          (typecase message
	    ; Pitch-bend message
	    (midi:pitch-bend-message 
	     (setf bend-time (midi:message-time message))
	     (setf bend-value (midi:message-value message)))
	    ; Time signature message
            (midi:time-signature-message
             (let ((nn (midi:message-numerator message))
                   (dd (midi:message-denominator message)))
               (setf pulses nn 
                     barlength (* nn (* *timebase* (expt 2 (- dd)))))))
	    ; Key signature message
            (midi:key-signature-message
             (let ((mi (midi:message-mi message))
                   (sf (midi:message-sf message)))
               (setf keysig sf mode (if (zerop mi) 0 9))))
            (midi:tempo-message
             ;; microseconds per quarter note -> BPM 
             (let ((tt (midi:message-tempo message)))
               (setf tempo (usecs-per-quarter-note->bpm tt))))
	    ; Note on message
            (midi:note-on-message
             (let ((velocity (midi:message-velocity message)))
               (unless (= velocity 0) 
                 (let* ((time (midi:message-time message))
			(midi-pitch (midi:message-key message))
			;; Note pitch
			; Pitch adjustment (if we have one for this note)
			(pitch-adjust (if (and bend (equal time bend-time))
					  (pitch-bend-to-cents bend-value)
					  0))
			; True pitch in cents
			(cent-pitch (+ (* midi-pitch 100) pitch-adjust))
			; Use either true pitch or MIDI value
			(cpitch (if cents cent-pitch midi-pitch))
			;;; Note time
                        (onset (midi-time->time time ppqn))
                        (midi-offset-time
                         (midi:message-time (find-matching-note-off message st)))
                        (offset-time (midi-time->time midi-offset-time ppqn))
                        (dur (- offset-time onset))
                        (midi-bioi (if (null previous-note-on-time) 
                                       0
                                       (- time previous-note-on-time)))
                        (bioi (if (zerop midi-bioi) 1
                                  (midi-time->time midi-bioi ppqn)))
                        (midi-deltast (if (null previous-note-off-time) 
                                          0 
                                          (- time previous-note-off-time)))
                        (deltast (midi-time->time midi-deltast ppqn)))
;;                    (format t "~&Note: ~6A ~6A ~6A ~6A ~6A~%" 
;;                            time midi-offset-time (- midi-offset-time time)
;;                            midi-deltast velocity)
                   (setf previous-note-off-time midi-offset-time
                         previous-note-on-time time)
		   (setf true-pitch (and true-pitch (or cents (eq pitch-adjust 0))))
		   (push (make-event-alist onset dur deltast bioi cpitch 
					   pitch-adjust cent-pitch keysig 
                                           mode barlength pulses phrase tracknum 
                                           velocity)
                         data)))))
	    ; Ignore other message types
            (midi:note-off-message)
            (midi:sequence/track-name-message)
            (midi:program-change-message))))
      ; Next track
      (incf tracknum) 
      ; Warn if not true pitch
      (unless true-pitch (format t "WARNING: MIDI import not using true pitch as encoded in pitch bend.")))))


; Convert pitch adjustment encoded as MIDI pitch bend value to cents
(defun pitch-bend-to-cents (bend)
  (let* ((bend-cents (/ (- bend 8192) 40.96)))
    (car (multiple-value-list (round bend-cents)))))

(defun make-event-alist (onset dur deltast bioi cpitch cpitch-adj cents keysig mode barlength 
                         pulses phrase voice dyn)
  (list (list :onset (round onset))
        (list :dur (round dur))
        (list :deltast (round deltast))
        (list :bioi (round bioi))
        (list :cpitch cpitch)
	(list :cpitch-adj cpitch-adj)
	(list :cents cents)
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

