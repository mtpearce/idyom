;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2020-02-09 14:20:44 marcusp>
;;;; ======================================================================

(cl:in-package #:db2midi)

(defvar *timebase* 96)
(defvar *midc* 60) 

(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

;;; Export data

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :mid)) path)
  "Export an entire dataset from the database." 
  (let* ((id (car (idyom-db:get-id d)))
         (*timebase* (idyom-db:get-timebase id))
         (*midc*     (idyom-db:get-midc id)))
    (dolist (c (idyom-db:get-compositions id))
      (export-data c type path))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :mid)) path)
  "Export an individual composition from the database."
  (let* ((id (idyom-db:get-id c))
         (title (idyom-db:get-description (first id) (second id)))
         (file (concatenate 'string path "/" title ".mid"))
         (*timebase* (idyom-db:get-timebase (first id) (second id)))
         (*midc* (idyom-db:get-midc (first id))))
    (db-events->midi (idyom-db:composition-events c) file)))

(defmethod export-data ((event-list list) (type (eql :mid)) path)
  "Export a list of events, which may be database events (mtp-event)
or music objects events (music-event)."
  (let ((first-event (car event-list)))
    (typecase first-event
      (idyom-db:mtp-event
       (let* ((first-id (idyom-db:get-id first-event))
              (title (idyom-db:get-description (first first-id) (second first-id)))
              (file (concatenate 'string path "/" title ".mid")))
         (db-events->midi event-list file))))))


;;; Database events

(defun db-event-lists->midi (event-lists file &key (format 1) (program *default-program*))
  "Writes a list of lists of database events <event-lists> to a midi
file <file> with each list of events written to a separate midi
channel."
  (let* ((tempo (idyom-db:get-attribute (car (car event-lists)) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (tracks (mapcar #'(lambda (x) 
                             (let* ((channel (idyom-db:get-attribute (car x) :voice))
                                    (channel-msg (make-instance 'midi:program-change-message :time 0 
                                                                :status (+ #xc0 channel) 
                                                                :program program)))
                               (incf program)
                               (cons channel-msg (cons tempo-msg (mapcan #'db-event->midi x))))) 
                         event-lists))
         (midifile (make-instance 'midi:midifile
                                  :format format
                                  :division (* (/ *timebase* 4) *tick-multiplier*)
                                  :tracks tracks)))
    (midi:write-midi-file midifile file)))

(defun db-events->midi (events file &key (format 1) (program *default-program*))
  "Writes a list of database events <events> to a midi file <file>."
  (let* ((channel (idyom-db:get-attribute (car events) :voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (idyom-db:get-attribute (car events) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (track (mapcan #'db-event->midi events))
         (midifile (make-instance 'midi:midifile
                                  :format format
                                  :division (* (/ *timebase* 4) *tick-multiplier*)
                                  :tracks (list 
                                           (cons tempo-msg
                                                 (cons channel-msg track))))))
    (midi:write-midi-file midifile file)
    midifile))

(defun db-event->midi (event)
  "Returns midi note on/off messages corresponding to the db event <event>."
  (let* ((non-onset (idyom-db:get-attribute event :onset))
         (noff-onset (+ non-onset (idyom-db:get-attribute event :dur)))
         (channel (idyom-db:get-attribute event :voice))
         (keynum  (round (+ (- 60 *midc*)
                            (idyom-db:get-attribute event :cpitch))))
         (velocity (idyom-db:get-attribute event :dyn)))
    (list (make-instance 'midi:note-on-message
                         :time (* non-onset *tick-multiplier*)
                         :status (+ #x90 channel)
                         :key keynum 
                         :velocity (if velocity velocity *default-velocity*))
          (make-instance 'midi:note-off-message
                         :time (* noff-onset *tick-multiplier*)
                         :status (+ #x80 channel)
                         :key keynum
                         :velocity (if velocity velocity *default-velocity*)))))
  

;;; Helper

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))
