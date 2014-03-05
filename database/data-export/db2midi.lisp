;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2014-03-05 11:37:39 marcusp>
;;;; ======================================================================

(cl:in-package #:db2midi)

(defvar *timebase* 96)
(defvar *midc* 60) 

(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

(defmethod export-data ((d mtp-admin:mtp-dataset) (type (eql :mid)) path)
  (setf *timebase* (mtp-admin::dataset-timebase d)
        *midc*     (mtp-admin::dataset-midc d))
  (dolist (c (mtp-admin::dataset-compositions d))
    (export-data c type path)))

(defmethod export-data ((c mtp-admin:mtp-composition) (type (eql :mid)) path)
  (let* ((title (mtp-admin::composition-description c))
         (file (concatenate 'string path "/" title ".mid")))
    (setf *timebase* (mtp-admin::composition-timebase c))
    (events->midi (mtp-admin::composition-events c) file)))

(defmethod export-data ((event-list list) (type (eql :mid)) path)
  (let* ((first-event (car event-list))
	 (first-id (identifier first-event))
         (title (mtp-admin:get-description (dataset-id first-id)
					   (composition-id first-id)))
         (file (concatenate 'string path "/" title ".mid")))
    (events->midi event-list file)))

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))

(defun event-lists->midi (event-lists file &key (format 1) (program *default-program*))
  (let* ((tempo (mtp-admin:get-attribute (car (car event-lists)) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (tracks (mapcar #'(lambda (x) 
                             (let* ((channel (mtp-admin:get-attribute (car x) :voice))
                                    (channel-msg (make-instance 'midi:program-change-message :time 0 
                                                                :status (+ #xc0 channel) 
                                                                :program program)))
                               (incf program)
                               (cons channel-msg (cons tempo-msg (mapcan #'event->midi x))))) 
                         event-lists))
         (midifile (make-instance 'midi:midifile
                                  :format format
                                  :division (* (/ *timebase* 4) *tick-multiplier*)
                                  :tracks tracks)))
    (midi:write-midi-file midifile file)))

(defun events->midi (events file &key (format 1) (program *default-program*))
  (let* ((channel (mtp-admin:get-attribute (car events) :voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (mtp-admin:get-attribute (car events) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (track (mapcan #'event->midi events))
         (midifile (make-instance 'midi:midifile
                                  :format format
                                  :division (* (/ *timebase* 4) *tick-multiplier*)
                                  :tracks (list 
                                           (cons tempo-msg
                                                 (cons channel-msg track))))))
    (midi:write-midi-file midifile file)
    midifile))

(defun event->midi (event)
  (let* ((non-onset (mtp-admin:get-attribute event :onset))
         (noff-onset (+ non-onset (mtp-admin:get-attribute event :dur)))
         (channel (mtp-admin:get-attribute event :voice))
         (keynum  (round (+ (- 60 *midc*)
                            (mtp-admin:get-attribute event :cpitch))))
         (velocity (mtp-admin:get-attribute event :dyn)))
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
  

