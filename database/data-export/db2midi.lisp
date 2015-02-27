;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2015-02-27 11:34:42 marcusp>
;;;; ======================================================================

(cl:in-package #:db2midi)

(defvar *timebase* 96)
(defvar *midc* 60) 

(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

(defmethod export-data ((d database:mtp-dataset) (type (eql :mid)) path)
  (let ((*timebase* (database::dataset-timebase d))
        (*midc*     (database::dataset-midc d)))
    (dolist (c (database::dataset-compositions d))
      (export-data c type path))))

(defmethod export-data ((c database:mtp-composition) (type (eql :mid)) path)
  ;; FIXME: *midc* is never set if export-data is called with a
  ;; composition directly.
  (let* ((title (database::composition-description c))
         (file (concatenate 'string path "/" title ".mid"))
         (*timebase* (database::composition-timebase c)))
    (events->midi (database::composition-events c) file)))

(defmethod export-data ((event-list list) (type (eql :mid)) path)
  (let* ((first-event (car event-list))
	 (first-id (identifier first-event))
         (title (database:get-description (dataset-id first-id)
					   (composition-id first-id)))
         (file (concatenate 'string path "/" title ".mid")))
    (events->midi event-list file)))

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))

(defun event-lists->midi (event-lists file &key (format 1) (program *default-program*))
  (let* ((tempo (database:get-attribute (car (car event-lists)) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (tracks (mapcar #'(lambda (x) 
                             (let* ((channel (database:get-attribute (car x) :voice))
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
  (let* ((channel (database:get-attribute (car events) :voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (database:get-attribute (car events) :tempo))
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
  (let* ((non-onset (database:get-attribute event :onset))
         (noff-onset (+ non-onset (database:get-attribute event :dur)))
         (channel (database:get-attribute event :voice))
         (keynum  (round (+ (- 60 *midc*)
                            (database:get-attribute event :cpitch))))
         (velocity (database:get-attribute event :dyn)))
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
  

