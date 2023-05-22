;;;; ======================================================================
;;;; File:       db2cmn.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-08-17 18:54:17 marcusp>                           
;;;; Time-stamp: <2023-05-22 14:34:07 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cm) 

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defobject midimsg (cm::event) ((msg :initform 0) (data :initform nil))
             (:parameters time msg data) (:writers)))

(defun make-key-signature (key &optional (mode ':major))
  (let ((sf nil))
    (setf mode
          (case mode
            ((:major major 0) 0)
            ((:minor minor 1) 1)
            (t (error "key signature mode not :major or :minor"))))
    (cond ((numberp key)
           (unless (<= -7 key 7)
             (error "Key signature must be between -7 (b) and 7 (#)."))
           (setf sf key))
          ((and key (symbolp key))
           (setf sf
                 (case key
                   ((cf) -7)
                   ((gf) -6)
                   ((df) -5)
                   ((af) -4)
                   ((ef) -3)
                   ((bf) -2)
                   ((f) -1)
                   ((c) 0)
                   ((g) 1)
                   ((d) 2)
                   ((a) 3)
                   ((e) 4)
                   ((b) 5)
                   ((fs) 6)
                   ((cs) 7)
                   (t
                    (error "Key signature key not one of: cf gf df af ef bf f c g d a e b fs cs."))))
           (when (= mode 1) (setf sf (max (- sf 3) -7)))
           (setf sf (if (< sf 0) (+ sf 256) sf)))
          (t (error "~s is not a number or symbol." key)))
    (make-meta-message +ml-file-key-signature-opcode+ sf mode)))

(defun metermsg (time numerator denominator)
  (multiple-value-bind (msg data)
      (make-time-signature numerator denominator)
    (new midimsg :time time :msg msg :data data)))

(defun keymsg (time key mode)
  (multiple-value-bind (msg data)
      (make-key-signature key mode)
    (new midimsg :time time :msg msg :data data)))

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))

(defun usecs->bpm (usecs)
  (floor (/ 60 (/ usecs 1000000))))

(defun tempomsg (time bpm)
  (multiple-value-bind (msg data)
      (make-tempo-change (bpm->usecs bpm))
    (new midimsg :time time :msg msg :data data)))

(defun sharps->key (sharps mode)
  "Accepts a key signature defined in terms of no. of sharps/flats
<sharps> (ranges from -7 to 7) and the <mode> (0 for major) anything
else for minor. Returns a CM/CMN key."
  (cond ((> sharps 0)
         (if (zerop mode)
             (nth (1- sharps) '("g" "d" "a" "e" "b" "fs" "cs"))
             (nth (1- sharps) '("e" "b" "fs" "cs" "gs" "ds" "as"))))
        ((< sharps 0)
         (if (zerop mode)
             (nth (1- (abs sharps)) '("f" "bf" "ef" "af" "df" "gf" "cf"))
             (nth (1- (abs sharps)) '("d" "g" "c" "f" "bf" "ef" "af"))))
        (t (if (zerop mode) "c" "a"))))

(defmethod object->midi ((obj midimsg))
  (midi-message->midi-event (midimsg-msg obj) :data (midimsg-data obj)
                            :time (object-time obj)))

(defmethod object->cmn ((obj midimsg))
  (let ((msg (midimsg-msg obj))
        (data (midimsg-data obj))
        (stave 1))
    (cond ((midi-meta-message-p msg)
           (cond ((tempo-change-p msg)
                  `(,stave (cmn::mm
                            ,(usecs->bpm (+ (ash (elt data 1) 16)
                                            (ash (elt data 2) 8)
                                            (elt data 3))))))
                 ((time-signature-p msg)
                  `(,stave (cmn::meter ,(elt data 1) ,(expt 2 (elt data 2)))))
                 ((key-signature-p msg)
                  (let* ((a (elt data 1))
                         (sharps (or (and (logtest a 128) (- a 256)) a))
                         (mode (elt data 2))
                         (cmn-key (cm::sharps->key sharps mode))
                         (cmn-mode (if (zerop mode) "major" "minor")))
                    `(,stave (cmn::key
                              ,(find-symbol (string-upcase
                                             (concatenate 'string cmn-key "-"
                                                          cmn-mode))
                                            (find-package :cmn))))))
                 (t (error "Unsupported midi meta event"))))
          (t (error "Unsupported midi event")))))


(cl:in-package #:db2cmn)

(defvar *default-channel* 0)
(defvar *default-amplitude* 64)
(defvar *default-tempo* 85) 
(defvar *default-timesig* '(4 4))
(defvar *default-keysig* '(cm::c :major))

(defvar *current-tempo* nil)
(defvar *current-timesig* nil)
(defvar *current-keysig* nil)
(defvar *current-time* 0)

(defvar *timebase* 96)
(defvar *midc* 6000) 

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :cmn)) path &key filename)
  (declare (ignore filename))
  (let ((*timebase* (idyom-db:dataset-timebase d))
        (*midc*     (idyom-db:dataset-midc d)))
    (dolist (c (idyom-db:dataset-compositions d))
      (export-data c type path))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :cmn)) path &key filename)
  (let* ((title (idyom-db:composition-description c))
         (file (if filename filename (concatenate 'string path "/" title ".cmn")))
         (*timebase* (idyom-db:composition-timebase c)))
    (write-composition (idyom-db:composition-events c) type file)))

(defmethod export-data ((event-list list) (type (eql :cmn)) path &key filename)
  (let* ((title (idyom-db:composition-description (car event-list)))
         (file (if filename filename (concatenate 'string path "/" title ".cmn"))))
    (write-composition event-list type file)))

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :eps)) path &key filename)
  (declare (ignore filename))
  (let ((*timebase* (idyom-db:dataset-timebase d))
        (*midc*     (idyom-db:dataset-midc d)))
    (dolist (c (idyom-db:dataset-compositions d))
      (export-data c type path))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :eps)) path &key filename)
  (let* ((title (idyom-db:composition-description c))
         (file (if filename filename (concatenate 'string path "/" title ".eps")))
         (*timebase* (idyom-db:composition-timebase c)))
    (write-composition (idyom-db:composition-events c) type file)))

(defmethod export-data ((event-list list) (type (eql :eps)) path &key filename)
  (let* ((title (idyom-db:get-attribute (car event-list) 
                                         :composition-description))
         (file (if filename filename (concatenate 'string path "/" title ".eps"))))
    (write-composition event-list type file)))

(defun write-composition (event-list type file)
  (let* ((first-event (car event-list))
         ;(tempo (get-tempo first-event))
         ;(keysig (get-keysig first-event))
         (timesig (get-timesig first-event))
         (*current-timesig* timesig)
         (*current-keysig* nil)
         (*current-tempo* nil)
         (*current-time* 0))
    (case type 
      (midi
       ;(cm::io file :timesig timesig :tempo tempo :keysig keysig)
       (cm::events (convert-events event-list) file))
      ((or eps cmn)
       (let ((*default-tempo* 60.0))
         (cm::events (convert-events event-list) file
                     :staffing `((1 :name "" :meter ,timesig))
                     :redundant-accidentals nil
                     :automatic-bars t
                     :automatic-naturals t
                     :automatic-rests t
                     :title-separation 2
                     ;:implicit-accidental-style :new-style
                     ;:implicit-accidental-duration 1
                     ;:exact-rhythms t
                     :automatic-ties t
                     :all-output-in-one-file t 
                     :size 24
                     :title description))))))

(defun convert-events (event-list)
  (reduce #'append (mapcar #'convert-event event-list)))

(defun convert-event (event)
  (set-tempo event)
  (let* ((keynum (get-keynum event))
         (time (get-time event))
         (duration (get-duration event))
         (amplitude (get-amplitude event))
         (tempo (get-tempo event))
         (keysig (get-keysig event))
         (timesig (get-timesig event))
         (channel (get-channel event))
         (note (list (cm::new cm::midi :time time :keynum keynum
                              :duration duration :amplitude amplitude
                              :channel channel))))
    (when (tempo-change-p event)
      (push (funcall #'cm::tempomsg *current-time* tempo) note))
    (when (timesig-change-p event)
      (push (apply #'cm::metermsg (cons *current-time* timesig)) note))
    (when (keysig-change-p event)
      (push (apply #'cm::keymsg (cons *current-time* keysig)) note))
    (setf *current-tempo* tempo
          *current-timesig* timesig
          *current-keysig* keysig
          *current-time* time)
    note))

(defun tempo-change-p (event)
  (not (eql (get-tempo event) *current-tempo*)))

(defun timesig-change-p (event)
  (not (equal (get-timesig event) *current-timesig*)))

(defun keysig-change-p (event)
  (not (equal (get-keysig event) *current-keysig*)))
    
(defun set-tempo (event)
  (setf cm::*tempo* (get-tempo event)))

(defun get-tempo (event)
  (let ((tempo (get-attribute event :tempo)))
    (if (or (null tempo) (zerop tempo))
        *default-tempo*
        tempo)))

(defun get-keynum (event)
  (round (+ (get-attribute event :cpitch) (- 60 *midc*))))

(defun get-time (event)
  (let ((onset (get-attribute event :onset)))
    (cm::rhythm (/ onset *timebase*))))

(defun get-duration (event)
  (let ((dur (get-attribute event :dur)))
    (cm::rhythm (/ dur *timebase*))))

(defun get-amplitude (event)
  (let ((dynamic (get-attribute event :dyn)))
    (if (or (null dynamic) (zerop dynamic))
        *default-amplitude*
        dynamic)))

(defun get-channel (event)
  (let ((voice (get-attribute event :voice)))
    (if (numberp voice) voice *default-channel*)))

(defun get-timesig (event)
  (let ((pulses (get-attribute event :pulses))
        (barlength (get-attribute event :barlength)))
    (list pulses
          (/ *timebase* (/ barlength pulses)))))
    
(defun get-keysig (event)
  (let* ((keysig (get-attribute event :keysig))
         (mode (get-attribute event :mode))
         (cm-mode (case mode
                    (0 :major)
                    (9 :minor)))
         (cm-key (find-symbol (string-upcase (cm::sharps->key keysig mode))
                              (find-package :cm))))
    (list cm-key cm-mode)))




