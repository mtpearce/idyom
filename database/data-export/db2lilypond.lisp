;;;; ======================================================================
;;;; File:       db2lilypond.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-07 10:13:24 marcusp>
;;;; Time-stamp: <2017-02-15 15:10:11 peter>
;;;; ======================================================================
;;;; 
;;;; TODO 
;;;; 
;;;;  * pitch spelling using mpitch/accidental if available 
;;;;  * align tied notes & consecutive rests with tactus beats 
;;;;
;;;; ======================================================================

;; FIXME: Global variables should be re-bound to preserve dynamic scope
;; rather than setf.

(cl:in-package #:db2lilypond)

(defvar *timebase* 96)
(defvar *midc* 60) 
(defvar *current-pulses* nil)
(defvar *current-barlength* nil)
(defvar *default-barlength* *timebase*) ;4/4
(defvar *current-keysig* nil)
(defvar *current-mode* nil)
(defvar *new-timesig* 0)
(defvar *tuplet* nil)

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :ly)) path &key filename)
  (declare (ignore filename))
  (let ((*timebase* (idyom-db::dataset-timebase d))
        (*midc*     (idyom-db::dataset-midc d)))
    (dolist (c (idyom-db::dataset-compositions d))
      (export-data c type path))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :ly)) path &key filename)
  ;; FIXME: *midc* is never set if export-data is called with a
  ;; composition directly.
  (declare (ignore filename))
  (let* ((title (idyom-db::composition-description c))
         (file (concatenate 'string path "/" title ".ly"))
         (*timebase* (idyom-db::composition-timebase c)))
    (with-open-file (s file :direction :output :if-exists :supersede 
                       :if-does-not-exist :create)
      (write-composition s (idyom-db::composition-events c) title))))

(defmethod export-data ((event-list list) (type (eql :ly)) path &key filename)
  (declare (ignore filename))
  (let* ((title (idyom-db::composition-description (car event-list)))
         (file (concatenate 'string path "/" title ".ly")))
    (with-open-file (s file :direction :output :if-exists :supersede 
                       :if-does-not-exist :create)
      (write-composition s event-list title))))

(defun write-composition (s event-list title)
    (format s "~&\\version \"2.6.3\"~%")
    (format s "~&\\include \"english.ly\"~2%")
    (format s "~&melody = {~%")
    (format s "~&  \\clef treble~%")
    (write-melody s event-list)
    (format s "~&}~2%")
    (format s "~&\\header {~%")
    (format s "~&  title = \"~A\"~%" title)
    (format s "~&}~2%")
    (format s "~&\\score {~%")
    (format s "~&  \\new Staff \\melody~%")
    (format s "~&  \\layout { indent = 0 }~%")
    (format s "~&}~%"))

(defun write-melody (s events)
  (let* ((*current-pulses* nil)
         (*current-barlength* nil)
         (*current-keysig* nil)
         (*current-mode* nil)
         (*tuplet* nil)
         (*new-timesig* 0)
         (onset1 (idyom-db:get-attribute (car events) :onset))
         (barlength1 (idyom-db:get-attribute (car events) :barlength))
         (write-rest? nil))
    (when (> onset1 0)
      (format s "~&  \\partial ~A~%" ; FIXME: Fails when (- 96 (mod 1092 96)). 
              (car (get-duration (- barlength1 (mod onset1 barlength1))))))
    (dolist (e events)
      (if (tied-p e)
          (multiple-value-bind (e1 e2)
              (split-tied-event e)
            (write-event s e1 write-rest?)
            (write-string " ~ " s)
            (write-event s e2 write-rest?))
          (write-event s e write-rest?))
      (setf write-rest? t)
      (write-string " " s))
    (format s "~&  \\bar \"|.\"~%")))
  
(defun tied-p (event)
  (let* ((onset (idyom-db:get-attribute event :onset))
         (note-off  (+ onset (idyom-db:get-attribute event :dur)))
         (barlength (idyom-db:get-attribute event :barlength))
         (barlength (if (null barlength) *default-barlength* barlength))
         (nbars (floor (- onset *new-timesig*) barlength))
         (barline (+ (* (1+ nbars) barlength) *new-timesig*)))
    (when (< onset barline note-off)
      t)))

(defun split-tied-event (event)
  (let* ((onset (idyom-db:get-attribute event :onset))
         (note-off  (+ onset (idyom-db:get-attribute event :dur)))
         (barlength (idyom-db:get-attribute event :barlength))
         (barline (+ (* (1+ (floor (- onset *new-timesig*) barlength)) barlength)
                     *new-timesig*))
         (e1 (idyom-db:copy-event event))
         (e2 (idyom-db:copy-event event)))
    (idyom-db:set-attribute e1 :dur (- barline onset))
    (idyom-db:set-attribute e2 :deltast 0)
    (idyom-db:set-attribute e2 :onset barline)
    (idyom-db:set-attribute e2 :dur (- note-off barline))
    (when (= (idyom-db:get-attribute event :phrase) -1)
      (idyom-db:set-attribute e1 :phrase 0))
    (values e1 e2)))

(defun write-event (s event &optional (write-rest? t))
  (if write-rest? 
      (write-rest s event)
      (write-string "  " s))
  (when (keysig-change-p event)
    (write-keysig s event)
    (setf *current-keysig* (idyom-db:get-attribute event :keysig)
          *current-mode*   (idyom-db:get-attribute event :mode)))
  (when (timesig-change-p event)
    (write-timesig s event)
    (setf *current-pulses*      (idyom-db:get-attribute event :pulses)
          *current-barlength*   (idyom-db:get-attribute event :barlength))
    (when write-rest?
      (setf *new-timesig*         (idyom-db:get-attribute event :onset))))
  (let* ((dur (idyom-db:get-attribute event :dur))
         (durations (get-duration dur)))
    (cond ((and (triplet-p dur) (null *tuplet*))
           (format s "~&  \\set tupletSpannerDuration = #(ly:make-moment 1 ~A)~%"
                   (max (/ *timebase* (/ *current-barlength* *current-pulses*))
                        (/ *timebase* (* dur 3))))
           (write-string "\\times 2/3 { " s)
           (setf *tuplet* t))
          ((and (not (triplet-p dur)) *tuplet*)
           (write-string "} " s)
           (setf *tuplet* nil)))
    (dolist (d durations)
      (write-note s event d))
    (when (= (idyom-db:get-attribute event :phrase) -1)
      (write-string "\\fermata" s))))

(defun timesig-change-p (event)
  (not (and (eql (idyom-db:get-attribute event :pulses) *current-pulses*)
            (eql (idyom-db:get-attribute event :barlength) *current-barlength*))))

(defun keysig-change-p (event)
  (not (and (eql (idyom-db:get-attribute event :keysig) *current-keysig*)
            (eql (idyom-db:get-attribute event :mode) *current-mode*))))

(defun write-note (s event duration)
  (multiple-value-bind (octave pitch-class)
      (floor (round (- (idyom-db:get-attribute event :cpitch)
                       (- *midc* 12)))
             12)
    (let* ((octave-token (if (plusp octave) "'" ","))
           (pitches (if (< (idyom-db:get-attribute event :keysig) 0)
                        '("c" "df" "d" "ef" "e" "f" "gf" "g" "af" "a" "bf" "b")
                        '("c" "cs" "d" "ds" "e" "f" "fs" "g" "gs" "a" "as" "b")))
           (pitch (nth pitch-class pitches)))
      (write-string pitch s)
      (dotimes (i (abs octave))
        (write-string octave-token s))
      (write-string duration s))))

(defun write-rest (s event)
  (let ((deltast (idyom-db:get-attribute event :deltast)))
    (when (> deltast 0)
      (dolist (d (get-duration deltast))
        (format s "r~A " d)))))
    
(defun get-duration (dur)
  (cond (;; Zero or negative duration - probably a bug but never mind 
         (<= dur 0)
         0)
        ;; standard duration 
        ((standard-duration-p (/ *timebase* dur))
         (list (write-to-string (/ *timebase* dur))))
        ;; triplet 
        ((triplet-p dur)
         (list (write-to-string (/ *timebase* (/ (* dur 3) 2)))))
        ;; dotted
        ((standard-duration-p (/ *timebase* (* 2 (/ dur 3))))
         (list (format nil "~A." (/ *timebase* (* 2 (/ dur 3))))))
        ;; double dotted 
        ((standard-duration-p (/ *timebase* (* 4 (/ dur 7))))
         (list (format nil "~A.." (/ *timebase* (* 4 (/ dur 7))))))
        (t (case (/ *timebase* dur)
             ;; dotted minim ~ semibreve
             (4/7 (list "2. ~ " "1"))
             ;; dotted minim ~ minim 
             (4/5 (list "2. ~ " "2"))
             ;; dotted minim ~ quaver 
             (8/7 (list "2. ~ " "8"))
             ;; minim ~ dotted quaver 
             (16/11 (list "2 ~ " "8."))
             ;; dotted crotchet ~ crotchet 
             (8/5 (list "4. ~ " "4"))
             ;; dotted crotchet ~ dotted quaver 
             (16/9 (list "4. ~ " "8."))
             ;; crotchet ~ dotted quaver 
             (16/7 (list "4 ~ " "8."))
             ;; dotted quaver ~ quaver 
             (16/5 (list "8. ~ " "8"))
             ;; staccato quaver (from density 21.5 and VI/Hindemith)
             (96/7 (list "8\\staccato"))
             ;; staccato semiquaver (from density 21.5 and VI/Hindemith)
             (96/5 (list "16\\staccato"))
             (t 
              (progn (format t "~&Unrecognized duration: ~A (crotchet = ~A).~%" dur *timebase*)
                     (list (format nil "Unrecognized duration: ~A" dur))))))))

(defun standard-duration-p (dur)
  (cond ((= dur 1) 
         t)
        ((not (integerp dur))
         nil)
        (t (standard-duration-p (/ dur 2)))))

(defun triplet-p (dur) 
  (standard-duration-p (/ *timebase* (/ (* dur 3) 2))))

(defun write-timesig (s event)
  (let ((pulses (idyom-db:get-attribute event :pulses))
        (barlength (idyom-db:get-attribute event :barlength)))
    (format s "~&  \\time ~A/~A~%" pulses (/ *timebase* (/ barlength pulses)))))

(defun write-keysig (s event) 
  (let ((keysig (idyom-db:get-attribute event :keysig))
        (mode (idyom-db:get-attribute event :mode)))
    (format s "~&  \\key ~A ~A~%" 
            (sharps->key keysig mode)
            (case mode (0 "\\major") (9 "\\minor")))))

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




  
