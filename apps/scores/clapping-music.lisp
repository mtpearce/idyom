;;;; ======================================================================
;;;; File:       clapping.lisp
;;;; Author:     marcusp <m.pearce@gold.ac.uk>
;;;; Created:    <2010-03-09 16:11:48 marcusp>
;;;; Time-stamp: <2010-10-22 11:26:31 marcusp>
;;;; ======================================================================
(cl:defpackage #:clapping
  (:use #:cl)
  (:export #:clapping #:predict-clapping #:select-clapping 
           #:clapping->midi)
  (:documentation "Analysis of Clapping Music by Steve Reich."))

(cl:in-package #:clapping) 


;;; Parameters

(defvar *clap-1* (list 1 1 1 0 1 1 0 1 0 1 1 0))
(defvar *k* (length *clap-1*))
(defvar *note-duration* 12)
(defvar *repeat-1* 13)
(defvar *repeat-2* 12)
(defvar *dataset-id* 101)

(defvar *tempo* 160) ; 160 - 184
(defvar *pitch* 37) ; rimshot 
(defvar *dynamics* '(0 55 70 95 120)) ; midi dynamics


;;; generate midi

(defun clapping->midi (path channels accents &key (dynamics *dynamics*))
  (let ((db2midi::*default-tempo* *tempo*))
    (db2midi::event-lists->midi (clapping channels accents :dynamics dynamics) path :format 1 :program 1)))

(defun make-composition-from-event-list (events)
  (let* ((interval (+ (amuse:timepoint (car events)) (amuse:duration (car events))))
         (composition 
          (amuse-mtp::make-mtp-composition :dataset-id *dataset-id*
                                           :composition-id 1
                                           :description "Clapping Music (1972) by Steve Reich."
                                           :time (amuse:timepoint (car events))
                                           :interval interval)))
    (sequence:adjust-sequence composition (length events)
                              :initial-contents events)
    composition))


;;; Representation

(defun clapping (channels accents &key (dynamics *dynamics*))
  ;; CHANNELS: 1 means both hands in one stream; 2 means one channel for each hand
  ;; 
  ;; ACCENTS: 
  ;; 
  ;; 0   - no accents; 
  ;; 1   - accent on downbeat; 
  ;; 2   - accent on first beat of clapper 2s rotated bar
  ;; 
  ;; DYNAMICS: 
  ;; 
  ;; 0 - no clap
  ;; 1 - 1 clapper
  ;; 2 - 1 clapper accented
  ;; 3 - 2 clappers
  ;; 4 - 2 clappers accented
  ;;
  (let ((clap1-events)
        (clap2-events)
        (onset 0)
        (previous-offset-1 0)
        (previous-offset-2 0)
        (event-id-1 0)
        (event-id-2 0))
    (dotimes (i *repeat-1*)
      (let ((bar1 *clap-1*)
            (bar2 (rotate-bar *clap-1* i)))
        (dotimes (j *repeat-2*)
          (dotimes (k *k*)
            ;; dyanamics
            (let* ((b1 (nth k bar1))
                   (b2 (nth k bar2))
                   (b3 (cond ((and (zerop b1) (zerop b2))
                              0)
                             ((and (= 1 b1) (= 1 b2))
                              3)
                             (t 1))))
              ;; accents
              (when (and (> accents 0) (> b3 0)
                         (or 
                          ;; accent on downbeat of clapper 1
                          (and (= accents 1) (zerop (mod k *k*)))
                          ;; accent on (rotated) downbeat of clapper 2
                          (and (= accents 2) (= (mod (- *k* k) *k*) (mod i (1- *repeat-1*)))))
                         (incf b3)))
              ;; print out
              ;;(format t "~&~A ~A ~A: ~A: ~A ~A ~A~%" (1+ i) (1+ j) (1+ k) onset b1 b2 b3)
              ;;(progn 
              ;;  (format t "~A" b3)
              ;;  (if (< k (1- *k*)) (format t " ") (format t "~%")))
              ;; Create events
              (if (= channels 1)
                  ;; 1 channel version
                  (when (not (zerop b3))
                    (push (clapping-event event-id-1 (- onset previous-offset-1) onset 9 (nth b3 dynamics)) clap1-events)
                    (incf event-id-1)
                    (setf previous-offset-1 (+ onset *note-duration*)))
                  ;; 2 channel version
                  (progn
                    (when (not (zerop b1))
                      (push (clapping-event event-id-1 (- onset previous-offset-1) onset 9 (nth b3 dynamics)) clap1-events)
                      (incf event-id-1)
                      (setf previous-offset-1 (+ onset *note-duration*)))
                    (when (not (zerop b2))
                      (push (clapping-event event-id-2 (- onset previous-offset-2) onset 9 (nth b3 dynamics)) clap2-events)
                      (incf event-id-2)
                      (setf previous-offset-2 (+ onset *note-duration*))))))
            (incf onset *note-duration*)))))
    (if (= channels 1)
        (list (nreverse clap1-events))
        ;; NB: This won't work yet for generating midi
        (list (nreverse clap1-events) (nreverse clap2-events)))))

(defun rotate-bar (bar n)
  (let ((n (mod n *repeat-1*)))
    (append (subseq bar n) (subseq bar 0 n))))

(defun clapping-event (event-id deltast onset voice &optional dyn)
  (make-instance 'amuse-mtp::mtp-event
                 :dataset-id     *dataset-id* 
                 :composition-id 1
                 :event-id       event-id
                 :voice          voice
                 ;;:onset          onset
                 ;;:dur            *note-duration*
                 :time           onset
                 :interval       *note-duration*
                 :deltast        deltast
                 :cpitch         *pitch*
                 :mpitch         nil
                 :accidental     nil
                 :keysig         nil 
                 :mode           nil
                 :barlength      nil
                 :pulses         nil 
                 :phrase         nil 
                 :dyn            dyn
                 :tempo          *tempo*))


;;; Modelling 

(defun predict-clapping (dataset-id)
  (multiple-value-bind (a b c)
      (resampling:output-information-content  
       (resampling:dataset-prediction dataset-id '(dyn bioi) '(dyn bioi) :models :stm :k 1))
    (declare (ignore a b))
    c))

;; (defun compute-path (models training-ids order attributes) 
;;   (format nil "~A~A~A~(~A~)-~{~A~^_~}~A~(~A~)-~{~(~A~)~^_~}.dat" 
;;           apps:*root-dir* 
;;           "R/clapping/data/"
;;           "clapping-"
;;           models
;;           (if (eql models :stm) (list "nil") training-ids)
;;           (if (null training-ids) "" "-")
;;           order
;;           (collapse-attribute-list attributes)))

;; (defun collapse-attribute-list (attributes)
;;   (mapcar #'(lambda (x) 
;;               (if (consp x)
;;                   (format nil "~{~(~A~)~^+~}" x)
;;                   (format nil "~(~A~)" x)))
;;           attributes))

;; (defun output-data (predictions path reference-cpitch pitch-classes)
;;   (let* ((entropies (car (prediction-sets:shannon-entropies predictions)))
;;          (codelengths (car (prediction-sets:codelengths predictions)))
;;          (probabilities 
;;           (mapcar #'cadr 
;;                   (car (prediction-sets:event-predictions predictions))))
;;          (partnum (partnum-column))
;;          (fignum (fignum-column))
;;          (repnum (repnum-column)) ; repetitions in alpha process 
;;          (eventnum (eventnum-column))
;;          (pitches (pitch-column reference-cpitch pitch-classes)))
;;     (with-open-file (o path :direction :output :if-exists :supersede)
;;       (format o "~&part figure alpha event pitch probability entropy codelength~%")
;;       (mapc #'(lambda (p f r ev pit pr e c)
;;                 (format o "~&~A ~A ~A ~A ~A ~,2F ~,2F ~,2F~%" 
;;                         p f r ev pit pr e c))
;;             partnum fignum repnum eventnum pitches probabilities entropies 
;;             codelengths)))
;;   path)
                
;; (defun partnum-column ()
;;   (let ((partnums '()))
;;     (dotimes (i (count-figures) (nreverse partnums))
;;       (let* ((fignum (1+ i))
;;              (partnum (get-part fignum)))
;;         (dotimes (j (get-repetition fignum))
;;           (dotimes (k (figure-length fignum))
;;             (push partnum partnums)))))))

;; (defun fignum-column ()
;;   (let ((fignums '()))
;;     (dotimes (i (count-figures) (nreverse fignums))
;;       (let ((fignum (1+ i)))
;;         (dotimes (j (get-repetition fignum))
;;           (dotimes (k (figure-length fignum))
;;             (push fignum fignums)))))))

;; (defun repnum-column ()
;;   (let ((repnums '()))
;;     (dotimes (i (count-figures) (nreverse repnums))
;;       (let ((fignum (1+ i)))
;;         (dotimes (j (get-repetition fignum))
;;           (let ((repnum (1+ j)))
;;             (dotimes (k (figure-length fignum))
;;               (push repnum repnums))))))))
      
;; (defun eventnum-column ()
;;   (let ((eventnums '()))
;;     (dotimes (i (count-figures) (nreverse eventnums))
;;       (let ((fignum (1+ i)))
;;         (dotimes (j (get-repetition fignum))
;;           (dotimes (k (figure-length fignum))
;;             (push (1+ k) eventnums)))))))

;; (defun pitch-column (reference pitch-classes)
;;   (let ((pitches '()))
;;     (dotimes (i (count-figures) (nreverse pitches))
;;       (let ((fignum (1+ i)))
;;         (dotimes (j (get-repetition fignum))
;;           (dolist (n (get-figure fignum))
;;             (push (clapping-cpitch n reference pitch-classes) pitches)))))))
