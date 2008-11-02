;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       conklin2db.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-06-28 18:54:17 marcusp>                           
;;;; Time-stamp: <2008-10-31 16:18:54 marcusp>                           
;;;; ======================================================================

(cl:in-package #:conklin2db)

(defvar *description* "Chorale melodies harmonised by J.S. Bach.")
(defvar *original-timebase* 16)
(defvar *timebase* 96)
(defvar *timebase-ratio* (/ *timebase* *original-timebase*))
(defvar *midc* 60)

(defmethod import-data ((type (eql :conklin)) path description id)
  (setf *description* description)
  (mtp-admin:insert-dataset (conklin2db path) id))

(defun conklin2db (input-file)
  (setf *timebase-ratio* (/ *timebase* *original-timebase*))
  (let ((dataset))
    (with-open-stream (in (open input-file :direction :input))
      (let ((composition (read in nil 'eof)))
        (loop 
         (if (eql composition 'eof)
             (return nil)
             (push (convert-composition composition) dataset)))))
    (append (list *description* *timebase* *midc*) (reverse dataset))))

(defun convert-composition (composition)
  (cons (format nil "~A" (car composition))
        (convert-events (cdr composition))))

(defun convert-events (event-list)
  (labels ((convert (events result)
             (if (null events) result
                 (convert (butlast events)
                          (cons (convert-event events) result)))))
    (convert event-list '())))

(defun convert-event (event-list)
  (let* ((events (reverse event-list))
         (second-event (nth 0 events))
         (first-event (nth 1 events)))
    (list (onset second-event)
          (dur second-event)
          (deltast first-event second-event)
          (cpitch second-event)
          (keysig second-event)
          (barlength second-event)
          (pulses second-event)
          (phrase first-event second-event)
          (list 'voice 1))))

(defun onset (event)
  (list 'onset (* (nth 1 (assoc 'st event)) *timebase-ratio*)))

(defun dur (event)
  (list 'dur (* *timebase-ratio* (nth 1 (assoc 'dur event)))))

(defun cpitch (event) (list 'cpitch (nth 1 (assoc 'pitch event))))

(defun keysig (event) (assoc 'keysig event))

(defun barlength (event)
  (list 'barlength (* *timebase-ratio* (nth 1 (assoc 'timesig event)))))

(defun pulses (event)
  (list 'pulses (case (nth 1 (assoc 'timesig event))
                  (12 3)
                  (16 4))))

(defun phrase (event1 event2)
  (if (null event1)
      (list 'phrase 1)
      (let ((fermata1 (nth 1 (assoc 'fermata event1)))
            (fermata2 (nth 1 (assoc 'fermata event2))))
        (cond ((= fermata2 1)
               (list 'phrase -1))
              ((= fermata1 1)
               (list 'phrase 1))
              (t (list 'phrase 0))))))

(defun deltast (event1 event2)
  (if (null event1)
      (list 'deltast (* *timebase-ratio* (nth 1 (assoc 'st event2))))
      (let ((dur1 (nth 1 (assoc 'dur event1)))
            (onset1 (nth 1 (assoc 'st event1)))
            (onset2 (nth 1 (assoc 'st event2))))
        (list 'deltast (* (- onset2 (+ onset1 dur1)) *timebase-ratio*)))))




