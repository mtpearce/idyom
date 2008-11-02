;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       tobias2db.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2007-04-19 11:15:57 marcusp>
;;;; Time-stamp: <2008-10-31 16:49:01 marcusp>
;;;; ======================================================================

(cl:in-package #:tobias2db) 

(defparameter *duration* 24)
(defvar *timebase* 96 "Basic time units per semibreve")
(defvar *middle-c* 60 "Chromatic integer mapping for c_4")

(defmethod import-data ((type (eql :tobias)) path description id)
  (mtp-admin:insert-dataset (tobias2db path description) id))

(defun tobias2db (path description) 
  (let ((data (get-data path)))
    (append (list description *timebase* *middle-c*) data)))

(defun get-data (path)
  (let ((pitch-sequences))
    (with-open-file (i path :direction :input)
      (do ((input (read-line i nil 'eof) (read-line i nil 'eof)))
          ((eq input 'eof))
        (let ((split-input nil))
          (cond ((find #\Tab input)
                 (setf split-input 
                       (split-string input (format nil "~C" #\Tab))))
                ((find #\Space input)
                 (setf split-input 
                       (split-string input (format nil "~C" #\Space))))
                (t (error "Unrecognised delimiter token.")))
          (push (mapcar #'parse-integer split-input) pitch-sequences))))
    (convert-pitch-sequences (nreverse pitch-sequences))))

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

(defun convert-pitch-sequences (pitch-sequences)
  (let ((id 0)
        (onset 0)
        (data '()))
    (dolist (pseq pitch-sequences (nreverse data))
      (let ((event-sequence ()))
        (dolist (pitch pseq)
          (push (make-event-alist pitch onset) event-sequence)
          (incf onset *duration*))
        (setf onset 0)
        (push (cons (format nil "~A" id) (nreverse event-sequence)) data))
      (incf id))))

(defun make-event-alist (pitch onset)
  (list (list :onset onset)
        (list :dur *duration*)
        (list :deltast 0)
        (list :cpitch pitch)
        (list :voice 1)))
 
