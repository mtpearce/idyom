;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-01-30 15:33:44 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Basic Viewpoints 

(define-basic-viewpoint onset (events)
  (amuse:timepoint (last-element events)))

(define-basic-viewpoint cpitch (events)
  (amuse:midi-pitch-number (last-element events)))

(define-basic-viewpoint dur (events)
  (amuse:duration (last-element events)))

(define-basic-viewpoint keysig (events)
  (amuse:key-signature-sharps 
   (car (amuse:get-applicable-key-signatures 
         (last-element events)
         nil))))

(define-basic-viewpoint mode (events)
  ;; NB: By default, amuse returns midi mode indicators: i.e., 0 =
  ;; major; 1 = minor. The viewpoints code has a more general
  ;; representation that is intended to cover the church modes as
  ;; well: 0 = major; 9 = minor reflecting the fact that the minor
  ;; mode corresponds to rotation of the pitch class set corresponding
  ;; to its relative major scale by 9 semitones (see Balzano, 1982). 
  (let ((midi-mode (amuse:key-signature-mode 
                    (car (amuse:get-applicable-key-signatures 
                          (last-element events)
                          nil)))))
    (case midi-mode 
      (0 0)
      (1 9))))
  
(define-basic-viewpoint tempo (events)
  (amuse:bpm 
   (car (amuse:get-applicable-tempi
         (last-element events)
         nil))))

(define-basic-viewpoint pulses (events)
  (amuse:beat-units-per-bar
   (car (amuse:get-applicable-time-signatures
         (last-element events)
         nil))))

(define-basic-viewpoint barlength (events)
  (let* ((last-element (last-element events))
         (timesig (car (amuse:get-applicable-time-signatures last-element nil)))
         (timebase (* (amuse:duration (amuse:crotchet last-element)) 4)))
    (/ (* (amuse:beat-units-per-bar timesig) timebase)
       (amuse:beat-units timesig))))

(define-basic-viewpoint deltast (events)
  (if (slot-exists-p (car (last events)) 'amuse-mtp::deltast)
      ;; Special case for MTP-EVENT
      (amuse-mtp::%mtp-deltast (car (last events)))
      (let* ((last-element (last-element events))
             (penultimate-element (penultimate-element events)))
        (cond ((and last-element penultimate-element)
               (- (amuse:timepoint last-element)
                  (+ (amuse:timepoint penultimate-element)
                     (amuse:duration penultimate-element))))
              ((and last-element (null penultimate-element))
               0)
              (t nil)))))

(define-basic-viewpoint bioi (events)
  ;; BIOI = Basic IOI (IOI as a basic feature) 
  (if (slot-exists-p (car (last events)) 'amuse-mtp::bioi)
      ;; Special case for MTP-EVENT
      (amuse-mtp::%mtp-bioi (car (last events)))
      (let* ((last-element (last-element events))
             (penultimate-element (penultimate-element events)))
        (cond ((and last-element penultimate-element)
               (- (amuse:timepoint last-element)
                  (amuse:timepoint penultimate-element)))
              ((and last-element (null penultimate-element))
               0)
              (t nil)))))

(define-basic-viewpoint phrase (events) 
  (let ((after  (amuse-segmentation:ground-truth-segmenter-after (car events)))
        (before (amuse-segmentation:ground-truth-segmenter-before (car events)))
        (last-element (last-element events)))
    (cond ((= 1 (amuse-segmentation:boundary-strength after last-element nil))
           1)
          ((= 1 (amuse-segmentation:boundary-strength before last-element nil))
           -1)
          (t 0))))

(define-basic-viewpoint mpitch (events) 
  (let ((result nil))
    (handler-case 
        (setf result 
              (+ (amuse:diatonic-pitch-mp
                  (last-element events))
                 12))
      (error () (setf result viewpoints:+undefined+)))
    result))

(define-basic-viewpoint accidental (events)
  (let ((result nil))
    (handler-case 
        (setf result (amuse:diatonic-pitch-accidental 
                      (amuse:diatonic-pitch (last-element events))))
      (error () (setf result viewpoints:+undefined+)))
    result))

(define-basic-viewpoint dyn (events)
  ;; TODO: we need an amuse interface to dynamics to make this general
  (let ((dyn (amuse-mtp::%mtp-dyn (car (last events)))))
    (if dyn
        dyn        
        viewpoints:+undefined+)))

(define-basic-viewpoint ornament (events)
  ;; TODO: we need an amuse interface to ornaments to make this general
  (let ((x (amuse-mtp::%mtp-ornament (car (last events)))))
    (if x x viewpoints:+undefined+)))

(define-basic-viewpoint voice (events)
  ;; TODO: we need an amuse interface to voicing to make this general
  (let ((x (amuse-mtp::%mtp-voice (car (last events)))))
    (if x x viewpoints:+undefined+)))

(define-basic-viewpoint comma (events)
  ;; TODO: we need an amuse interface to commas to make this general
  (let ((x (amuse-mtp::%mtp-comma (car (last events)))))
    (if x x viewpoints:+undefined+)))

(define-basic-viewpoint articulation (events)
  ;; TODO: we need an amuse interface to articulations to make this general
  (let ((x (amuse-mtp::%mtp-articulation (car (last events)))))
    (if x x viewpoints:+undefined+)))


 
                    


