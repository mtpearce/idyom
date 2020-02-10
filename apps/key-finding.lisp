;;;; ======================================================================
;;;; File:       key-finding.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2019-03-16 14:14:59 marcusp>                           
;;;; Time-stamp: <2020-02-10 16:56:56 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;; The Krumhansl-Schmuckler key-finding algorithm. 
;;;;
;;;; ======================================================================

(defpackage #:key-finding
  (:use #:cl)
  (:export #:find-keys #:find-key)
  (:documentation "The Krumhansl-Schmuckler key-finding algorithm."))

(cl:in-package #:key-finding)

;; Krumhansl & Kessler (1982)
(defvar *kk-major* '(6.35 2.23 3.48 2.33 4.38 4.09 2.52 5.19 2.39 3.66 2.29 2.88))
(defvar *kk-minor* '(6.33 2.68 3.52 5.38 2.60 3.53 2.54 4.75 3.98 2.69 3.34 3.17))

;; Temperley, D. (1999). Music Perception, 17, 65-100. 
(defvar *temperley-major* '(5 2 3.5 2 4.5 4 2 4.5 2 3.5 1.5 4))
(defvar *temperley-minor* '(5 2 3.5 4.5 2 4 2 4.5 3.5 2 1.5 4))

(defun find-keys (dataset-id &key (method :temperley) (update-db? nil))
  (let ((count (idyom-db:count-compositions dataset-id))
        (correct 0))
    (dotimes (i count)
      (let* ((key (find-key dataset-id i :method method))
             (keysig (first key))
             (mode (second key))
             (tonic (third key))
             (coefficient (fourth key))
             (actual-tonic (car (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'referent) (md:get-music-objects dataset-id i))))
             (actual-keysig (idyom-db:get-attribute (idyom-db:get-event dataset-id i 0) 'keysig))
             (actual-mode (idyom-db:get-attribute (idyom-db:get-event dataset-id i 0) 'mode)))
        #.(clsql:locally-enable-sql-reader-syntax)
        (when update-db?
          (clsql:update-records [mtp-event] :av-pairs `((keysig ,keysig) (mode ,mode)) :where [and [= [dataset-id] dataset-id] [= [composition-id] i]]))
        #.(clsql:restore-sql-reader-syntax-state)
        (if (and actual-keysig mode (= keysig actual-keysig) (= mode actual-mode))
            (incf correct)
            (format t "~&~A ~A | ~A ~A ~A | ~A ~A ~A | ~A~%" dataset-id i tonic mode keysig actual-tonic actual-mode actual-keysig coefficient))))
    (format t "~&Score = ~A / ~A (~A\%)~%" correct count (utils:round-to-nearest-decimal-place (* (float (/ correct count)) 100) 0))))

(Defun find-key (dataset-id composition-id &key (method :temperley))
  (let ((pcd (pitch-class-distribution dataset-id composition-id))
        (major-profile (case method
                         (:temperley *temperley-major*)
                         (t *kk-major*)))
        (minor-profile (case method
                         (:temperley *temperley-minor*)
                         (t *kk-minor*)))
        (matching-profile nil)
        (coefficient 0)
        (tonic 0)
        (mode 0))
    (dotimes (i 12)
      (let* ((key-profile (utils:rotate major-profile i))
             (cor (utils:cor key-profile (mapcar #'cadr pcd))))
        (when (> cor coefficient)
          (setf coefficient cor
                matching-profile key-profile
                tonic (mod (- 12 i) 12)
                mode 0))))
    (dotimes (i 12)
      (let* ((key-profile (utils:rotate minor-profile i))
             (cor (utils:cor key-profile (mapcar #'cadr pcd))))
        (when (> cor coefficient)
          (setf coefficient cor
                matching-profile key-profile
                tonic (mod (- 12 i) 12)
                mode 9))))
    (let ((keysig (tonic->keysig tonic mode)))
      (list keysig mode tonic coefficient))))

(defun tonic->keysig (tonic mode)
  (if (= mode 0)
      ;; major
      (if (oddp tonic)
          (- tonic 6)
          (- tonic (* 12 (floor (/ tonic 7)))))
      ;; minor
      (if (evenp tonic)
          (- tonic 3)
          (let ((keysig (- tonic 9)))
            (if (> (abs keysig) 7) (mod keysig 12) keysig)))))

(defun pitch-class-distribution (dataset-id composition-id)
  (let ((pitch (viewpoints:viewpoint-sequence
                (viewpoints:get-viewpoint 'cpitch-class)
                 (md:get-music-objects dataset-id composition-id)))
        (dur (viewpoints:viewpoint-sequence
              (viewpoints:get-viewpoint 'dur)
              (md:get-music-objects dataset-id composition-id))))
    (let ((pcd (make-hash-table))
          (total-dur 0))
      (dotimes (i 12)
        (setf (gethash i pcd) 0))
      (do ((p pitch (cdr p))
           (d dur (cdr d)))
          ((or (null p) (null d)))
        (let ((p (car p))
              (d (car d)))
          (setf (gethash p pcd) (+ (gethash p pcd) d)
                total-dur (+ total-dur d))))
      (sort (mapcar #'(lambda (x) (list (car x) (/ (cadr x) total-dur)))
                    (utils:hash-table->alist pcd))
            #'<
            :key #'car))))

(defun durational-accent (duration &key (tau 0.5) (index 2))
  "Parncutt, R. (1994). Music Perception. 11(4), 409-464."
  (- 1 (expt (exp (- (/ duration tau))) index)))
