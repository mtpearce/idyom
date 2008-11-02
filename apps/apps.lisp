;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       apps.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2005-11-27 16:27:35 marcusp>
;;;; Time-stamp: <2008-10-06 10:23:56 marcusp>
;;;; ======================================================================

(cl:in-package #:apps) 

;;; Paths 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *root-dir*
    (concatenate 'string
                 #+cmu (namestring (car (ext:search-list "home:")))
                 #+sbcl (sb-ext:posix-getenv "HOME")
                 #+allegro "/home/marcusp"
                 "/research/projects/idm/code/"))
  (setf mvs:*ep-cache-dir* (string-append *root-dir* "lisp/data/cache/")))

;;; Data

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defvar *datasets* '())
;;   (defvar *data-dir* "/home/marcusp/research/data/"))

;; (defmacro create-dataset (path description type)
;;   `(push (list ,path ,description ,type) *datasets*))

;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/nova/")
;;  "Songs and ballads from Nova Scotia, Canada." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/chorales/")
;;  "Chorale melodies harmonised by J.S. Bach." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "lisp/chorales100.lisp")
;;  "Chorale melodies harmonised by J.S. Bach." 
;;  :lisp)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/elsass/")
;;  "Alsatian folk songs from the Essen Folk Song Collection." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/jugoslav/")
;;  "Yugoslavian folk songs from the Essen Folk Song Collection." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/schweiz/")
;;  "Swiss folk songs from the Essen Folk Song Collection." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/oesterrh/")
;;  "Austrian folk songs from the Essen Folk Song Collection." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/fink/")
;;  "German folk songs from the Essen Folk Song Collection: fink." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/erk/")
;;  "German folk songs from the Essen Folk Song Collection: erk." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/boehme/")
;;  "German folk songs from the Essen Folk Song Collection: boehme." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/ballad/")
;;  "German folk songs from the Essen Folk Song Collection: ballad." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/allerkbd/")
;;  "German folk songs from the Essen Folk Song Collection: allerkbd." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/altdeu/")
;;  "German folk songs from the Essen Folk Song Collection: altdeu." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/dva/")
;;  "German folk songs from the Essen Folk Song Collection: dva." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/zuccal/")
;;  "German folk songs from the Essen Folk Song Collection: zuccal." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/europa/deutschl/kinder/")
;;  "German folk songs from the Essen Folk Song Collection: kinder." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/shanxi237/")
;;  "Chinese folk songs (Shanxi region) from the Essen Folk Song Collection."
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/psychology/cudlun95/")
;;  "Single interval contexts used in the experiments of Cuddy and Lunney (1995)."
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/psychology/schell96/")
;;  "British folk song fragments used in the experiments of Schellenberg (1996)."
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/psychology/FranklandCohen04/")
;;  "Melodies used in the experiments of Frankland and Cohen (2004)." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/asia/china/natmin/")
;;  "Chinese folk songs from the Essen Folk Song Collection: natmin." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/asia/china/han/")
;;  "Chinese folk songs from the Essen Folk Song Collection: han." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/essen/asia/china/shanxi/")
;;  "Chinese folk songs from the Essen Folk Song Collection: shanxi." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/sagrillo/ireland/")
;;  "Irish Folksongs encoded by Daiman Sagrillo." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/sagrillo/lorraine/")
;;  "Folksongs from Lorraine encoded by Daiman Sagrillo." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "kern/sagrillo/lux/")
;;  "Folksongs from Luxembourg encoded by Daiman Sagrillo." 
;;  :krn)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "lisp/gradus.lisp")
;;  "Gradus for Soprano Sax by Philip Glass (February 1968)." 
;;  :lisp)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "lisp/bach-bass-arias.lisp")
;;  "Bass arias from Bach's cantatas."
;;  :lisp)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "lisp/bach-soprano-arias.lisp")
;;  "Soprano arias from Bach's cantatas."
;;  :lisp)
;; (create-dataset 
;;  (concatenate 'string *data-dir* "midi/segmentation/")
;;  "Pop melodies used in a segmentation experiment by Daniel Muellensiefen."
;;  :mid)
;; ;; (create-dataset 
;; ;;  (concatenate 'string *data-dir* "midi/Bach/CelloSuites/all/")
;; ;;  "J. S. Bach's Cello Suites."
;; ;;  :mid)

;; (defun populate-database ()
;;   (md:initialise-database)
;;   (let ((dataset-id 0))
;;     (mapc #'(lambda (d)
;;               (let ((path (first d))
;;                     (description (second d))
;;                     (type (third d)))
;;                 (md:import-data type path description dataset-id)
;;                 (incf dataset-id)))
;;           (reverse *datasets*))
;;     ;(import-bass-arias dataset-id)
;;     ;(import-soprano-arias (1+ dataset-id))
;;     ))
      
;; (defun import-bass-arias (id)
;;   (let* ((base-dir "/home/marcusp/research/projects/vostroost/data/Arias/")
;;          (data-dir (concatenate 'string base-dir "Bass/"))
;;          (filenames (utils:read-object-from-file 
;;                      (concatenate 'string base-dir "bass-arias.lisp")))
;;          (header (list "Bass arias from Bach's cantatas." 96 60)))
;;     (%import-arias id data-dir filenames header)))

;; (defun import-soprano-arias (id)
;;   (let* ((base-dir "/home/marcusp/research/projects/vostroost/data/Arias/")
;;          (data-dir (concatenate 'string base-dir "Soprano/"))
;;          (filenames (utils:read-object-from-file 
;;                      (concatenate 'string base-dir "soprano-arias.lisp")))
;;          (header (list "Soprano arias from Bach's cantatas." 96 60)))
;;     (%import-arias id data-dir filenames header)))
        
;; (defun %import-arias (id data-dir filenames header)
;;   (let ((data '()))
;;     (dolist (f filenames)
;;       (let* ((filename (car f))
;;              (spine (cadr f))
;;              (filepath (concatenate 'string data-dir filename))
;;              (kern2db::*voices* (list spine)))
;;         (push (fourth (kern2db::kern2db filepath f)) data)))
;;     (md:insert-dataset (append header (reverse data)) id)))


;; ;;; Analysis 

;; (defun phrased-frequencies (dataset-ids feature phrase-points) 
;;   (let* ((viewpoint (viewpoints:get-viewpoint feature))
;;          (phrase-feature (case phrase-points 
;;                     (-1 (list (list feature 'liph)))
;;                     (0  (list feature))
;;                     (1  (list (list feature 'fiph)))))
;;          (phrase-viewpoint (viewpoints:get-viewpoint phrase-feature))
;;          (dataset (apply #'md:get-event-sequences dataset-ids))
;;          (data (reduce #'append 
;;                        (viewpoints:viewpoint-sequences phrase-viewpoint dataset)))
;;          (dist nil))
;;     (viewpoints:set-alphabet-from-dataset viewpoint dataset)
;;     (dolist (a (viewpoints:viewpoint-alphabet viewpoint) nil)
;;       (push (list a (float 
;;                      (/ (count (list a phrase-points) data :test #'equal)
;;                         (count phrase-points data :key #'cadr :test #'=))
;;                      0.0d0)) 
;;             dist))
;;     (dolist (d dist nil) 
;;       (format t "~&~3,S: ~,2F~%" (car d) (cadr d)))
;;     dist))

;; (defun frequencies (dataset-ids feature) 
;;   (let* ((viewpoint (viewpoints:get-viewpoint feature))
;;          (dataset (apply #'md:get-event-sequences dataset-ids))
;;          (data (reduce #'append 
;;                        (viewpoints:viewpoint-sequences viewpoint dataset)))
;;          (dist nil))
;;     (viewpoints:set-alphabet-from-dataset viewpoint dataset)
;;     (dolist (a (viewpoints:viewpoint-alphabet viewpoint) nil)
;;       (push (list a (count a data :test #'equal))
;;             dist))
;;     (dolist (d dist nil)
;;       (format t "~&~S: ~S~%" (car d) (cadr d)))
;;     dist))

;; (defun foo (dataset-id composition-id feature)
;;   (let* ((data (md:get-event-sequence dataset-id composition-id))
;;          (viewpoint (viewpoints:get-viewpoint feature))
;;          (vdata (viewpoints:viewpoint-sequence viewpoint data)))
;;     (viewpoints:set-alphabet-from-dataset viewpoint (list data))
;;     (let ((ppm (ppm:make-ppm (viewpoints:viewpoint-alphabet viewpoint)
;;                              :escape :c :mixtures t
;;                              :update-exclusion nil :order-bound nil)))
;;       (ppm:model-sequence ppm vdata :construct? t :predict? nil)
;;       (ppm:write-model-to-postscript ppm)
;;       ppm)))

;; (defun isochronous-p (event-sequence) 
;;   (and 
;;    (every #'(lambda (x) (= (md:get-attribute (car event-sequence) 'dur) x))
;;           (mapcar #'(lambda (event) 
;;                       (md:get-attribute event 'dur)) 
;;                   event-sequence))
;;    (every #'zerop (cdr (mapcar #'(lambda (event) 
;;                                    (md:get-attribute event 
;;                                                      'deltast)) 
;;                                event-sequence)))))

;; (defun get-isochronous-compositions (&rest dataset-ids) 
;;   (mapc #'(lambda (sequence) 
;;             (when (isochronous-p sequence)
;;               (format t "~&~{~A ~}~%" (md:get-id (car sequence)))))
;;         (apply #'md:get-event-sequences dataset-ids))
;;   nil)

;; (defun get-alphabet-from-dataset (feature dataset) 
;;   (let ((viewpoint (viewpoints:get-viewpoint feature)))
;;     (viewpoints:set-alphabet-from-dataset viewpoint dataset)
;;     (viewpoints:viewpoint-alphabet viewpoint)))

