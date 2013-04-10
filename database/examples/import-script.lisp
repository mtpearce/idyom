;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       import-script.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2008-11-03 11:24:51 marcusp>
;;;; Time-stamp: <2008-11-03 11:48:02 marcusp>
;;;; ======================================================================

;; This is the script I use to import data into the amuse-mtp
;; backend. It is not supposed to be general purpose.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *datasets* '())
  (defvar *data-dir* "/home/marcusp/research/data/"))

(defmacro create-dataset (path description type)
  `(push (list ,path ,description ,type) *datasets*))

(create-dataset 
 (concatenate 'string *data-dir* "kern/nova/")
 "Songs and ballads from Nova Scotia, Canada." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/chorales/")
 "Chorale melodies harmonised by J.S. Bach." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "lisp/chorales100.lisp")
 "Chorale melodies harmonised by J.S. Bach." 
 :lisp)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/elsass/")
 "Alsatian folk songs from the Essen Folk Song Collection." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/jugoslav/")
 "Yugoslavian folk songs from the Essen Folk Song Collection." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/schweiz/")
 "Swiss folk songs from the Essen Folk Song Collection." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/oesterrh/")
 "Austrian folk songs from the Essen Folk Song Collection." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/fink/")
 "German folk songs from the Essen Folk Song Collection: fink." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/erk/")
 "German folk songs from the Essen Folk Song Collection: erk." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/boehme/")
 "German folk songs from the Essen Folk Song Collection: boehme." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/ballad/")
 "German folk songs from the Essen Folk Song Collection: ballad." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/allerkbd/")
 "German folk songs from the Essen Folk Song Collection: allerkbd." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/altdeu/")
 "German folk songs from the Essen Folk Song Collection: altdeu." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/dva/")
 "German folk songs from the Essen Folk Song Collection: dva." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/zuccal/")
 "German folk songs from the Essen Folk Song Collection: zuccal." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/europa/deutschl/kinder/")
 "German folk songs from the Essen Folk Song Collection: kinder." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/shanxi237/")
 "Chinese folk songs (Shanxi region) from the Essen Folk Song Collection."
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/psychology/cudlun95/")
 "Single interval contexts used in the experiments of Cuddy and Lunney (1995)."
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/psychology/schell96/")
 "British folk song fragments used in the experiments of Schellenberg (1996)."
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/psychology/FranklandCohen04/")
 "Melodies used in the experiments of Frankland and Cohen (2004)." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/asia/china/natmin/")
 "Chinese folk songs from the Essen Folk Song Collection: natmin." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/asia/china/han/")
 "Chinese folk songs from the Essen Folk Song Collection: han." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/essen/asia/china/shanxi/")
 "Chinese folk songs from the Essen Folk Song Collection: shanxi." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/sagrillo/ireland/")
 "Irish Folksongs encoded by Daiman Sagrillo." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/sagrillo/lorraine/")
 "Folksongs from Lorraine encoded by Daiman Sagrillo." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "kern/sagrillo/lux/")
 "Folksongs from Luxembourg encoded by Daiman Sagrillo." 
 :krn)
(create-dataset 
 (concatenate 'string *data-dir* "lisp/gradus.lisp")
 "Gradus for Soprano Sax by Philip Glass (February 1968)." 
 :lisp)
(create-dataset 
 (concatenate 'string *data-dir* "lisp/bach-bass-arias.lisp")
 "Bass arias from Bach's cantatas."
 :lisp)
(create-dataset 
 (concatenate 'string *data-dir* "lisp/bach-soprano-arias.lisp")
 "Soprano arias from Bach's cantatas."
 :lisp)
(create-dataset 
 (concatenate 'string *data-dir* "midi/segmentation/")
 "Pop melodies used in a segmentation experiment by Daniel Muellensiefen."
 :mid)
(create-dataset 
 "/home/marcusp/research/projects/idm/data/Hymns/data/Segmentation/b/"
 "Hymn melodies from Hymns Ancient and Modern."
 :mid)

;; (create-dataset 
;;  (concatenate 'string *data-dir* "midi/Bach/CelloSuites/all/")
;;  "J. S. Bach's Cello Suites."
;;  :mid)

(defun populate-database ()
  (mtp-admin:initialise-database)
  (let ((dataset-id 0))
    (mapc #'(lambda (d)
              (let ((path (first d))
                    (description (second d))
                    (type (third d)))
                (mtp-admin:import-data type path description dataset-id)
                (incf dataset-id)))
          (reverse *datasets*))
    ;(import-bass-arias dataset-id)
    ;(import-soprano-arias (1+ dataset-id))
    ))
      
(defun import-bass-arias (id)
  (let* ((base-dir "/home/marcusp/research/projects/vostroost/data/Arias/")
         (data-dir (concatenate 'string base-dir "Bass/"))
         (filenames (mtp-admin::read-object-from-file 
                     (concatenate 'string base-dir "bass-arias.lisp")))
         (header (list "Bass arias from Bach's cantatas." 96 60)))
    (%import-arias id data-dir filenames header)))

(defun import-soprano-arias (id)
  (let* ((base-dir "/home/marcusp/research/projects/vostroost/data/Arias/")
         (data-dir (concatenate 'string base-dir "Soprano/"))
         (filenames (mtp-admin::read-object-from-file 
                     (concatenate 'string base-dir "soprano-arias.lisp")))
         (header (list "Soprano arias from Bach's cantatas." 96 60)))
    (%import-arias id data-dir filenames header)))
        
(defun %import-arias (id data-dir filenames header)
  (let ((data '()))
    (dolist (f filenames)
      (let* ((filename (car f))
             (spine (cadr f))
             (filepath (concatenate 'string data-dir filename))
             (kern2db::*voices* (list spine)))
        (push (fourth (kern2db::kern2db filepath f)) data)))
    (mtp-admin:insert-dataset (append header (reverse data)) id)))
