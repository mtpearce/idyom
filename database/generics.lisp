;;;; ======================================================================
;;;; File:       generics.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2004-10-28 11:56:59 marcusp>
;;;; Time-stamp: <2022-08-03 18:30:19 marcusp>
;;;; ======================================================================

(cl:in-package #:idyom-db)

(defgeneric export-data (object type dir &key filename)
  (:documentation "Exports <object> in format <type> to directory
  <dir>. This directory is made if it doesn't exist already. The
  optional argument <filename> specifies the filename for the exported
  file, including extension, as a string (e.g. audio.mid). The
  <filename> argument is ignored if more than one exported file is
  created (e.g. when exporting a dataset). Returns the path of the
  output at the greatest detail possible: if the output is one file, a
  file path is returned, whereas if the output is several files, the
  containing directory is returned."))

(defgeneric preview (object &key temp-dir play-audio display-score)
  (:documentation "Previews <object> using <temp-dir> as the temporary
  directory. If <play-audio>, an audio preview is played. If
  <display-score>, a pdf score is opened. Returns t if the user
  requests to break out of a loop, nil otherwise."))

(defgeneric import-data (type path description id))
(defgeneric insert-composition (dataset composition id))
(defgeneric insert-event (composition event id))
(defgeneric get-attribute (event attribute))
(defgeneric set-attribute (event attribute value))
(defgeneric copy-event (object))

