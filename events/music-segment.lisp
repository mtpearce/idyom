;;; Basic version of amuse-segment package.
;;;


(cl:in-package #:cl-user)

(defpackage #:music-segment
  (:use #:common-lisp)
  (:export "GROUND-TRUTH-SEGMENTER-AFTER" "GROUND-TRUTH-SEGMENTER-BEFORE" 
	   "BOUNDARY-STRENGTH")
  (:documentation "Music segmentation."))


(cl:in-package #:music-segment)

(defun ground-truth-segmenter-after (event) 0)
(defun ground-truth-segmenter-before (event) 0)
(defun boundary-strength (x event y) 0)
