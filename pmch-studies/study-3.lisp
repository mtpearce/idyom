;;; =======================================================================
;;;; File:       study-3.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-26 19:12:50 peter>                        
;;;; Time-stamp: <2017-07-26 19:17:20 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides utility functions for Study 3 of Peter's PhD,
;;;; primarily the stimulus generation part.

(cl:in-package #:pmch-s3)

(defparameter *genres* '(:classical :popular :jazz))

(defparameter *h-cpitch-analysis-files*
  '("/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-1-harmonic-reduction-t/resampling-training-set-size-987/h-cpitch/dat_from_idyom/1-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"
    "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-2-harmonic-reduction-nil/resampling-training-set-size-714/h-cpitch/dat_from_idyom/2-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"
    "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-3-harmonic-reduction-nil/resampling-training-set-size-1024/h-cpitch/dat_from_idyom/3-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"))
