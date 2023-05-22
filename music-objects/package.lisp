(cl:defpackage #:music-objects
  (:use #:common-lisp)
  (:nicknames #:mo #:music-data #:md)
  (:export 
   ;; Music objects & their properties
   #:music-object #:music-dataset #:music-composition #:music-event #:music-slice
   #:music-sequence #:melodic-sequence #:harmonic-sequence
   #:onset #:chromatic-pitch #:duration #:key-signature #:mode
   #:tempo #:pulses #:barlength #:deltast #:bioi #:phrase
   #:morphetic-pitch #:accidental #:dynamics #:ornament #:voice
   #:comma #:articulation #:description #:midc #:timebase #:vertint12
   ;; identifiers
   #:dataset-identifier #:composition-identifier #:event-identifier
   #:get-dataset-index #:get-composition-index #:get-event-index
   #:make-dataset-id #:make-composition-id #:make-event-id 
   #:copy-identifier #:get-identifier
   #:lookup-dataset #:lookup-composition #:lookup-event
   ;; getting music objects from db
   #:get-dataset #:get-composition #:get-event
   #:get-music-objects #:get-event-sequence #:get-event-sequences
   #:get-harmonic-sequence #:get-harmonic-sequences
   ;; accessing properties of music objects
   #:get-attribute #:set-attribute #:count-compositions #:get-description
   #:copy-event #:music-symbol #:*md-music-slots* #:*md-time-slots*
   ;; representation
   #:*timebase*
   ;; midi export
   #:export-midi #:preview
   )
  (:documentation "Representations of music objects."))

