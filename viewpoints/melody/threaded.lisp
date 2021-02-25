;;;; ======================================================================
;;;; File:       threaded.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 marcusp>
;;;; Time-stamp: <2016-04-14 10:14:38 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Threaded viewpoints

;; cpint
(define-threaded-viewpoint thrbar cpint fib md:melodic-sequence)
(define-threaded-viewpoint thrfiph cpint fiph md:melodic-sequence)
(define-threaded-viewpoint thrliph cpint liph md:melodic-sequence)
(define-threaded-viewpoint thrqu cpint crotchet md:melodic-sequence)
(define-threaded-viewpoint thrtactus cpint tactus md:melodic-sequence)

;; cpintfref
(define-threaded-viewpoint thrintfrefliph cpintfref liph md:melodic-sequence)


