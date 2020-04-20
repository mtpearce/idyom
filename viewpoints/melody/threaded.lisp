;;;; ======================================================================
;;;; File:       threaded.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 marcusp>
;;;; Time-stamp: <2019-03-25 13:01:24 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Threaded viewpoints

;; cpint
(define-threaded-viewpoint thr-cpint-fib cpint fib md:melodic-sequence)            ;; thrbar
(define-threaded-viewpoint thr-cpint-fiph cpint fiph md:melodic-sequence)          ;; thrfiph
(define-threaded-viewpoint thr-cpint-liph cpint liph md:melodic-sequence)          ;; thrliph
(define-threaded-viewpoint thr-cpint-crotchet cpint crotchet md:melodic-sequence)  ;; thrqu
(define-threaded-viewpoint thr-cpint-tactus cpint tactus md:melodic-sequence)      ;; thrtactus

;; cpintfref
(define-threaded-viewpoint thr-cpintfref-liph cpintfref liph md:melodic-sequence)  ;; thrintfrefliph
(define-threaded-viewpoint thr-cpintfref-fib cpintfref fib md:melodic-sequence)

;; cpint x cpintfref
(define-threaded-viewpoint thr-cpint_cpintfref-liph (cpint cpintfref) liph md:melodic-sequence)
(define-threaded-viewpoint thr-cpint_cpintfref-fib (cpint cpintfref) fib md:melodic-sequence)
