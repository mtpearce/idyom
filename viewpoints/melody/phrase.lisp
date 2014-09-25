;;;; ======================================================================
;;;; File:       phrase.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 marcusp>
;;;; Time-stamp: <2014-09-25 11:02:22 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Phrases

;; Is this note the first in a phrase?
(define-viewpoint (fiph test (phrase))
    ((events md:melodic-sequence) element) 
  :function (let ((phrase (phrase events)))
              (cond ((undefined-p phrase) +undefined+)
                    ((= phrase 1) 1)
                    (t 0)))
  :function* (if (= element 1) 1 (list -1 0)))

;; Is this note the last in a phrase?
(define-viewpoint (liph test (phrase))
    ((events md:melodic-sequence) element) 
  :function (let ((phrase (phrase events)))
              (cond ((undefined-p phrase) +undefined+)
                    ((= phrase -1) 1)
                    (t 0)))
  :function* (if (= element 1) -1 (list 1 0)))

;; Finds the duration of the preceding phrase iff the last event in
;; the sequence is at a phrase boundary.
(define-viewpoint (lphrase derived (phrase))
    ((events md:melodic-sequence) element) 
  :function (let ((e2 (last-element events)))
              (if (null e2) +undefined+
                  (let* ((phrase (phrase (list e2)))
                         (e1 (case phrase
                               (0 +undefined+)
                               (1 (last-element 
                                   (strip-until-true (get-viewpoint 'fiph)
                                                     (butlast events))))
                               (-1 (last-element 
                                    (strip-until-true (get-viewpoint 'liph)
                                                      (butlast events)))))))
                    (cond ((undefined-p e1) +undefined+)
                          ((null e1) 0)
                          (t (ioi (list e1 e2)))))))
  ;; TODO: function* 
  )



