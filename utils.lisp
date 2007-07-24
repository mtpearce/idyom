(cl:in-package #:viewpoints)

;;; lists 

(defun flatten (list)
  "Recursively, flattens a list of lists into a single flat list."
  (labels ((flat (list result)
             (if (null list) (reverse result)
                 (let ((head (car list))
                       (tail (cdr list)))
                   (if (atom head)
                       (flat tail (cons head result))
                       (flat tail (append (flatten head) result)))))))
    (flat list '())))


(defun last-element (list)
  "Returns the last element of a list."
  (car (reverse list)))

(defun penultimate-element (list) 
  "Returns the penultimate element of a list."
  (nth 1 (reverse list)))

;;; viewpoints 

(defun strip-until-true (test-viewpoint events)
  "Return the longest prefix of the list EVENTS such that
TEST-VIEWPOINT returns true (1 rather than 0)."
  (cond ((null events) '())
        ((undefined-p (viewpoint-element test-viewpoint events))
         (strip-until-true test-viewpoint (butlast events)))
        ((= (viewpoint-element test-viewpoint events) 1) events)
        (t (strip-until-true test-viewpoint (butlast events)))))
