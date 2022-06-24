;;;; ======================================================================
;;;; File:       gct2021.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2022-06-20 19:09:17 marcusp>                           
;;;; Time-stamp: <2022-06-24 14:17:29 marcusp>                           
;;;; ======================================================================

;;; An implementation of the updated General Chord Type (GCT)
;;; algorithm (Giannos & Cambouropoulos, 2021, 8th International
;;; Conference on Digital Libraries for Musicology)

(cl:in-package #:viewpoints)

(defvar *tonal-consonance-vector*  '(0 7 5 1 1 2 3 1 2 2 4 6))
(defvar *atonal-consonance-vector* '(0 1 1 1 1 1 1 2 2 2 2 2))

(defun general-chord-type (h-cpitch consonance-vector)
  (let* ((pitch-classes (mapcar #'(lambda (x) (mod x 12)) h-cpitch))
         (pitch-class-set (remove-duplicates pitch-classes :test #'=))
         (l (length pitch-class-set))
         (current-state (mapcar #'list
                                (mapcar #'list pitch-class-set)
                                (mapcar #'(lambda (x) (remove x pitch-class-set :test #'=)) pitch-class-set)
                                (make-list l :initial-element 0))))
    (loop while (not (some #'(lambda (x) (= (length x) l)) (mapcar #'car current-state)))
       do (setf current-state (get-next-state current-state consonance-vector)))
    (if (= (length current-state) 1)
        (state->gct (car current-state))
        (state->gct (car (sort current-state
                               #'(lambda (x y) (lowest-root-p x y h-cpitch pitch-classes)) :key #'car))))))

(defun lowest-root-p (pcs1 pcs2 h-cpitch pitch-classes)
  (let ((root1 (elt h-cpitch (position (car pcs1) pitch-classes)))
        (root2 (elt h-cpitch (position (car pcs2) pitch-classes))))
    (< root1 root2)))

(defun state->gct (state)
  (let* ((pc-set (car state))
         (root (car pc-set))
         (chord-type (mapcar #'(lambda (x) (utils:subtract-mod-n x root 12)) pc-set))
         (prev 0)
         (chord-type (mapcar #'(lambda (x) (prog1 (if (< x prev) (+ x 12) x) (setf prev x))) chord-type)))
    (list root chord-type)))

(defun chord-type (gct) (second gct))

(defun get-next-state (state consonance-vector)
  (let ((scores (make-hash-table))
        (min-score 1000))
    (dolist (s state)
      (let ((current (first s))
            (remaining (second s))
            (score (third s)))
        (declare (ignorable score))
        (dolist (r remaining)
          (let* ((candidate (append current (list r)))
                 (new-remaining (remove r remaining :test #'=))
                 (new-score (get-new-score r current consonance-vector))
                 (new-state (list candidate new-remaining new-score)))
            ;; (print new-state)
            (when (< new-score min-score) (setf min-score new-score))
            (setf (gethash new-score scores)
                  (cons new-state (gethash new-score scores)))))))
    (gethash min-score scores)))

(defun get-new-score (new-pitch-class pitch-classes consonance-vector)
  (let* ((intervals (mapcar #'(lambda (x) (utils:subtract-mod-n new-pitch-class x 12)) pitch-classes))
         (scores (mapcar #'(lambda (x) (elt consonance-vector x)) intervals))
         (score (reduce #'+ scores)))
    score))

                 

         
