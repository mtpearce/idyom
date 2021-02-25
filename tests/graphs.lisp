;; Collection of functions that operate on directed graphs represented by vectors.
;; A vector E of length N describes a graph G of order N (the number of vertices in the
;; graph equals N). The incoming edges of a vertex V are represented by a list of integers
;; stored at position V in E. The integers denote vertices whose incoming edges are stored
;; at the corresponding positions in E.


(cl:in-package #:graphs)

(5am:def-suite graphs)
(5am:in-suite graphs)

(5am:test make-graph )

;(defun make-graph (vertices edges-fn &key (test #'eq))
;  (map 'vector
;      (lambda (v)
;	 (mapcar (lambda (v) (position v vertices :test test)) (funcall edges-fn v)))

(5am:test distance
  (5am:is (eq (distance 0 0 (vector nil)) 0))
  (5am:is (eq (distance 0 1 (vector nil '(0))) 1))
  (5am:is (eq (distance 0 2 (vector nil '(0) '(1 3) '(1))) 2))
  (5am:is (eq (distance 0 2 (vector nil '(0) '(3) '(1))) 3)))

;(defun distance (v1 v2 edges)
;  "Given a DAG represented by EDGES, find the minumum path length
;between two vertices, V1 and V2, following the direction of edges. 
;Return NIL if no path is found."
;  (if (eq v1 v2) 0
;      (let ((incoming (svref edges v1)))
;	(unless (null incoming)
;	  (let* ((distances (remove-if
;			     #'null (mapcar (lambda (v)
;					      (distance v v2 edges))
;					    incoming))))
;	    (unless (null distances)
;	      (1+ (apply #'min distances))))))))
;       vertices))

(5am:test (depth :depends-on (and . (roots distance))) )

;(defun depth (vertex edges)
;  (apply #'min
;	 (mapcar (lambda (root) (distance root vertex edges))
					;		 (roots edges))))

(5am:test leafs)
(5am:test roots)

(5am:test inverse-edges)
(5am:test parents)

(5am:test sub
  (let ((g (vector nil '(0) '(0))))
    (5am:is (equalp (sub g '(0 1)) (vector nil '(0))))
    (5am:is (equalp (sub g '(1 0)) (vector '(1) nil)))
    (5am:is (equalp (sub g '(0 2)) (vector nil '(0))))))




