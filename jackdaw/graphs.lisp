;; Collection of functions that operate on directed graphs represented by vectors.
;; A vector E of length N describes a graph G of order N (the number of vertices in the
;; graph equals N). The incoming edges of a vertex V are represented by a list of integers
;; stored at position V in E. The integers denote vertices whose incoming edges are stored
;; at the corresponding positions in E.

(cl:in-package #:graphs)

(define-condition dag-contains-cycle (error) ())

(defclass dag ()
  ((vertices :initarg :vertices :accessor vertices)))

(defclass edge-vector-dag (dag)
  ((edges :initarg :edges :reader %edges :type vector)))

(defclass edge-function-dag (dag)
  ((edge-function :initarg :edge-function :accessor edge-function)))

(defgeneric roots (dag))
(defgeneric leafs (dag))
(defgeneric inversed (dag))
(defgeneric edges (dag vertex))
(defgeneric depth (dag vertex))
(defgeneric depth (dag vertex))
(defgeneric distance (dag from to))
(defgeneric edge-set (dag &rest vertices))
(defgeneric ancestor-set (dag &rest vertices))
(defgeneric topological-sort (dag &optional vertices visited result))
(defgeneric visit (dag vertex visited result))
(defgeneric to-edge-vector-dag (dag &key test key))

(defmethod order ((graph dag))
  (length (vertices graph)))

(defmethod edges ((graph edge-vector-dag) vertex)
  (svref (%edges graph) vertex))

(defmethod edges ((graph edge-function-dag) vertex)
  (funcall (edge-function graph) vertex))

(defmethod to-edge-vector-dag ((graph dag) &key (test #'eq) (key #'identity))
  (let ((vertices (vertices graph)))
    (make-edge-vector-dag
     (map 'vector
	  (lambda (v)
	    (mapcar (lambda (v) (position v vertices :test test :key key))
		    (edges graph v)))
	  (vertices graph)))))

(defun make-edge-vector-dag (edges)
  (make-instance 'edge-vector-dag
		 :edges edges
		 :vertices (loop for v below (length edges) collect v)))

(defmethod depth ((graph dag) vertex)
  (apply #'min
	 (remove-if
	  #'null
	  (mapcar (lambda (root) (distance graph root vertex)) (roots graph)))))

(defmethod distance ((graph dag) v1 v2)
  "Given a DAG represented by EDGES, find the minimal path length
from V1 to V2. Return NIL if no path is found."
  (if (eq v1 v2) 0
      (let ((incoming (edges graph v2)))
	;; Return NIL if V2 has no incoming edges
	(unless (null incoming)
	  (let* ((distances
		  (remove-if #'null
			     (mapcar (lambda (v) (distance graph v1 v)) incoming))))
	    (unless (null distances)
	      (1+ (apply #'min distances))))))))

(defmethod leafs ((graph dag))
  "EDGES is a vector that stores the incoming edges per vertex.
Convert this table to a vector of outgoing edges per vertex and
return the indices of vertex with no outgoing edges."
  (let* ((inversed (inversed graph)))
    (roots inversed)))

(defmethod roots ((graph dag))
  (loop for v in (vertices graph) if
       (null (edges graph v))
     collect v))

(defmethod ancestor-set ((graph dag) &rest vertices)
  (let ((parents (apply #'edge-set (cons graph vertices))))
    (unless (null parents)
      (union parents (apply #'ancestor-set (cons graph parents))))))

(defmethod edge-set ((graph dag) &rest vertices)
  (remove-duplicates
   (apply #'append (mapcar (lambda (v)
			     (if (listp v) v (edges graph v)))
			   vertices))))

(defmethod inversed ((graph dag))
  (make-instance 'edge-function-dag
		 :vertices (vertices graph)
		 :edge-function
		 (lambda (v) (loop for vertex in (vertices graph)
				if (member v (edges graph vertex))
				collect v))))

(defmethod topological-sort ((graph dag)
			     &optional
			       (vertices (loop for v below (order graph) collect v))
			       (visited (make-array (order graph) :initial-element :no))
			       result)
  (let* ((org-vertices (vertices graph))
	 (edge-vector-dag (to-edge-vector-dag graph))
	 (sorted-vertices (topological-sort edge-vector-dag vertices visited result)))
    (mapcar (lambda (v) (elt org-vertices v)) sorted-vertices)))

(defmethod topological-sort ((graph edge-vector-dag)
			     &optional
			       (vertices (loop for v below (order graph) collect v))
			       (visited (make-array (order graph) :initial-element :no))
			       result)
  "Sort the vertices of the graph represented by EDGES topologically. Return a list
of vertex indices."
  (if (null vertices) result
      (let ((vertex (car vertices))
	    (remaining (cdr vertices)))
	(topological-sort graph remaining visited (visit graph vertex visited result)))))

(defmethod visit ((graph edge-vector-dag) vertex visited result)
  "A node is a CONS whose CDR is its children and whose CAR is another
CONS the CAR of which is its mark and whose CDR is the a feature index."
  (let ((children (edges graph vertex)))
    (case (svref visited vertex)
      (:yes result)
      (:in-progress
       (error 'dag-contains-cycle))
      (:no
       (progn
	 (setf (svref visited vertex) :in-progress)
	 (let ((result (topological-sort graph children visited result)))
	   (setf (svref visited vertex) :yes)
	   (cons vertex result)))))))



