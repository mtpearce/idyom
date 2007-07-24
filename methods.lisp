(cl:in-package #:viewpoints)

;;; viewpoint-sequence

(defmethod viewpoint-sequences ((v viewpoint) sequences)
  (mapcar #'(lambda (s) (viewpoint-sequence v s)) sequences))

(defmethod viewpoint-sequence ((v viewpoint) (m amuse:monody))
  (viewpoint-sequence v (coerce m 'list)))

(defmethod viewpoint-sequence ((v viewpoint) (event-list list))
  (labels ((events->viewpoint (events sequence)
             (if (null events) sequence
                 (let ((element (viewpoint-element v events)))
                   (if (undefined-p element)
                       (events->viewpoint (butlast events) sequence)
                       (events->viewpoint (butlast events)
                                          (cons element sequence)))))))
    (events->viewpoint event-list '())))

;;; viewpoint-element 

(defmethod viewpoint-element ((v viewpoint) (m amuse:monody))
  (viewpoint-element v (coerce m 'list)))

(defmethod viewpoint-element ((v viewpoint) (event-list list))
  (funcall (type-of v) event-list))

(defmethod viewpoint-element ((th threaded) (event-list list))
  (funcall (type-of th) event-list))

(defmethod viewpoint-element ((l linked) (event-list list))
  (let ((viewpoints (viewpoint-links l)))
    (mapcar #'(lambda (a) (funcall (type-of a) event-list))
            viewpoints)))

;;; Others 

(defmethod viewpoint-alphabet ((v viewpoint)) (%viewpoint-alphabet v))

(defmethod viewpoint-typeset ((v viewpoint)) (%viewpoint-typeset v))

(defmethod viewpoint-links ((v viewpoint)) v)
(defmethod viewpoint-links ((l linked)) (%viewpoint-links l))

