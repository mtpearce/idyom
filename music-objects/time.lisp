;;;; ======================================================================
;;;; File:       time.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-04 12:00:19 marcusp>
;;;; Time-stamp: <2014-09-07 12:13:56 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

;; classes

(defclass time-point () 
  ((onset :initarg :onset :accessor onset)))

(defclass time-interval ()
  ((dur :initarg :duration :accessor duration)))

(defclass anchored-time-interval (time-point time-interval) ())


;; (defgeneric time+ (o1 o2)
;;  (:documentation "Addition for time points"))
;; (defgeneric time- (o1 o2)
;;  (:documentation "Subtraction for time points"))
;; (defgeneric duration+ (o1 o2)
;;  (:documentation "Addition for time intervals"))
;; (defgeneric duration- (o1 o2)
;;  (:documentation "Subtraction for time intervals"))
;; 
;; time+:     <point>      <interval> -> <point> 
;;            <interval>   <point>    -> ERROR 
;;            <point>      <point>    -> <point>    ;; how should precedence work for anchored-time-intervals?
;; 
;; time-:     <point>      <interval> -> <point>    ;; are negative time points allowed?
;;            <interval>   <point>    -> ERROR 
;;            <point>      <point>    -> <interval> ;; how should precedence work for anchored-time-intervals?
;; 
;; duration+: <interval>   <interval> -> <interval> 
;; duration-: <interval>   <interval> -> <interval> ;; are negative intervals allowed?
;;  
;; ** not clear these are necessary. **


;; time-point

(defgeneric time> (o1 o2)
  (:documentation "> operator for time-points"))
(defgeneric time< (o1 o2)
  (:documentation "< operator for time-points")
  (:method (o1 o2) (time> o2 o1)))
(defgeneric time= (o1 o2)
  (:documentation "= operator for time-points")) 
(defgeneric time>= (o1 o2)
  (:documentation ">= operator for time-points")
  (:method (o1 o2) (or (time> o1 o2) (time= o1 o2))))
(defgeneric time<= (o1 o2)
  (:documentation "<= operator for time-points")
  (:method (o1 o2) (or (time< o1 o2) (time= o1 o2))))
(defgeneric time/= (o1 o2)
  (:documentation "not = operator for time-points")
  (:method (o1 o2) (not (time= o1 o2))))

(defmethod time> ((o1 time-point) (o2 time-point))
  (> (onset o1) (onset o2)))

(defmethod time= ((o1 time-point) (o2 time-point))
  (= (onset o1) (onset o2)))


;; time-interval

(defgeneric duration> (o1 o2)
  (:documentation "> operator for time-intervals"))
(defgeneric duration< (o1 o2)
  (:documentation "< operator for time-intervals")
  (:method (o1 o2) (duration> o2 o1)))
(defgeneric duration= (o1 o2)
  (:documentation "= operator for time-intervals")) 
(defgeneric duration>= (o1 o2) 
  (:documentation ">= operator for time-intervals")
  (:method (o1 o2) (or (duration> o1 o2) (duration= o1 o2))))
(defgeneric duration<= (o1 o2)
  (:documentation "<= operator for time-intervals")
  (:method (o1 o2) (or (duration< o1 o2) (duration= o1 o2))))
(defgeneric duration/= (o1 o2) 
  (:documentation "not = operator for time-intervals")
  (:method (o1 o2) (not (duration= o1 o2))))

(defmethod duration> ((o1 time-interval) (o2 time-interval))
  (> (duration o1) (duration o2)))

(defmethod duration= ((o1 time-interval) (o2 time-interval))
  (= (duration o1) (duration o2)))


;; anchored-time-interval

(defgeneric end-time (object)
  (:documentation "The end time of a temporal object."))

(defmethod end-time ((o anchored-time-interval))
  (make-instance 'time-point :onset (+ (onset o) (duration o))))

;; Allen's (1984) interval relations 

(defgeneric during (o1 o2) (:documentation "o1 is fully contained within o2."))
(defgeneric starts (o1 o2) (:documentation "o1 shares the same beginning as o2 but ends before o2 ends."))
(defgeneric finishes (o1 o2) (:documentation "o1 shares the same end as o2 but begins after o2 begins."))
(defgeneric before (o1 o2) (:documentation "o1 begins and ends before o2 begins."))
(defgeneric overlap (o1 o2) (:documentation "o1 starts before o2 and they overlap."))
(defgeneric meets (o1 o2) (:documentation "o1 is before o2 but there is no interval between them."))
;; nb we already have EQUALS: duration=

;; extensions

(defgeneric within (o1 o2)
  (:method (o1 o2) (or (starts o1 o2) (during o1 o2) (finishes o1 o2))))

(defgeneric disjoint (o1 o2)
  (:method (o1 o2) 
    (or (before o1 o2) (meets o1 o2) (meets o2 o1) (before o2 o1))))

;; 

(defmethod during ((o1 anchored-time-interval) (o2 anchored-time-interval))
  (and (time> o1 o2) (time< (end-time o1) (end-time o2))))

(defmethod starts ((o1 anchored-time-interval) (o2 anchored-time-interval))
  (and (time= o1 o2) (time< (end-time o1) (end-time o2))))

(defmethod finishes ((o1 anchored-time-interval) (o2 anchored-time-interval))
  (and (time> o1 o2) (time= (end-time o1) (end-time o2))))

(defmethod before ((o1 anchored-time-interval) (o2 anchored-time-interval))
  (time< (end-time o1) o2))

(defmethod overlap ((o1 anchored-time-interval) (o2 anchored-time-interval))
  (and (before o1 o2) (time> (end-time o1) o2) (time< (end-time o1) (end-time o2))))

(defmethod meets ((o1 anchored-time-interval) (o2 anchored-time-interval))
  (and (time< o1 o2) (time= (end-time o1) o2)))





