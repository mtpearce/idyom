;;;; ======================================================================
;;;; File:       generation.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-08-21 18:54:17 marcusp>                           
;;;; Time-stamp: <2020-04-13 15:39:45 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   Supports the generation of new compositions from multiple viewpoint 
;;;;   systems using a variety of methods including random walks and MCMC 
;;;;   methods such as Gibbs and Metropolis sampling. 
;;;;
;;;; ======================================================================

(defpackage #:generation 
  (:use #:cl #:utils #:md #:viewpoints #:ppm #:mvs #:prediction-sets 
        #:resampling)
  (:export #:idyom-generation)
  (:documentation "Generation of melodic compositions."))

(cl:in-package #:generation)

(defgeneric sampling-predict-sequence (mvs sequence cpitch))
(defgeneric complete-prediction (mvs sequence cached-prediction))
(defgeneric metropolis-sampling (mvs sequence iterations events position threshold random-state))
(defgeneric gibbs-sampling (mvs sequence iterations random-state threshold))
(defgeneric gibbs-sequence-distribution (mvs sequences cpitch))
(defgeneric random-walk (mvs sequence context-length random-state threshold))
(defgeneric generate-event (mvs event predictions random-state threshold))

;; ========================================================================
;; Top-level
;; ========================================================================

(defun idyom-generation (dataset-id base-id basic-attributes attributes
                         &key
                           (models :both+)
                           (method :metropolis)
                           (context-length nil)
                           (iterations 100)
                           (events nil)
                           (position :backward)
                           pretraining-ids
                           description
                           (random-state cl:*random-state*)
                           (threshold nil)
                           (use-ltms-cache? t)
                           (output-file-path nil))
  (declare (ignorable description output-file-path))
  (mvs:set-models models)
  (initialise-prediction-cache dataset-id attributes)
  (let* ((dataset (md:get-event-sequences (list dataset-id)))
         (pretraining-set (get-pretraining-set pretraining-ids))
         (viewpoints (get-viewpoints attributes))
         (basic-viewpoints (get-basic-viewpoints basic-attributes 
                                                 (append dataset pretraining-set)))
         (resampling-set (generate-resampling-set dataset base-id))
         (training-set (get-training-set dataset resampling-set))
         (training-set (append pretraining-set training-set))
         (test-set (get-test-set dataset resampling-set))
         (ltms (get-long-term-models viewpoints training-set pretraining-ids
                                     dataset-id (format nil "~Agen" base-id)
                                     nil nil nil use-ltms-cache?))
         (mvs (make-mvs basic-viewpoints viewpoints ltms))
         (context-length (if (and (null context-length) (eql method :random))
                             (get-context-length (car test-set))
                             context-length))
                             
         (sequence
          (case method
            (:metropolis (metropolis-sampling mvs (car test-set) iterations events position threshold random-state))
            (:gibbs (gibbs-sampling mvs (car test-set) iterations random-state threshold))
            (:random (random-walk mvs (car test-set) context-length random-state threshold))
            (otherwise (metropolis-sampling mvs (car test-set) iterations random-state threshold)))))
    ;; (write-prediction-cache-to-file dataset-id attributes)
    ;; (print (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'cpitch) sequence))
    (when (and output-file-path sequence)
      (md:export-data sequence :mid output-file-path))
    sequence))

(defun get-pretraining-set (dataset-ids)
  (md:get-event-sequences dataset-ids))

(defun get-context-length (sequence)
  (1+ (position-if #'(lambda (e) (= (md:get-attribute e 'phrase) -1)) sequence)))
  
(defun generate-resampling-set (dataset base-id)
  (let* ((high-id (1- (length dataset)))
         (low-id 0)
         (dataset-ids (utils:generate-integers low-id high-id))
         (base-id (cond ((< base-id low-id) low-id)
                        ((> base-id high-id) high-id)
                        (t base-id)))
         (training-set (remove base-id dataset-ids)))
    (list (list 'resampling::test (list base-id))
          (list 'resampling::train training-set))))
          
(defun sample (distribution random-state threshold)
  (if (eq threshold :max)
      (sample-max distribution)
      (let* ((d distribution)
             (d (if threshold (remove-if #'(lambda (x) (< (cadr x) threshold)) d) d))
             (d (prediction-sets:normalise-distribution d))
             (d (if (null d) (list (list (sample-max distribution) 1.0)) d))
             (r (random 1.0 random-state))
             (cum-prob 0.0))
        (when mvs::*debug* (format t "~&R: ~A; Sum: ~A~%" r (reduce #'+ d :key #'cadr)))
        (dolist (ep d (values-list (car (last d))))
          (when mvs::*debug* (format t "~&EP: ~A; CP: ~A~%" ep cum-prob))
          (destructuring-bind (attribute-value probability) ep
            (incf cum-prob probability)
            (when (>= cum-prob r)
              (return (values attribute-value probability))))))))

(defun sample-max (distribution) 
  (caar (sort (copy-list distribution) #'> :key #'cadr)))

(defun random-index (sequence random-state &optional (context-length 0))
  (let* ((effective-length (- (length sequence) context-length)))
    (+ (random effective-length random-state) context-length)))


;; ========================================================================
;; SEQUENCE PREDICTION
;; ======================================================================== 

(defmethod sampling-predict-sequence ((m mvs) sequence cpitch)
  (mvs:operate-on-models m #'ppm:reinitialise-ppm :models 'mvs::stm)
  (let* ((cpitch-sequence (viewpoint-sequence cpitch sequence))
         (cached-prediction (cached-prediction cpitch-sequence))
         (prediction (complete-prediction m sequence cached-prediction)))
    (when mvs::*debug*
      (format t "~&Original length: ~A; Cached length: ~A; Final length: ~A~%" 
              (length cpitch-sequence) (length cached-prediction) 
              (length prediction)))
    (cache-predictions prediction)
    prediction))

(defmethod complete-prediction ((m mvs) sequence cached-prediction)
  (let* ((from-index (length cached-prediction))
         (prediction 
          (car 
           (mvs:model-sequence m sequence :construct? nil :predict? t 
                               :predict-from from-index
                               :construct-from from-index))))
    ;;(format t "~&Cache Length: ~A" from)
    (append cached-prediction (get-sequence prediction))))
        
(defun get-sequence (sequence-prediction)
  (mapcar #'(lambda (x) (list (prediction-element x) (prediction-set x)))
          (prediction-set sequence-prediction)))


;; ========================================================================
;; CACHING PREDICTIONS 
;; ======================================================================== 

(defvar *prediction-cache* '())

(defvar *prediction-cache-directory* mvs:*ep-cache-dir*)

(defun get-prediction-cache-filename (dataset-id viewpoints)
  (string-append (namestring *prediction-cache-directory*)
                 (reduce #'(lambda (&optional (x "") (y ""))
                             (string-append x y))
                         (viewpoints:get-viewpoints viewpoints)
                         :key #'(lambda (x) (string-append (viewpoint-name x)
                                                           "_")))
                 (format nil "~S" dataset-id)
                 ".cache"))

(defun write-prediction-cache-to-file (dataset-id viewpoints)
  (let ((filename (get-prediction-cache-filename dataset-id viewpoints)))
    (write-object-to-file *prediction-cache* filename :generation)
    (format t "~%Written prediction-cache to ~A." filename)))

(defun initialise-prediction-cache (dataset-id viewpoints)
  (let ((filename (get-prediction-cache-filename dataset-id viewpoints)))
    (if (file-exists filename)
        (setf *prediction-cache* (read-object-from-file filename :generation))
        (setf *prediction-cache* (make-node)))))
    
(defstruct node
  (symbol nil)
  (probability nil)
  (children nil))

(defun cached-prediction (sequence)
  (labels ((cached (node sequence result)
             (if (null sequence) (reverse result)
                 (let* ((symbol (car sequence))
                        (child (find-child node symbol)))
                   (if (null child) (reverse result)
                       (cached child (cdr sequence)
                               (cons (node->list node child) result)))))))
    (cached *prediction-cache* sequence '())))

(defun node->list (node child)
  (list (node-symbol child)
        (mapcar #'(lambda (x) (list (node-symbol x) (node-probability x)))
                (node-children node))))

(defun find-child (node symbol)
  (find symbol (node-children node) :test #'eql :key #'node-symbol))
 
(defun cache-predictions (predictions)
  (labels ((cache (node predictions)
             (unless (null predictions)
               (let* ((head (car predictions))
                      (symbol (car head))
                      (distribution (cadr head))) 
                 (add-children node distribution)
                 (cache (find-child node symbol) (cdr predictions))))))
    (cache *prediction-cache* predictions)))

(defun add-children (node list)
  (dolist (i list)
    (let ((symbol (car i))
          (probability (cadr i)))
      (unless (find-child node symbol)
        (push (make-node :symbol symbol :probability probability)
              (node-children node))))))

(defun cache->ps (&optional (filename "cache.ps"))
  (let ((psgraph:*fontsize* 14)
        (psgraph:*second-fontsize* 12)
        (psgraph:*boxradius* 10) 
        (psgraph:*boxedge* 10)
        (psgraph:*boxgray* "0")
        (psgraph:*edgegray* "0")
        (psgraph:*extra-x-spacing* 90)
        (psgraph:*extra-y-spacing* 20))
    (with-open-file (s filename :direction :output
                     :if-exists :supersede)
      (psgraph:psgraph s *prediction-cache*
                       #'node-children
                       #'(lambda (x) (list (node-symbol x) (node-probability x)))
                       t nil #'eq nil))))


;; ========================================================================
;; METROPOLIS SAMPLING
;; ======================================================================== 

(defmethod metropolis-sampling ((m mvs) sequence iterations events position threshold random-state)
  "Using Metropolis-Hastings sampling, sample new pitches for the
melody <sequence>. The number of iterations can be controlled by
<iterations> which specifies a number of iterations of sampling (one
note considered per iteration) or <events> which continues until the
specified proportion of events has been changed. If both <iterations>
and <events> are specified, the specified number of iterations are run
but may be cut short if the specified proportion of events changed is
reached. The <position> parameter controls how notes are selected for
sampling: :forward proceeds forwards through the melody from the first
note to the last; :backward proceeds from the last note to the
first; :random selects note position randomly on each iteration. The
diversity of the sampling can be controlled with the <threshold>
parameter which limits sampling to events with probability above the
specified threshold (if specified) or only the highest probability
event if :max is specified. <random-state> allows the user to supply a
random state, allowing exact replication."
  ;; 
  ;; - threshold: 0 - 1, only consider events with probability above the threshold
  ;; - percentage: 0 - 1, only consider the top X% of events of the distribution
  ;; - cumulative: 0 - 1, only consider events within the specified cumulative probability
  ;; - temperature: 0 - ?, 
  ;; 
  (let* ((sequence (coerce sequence 'list))
         (pitch-sequence (mapcar #'(lambda (x) (get-attribute x 'cpitch)) sequence))
         (l (length sequence))
         (cpitch (viewpoints:get-viewpoint 'cpitch))
         (predictions (sampling-predict-sequence m sequence cpitch))
         (original-p (seq-probability predictions))
         (changed-events nil))
    (do ((iteration 0 (1+ iteration)))
        ((or (and events (>= (/ (length changed-events) l) events))
             (and iterations (>= iteration iterations)))
         (progn
           (format t "~&~A events out of ~A changed (~A)."
                   (length changed-events) l
                   (utils:round-to-nearest-decimal-place (* 100 (/ (length changed-events) l)) 0))
           sequence))
      (let* ((index (case position (:forward (mod iteration l)) (:backward (- l (mod iteration l) 1)) (t (random-index sequence random-state))))
             (distribution (metropolis-event-distribution predictions index))
             (new-sequence
              (metropolis-new-sequence sequence distribution index random-state threshold))
             (old-cpitch (get-attribute (nth index sequence) 'cpitch))
             (new-cpitch (get-attribute (nth index new-sequence) 'cpitch)))
        (unless (eql old-cpitch new-cpitch)
          (when mvs::*debug*
            (format t "~&Old cpitch: ~A; New cpitch: ~A" 
                    old-cpitch new-cpitch)
            (format t "~&new-sequence: ~A; old-sequence: ~A~%" 
                    (length new-sequence) (length sequence)))
          (let* ((new-predictions
                  (sampling-predict-sequence m new-sequence cpitch))
                 (old-ep (nth 1 (assoc old-cpitch distribution :test #'=)))
                 (new-ep (nth 1 (assoc new-cpitch distribution :test #'=)))
                 (old-p (seq-probability predictions))
                 (new-p (seq-probability new-predictions))
                 (select?
                  (metropolis-select-new-sequence? old-p new-p old-ep new-ep))
                 (next-sequence (if select? new-sequence sequence))
                 (next-predictions (if select? new-predictions predictions))
                 (next-cpitch (if select? new-cpitch old-cpitch)))
            (when select?
              (pushnew index changed-events :test #'=)
              (format t 
               "~&Iteration ~A; Index ~A; Orig ~A; Old ~A; New ~A; Select ~A; EP ~A; P ~A; OP ~A."
               iteration index (nth index pitch-sequence) old-cpitch new-cpitch 
               next-cpitch 
               (if (> new-ep old-ep) "+" "-")
               (if (> new-p old-p) "+" "-") 
               (if (> new-p original-p) "+" "-")))
            (setf sequence next-sequence predictions next-predictions)))))))

(defun metropolis-event-distribution (predictions index)
  (nth 1 (nth index predictions)))

(defun metropolis-new-sequence (sequence distribution index random-state threshold)
  (let* ((sequence-1 (subseq sequence 0 index))
         (event (nth index sequence))
         (sequence-2 (subseq sequence (1+ index)))
         (new-event (copy-event event))
         (new-cpitch (sample distribution random-state threshold)))
    ;; (format t "~&New cpitch is ~A~%" new-cpitch)
    (set-attribute new-event 'cpitch new-cpitch)
    (append sequence-1 (list new-event) sequence-2)))

(defun metropolis-select-new-sequence? (old-p new-p old-ep new-ep)
  (let* ((n (* new-p old-ep))
         (d (* old-p new-ep))
         (p (if (or (zerop n) (zerop d)) 0.0 (min 1 (/ n d))))
         (r (random 1.0)))
;;     (when (<= r p)
;;       (format t "~&  old-p: ~A; old-ep: ~A;~&  new-p: ~A; new-ep: ~A~%"
;;               old-p old-ep new-p new-ep)
;;       (format t "~&  P: ~A; R: ~A~%" p r))
    (< r p)))

(defun seq-probability (sequence-prediction)
  (reduce #'* sequence-prediction
          :key #'(lambda (x) (nth 1 (assoc (nth 0 x) (nth 1 x))))))
  

;; ========================================================================
;; GIBBS SAMPLING 
;; ======================================================================== 

(defmethod gibbs-sampling ((m mvs) sequence iterations random-state threshold)
  (let* ((sequence (coerce sequence 'list))
         (pitch-sequence (mapcar #'(lambda (x) (get-attribute x 'cpitch))
                                 sequence))
         (l (length sequence))
         (cpitch (viewpoints:get-viewpoint 'cpitch))
         (predictions (sampling-predict-sequence m sequence cpitch))
         (original-p (seq-probability predictions)))
    (dotimes (i iterations sequence)
      (let* (;(index (random-index sequence))
             (index (- l (mod i l) 1))
             (new-sequences (gibbs-new-sequences sequence index))
             (distribution (gibbs-sequence-distribution m new-sequences cpitch))
             (new-sequence (sample distribution random-state threshold))
             (old-cpitch (get-attribute (nth index sequence) 'cpitch))
             (new-cpitch (get-attribute (nth index new-sequence) 'cpitch))
             (old-p (seq-probability (sampling-predict-sequence m sequence cpitch)))
             (new-p 
              (seq-probability (sampling-predict-sequence m new-sequence cpitch))))
        (format t 
                "~&Iteration ~A; Index ~A; Orig ~A; Old ~A; New ~A; P ~A; OP ~A.~%"
                i index (nth index pitch-sequence) old-cpitch new-cpitch 
                (if (> new-p old-p) "+" "-") 
                (if (> new-p original-p) "+" "-"))
        (setf sequence new-sequence)))))

(defmethod gibbs-sequence-distribution ((m mvs) sequences cpitch)
  (let ((sequence-predictions
         (mapcar #'(lambda (s) 
                     ;; (mvs:model-sequence m s :construct? nil :predict? t))
                     (sampling-predict-sequence m s cpitch))
                 sequences)))
    (normalise-distribution
     (mapcar #'(lambda (s p) (list s (seq-probability (car p))))
             sequences sequence-predictions))))

(defun gibbs-new-sequences (sequence index)
  (let* ((sequence-1 (subseq sequence 0 index))
         (event (nth index sequence))
         (sequence-2 (subseq sequence (1+ index)))
         (new-events
          (alphabet->events (viewpoints:get-viewpoint 'cpitch)
                            (append sequence-1 (list event)))))
    (mapcar #'(lambda (e) (append sequence-1 (list e) sequence-2))
            new-events)))


;; ========================================================================
;; RANDOM WALK 
;; ======================================================================== 

(defmethod random-walk ((m mvs) sequence context-length random-state threshold)
  (let* ((sequence (coerce sequence 'list))
         (event-count (length sequence))
         (viewpoint-count (mvs:count-viewpoints m))
         (new-sequence '())
         (ltm-locations
          (make-sequence 'vector viewpoint-count :initial-element (ppm:get-root)))
         (stm-locations
          (make-sequence 'vector viewpoint-count :initial-element (ppm:get-root)))
         (last-attribute-predictions nil))
    (dotimes (event-index event-count)
      (let* ((subsequence (subseq sequence 0 (1+ event-index)))
             (current-event (nth event-index sequence)) 
             ;(previous-events (subseq sequence 0 event-index))
             (event-array (mvs:get-event-array m subsequence)))
        ;; 1. predict event without adding it
        ;;(mvs::old-set-model-alphabets m event-array subsequence (mvs:mvs-basic m))
        (multiple-value-bind (d1 d2 ltm-prediction-sets stm-prediction-sets)
            (mvs:model-event m event-array subsequence 
                             :ltm-locations ltm-locations 
                             :stm-locations stm-locations 
                             :construct? nil :predict? t)
          (declare (ignore d1 d2))
          ;; (when mvs::*debug*
          ;;   (dolist (ps (list stm-prediction-sets ltm-prediction-sets))
          ;;     (dolist (set (reverse ps))
          ;;       (format t "~a ~a ~a~%" event-index 
          ;;               (viewpoint-name (prediction-sets:prediction-viewpoint set))
          ;;               (prediction-sets:prediction-set set)))))
          (if (< event-index context-length)
              (push current-event new-sequence)
              (unless (and (null ltm-prediction-sets)
                           (null stm-prediction-sets))
              ;; 2. choose symbol from distribution (for each basic viewpoint)
              ;; 3. generate event and append to sequence
                (when mvs::*debug*
                  (format t "~2&~2,0@TActual Event ~A: ~A~%" event-index event-array))
                (let* ((predictions (mvs:combine-predictions m
                                                             ltm-prediction-sets
                                                             stm-prediction-sets
                                                             subsequence)))
                  (multiple-value-bind (new-event attribute-predictions)
                      (generate-event m current-event predictions random-state threshold)
                    (setf last-attribute-predictions attribute-predictions)
                    (push new-event new-sequence)))))))
      ;; 4. add new event to models without predicting 
      (let* ((subsequence (reverse new-sequence))
             ;(current-event (nth event-index subsequence)) 
             ;(previous-events (subseq subsequence 0 event-index))
             (event-array (mvs:get-event-array m subsequence)))
        ;;(mvs::old-set-model-alphabets m event-array subsequence (mvs:mvs-basic m))
        (print (list event-index (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'cpitch) (subseq sequence 0 (1+ event-index)))
                     (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'cpitch) subsequence)))
        (when mvs::*debug* 
          (when (>= event-index context-length)
            (format t "~&~2,0@TGenerated Event ~A: ~A~%" event-index event-array)))
        (multiple-value-bind (ltm-next-locations stm-next-locations d3 d4)
            (mvs:model-event m event-array subsequence
                             :construct? t :predict? nil
                             :ltm-locations ltm-locations 
                             :stm-locations stm-locations)
          (declare (ignore d3 d4))
          (setf ltm-locations ltm-next-locations
                stm-locations stm-next-locations)
          (mvs:operate-on-models m #'increment-event-front))))
    ;(format t "~&~2,0@TSentinel Event")
    ;(mvs:operate-on-models m #'model-sentinel-event :models 'ltm
    ;                   :ltm-args (list (coerce ltm-locations 'list)))
    ;(format t "~&~2,0@Tinitialise-vrtual-nodes")
    ;(mvs:operate-on-models m #'initialise-virtual-nodes)
    (values (reverse new-sequence) last-attribute-predictions)))

(defmethod generate-event ((m mvs) event predictions random-state threshold)
  (let ((predictions (select-from-distributions predictions random-state threshold)))
    (dolist (p predictions (values event predictions))
      (setf
       (slot-value event (find-symbol (symbol-name (car p)) (find-package :music-data)))
       (nth 2 p)))))

(defun select-from-distributions (predictions random-state threshold)
  (let ((selected-attributes
         (mapcar #'(lambda (x)
                     (multiple-value-bind (attribute-value probability)
                         (sample
                          (prediction-sets:prediction-set x)
                          random-state threshold)
                       (list (viewpoint-type (prediction-sets:prediction-viewpoint x))
                             (prediction-sets:prediction-element x)
                             attribute-value
                             probability
                             (- (log probability 2))
                             (prediction-sets:shannon-entropy
                              (prediction-sets:prediction-set x))
                             (prediction-sets:prediction-set x))))
                 predictions)))
    (when mvs::*debug*
      (format t "~{~{viewpoint: ~A, original: ~A, generated: ~A, p: ~A, h: ~A, H: ~A~%~2Tdistribution: ~A~&~}~}"
              selected-attributes))
    selected-attributes))
