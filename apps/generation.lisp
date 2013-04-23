;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       generation.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-08-21 18:54:17 marcusp>                           
;;;; Time-stamp: <2013-04-22 23:46:52 jeremy>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   Supports the generation of new compositions from multiple viewpoint 
;;;;   systems using a variety of methods including random walks and MCMC 
;;;;   methods such as Gibbs and Metropolis sampling. 
;;;;
;;;; TODO 
;;;;
;;;;   o generate by selecting event with highest probablity at each stage
;;;;   o try without STM 
;;;; 
;;;; ======================================================================

(cl:in-package #:generation)

(defgeneric sampling-predict-sequence (mvs sequence cpitch))
(defgeneric complete-prediction (mvs sequence cached-prediction))
(defgeneric metropolis-sampling (mvs sequence iterations))
(defgeneric gibbs-sampling (mvs sequence iterations))
(defgeneric gibbs-sequence-distribution (mvs sequences cpitch))
(defgeneric random-walk (mvs sequence context-length))
(defgeneric generate-event (mvs event predictions))


;; ========================================================================
;; CHORALE GENERATION 
;; ========================================================================

(defvar *compositional* '(thrfiph cpintfip (cpint dur-ratio) (cpintfref dur) 
                          thrtactus (cpintfref fib) (cpitch dur) 
                          (cpintfref cpintfip) (cpint dur)))
(defvar *compositional+* 
  '(CPINTFIPH (CPINTFREF DUR) (CPINT INSCALE) (CPINT DUR-RATIO) 
    (CPINTFREF LIPH) THRFIPH (CPITCH DUR) (CPINTFREF CPINTFIP) 
    (CPINTFREF MODE) (CPINT DUR)))
(defvar *perceptual* '(cpintfip (cpintfref dur-ratio) thrfiph))
(defvar *cpitch* '(cpitch))
(defvar *chorales* '(7 56 105 107 123 155 173))

(defun generate-chorales (method model &key (models :both+) 
                          (chorales *chorales*))
  (cond ((eql model :compositional+)
         (if (eql method :random)
             (mapcar #'(lambda (x)
                         (compositional+-random-generation x :models models))
                     chorales)
             (mapcar #'(lambda (x)
                         (compositional+-chorale-generation x :models models))
                     chorales)))
        ((eql model :compositional)
         (if (eql method :random)
             (mapcar #'(lambda (x)
                         (compositional-random-generation x :models models))
                     chorales)
             (mapcar #'(lambda (x)
                         (compositional-chorale-generation x :models models))
                     chorales)))
        ((eql model :perceptual)
         (if (eql method :random)
             (mapcar #'(lambda (x)
                         (perceptual-random-generation x :models models))
                     chorales)
             (mapcar #'(lambda (x)
                         (perceptual-chorale-generation x :models models))
                     chorales)))
        ((eql model :cpitch)
         (if (eql method :random)
             (mapcar #'(lambda (x)
                         (cpitch-random-generation x :models models))
                     chorales)
             (mapcar #'(lambda (x)
                         (cpitch-chorale-generation x :models models))
                     chorales)))))

(defun compositional+-chorale-generation (base-id &key (iterations 5000)
                                              (models :both+))
  (mvs:set-models models)
  (chorale-generation *compositional+* base-id :metropolis 
                      :iterations iterations
                      :description (get-file-description base-id 
                                                         "compositional+"
                                                         iterations models
                                                         :metropolis)))

(defun compositional+-random-generation (base-id &key (context-length nil)
                                             (models :both+))
  (mvs:set-models models)
  (let ((context-length
         (if (null context-length)
             (get-context-length (md:get-event-sequence 1 base-id))
             context-length)))
    (chorale-generation *compositional+* base-id :random
                        :context-length context-length
                        :description (get-file-description base-id
                                                           "compositional+"
                                                           context-length
                                                           models :random))))
        
(defun compositional-chorale-generation (base-id &key (iterations 5000)
                                                 (models :both+))
  (mvs:set-models models)
  (chorale-generation *compositional* base-id :metropolis :iterations iterations
                      :description (get-file-description base-id "compositional"
                                                         iterations models
                                                         :metropolis)))

(defun compositional-random-generation (base-id &key (context-length nil)
                                                (models :both+))
  (mvs:set-models models)
  (let ((context-length
         (if (null context-length)
             (get-context-length (md:get-event-sequence 1 base-id))
             context-length)))
    (chorale-generation *compositional* base-id :random
                        :context-length context-length
                        :description (get-file-description base-id
                                                           "compositional"
                                                           context-length
                                                           models :random))))

(defun perceptual-chorale-generation (base-id &key (iterations 5000)
                                              (models :both+))
  (mvs:set-models models)
  (chorale-generation *perceptual* base-id :metropolis :iterations iterations
                      :description (get-file-description base-id "perceptual"
                                                         iterations models
                                                         :metropolis )))

(defun perceptual-random-generation (base-id &key (context-length nil)
                                         (models :both+))
  (mvs:set-models models)
  (let ((context-length
         (if (null context-length)
             (get-context-length (md:get-event-sequence 1 base-id))
             context-length)))
  (chorale-generation *perceptual* base-id :random
                      :context-length context-length
                      :description (get-file-description base-id "perceptual"
                                                         context-length
                                                         models :random))))

(defun cpitch-chorale-generation (base-id &key (iterations 5000)
                                          (models :both+))
  (mvs:set-models models)
  (chorale-generation *cpitch* base-id :metropolis :iterations iterations
                      :description (get-file-description base-id "cpitch"
                                                         iterations models
                                                         :metropolis)))

(defun cpitch-random-generation (base-id &key (context-length nil)
                                         (models :both+))
  (mvs:set-models models)
  (let ((context-length
         (if (null context-length)
             (get-context-length (md:get-event-sequence 1 base-id))
             context-length)))
    (chorale-generation *cpitch* base-id :random :context-length context-length
                        :description (get-file-description base-id "cpitch"
                                                           context-length
                                                           models :random))))

(defun chorale-generation (attributes base-id method &key (iterations 5000)
                                      description
                                      (context-length nil))
  (dataset-generation 1 '(cpitch) attributes base-id :method method
                      :iterations iterations :pretraining-ids '(2)
                      :description description :context-length context-length))

(defun get-file-description (base-id type iterations models method)
  (let* ((file-id (1+ base-id))
         (method (case method (:metropolis "metro") (:random "random")))
         (zeros (cond ((< file-id 10) "00")
                      ((< file-id 100) "0")
                      (t "")))
         (models (case models (:ltm "-ltm") (:stm "-stm")
                       (otherwise ""))))
    (format nil "chor~A~A-~A-~A~A~A"
            zeros file-id type method iterations models)))


;; ========================================================================
;; DATASET GENERATION 
;; ========================================================================

(defun dataset-generation (dataset-id basic-attributes attributes base-id
                           &key
                           (method :metropolis)
                           (context-length nil)
                           (iterations 100) 
                           pretraining-ids
                           description 
                           (output-file-path "/tmp"))
  (declare (ignorable description output-file-path))
  (initialise-prediction-cache dataset-id attributes)
  (let* ((dataset (md:get-event-sequences dataset-id))
         (pretraining-set (get-pretraining-set pretraining-ids))
         (viewpoints (get-viewpoints attributes))
         (basic-viewpoints (get-basic-viewpoints basic-attributes dataset))
         (resampling-set (generate-resampling-set dataset base-id))
         (training-set (get-training-set dataset resampling-set))
         (training-set (append pretraining-set training-set))
         (test-set (get-test-set dataset resampling-set))
         (ltms (get-long-term-models viewpoints training-set pretraining-ids
                                     dataset-id (format nil "~Agen" base-id)
                                     nil))
         (mvs (make-mvs basic-viewpoints viewpoints ltms))
         (context-length (if (and (null context-length) (eql method :random))
                             (get-context-length (car test-set))
                             context-length))
                             
         (sequence
          (case method
            (:metropolis (metropolis-sampling mvs (car test-set) iterations))
            (:gibbs (gibbs-sampling mvs (car test-set) iterations))
            (:random (random-walk mvs (car test-set) context-length))
            (otherwise (metropolis-sampling mvs (car test-set) iterations)))))
    ;(write-prediction-cache-to-file dataset-id attributes)
    ;(print (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'cpitch) sequence))
    ;(mtp-admin:export-data sequence :ps "/tmp")
    (mtp-admin:export-data sequence :mid output-file-path)
    sequence))

(defun get-pretraining-set (dataset-ids)
  (let* ((d1 (when (member 2 dataset-ids :test #'=)
               (remove-if-not #'(lambda (x)
                                  (member (cadr (mtp-admin:get-id (car x))) (unique-ids-2)
                                          :test #'=))
                              (md:get-event-sequences 2))))
         (drest (apply #'md:get-event-sequences
                       (remove-if #'(lambda (x) (= x 2)) dataset-ids))))
    (append d1 drest)))

(defun unique-ids-2 ()
  '(2 3 6 7 8 9 10 11 14 15 17 18 20 21 26 28 31 32 34 36 38 39 40 42 43 44 45
    46 47 48 50 52 54 56 57 58 60 65 69 72 81 93 97))

(defun get-context-length (sequence)
  (1+ (position-if #'(lambda (e) (= (mtp-admin::get-attribute e 'phrase) -1)) sequence)))
  
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
          
(defun select-from-distribution (distribution)
  (let ((r (random 1.0))
        (cum-prob 0.0))
    ;(format t "~&R: ~A; Sum: ~A~%"
    ;        r (reduce #'+ distribution :key #'cadr))
    (dolist (ep distribution)
      ;(format t "~&EP: ~A; CP: ~A~%" ep cum-prob)
      (incf cum-prob (cadr ep))
      (when (>= cum-prob r)
        (return (car ep))))))
;;         (ranges (distribution->ranges distribution)))
;;     (dotimes (i (length distribution))
;;       (format t "~&~A ~A ~A ~A ~A~%" 
;;               (car (nth i distribution))
;;               (nth 1 (nth i distribution))
;;               (nth 1 (nth i ranges))
;;               (nth 2 (nth i ranges))
;;               (- (nth 2 (nth i ranges)) (nth 1 (nth i ranges)))))
;;     (print r) 
;;     (car (find-if #'(lambda (x)
;;                       (let ((low (nth 1 x)) (high (nth 2 x)))
;;                         (and (> r low) (<= r high))))
;;                   ranges))))

(defun select-max-from-distribution (distribution) 
  (caar (sort (copy-list distribution) #'> :key #'cadr)))

(defun distribution->ranges (distribution)
  (let ((new-distribution '())
        (sum 0))
    (dolist (s distribution (reverse new-distribution))
      (let* ((symbol (car s))
             (probability (cadr s))
             (new-sum (+ sum probability)))
        (push (list symbol sum new-sum) new-distribution)
        (setf sum new-sum)))))

(defun random-index (sequence)
  (let* ((context-length 0) ;(get-context-length sequence)) 
         (effective-length (- (length sequence) context-length)))
    (+ (random effective-length) context-length)))


;; ========================================================================
;; CACHING PREDICTIONS 
;; ======================================================================== 

(defvar *prediction-cache* '())

(defvar *prediction-cache-directory* mvs:*ep-cache-dir*)

(defun get-prediction-cache-filename (dataset-id viewpoints)
  (string-append *prediction-cache-directory*
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

(defmethod sampling-predict-sequence ((m mvs) sequence cpitch)
  (mvs:operate-on-models m #'ppm:reinitialise-ppm :models 'mvs::stm)
  (let* ((cpitch-sequence (viewpoint-sequence cpitch sequence))
         (cached-prediction (cached-prediction cpitch-sequence))
         (prediction (complete-prediction m sequence cached-prediction)))
    ;(format t "~&Original length: ~A; Cached length: ~A; Final length: ~A~%" 
    ;        (length cpitch-sequence) (length cached-prediction) 
    ;        (length prediction))
    (cache-predictions prediction)
    prediction))

(defmethod complete-prediction ((m mvs) sequence cached-prediction)
  (let* ((from-index (length cached-prediction))
         (prediction 
          (car 
           (mvs:model-sequence m sequence :construct? nil :predict? t 
                               :predict-from from-index
                               :construct-from from-index))))
    ;(format t "~&Cache Length: ~A" from)
    (append cached-prediction (get-sequence prediction))))
        
(defun get-sequence (sequence-prediction)
  (mapcar #'(lambda (x) (list (prediction-element x) (prediction-set x)))
          (prediction-set sequence-prediction)))

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

(defmethod metropolis-sampling ((m mvs) sequence iterations)
  ;(initialise-prediction-cache) 
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
             (distribution (metropolis-event-distribution predictions index))
             (new-sequence
              (metropolis-new-sequence sequence distribution index))
             (old-cpitch (get-attribute (nth index sequence) 'cpitch))
             (new-cpitch (get-attribute (nth index new-sequence) 'cpitch)))
        (unless (eql old-cpitch new-cpitch)
          ;(format t "~&Old cpitch: ~A; New cpitch: ~A" 
          ;        old-cpitch new-cpitch)
          ;(format t "~&new-sequence: ~A; old-sequence: ~A~%" 
          ;        (length new-sequence) (length sequence))
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
              (format t 
               "~&Iteration ~A; Index ~A; Orig ~A; Old ~A; New ~A; Select ~A; EP ~A; P ~A; OP ~A."
               i index (nth index pitch-sequence) old-cpitch new-cpitch 
               next-cpitch 
               (if (> new-ep old-ep) "+" "-")
               (if (> new-p old-p) "+" "-") 
               (if (> new-p original-p) "+" "-")))
            (setf sequence next-sequence predictions next-predictions)))))))

(defun metropolis-event-distribution (predictions index)
  (nth 1 (nth index predictions)))

(defun metropolis-new-sequence (sequence distribution index)
  (let* ((sequence-1 (subseq sequence 0 index))
         (event (nth index sequence))
         (sequence-2 (subseq sequence (1+ index)))
         (new-event (copy-event event))
         (new-cpitch (select-from-distribution distribution)))
         ;(new-cpitch (select-max-from-distribution distribution)))
    ;(format t "~&New cpitch is ~A~%" new-cpitch)
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

(defmethod gibbs-sampling ((m mvs) sequence iterations)
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
             (new-sequence (select-from-distribution distribution))
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

(defmethod random-walk ((m mvs) sequence context-length)
  (let* ((sequence (coerce sequence 'list))
         (event-count (length sequence))
         (viewpoint-count (mvs:count-viewpoints m))
         (new-sequence '())
         (ltm-locations
          (make-sequence 'vector viewpoint-count :initial-element *root*))
         (stm-locations
          (make-sequence 'vector viewpoint-count :initial-element *root*)))
    (dotimes (event-index event-count)
      (let* ((subsequence (subseq sequence 0 (1+ event-index)))
             (current-event (nth event-index sequence)) 
             ;(previous-events (subseq sequence 0 event-index))
             (event-array (mvs:get-event-array m subsequence)))
        ;; 1. predict event without adding it
        (mvs::old-set-model-alphabets m event-array subsequence  
                                      (mvs:mvs-basic m))
        (multiple-value-bind (d1 d2 ltm-prediction-sets stm-prediction-sets)
            (mvs:model-event m event-array subsequence 
                             :ltm-locations ltm-locations 
                             :stm-locations stm-locations 
                             :construct? nil :predict? t)
          (declare (ignore d1 d2))
          (if (< event-index context-length)
              (push current-event new-sequence)
              (unless (and (null ltm-prediction-sets)
                           (null stm-prediction-sets))
              ;; 2. choose symbol from distribution (for each basic viewpoint)
              ;; 3. generate event and append to sequence
                (format t "~2&~2,0@TActual Event ~A: ~A~%"
                        event-index event-array)
                (let* ((basic-viewpoints (mvs:mvs-basic m))
                       (predictions (mvs:combine-predictions basic-viewpoints 
                                                             ltm-prediction-sets
                                                             stm-prediction-sets
                                                             subsequence))
                       (new-event (generate-event m current-event predictions)))
                  (push new-event new-sequence))))))
      ;; 4. add new event to models without predicting 
      (let* ((subsequence (reverse new-sequence))
             ;(current-event (nth event-index subsequence)) 
             ;(previous-events (subseq subsequence 0 event-index))
             (event-array (mvs:get-event-array m subsequence)))
        (mvs::old-set-model-alphabets m event-array subsequence 
                                      (mvs:mvs-basic m))
        (when (>= event-index context-length)
          (format t "~&~2,0@TGenerated Event ~A: ~A~%" event-index event-array))
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
    (reverse new-sequence)))

(defmethod generate-event ((m mvs) event predictions)
  (let ((predictions (select-from-distributions predictions)))
    (dolist (p predictions event)
      (setf 
       (slot-value event (find-symbol (symbol-name (car p)) (find-package :music-data)))
       (nth 2 p)))))

(defun select-from-distributions (predictions)
  (mapcar #'(lambda (x) 
              (list (viewpoint-type (prediction-sets:prediction-viewpoint x))
                    (prediction-sets:prediction-element x)
                    (select-max-from-distribution 
                     (prediction-sets:prediction-set x))))
                    ;(select-from-distribution 
                    ; (prediction-sets:prediction-set x))))
          predictions))
    
