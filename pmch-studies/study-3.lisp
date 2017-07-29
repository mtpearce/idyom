;;; =======================================================================
;;;; File:       study-3.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-26 19:12:50 peter>                        
;;;; Time-stamp: <2017-07-29 22:40:58 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides utility functions for Study 3 of Peter's PhD,
;;;; primarily the stimulus generation part.

(cl:in-package #:pmch-s3)

(defparameter *genres* '(:classical :popular :jazz))
(defparameter *genre-dataset-ids* '(1 2 3))
(defparameter *genre-reduce-harmony* '(t nil nil))
(defparameter *num-ic-categories* 10)
(defparameter *num-stimuli-per-ic-category* 3)
(defparameter *num-chords-in-stimulus* 10)
(defparameter *target-chord-position* 5) ;; 0-indexed
(defparameter *tempo* 70)

(defparameter *h-cpitch-analysis-files*
  '("/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-1-harmonic-reduction-t/resampling-training-set-size-987/h-cpitch/dat_from_idyom/1-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"
    "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-2-harmonic-reduction-nil/resampling-training-set-size-714/h-cpitch/dat_from_idyom/2-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"
    "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-3-harmonic-reduction-nil/resampling-training-set-size-1024/h-cpitch/dat_from_idyom/3-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"))

(defun load-h-cpitch-analyses ()
  ;; Note that 1-indexing is converted to 0-indexing
  (loop for path in *h-cpitch-analysis-files*
     collect (let* ((raw (cl-csv:read-csv (pathname path) :separator #\tab
					  :escape "\\\""))
		    (header (car raw))
		    (body (cdr raw))
		    (composition-id (make-array (length body) :element-type 'integer))
		    (event-id (make-array (length body) :element-type 'integer))
		    (info-content (make-array (length body)))
		    (composition-id-col (position-if #'(lambda (x)
							 (string= x "melody.id"))
						     header))
		    (event-id-col (position-if #'(lambda (x)
						   (string= x "note.id"))
					       header))
		    (info-content-col
		     (position-if #'(lambda (x)
				      (string= x "information.content"))
				  header)))
	       (loop
		  for i from 0 to (1- (length body))
		  for line in body
		  do
		    (setf (aref composition-id i)
			  (1- (parse-integer (nth composition-id-col line))))
		    (setf (aref event-id i)
			  (1- (parse-integer (nth event-id-col line))))
		    (setf (aref info-content i)
			  (utils:parse-number (nth info-content-col line))))
	       (list (cons :c-id composition-id)
		     (cons :e-id event-id)
		     (cons :ic info-content)))))

(defun get-num-pcs (dataset-id reduce-harmony)
  "Given a <dataset-id> and the Boolean <reduce-harmony> (whether or
not to reduce the harmony in the given dataset), returns a vector
corresponding to the number of pitch classes in each chord of that
dataset, ordered by composition ID and event ID."s
  (let* ((harmonic-reduction (if reduce-harmony
				 :regular-harmonic-rhythm
				 :none))
	 (music-data (md:get-music-objects (list dataset-id) nil
					   :voices nil :texture :harmony
					   :harmonic-reduction harmonic-reduction
					   :slices-or-chords :chords
					   :remove-repeated-chords t))
	 (num-pcs (mapcan #'(lambda (c)
			      (viewpoints:viewpoint-sequence
			       (viewpoints:get-viewpoint 'num-pcs-in-chord)
			       c))
			  music-data)))
    (coerce num-pcs 'vector)))

(defun choose-stimuli-in-ic-category (candidate-stimuli used-compositions)
  "Chooses a set of stimuli for a particular IC category from
<candidate-stimuli>, ensuring that no stimulus is drawn from <used-compositions>.
Side-effect: <used-compositions> is updated to add the compositions used
by each new stimulus."
  (let ((shuffled-candidates (utils:shuffle candidate-stimuli)))
    ;; Remove candidates from a composition that has been used already
    (loop for i from 1 to *num-stimuli-per-ic-category*
       do
	 (setf shuffled-candidates 
	       (remove-if #'(lambda (stimulus)
			      (member (cdr (assoc :c-id stimulus))
				      used-compositions
				      :test #'=))
			  shuffled-candidates))
       ;; Choose the next stimulus from the beginning of the shuffled list
       collect (let* ((stimulus (car shuffled-candidates))
		      (c-id (cdr (assoc :c-id stimulus))))
		 (push c-id used-compositions)
		 (assert (not (null stimulus)))
		 stimulus))))	      

(defun get-candidate-stimuli
    (c-ids e-ids ics
     num-events quantiles
     composition-lengths
     desired-ic-category
     used-compositions)
  (let ((desired-num-chords-post-target (- *num-chords-in-stimulus*
					   *target-chord-position* 1)))
    (loop for j from 0 to (1- num-events)
       as c-id = (aref c-ids j)
       as e-id = (aref e-ids j)
       as ic = (aref ics j)
       as ic-category = (utils:assign-to-quantile ic quantiles)
       when (and
	     (= ic-category desired-ic-category)
	     (>= e-id *target-chord-position*)
	     (let* ((composition-length
		     (gethash c-id composition-lengths))
		    (num-chords-available-post-target
		     (- composition-length e-id 1)))
	       (>= num-chords-available-post-target
		   desired-num-chords-post-target))
	     (not (member c-id used-compositions :test #'=)))
       collect
	 (list (cons :c-id c-id)
	       (cons :e-id e-id)
	       (cons :ic ic)
	       (cons :ic-category ic-category)))))

(defun get-composition-lengths
    (num-events c-ids e-ids)
  (loop
     with h = (make-hash-table)
     for x from 0 to (1- num-events)
     do
       (let* ((c-id (aref c-ids x))
	      (e-id (aref e-ids x))
	      (prev-max (gethash c-id h)))
	 (setf (gethash c-id h)
	       (if (null prev-max)
		   (1+ e-id)
		   (max (1+ e-id) prev-max))))
     finally (return h)))


;;;; Method

(defun generate-stimuli (&optional (output-dir "/Users/peter/tmp/"))
  (let* ((stimuli (construct-stimuli))
	 (stimuli (add-metadata stimuli)))
    (when output-dir
      (save-stimuli stimuli output-dir))))
    ;; stimuli))

(defun save-stimuli (stimuli output-dir)
  (let* ((output-dir (ensure-directories-exist
		      (utils:ensure-directory output-dir)))
	 (metadata-path (merge-pathnames output-dir "metadata.csv"))
	 (audio-path (merge-pathnames (make-pathname :directory
						     '(:relative "audio"))
				      output-dir)))
    (save-metadata stimuli metadata-path)
    (save-audio stimuli (ensure-directories-exist audio-path))))

(defun save-metadata (stimuli metadata-path)
  (let ((headers (mapcar #'(lambda (stimulus)
			     (mapcar #'(lambda (slot)
					 (string-downcase (symbol-name
							   (car slot))))
				     stimulus))
			 stimuli)))
    (assert (utils:all-eql headers :predicate #'equal))
    (let* ((header (car headers))
	   (body (mapcar #'(lambda (stimulus)
			     (mapcar #'(lambda (slot) (cdr slot))
				     stimulus))
			 stimuli))
	   (output (cons header body)))
      (with-open-file (stream metadata-path :direction :output
			      :if-exists :supersede)
	(cl-csv:write-csv output :stream stream)))))

(defun save-audio (stimuli output-path)
  (utils:message (format nil "Saving MIDI files for ~A stimuli..."
			 (length stimuli)))
  (let ((midi-path (ensure-directories-exist
		    (merge-pathnames (make-pathname :directory
						    '(:relative "midi"))
				     output-path)))
	(bar (utils:initialise-progress-bar (length stimuli))))
    (loop
       for stimulus in stimuli
       for i from 1
       as label = (cdr (assoc :label stimulus))
       as filename = (merge-pathnames midi-path (format nil "~A.mid" label))
       as first-e-id = (cdr (assoc :first-e-id stimulus))
       as last-e-id = (cdr (assoc :last-e-id stimulus))
       as dataset-id = (cdr (assoc :dataset-id stimulus))
       as c-id = (cdr (assoc :c-id stimulus))
       as reduce-harmony = (cdr (assoc :reduce-harmony stimulus))
       as harmonic-reduction = (if reduce-harmony
				   :regular-harmonic-rhythm
				   :none)
       as composition = (car (md:get-music-objects
			      dataset-id c-id
			      :voices nil :texture :harmony
			      :harmonic-reduction harmonic-reduction
			      :slices-or-chords :chords
			      :remove-repeated-chords t))
       as music-stimulus = (md:regularize-rhythm
			    (md:subsequence composition first-e-id last-e-id)
			    :tempo *tempo*)
       do
	 (md:export-midi music-stimulus midi-path :filename filename)
	 (utils:update-progress-bar bar i))))


(defun add-metadata (stimuli)
  (let ((id 0))
    (loop
       for genre in stimuli
       for dataset-id in *genre-dataset-ids*
       for reduce-harmony in *genre-reduce-harmony*
       as genre-symbol = (car genre)
       as genre-label = (string-downcase (symbol-name genre-symbol))
       as genre-stimuli = (cdr genre)
       append (loop
		 for stimulus in genre-stimuli
		 as stimulus-id = (incf id)
		 as c-id = (cdr (assoc :c-id stimulus))
		 as e-id = (cdr (assoc :e-id stimulus))
		 as first-e-id = (- e-id *target-chord-position*)
		 as last-e-id = (1- (+ first-e-id *num-chords-in-stimulus*))
		 as ic-category = (cdr (assoc :ic-category stimulus))
		 as label = (format nil
				    "id=~A_genre=~A_c-id=~A_e-id=~A_ic_category_~A"
				    stimulus-id genre-label c-id e-id ic-category)
		 collect
		   (append (list (cons :id stimulus-id)
				 (cons :label label)
				 (cons :genre genre-symbol)
				 (cons :dataset-id dataset-id)
				 (cons :reduce-harmony reduce-harmony)
				 (cons :first-e-id first-e-id)
				 (cons :last-e-id last-e-id))
			   stimulus)))))

(defun construct-stimuli ()
  (let ((h-cpitch-analyses (load-h-cpitch-analyses)))
    (loop
       for genre in *genres*
       for analysis in h-cpitch-analyses
       collect
	 (cons genre
	       (let* ((c-ids (cdr (assoc :c-id analysis)))
		      (e-ids (cdr (assoc :e-id analysis)))
		      (ics (cdr (assoc :ic analysis)))
		      (num-events (length ics))
		      (used-compositions nil)
		      (composition-lengths
		       (get-composition-lengths num-events c-ids e-ids))
		      (quantiles (utils:quantiles (cdr (assoc :ic analysis))
						  *num-ic-categories*)))
		 ;; Loop over IC categories (which are 1-indexed)
		 (loop
		    for desired-ic-category from 1 to *num-ic-categories*
		    ;; Loop over events
		    append
		      (let* ((candidate-stimuli
			      (get-candidate-stimuli c-ids e-ids ics
						     num-events quantiles
						     composition-lengths
						     desired-ic-category
						     used-compositions)))
			(choose-stimuli-in-ic-category candidate-stimuli
						       used-compositions))))))))

;;;; TODO
;; Synthesise mp3
;; Add tests
;; Add random state for reproducibility
