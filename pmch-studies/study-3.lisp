;;; =======================================================================
;;;; File:       study-3.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-26 19:12:50 peter>                        
;;;; Time-stamp: <2017-11-12 16:12:42 peter>                           
;;;; =======================================================================

;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Provides utility functions for Study 3 of Peter's PhD,
;;;; termed 'HarmonyPerception', primarily the stimulus generation part.

;; 

(cl:in-package #:pmch-s3)

;;;; ======================================================================
;;;; Constructing the stimuli, with initial analyses ======================
;;;; ======================================================================

(defparameter *genres* '(:classical :popular :jazz))
(defparameter *genre-dataset-ids* '(1 2 3))
(defparameter *genre-reduce-harmony* '(t nil nil))
(defparameter *num-ic-categories* 1)
(defparameter *num-stimuli-per-ic-category* 510)
(defparameter *num-stimuli-per-ic-category-intermediate* 510) ;; intermediate filtering threshold imposed for computational tractability
(defparameter *num-chords-in-stimulus* 8)
(defparameter *target-chord-position* 5) ;; 0-indexed
(defparameter *tempo* 60)
(defparameter *exclude-unisons* t)
(defparameter *filter-stm* nil)
(defparameter *min-stm-ic* 2.0)
(defparameter *mp3-bit-rate* 256)

(defparameter *h-cpitch-analysis-files*
  '("/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-1-harmonic-reduction-t/resampling-training-set-size-987/h-cpitch/dat_from_idyom/1-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"
    "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-2-harmonic-reduction-nil/resampling-training-set-size-714/h-cpitch/dat_from_idyom/2-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"
    "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/data-raw/data-6/data/predictions/pretraining-none/test-dataset-3-harmonic-reduction-nil/resampling-training-set-size-1024/h-cpitch/dat_from_idyom/3-h-cpitch-h-cpitch-nil-nil-harmony-nil-30-ltm-nil-t-nil-c-nil-t-t-x-2.5.dat"))

(defparameter *random-seed* 0)

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

(defun get-num-pcs (dataset)
  "Given a <dataset>, returns a vector
corresponding to the number of pitch classes in each chord of that
dataset, ordered by composition ID and event ID."
  (let* ((num-pcs (mapcan #'(lambda (c)
			      (viewpoints:viewpoint-sequence
			       (viewpoints:get-viewpoint 'num-pcs-in-chord)
			       c))
			  dataset)))
    (coerce num-pcs 'vector)))

(defun choose-stimuli-in-ic-category (candidate-stimuli used-compositions)
  "Chooses a set of stimuli for a particular IC category from
<candidate-stimuli>, ensuring that no stimulus is drawn from <used-compositions>.
The old side effects on <used-compositions> have been removed."
  (let ((used-compositions (copy-list used-compositions))
	(shuffled-candidates (utils:shuffle candidate-stimuli)))
    ;; Remove candidates from a composition that has been used already
    (loop for i from 1 to *num-stimuli-per-ic-category-intermediate*
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
     used-compositions
     num-pcs-in-chords)
  ;; Note: num-events = (length ics) = (length c-ids) etc.
  (assert (utils:all-eql (list (length c-ids) (length e-ids)
			       (length ics) num-events
			       (length num-pcs-in-chords))))
  (let ((desired-num-chords-post-target (- *num-chords-in-stimulus*
					   *target-chord-position* 1)))
    (loop for j from 0 to (1- num-events)
       as c-id = (aref c-ids j)
       as e-id = (aref e-ids j)
       as first-e-id = (- e-id *target-chord-position*)
       as last-e-id = (1- (+ first-e-id *num-chords-in-stimulus*))
       as ic = (aref ics j)
       as ic-category = (utils:assign-to-quantile ic quantiles)
       as count-pcs-in-chords = (loop
				   with start = (- j *target-chord-position*)
				   with end = (1- (+ start *num-chords-in-stimulus*))
				   for i from (max start 0) to (min end (1- num-events))
				 collect (aref num-pcs-in-chords i))
       when (and
	     (= ic-category desired-ic-category)
	     (>= e-id *target-chord-position*)
	     (let* ((composition-length
		     (gethash c-id composition-lengths))
		    (num-chords-available-post-target
		     (- composition-length e-id 1)))
	       (>= num-chords-available-post-target
		   desired-num-chords-post-target))
	     (not (member c-id used-compositions :test #'=))
	     (if *exclude-unisons*
		 (every #'(lambda (x) (> x 1))
			count-pcs-in-chords)
		 t))
       collect
	 (list (cons :c-id c-id)
	       (cons :e-id e-id)
	       (cons :first-e-id first-e-id)
	       (cons :last-e-id last-e-id)
	       (cons :num-pcs-in-chords count-pcs-in-chords)
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
  (ensure-directories-exist (utils:ensure-directory output-dir))
  (let* ((stimuli (construct-stimuli))
	 (stimuli (add-metadata stimuli)))
    (when output-dir
      (save-stimuli stimuli output-dir))))
    ;; stimuli))

(defun save-stimuli (stimuli output-dir)
  (let* ((output-dir (ensure-directories-exist
		      (utils:ensure-directory output-dir)))
	 (metadata-path (merge-pathnames output-dir "lisp_metadata.csv"))
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
  (utils:message (format nil "Saving audio files for ~A stimuli..."
			 (length stimuli)))
  (let ((midi-path (merge-pathnames (make-pathname :directory
						   '(:relative "midi"))
				    output-path))
	(wav-path (merge-pathnames (make-pathname :directory
						  '(:relative "wav"))
				   output-path))
	(mp3-path (merge-pathnames (make-pathname :directory
						  '(:relative "mp3"))
				   output-path))
	(pdf-path (merge-pathnames (make-pathname :directory
						  '(:relative "pdf"))
				   output-path))
	(bar (utils:initialise-progress-bar (length stimuli))))
    (ensure-directories-exist midi-path)
    (ensure-directories-exist wav-path)
    (ensure-directories-exist mp3-path)
    (ensure-directories-exist pdf-path)
    (loop
       for stimulus in stimuli
       for i from 1
       as label = (cdr (assoc :label stimulus))
       as filename-midi = (merge-pathnames midi-path (format nil "~A.mid" label))
       as filename-wav = (merge-pathnames wav-path (format nil "~A.wav" label))
       as filename-mp3 = (merge-pathnames mp3-path (format nil "~A.mp3" label))
       as filename-pdf = (merge-pathnames pdf-path (format nil "~A.pdf" label))
       as music-data = (cdr (assoc :music-data stimulus))
       do
	 (md:export-midi music-data midi-path :filename filename-midi)
	 (midi->wav filename-midi filename-wav)
	 (wav->mp3 filename-wav filename-mp3)
	 (md:midi->pdf filename-midi filename-pdf)
	 (utils:update-progress-bar bar i))))

(defun midi->wav (midi-file wav-file)
  (ensure-directories-exist wav-file)
  (sb-ext:run-program "/usr/local/Cellar/timidity/2.14.0/bin/timidity"
		      (list "-Ow" "-o" (namestring wav-file) (namestring midi-file))))

(defun wav->mp3 (wav-file mp3-file)
  (ensure-directories-exist mp3-file)
  (ensure-directories-exist wav-file)
  (sb-ext:run-program "/usr/local/Cellar/lame/3.99.5/bin/lame"
		      (list "-b" (format nil "~A" *mp3-bit-rate*)
			    (namestring wav-file) (namestring mp3-file))))


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
		 as first-e-id = (cdr (assoc :first-e-id stimulus))
		 as last-e-id = (cdr (assoc :last-e-id stimulus))
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
				 (cons :target-chord-position-1-indexed (1+ *target-chord-position*)))
			   stimulus)))))

(defun construct-stimuli ()
  (let ((h-cpitch-analyses (load-h-cpitch-analyses))
	(*random-state* (sb-ext:seed-random-state *random-seed*)))
    (loop
       for genre in *genres*
       for dataset-id in *genre-dataset-ids*
       for reduce-harmony in *genre-reduce-harmony*
       for analysis in h-cpitch-analyses
       collect
	 (cons genre
	       (let* ((harmonic-reduction (if reduce-harmony
					      :regular-harmonic-rhythm
					      :none))
		      (music-data (md:get-music-objects
				   (list dataset-id) nil
				   :voices nil :texture :harmony
				   :harmonic-reduction harmonic-reduction
				   :slices-or-chords :chords
				   :remove-repeated-chords t))
		      (c-ids (cdr (assoc :c-id analysis)))
		      (e-ids (cdr (assoc :e-id analysis)))
		      (ics (cdr (assoc :ic analysis)))
		      (num-pcs-in-chords (get-num-pcs music-data))
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
						     used-compositions
						     num-pcs-in-chords))
			     (refined-candidates
			      (choose-stimuli-in-ic-category candidate-stimuli
							     used-compositions))
			     (refined-candidates
			      (mapcar #'(lambda (s)
					  (acons :music-data
						 (md:regularize-rhythm
						  (md:subsequence (nth (cdr (assoc :c-id s)) music-data)
								  (cdr (assoc :first-e-id s))
								  (cdr (assoc :last-e-id s)))
						  :tempo *tempo*)
						 s))
				      refined-candidates))
			     (refined-candidates
			      (mapcar #'(lambda (s)
					  (acons :h-cpitch
						 (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint
										 'h-cpitch)
										(cdr (assoc :music-data s)))
						 s))
				      refined-candidates))
			     (refined-candidates
			      (mapcar #'(lambda (s)
			     		  (acons :h-cpitch-stm-ic (get-stm-ic (cdr (assoc :music-data s))) s))
			     	      refined-candidates))
			     (refined-candidates
			      (mapcar #'(lambda (s)
					  (acons :milne-full-seq
						 (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint
										 'h-cpc-milne-sd-cont=min)
										(cdr (assoc :music-data s)))
						 s))
				      refined-candidates))
			     (refined-candidates
			      (mapcar #'(lambda (s)
					  (acons :milne-target
						 (viewpoints:viewpoint-element (viewpoints:get-viewpoint
										'h-cpc-milne-sd-cont=min)
									       (subseq (cdr (assoc :music-data s))
										       0 (1+ *target-chord-position*)))
						 s))
				      refined-candidates))	 
			     (refined-candidates (if *filter-stm*
						     (remove-if #'(lambda (s) (< (cdr (assoc :h-cpitch-stm-ic s))
										 *min-stm-ic*))
								refined-candidates)
						     refined-candidates))
			     (refined-candidates (utils:sample *num-stimuli-per-ic-category*
							       refined-candidates)))
			(setf used-compositions (append (mapcar #'(lambda (s)
								    (cdr (assoc :c-id s)))
								refined-candidates)
							used-compositions))
		        refined-candidates)))))))

(defun break-me (x)
  (break x))

(defun get-stm-ic (stimulus)
  (let* ((stimulus (coerce stimulus 'list))
	 (viewpoints::*basic-types* (list :h-cpitch))
	 ;; Initialise MVS parameters (see idyom:resampling)
	 (models :stm)
	 (ltmo mvs::*ltm-params*)
	 (stmo mvs::*stm-params*)
	 (ltmo (apply #'resampling:check-model-defaults
		      (cons mvs::*ltm-params* ltmo)))
         (stmo (apply #'resampling:check-model-defaults
		      (cons mvs::*stm-params* stmo)))
         (mvs::*models* models)
         (mvs::*ltm-order-bound* (getf ltmo :order-bound))
         (mvs::*ltm-mixtures* (getf ltmo :mixtures))
         (mvs::*ltm-update-exclusion* (getf ltmo :update-exclusion))
         (mvs::*ltm-escape* (getf ltmo :escape))
         (mvs::*stm-order-bound* (getf stmo :order-bound))
         (mvs::*stm-mixtures* (getf stmo :mixtures))
         (mvs::*stm-update-exclusion* (getf stmo :update-exclusion))
         (mvs::*stm-escape* (getf stmo :escape))
	 ;; Initialise an MVS
	 (test-set (list stimulus))
	 (sources (list (viewpoints:get-viewpoint 'h-cpitch)))
	 (targets (viewpoints:get-basic-viewpoints '(h-cpitch) test-set))
	 (ltms (resampling:get-long-term-models sources nil
					        nil nil
						nil nil 
					        nil :harmony
						nil))
	 (mvs (mvs:make-mvs targets sources ltms))
	 (cl-user::*idyom-message-detail-level* 0)
	 (predictions (mvs:model-dataset mvs test-set
					 :construct? t :predict? t
					 :detail 1))
	 (predictions (car (gethash :mean.information.content (utils:data predictions)))))
    predictions))

;;;; ======================================================================
;;;; Performing further analyses on the stimuli ==== ======================
;;;; ======================================================================

(defun further-analyses
    (&key
       (genres-to-analyse (list :popular))
       (input-file
	"/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyPerception/interface/www/stimuli/lisp_metadata.csv")
       (output-dir
	"/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyPerception/interface/www/stimuli/further-idyom-analyses/")
       (downsample t)
       (do-single-viewpoint-analyses t)
       (do-multiple-viewpoint-analyses t))
  (assert (listp genres-to-analyse))
  (let ((stimuli-all-genres (import-initial-stimulus-file input-file))
	(output-dir (utils:ensure-directory output-dir))
	(viewpoints (list-viewpoints :downsample downsample)))
    (loop
       for genre in genres-to-analyse
       do (let* ((genre-stimuli (get-genre-stimuli stimuli-all-genres genre :downsample downsample))
		 (genre-output-dir
		  (merge-pathnames (make-pathname :directory (list :relative (string-downcase (symbol-name genre))))
				   output-dir))
		 (genre-single-viewpoint-output-dir
		  (merge-pathnames (make-pathname :directory (list :relative "single-viewpoint"))
				   genre-output-dir))
		 (genre-multiple-viewpoint-output-dir
		  (merge-pathnames (make-pathname :directory (list :relative "multiple-viewpoint"))
				   genre-output-dir)))
	    (when do-single-viewpoint-analyses
	      (single-viewpoint-analyses genre-stimuli viewpoints
					 genre-single-viewpoint-output-dir))
	    (when do-multiple-viewpoint-analyses
	      (multiple-viewpoint-analyses genre-stimuli
					   genre-single-viewpoint-output-dir
					   genre-multiple-viewpoint-output-dir))))
    stimuli-all-genres))

(defun import-initial-stimulus-file (input-file)
  "Imports the initial csv file describing the stimuli, as constructed by <generate-stimuli>."
  (let* ((csv (cl-csv:read-csv (pathname input-file)))
	 (header (car csv))
	 (body (cdr csv)))
    (assert (= (length header)
	       (length (car body))))
    (let ((types (list (cons :id :literal)
		       (cons :label :string)
		       (cons :genre :keyword)
		       (cons :dataset-id :literal)
		       (cons :reduce-harmony :literal)
		       (cons :TARGET-CHORD-POSITION-1-INDEXED :literal)
		       (cons :milne-target :literal)
		       (cons :milne-full-seq :literal)
		       (cons :h-cpitch-stm-ic :literal)
		       (cons :h-cpitch :literal)
		       (cons :music-data :string)
		       (cons :c-id :literal)
		       (cons :e-id :literal)
		       (cons :first-e-id :literal)
		       (cons :last-e-id :literal)
		       (cons :num-pcs-in-chords :literal)
		       (cons :ic :literal)
		       (cons :ic-category :literal))))
      (loop
	 for line in body
	 collect (loop
		    for field in header
		    for elt in line
		    collect
		      (let* ((field-symbol (intern (string-upcase field) 'keyword))
			     (field-type (cdr (assoc field-symbol types)))
			     (elt-parsed (case field-type
					   (:literal (if (string= elt "") nil (read-from-string elt)))
					   (:string elt)
					   (:keyword (intern (string-upcase elt) 'keyword))
					   (t (error "Unrecognised column type")))))
			(cons field-symbol elt-parsed)))))))

(defun get-genre-stimuli (stimuli-all-genres genre &key downsample)
  (let ((all-genre-stimuli
	 (remove-if-not #'(lambda (stimulus) (eql (cdr (assoc :genre stimulus))
						  genre))
			stimuli-all-genres)))
    (if downsample
	(subseq all-genre-stimuli 0 5)
	all-genre-stimuli)))	

(defun list-viewpoints (&key downsample)
  "Lists the viewpoints to be analysed in the present study."
  (let ((all-viewpoints
	 '(h-bass-cpc
	   h-bass-cpcint
	   h-bass-csd
	   h-bass-int-from-gct-root
	   h-cpc-int-from-bass
	   h-cpc-int-from-gct-root
	   h-cpc-milne-sd-cont=min
	   h-cpc-vl-dist-p=1
	   h-cpitch
	   h-cpitch-class-set
	   h-csd
	   h-gct-3rd-type
	   h-gct-7th-type
	   h-gct-base
	   h-gct-ext
	   h-gct-meeus-int
	   h-gct-root-5ths-dist
	   h-gct-root-cpc
	   h-gct-root-cpcint
	   h-gct-root-csd
	   h-hash-12
	   h-hedges-chord-type
	   h-hutch-rough
	   (h-csd h-bass-csd)
	   (h-cpc-int-from-bass h-bass-cpcint)
	   (h-cpc-int-from-gct-root h-gct-root-cpcint))))
    (if downsample
	(subseq all-viewpoints 0 3)
	all-viewpoints)))

(defun format-viewpoint-name (viewpoint)
  (assert (or (listp viewpoint) (symbolp viewpoint)))
  (string-downcase
   (if (listp viewpoint)
       (format nil "~{~A~^-x-~}"
	       (mapcar #'symbol-name
		       viewpoint))
       (symbol-name viewpoint))))

(defun single-viewpoint-analyses (stimuli viewpoints output-dir)
  "Performs single-viewpoint analyses. <output-dir> should be
the path to the desired output directory, which will be created if it 
doesn't exist.
The top level of the output is a set of text files corresponding
to single-viewpoint analysis output from IDyOM."
  (assert (listp stimuli))
  (assert (> (length stimuli) 0))
  (assert (listp viewpoints))
  (let* ((output-dir (ensure-directories-exist (utils:ensure-directory output-dir)))
	 (dataset-id (cdr (assoc :dataset-id (car stimuli))))
	 (reduce-harmony (cdr (assoc :reduce-harmony (car stimuli))))
	 (harmonic-reduction (if reduce-harmony
				 :regular-harmonic-rhythm
				 :none)))
    (utils:message (format nil "Loading dataset ~A, harmonic-reduction = ~A..."
			   dataset-id harmonic-reduction))
    (let ((music-data (md:get-music-objects (list dataset-id) nil
					    :voices nil :texture :harmony
					    :harmonic-reduction harmonic-reduction
					    :slices-or-chords :chords
					    :remove-repeated-chords t)))
      (loop
	 for stimulus in stimuli
	 for i from 1
	 with n = (length stimuli)
	 do (let* ((stimulus-id (cdr (assoc :id stimulus)))
		   (c-id (cdr (assoc :c-id stimulus)))
		   (first-e-id (cdr (assoc :first-e-id stimulus)))
		   (last-e-id (cdr (assoc :last-e-id stimulus)))
		   (stimulus-output-dir (merge-pathnames (make-pathname :directory
									(list :relative
									      (princ-to-string stimulus-id)))
							 output-dir))
		   (music-data-pretraining (remove-if #'(lambda (c) (= (md:get-composition-index (md:get-identifier c))
								       c-id))
						      music-data))
		   (music-data-test-composition (find c-id music-data
						      :key #'(lambda (c) (md:get-composition-index
									  (md:get-identifier c)))))
		   (music-data-test-stimulus (md:subsequence music-data-test-composition
							     first-e-id last-e-id)))
	      (utils:message (format nil "Running SV analyses for stimulus ~A/~A..." i n))
	      (utils:message (format nil "Chords: ~A" (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'h-cpitch)
										     music-data-test-stimulus)))
	      (loop for model in '(:ltm :stm)
		 do (loop for viewpoint in viewpoints
		       do (let* ((viewpoint-label (format-viewpoint-name viewpoint))
				 (model-label (string-downcase (symbol-name model)))
				 (model-viewpoint-label (concatenate 'string model-label "-" viewpoint-label))
				 (output-filename (make-pathname :name model-viewpoint-label :type "txt")))
			    (single-vp-analyse-stimulus music-data-pretraining
							music-data-test-stimulus
							viewpoint
							model
							stimulus-output-dir
							output-filename)))))))))

(defun single-vp-analyse-stimulus (music-data-pretraining
				   music-data-test-stimulus
				   viewpoint
				   model
				   stimulus-output-dir
				   output-filename)
  (assert (member model '(:ltm :stm)))
  (assert (listp music-data-pretraining))
  (assert (typep music-data-test-stimulus 'md:music-object))
  (utils:message (format nil "Viewpoint = ~A, model = ~A." viewpoint model))
  (ensure-directories-exist (utils:ensure-directory stimulus-output-dir))
  (let ((viewpoints::*basic-types* (list :h-cpitch)))
    (idyom:idyom
     (list music-data-test-stimulus)
     '(h-cpitch) (list viewpoint)
     :k 1 :texture :harmony :models model
     :pretraining-ids music-data-pretraining
     ;; Note: we don't have to provide harmonic-reduction or repeated-chords arguments
     ;; because the database is being bypassed
     :separator #\tab :detail 2.5
     :use-resampling-set-cache? nil
     :slices-or-chords :chords
     :num-quantiles 12
     :use-ltms-cache? nil
     :overwrite t
     :output-path stimulus-output-dir
     :output-filename output-filename)))

(defun multiple-viewpoint-analyses
    (stimuli single-viewpoint-output-dir  multiple-viewpoint-output-dir)
  ;; Compile the viewpoint analyses into one probability matrix
  ;; Find optimal weights using R and save the resulting probability profile
  nil)

