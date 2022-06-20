;;;;
;;;; An implementation of R dataframes 
;;;;

(cl:in-package #:utils)

;;; A dataframe class

(defclass dataframe ()
  ((data :initform (make-hash-table :test #'equal) :accessor data)
   (num-rows :initform 0 :accessor num-rows))
  (:documentation "A <dataframe> efficiently accumulates and stores text
data in a tabular form. Columns are identified by unique IDs, and are
stored as lists within a hash table.  Note that lists are accumulated
in reverse order, so that appending to a column can be achieved by
consing a new value to the beginning of the list."))


;; accessing data 

(defgeneric get-column-names (dataframe)
  (:documentation "Return a list of column names in <dataframe>"))

(defmethod get-column-names ((df dataframe))
  (loop for key being the hash-keys of (data df) collect key))

(defgeneric get-column (column dataframe)
  (:documentation "Gets column with ID <column> from <dataframe>."))

(defmethod get-column ((column symbol) (dataframe dataframe))
  (reverse (gethash column (data dataframe)))) ;; should this be reversed? 

(defgeneric get-columns (columns dataframe)
  (:documentation "Returns the data for <columns> in <dataframe>"))

(defmethod get-columns (columns (df dataframe))
  (mapcar #'(lambda (column) (get-column column df)) columns))


;; adding data

(defgeneric add-row (row place)
  (:documentation "Adds a new row, <row>, to a data storage object, <place>, and returns the new storage object."))

(defmethod add-row ((row hash-table) (place dataframe))
  (let ((old-keys (loop for key being the hash-keys of (data place) collect key))
	(new-keys (loop for key being the hash-keys of row collect key)))
    (if (utils:any-duplicated new-keys)
	(error "Duplicated keys are not allowed when adding new rows."))
    (let ((old-unmatched-keys (set-difference old-keys new-keys))
	  (new-matched-keys (intersection old-keys new-keys))
	  (new-unmatched-keys (set-difference new-keys old-keys)))
      (dolist (key old-unmatched-keys)
	(push nil (gethash key (data place))))
      (dolist (key new-matched-keys)
	(push (gethash key row) (gethash key (data place))))
      (dolist (key new-unmatched-keys)
	(setf (gethash key (data place))
	      (cons (gethash key row)
		    (make-list (num-rows place) :initial-element nil))))
      (incf (num-rows place))
      place)))


;; Deleting data

(defgeneric remove-columns-except (columns-to-keep dataframe)
  (:documentation "Removes columns with IDs not in the list <columns-to-keep> from <dataframe>, and returns the new storage object."))

(defmethod remove-columns-except (columns-to-keep (dataframe dataframe))
  (loop for key being the hash-keys of (data dataframe)
     do (if (not (member key columns-to-keep))
	    (remhash key (data dataframe))))
  dataframe)


;; binding dataframes together by row

(defgeneric bind-by-row (dataframe &rest dataframes)
  (:documentation "Destructively appends <dataframes> by row to <dataframe>."))

(defmethod bind-by-row ((dataframe dataframe) &rest dataframes)
  (dolist (new-dataframe dataframes dataframe)
    (let ((old-keys (loop for key being the hash-keys of (data dataframe)
		       collect key))
	  (new-keys (loop for key being the hash-keys of (data new-dataframe)
		       collect key))
	  (num-new-rows (num-rows new-dataframe)))
      (if (utils:any-duplicated new-keys)
	  (error "Duplicated keys are not allowed when adding new rows."))
      (let ((old-unmatched-keys (set-difference old-keys new-keys))
	    (new-matched-keys (intersection old-keys new-keys))
	    (new-unmatched-keys (set-difference new-keys old-keys)))
	(dolist (key old-unmatched-keys)
	  (push (make-list num-new-rows :initial-element nil)
		(gethash key (data dataframe))))
	(dolist (key new-matched-keys)
	  (setf (gethash key (data dataframe))
		(nconc (gethash key (data new-dataframe))
		       (gethash key (data dataframe)))))
	(dolist (key new-unmatched-keys)
	  (setf (gethash key (data dataframe))
		(nconc (gethash key new-dataframe)
		       (make-list (num-rows dataframe) :initial-element nil))))
	(incf (num-rows dataframe) num-new-rows)))))   


;; Sorting dataframes

(defgeneric sort-by-columns (data columns &key descending)
  (:documentation "Sorts a dataframe <data> by columns. <columns> should be a
list of column names, in decreasing order of priority. <ascending> is a
Boolean variable that determines whether the dataframe is sorted in
ascending order or in descending order."))

(defmethod sort-by-columns ((dataframe dataframe) (columns list) &key descending)
  (let* ((row-nums (loop for i from 0 to (1- (num-rows dataframe)) collect i))
	 (predicate (if descending #'< #'>)))
    ;; Coerce columns to vectors
    (maphash #'(lambda (key column)
		 (setf (gethash key (data dataframe))
		       (coerce column 'vector)))
	     (data dataframe))
    (dolist (column (reverse columns))
      (setf row-nums (stable-sort row-nums
				  predicate
				  :key #'(lambda (x)
					   (svref (gethash column
							   (data dataframe))
						  x)))))
    ;; Reorder columns and save as lists
    (maphash #'(lambda (key column)
		 (setf (gethash key (data dataframe))
		       (loop for i in row-nums
			  collect (svref column i))))
	     (data dataframe))
    dataframe))


;;; Conversion to and from lists

(defgeneric as-list (data))
(defmethod as-list ((data dataframe))
  (let* ((columns (loop
		     for key being the hash-keys of (data data)
		     using (hash-value value)
		     collect (cons (string-downcase (symbol-name key))
				   (reverse value))))
	 (columns (coerce columns 'vector))
	 (num-rows (num-rows data))
	 (num-cols (array-dimension columns 0)))
    (assert (> num-rows 0))
    (assert (> num-cols 0))
    (assert (eql (num-rows data) (1- (length (svref columns 0)))))
    (let ((list nil))
      (dotimes (i (1+ (num-rows data)))
	(let ((row nil))
	  (dotimes (j num-cols)
	    (let* ((token (pop (svref columns j))))
	      (push token row)))
	  (push (reverse row) list)))
      (reverse list))))

(defgeneric as-dataframe (obj)
  (:documentation "Converts <obj> into a dataframe representation."))
(defmethod as-dataframe ((obj list))
  (let ((format-error "<obj> must be a list of assoc-lists."))
    (loop
       for row in obj
       with df = (make-instance 'dataframe)
       do (progn
	    (when (not (listp row)) (error format-error))
	    (loop
	       for cons in row
	       with row  = (make-hash-table)
	       do (progn
		    (when (not (consp cons)) (error format-error))
		    (let ((key (car cons))
			  (value (cdr cons)))
		      (when (not (null (nth-value 1 (gethash key row))))
			(error "Duplicated keys not allowed in <obj>."))
		      (setf (gethash key row) value)))
	       finally (add-row row df)))
       finally (return df))))


;; Printing dataframes

(defgeneric print-data (data stream &key separator order-by-key
				      null-token)
  (:documentation "Prints <data> to <stream>. If <order-by-key>, then the output
is ordered by key."))

(defmethod print-data ((data dataframe) destination
		       &key (separator " ") order-by-key null-token)
  (let* ((columns (loop
		     for key being the hash-keys of (data data)
		     using (hash-value value)
		     collect (cons (string-downcase (symbol-name key))
				   (reverse value))))
	 (columns (if order-by-key
		      (sort columns #'string< :key #'car)
		      columns))
	 (columns (coerce columns 'vector))
	 (num-rows (num-rows data))
	 (num-cols (array-dimension columns 0)))
    (assert (> num-rows 0))
    (assert (> num-cols 0))
    (assert (eql (num-rows data) (1- (length (svref columns 0)))))
    (dotimes (i (1+ (num-rows data)))
      (dotimes (j num-cols)
	(let* ((token (pop (svref columns j)))
	       (token (if (and (null token) null-token) null-token token)))
	  (format destination "~A" token)
	  (when (< j (1- num-cols)) (format destination "~A" separator))))
      (format destination "~&"))))

(defgeneric write-csv (obj path))
(defmethod write-csv ((obj dataframe) path)
  (cl-csv:write-csv (as-list obj) :stream path))
