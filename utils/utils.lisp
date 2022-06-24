;;;; ======================================================================
;;;; File:       utils.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-16 16:59:20 marcusp>
;;;; Time-stamp: <2022-06-22 20:20:24 marcusp>
;;;; ======================================================================

(cl:in-package #:utils)

;;;===========================================================================
;;; User interaction 
;;;===========================================================================

(defun ask-user-y-n-question (question)
  "Asks user a yes or no <question> over the command line.
   Returns t for yes, and nil for no."
  (format t "~%~A (y/n)~%" question)
  (let ((res (read)))
    (cond ((or (string= res "y")
	       (string= res "Y"))
	   t)
	  ((or (string= res "n")
	       (string= res "N"))
	   nil)
	  (t (ask-user-y-n-question question)))))

(defun message (text &key (detail 1) (add-new-line t))
  "Prints a status message (<text>) to standard-output, 
   and forces the output to appear immediately.
   If <add-new-line> is true, then a new line
   marker is appended to the message.

   <detail-level> decribes the detail level of the
   message, and can take the value 1, 2, or 3.
   1: highest-level status information, suitable
      default setting for non-technical user
   2: medium-level status information, e.g. RAM usage, 
      suitable default setting for advanced user
   3: low-level status information, intended only
      to be activated for debugging purposes.

   The current message printing detail level is determined
   by the variable <cl-user::*idyom-message-detail-level*>. Messages
   are only printed if their <detail-level> is less than or
   equal to the current value of <cl-user::*idyom-message-detail-level*>.

   Progress bars display at detail levels 1 and 2
   but not at detail level 3. These progress bars are
   disrupted if other messages print before the progress
   bar is finished. Messages within routines with 
   progress bars therefore must take detail level 3."
  (if (<= detail cl-user::*idyom-message-detail-level*)
      (progn
	(if add-new-line (format t "~%"))
	(format t text)
	(force-output))))

(defstruct progress-bar
  value num-blocks min max display-width)

(defun initialise-progress-bar
    (max &key (min 0) (initial 0) (display-width 60))
  "Initialises a progress bar object for the purpose
   of tracking an iterative operation. The progress bar
   can subsequently be updated using the <update-progress-bar>
   function.
   Whether or not the progress bar is actually displayed
   is determined by the variable <idyom::*message-detail-level*>.
   Bars are only displayed when this variable takes the 
   values 1 or 2 (i.e. not 3). 
   Progress bars are disrupted if other messages print before 
   the progress bar is finished. Messages within routines with 
   progress bars therefore must take detail level 3."
  (let ((bar (make-progress-bar
	      :value 0 :num-blocks 0
	      :min min :max max
	      :display-width display-width)))
    (if (member cl-user::*idyom-message-detail-level* '(1 2))
	(progn 
	  (format t "~%| Progress: ")
	  (dotimes (i (- display-width 13)) (format t "-"))
	  (format t "|~%")
	  (force-output)))
    (update-progress-bar bar initial)
    bar))

(defun update-progress-bar (bar value)
  (let* ((num-blocks (floor (* (progress-bar-display-width
				bar)
			       (/ value (progress-bar-max
					 bar)))))
	 (num-blocks-to-add (- num-blocks
			       (progress-bar-num-blocks
				bar))))
    (setf (progress-bar-num-blocks bar) num-blocks)
    (if (member cl-user::*idyom-message-detail-level* '(1 2))
	(progn 
	  (dotimes (i num-blocks-to-add)
	    (write-char #\=))
	  (force-output)))))

(defmacro dolist-pb
    ((var list &optional result) &body body)
  "A version of dolist that displays a progress
   bar tracking the iterative process."
  `(let* ((num-items (length ,list))
	  (counter 0)
	  (bar (initialise-progress-bar num-items)))
     (dolist (,var ,list ,result)
       ,@body
       (incf counter)
       (update-progress-bar bar counter))))

(defmacro dotimes-pb
    ((var count &optional result) &body body)
  "A version of dotimes that displays a progress
   bar tracking the iterative process."
  `(let* ((counter 0)
	  (bar (initialise-progress-bar ,count)))
     (dotimes (,var ,count ,result)
       ,@body
       (incf counter)
       (update-progress-bar bar counter))))


;;;===========================================================================
;;; Numerical 
;;;===========================================================================

(defun round-to-nearest-decimal-place (number &optional (decimal-places 2))
  "Rounds <number> to the number of decimal places specified by
   <decimal-places>."
  (let ((factor (expt 10 decimal-places)))
    (/ (fround (* number factor)) factor)))

(defun approx-equal (x y &optional (decimal-places 5))
  "Returns whether numbers <x> and <y> are approximately equal.
<decimal-places> determines the degree of equality."
  (assert (numberp x))
  (assert (numberp y))
  (assert (integerp decimal-places))
  (= (round-to-nearest-decimal-place x decimal-places)
     (round-to-nearest-decimal-place y decimal-places)))

(defun average (&rest numbers)
  "Returns the average of <numbers>."
  (if (null numbers) 
      0
      (float (/ (reduce #'+ numbers) (length numbers)))))

(defun sd (&rest numbers)
  "Returns the standard deviation of <numbers>."
  (if (null numbers)
      0
      (let ((mean (apply #'average numbers)))
        (sqrt (/ (reduce #'+ (mapcar #'(lambda (x) (expt (- x mean) 2)) numbers))
                 (- (length numbers) 1))))))

(defun cor (x y)
  "Computes the Pearson correlation coefficient between <x> and <y>,
which should be lists of numbers of equal length."
  (let ((x-mean (apply #'average x))
        (y-mean (apply #'average y))
        (x-sd (apply #'sd x))
        (y-sd (apply #'sd y))
        (n (length x)))
    (/ (- (apply #'+ (mapcar #'* x y))
          (* n x-mean y-mean))
       (* (- n 1) x-sd y-sd))))
       
(defun cumsum (&rest numbers)
  "Returns a list of length |<numbers>| - 1 containing cumulative sums."
  (let ((cs))
    (dolist (item numbers)
      (push (+ item (or (first cs) 0)) cs))
    (nreverse cs)))

(defun generate-integers (low high)
  "Returns a list containing all the integers between <low> and
   <high> inclusive."
  (if (> low high) '()
      (cons low (generate-integers (+ low 1) high))))

(defun nth-root (number n)
  "Returns the <n>th root of <number>."
  (expt number (/ 1 n)))

(defun powerset(l)
  "Returns the power set of list <l>."
  (if (null l)
      '(nil)
      (let ((ps (powerset (cdr l))))
        (append ps (mapcar #'(lambda (x) (cons (car l) x)) ps)))))

(defun quotient (x y)
  (car (multiple-value-list (truncate (/ x y)))))

(defun factorial (n)
  "Calculates the factorial of <n>." 
  (if (= n 0) 1) (* n (factorial (- n 1))))

(defun n-permutations (n r)
  "Returns the no. of permutations of <n> different items taken <r> at a time."
  (/ (factorial n) (factorial (- n r))))

(defun n-combinations (n r)
  "Returns the no. of combinations of <n> different items taken <r> at a time."
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defun range (max &key (min 0) (step 1))
  "Returns a list of numbers from <min> (default=0) to <max> by steps of <step> (default=1)"
   (loop for n from min below max by step
      collect n))

(defun parse-number (string)
  "Coerces a string representation of a number to numeric type."
  (with-input-from-string (input string)
    (read input)))

(defun quantiles (numbers num-quantiles)
  "Takes a sequence of numbers, <numbers>, and computes the locations
of <num-quantiles> quantiles of equal size. Returns an ordered list of the
non-trivial thresholds for these quantiles (i.e. excludes the 0th percentile 
and the 100th percentile). Uses linear interpolation of the 
empirical distribution function."
  (assert (every #'numberp numbers))
  (let* ((sorted (sort (coerce (copy-seq numbers) 'vector) #'<))
	 (n (length sorted)))
    (loop
       for k from 1 to (- num-quantiles 1)
       collect (float (let* ((p (/ k num-quantiles))
			     (h (* n p)))
			(cond ((< p (/ 1 n)) (svref sorted 0))
			      ((= p 1) (svref sorted (1- n)))
			      (t (+ (svref sorted (1- (floor h)))
				    (* (- h (floor h))
				       (- (svref sorted (floor h))
					  (svref sorted (1- (floor h)))))))))))))

(defun assign-to-quantile (number quantiles)
  "Given a number <number> and a list <quantiles> identifying a set of 
quantiles as produced by the function QUANTILES, returns the 1-indexed
quantile into which <number> falls."
  (assert (numberp number))
  (assert (listp quantiles))
  (assert (equal quantiles (sort quantiles #'<)))
  (let* ((num-quantiles (1+ (length quantiles)))
	 (match (position-if #'(lambda (x) (<= number x)) quantiles)))
    (if match
	(1+ match)
	num-quantiles)))	       

(defun subtract-mod-n (x y n)
  (if (<= y x)
      (- x y)
      (- (+ n x) y)))

;;;===========================================================================
;;; Sequences
;;;===========================================================================

(defun last-element (sequence)
  "Returns the last element of a sequence."
  (when sequence
    (elt (reverse sequence) 0)))

(defun penultimate-element (sequence) 
  "Returns the penultimate element of a sequence."
  (when sequence
    (elt (reverse sequence) 1)))

(defun last-n (sequence &optional (n 1))
  "Return the last n elements of a sequence."
  (subseq sequence (- (length sequence) n)))
          
(defun butlast-n (sequence &optional (n 1))
  "Return a sequence with the last n elements removed."
  (subseq sequence 0 (- (length sequence) n)))

(defun shuffle (sequence)
  "Shuffles a sequence into a random order. 
Borrowed from https://www.pvk.ca/Blog/Lisp/trivial_uniform_shuffling.html"
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1d0)))
                       sequence)
                  #'< :key #'cdr)))

(defun sample (n sequence)
  "Takes a random sample of size <n> from <sequence> without replacement."
  (assert (integerp n))
  (assert (>= n 0))
  (assert (<= n (length sequence)))
  (let ((shuffled (shuffle sequence)))
    (subseq shuffled 0 n)))


;;;===========================================================================
;;; Strings 
;;;===========================================================================

(defun list->string (list)
  (format nil "~{~A~^ ~}" list))

(defun string-append (&rest args)
  "Concatenates its string arguments <args>."
  (apply #'concatenate 'string args))

(defun split-string (string separator)
  "Takes a string object and returns a list of strings corresponding to each
   <separator> delimited sequence of characters in that string."
  (labels ((find-words (char-list word result)
             (cond ((null char-list) (reverse (cons word result)))
                   ((not (string= (car char-list) separator))
                    (find-words (cdr char-list)
                                (string-append word (list (car char-list)))
                                result))
                   (t (find-words (cdr char-list) "" (cons word result))))))
    (find-words (coerce string 'list) "" '())))

;; (defun regex (expression string)
;;   "Interface to the re:regexec function which returns the first sub-string
;;    in <string> matching <expression>."
;;   (if (null string)
;;       nil
;;       (let ((match (re:regexec string expression)))
;;         (if (null match) nil
;;             (subseq string (car (aref match 0)) (cadr (aref match 0)))))))


;;;===========================================================================
;;; Lists
;;;===========================================================================

(defun random-select (list n)
  "Given a <list> and a number <n> returns two values: the first is a
   list containing <n> elements drawn at random from
   from <list> without replacement; and the second  is <list> with
   those elements removed."
  (labels ((rs (list n result new-list)
             (cond ((= n 0) (values (reverse result) (reverse (append list new-list))))
                   ((< (random 1.0 (make-random-state t)) (/ n (length list)))
                    (rs (cdr list) (- n 1) (cons (car list) result) new-list))
                   (t (rs (cdr list) n result (cons (car list) new-list))))))
    (rs list n nil nil)))

(defun insertion-sort (list predicate)
  "Non-destructively sorts <list> according to <predicate>."
  (labels ((insert (n list)
             (cond ((null list)
                    (list n))
                   ((funcall predicate n (car list))
                    (cons n list))
                   (t (cons (car list) (insert n (cdr list)))))))
    (if (null list)
        '()
        (insert (car list) (insertion-sort (cdr list) predicate)))))

;from http://groups.google.co.uk/groups?hl=en&lr=&ie=UTF-8&threadm=u7wv7krnvv.fsf%40sol6.ebi.ac.uk&rnum=2&prev=/groups%3Fq%3D%2522cartesian%2Bproduct%2522%2Bgroup:comp.lang.lisp.*%2Bgroup:comp.lang.lisp.*%2Bgroup:comp.lang.lisp.*%26hl%3Den%26lr%3D%26ie%3DUTF-8%26group%3Dcomp.lang.lisp.*%26selm%3Du7wv7krnvv.fsf%2540sol6.ebi.ac.uk%26rnum%3D2
(defun cartesian-product (&rest lists)
  "Computes the cartesian product of <lists>."
  (if (null lists)
      (list nil)
      (let ((list (car lists))
	    (lists (cdr lists)))
	(let ((cartesian-product-of-lists (apply #'cartesian-product lists)))
	  (mapcan #'(lambda (x)
		      (mapcar #'(lambda (y) (cons x y))
			      cartesian-product-of-lists))
		  list)))))

(defun flatten (list)
  "Flatten nested lists."
  (labels ((flat (list result)
             (if (null list) (reverse result)
                 (let ((head (car list))
                       (tail (cdr list)))
                   (if (atom head)
                       (flat tail (cons head result))
                       (flat tail (append (flatten head) result)))))))
    (flat list '())))

(defun flatten-order (x)
  "Flatten nested lists, preserving element order."
  (cond ((null x) nil)
    ((listp x) (append (flatten-order (car x)) (flatten-order (cdr x))))
    (t (list x))))

(defun count-frequencies (x p)
  (let* ((flat (flatten x))
	 (unique (sort (remove-duplicates flat) p)))
    (pairlis unique
	     (mapcar #'(lambda (y) (count y flat)) unique))))

(defun numeric-frequencies (x)
  (count-frequencies x #'>))

;from http://cs-www.cs.yale.edu/homes/dvm/nil.html
(defun combinations (l n)
  "Compute a list of all combinations of elements of l taken n at a time."
  (if (> n (length l)) '()
      ;; From now on we know 0 =< n =< (length l)
      (labels ((proper-combos (l n)
                 (cond ((= n 0) (list '()))
                       ((= (length l) n) (list l))
                       (t (nconc (proper-combos (cdr l) n)
                                 (mapcar (lambda (c) (cons (car l) c))
                                         (proper-combos (cdr l) (- n 1))))))))
        (proper-combos l n))))     

(defun find-duplicates (list &key (test #'eql) (key #'identity))
  "Return lists of duplicates"
     (when list
       (let (matches (item (first list)))
         (dolist (elt (rest list))
           (when (funcall test (funcall key item) (funcall key elt))
             (push elt matches)))
         (if matches
             (cons (cons (first list) matches)
               (find-duplicates (set-difference (rest list) matches)
                                :test test :key key))
             (find-duplicates (rest list) :test test :key key)))))

(defun any-duplicated (list &key (test #'eql) (key #'identity))
  "Returns T if any elements of <list> are duplicated."
  (not (eql (length list)
	    (length (remove-duplicates list :test test :key key)))))

(defun rotate (list n)
  (let ((n (mod n (length list))))
    (append (nthcdr n list) (subseq list 0 n))))

(defun permutations (list)
  (if (null list) 
      (list nil)
      (mapcan #'(lambda (x)
                  (mapcar #'(lambda (y) 
                              (cons x y))
                          (permutations (remove x list :count 1)))) 
              list)))

(defun remove-nth (n list)
  "Removes the nth item from a list non-destructively."
  (remove-if #'(lambda (x) (declare (ignore x)) t)
	     list :start n :end (1+ n)))

(defun remove-by-position (list positions)
  (labels ((rbp (l i r)
             (cond ((null l)
                    (reverse r))
                   ((member i positions :test #'=)
                    (rbp (cdr l) (1+ i) r))
                   (t (rbp (cdr l) (1+ i) (cons (car l) r))))))
    (rbp list 0 nil)))

(defun all-positions-if (predicate list)
  "Returns positions of all elements of <list> that satisfy <predicate>."
  (let ((result nil))
    (dotimes (i (length list))
      (if (funcall predicate (nth i list))
          (push i result)))
    (nreverse result)))

(defun insert-after (list position item)
  "Destructively inserts <item> into the index <position> in <list>,
   shifting all later elements forward by one position."
  (push item (cdr (nthcdr position list)))
  list)

(defun all-eql
    (list &key (predicate #'eql))
  "Tests whether all elements of a given <list> are 
   equal according to <predicate> (defaults to EQL).
   Also returns t if the list is empty."
  (if (< (length list) 2)
      t
      (if (not (funcall predicate
			(first list) (second list)))
	  nil
	  (all-eql (cdr list) :predicate predicate))))

;;;===========================================================================
;;; Assoc-lists
;;;===========================================================================

(defun update-alist (alist &rest new-entries)
  "Returns a version of <alist> updated with <new-entries> which must be 
   key-value pairs. If the value is nil then the pair is not added to the 
   alist unless the key is 'correct-onsets which is the only key in the
   environment allowed to have null values. Does not modify original list."
  (flet ((insert-entry (alist force new-entry)                           
           (cond ((and (null force) (null (cadr new-entry)))              
                  alist)
                 ((assoc (car new-entry) alist)
                  (substitute-if new-entry #'(lambda (key)
                                               (eql key (car new-entry)))
                                 alist :key #'car))
                 (t
                  (cons new-entry alist)))))
    (let* ((entry (car new-entries))
           (force (if (eql (car entry) 'correct-onsets) t nil)))
      (if (null entry)
          alist
          (apply #'update-alist (insert-entry alist force entry)
                 (cdr new-entries))))))

;;;===========================================================================
;;; Nested lists
;;;===========================================================================


(defun nposition (x xs) 
  "Index of first element equal to or containing x"
  (position-if #'(lambda (y) (nmember x y)) xs))

(defun npositions (x xs &key (index 0))
  "Indices of all elements equal to or containing x"
  (if (or (not (listp xs)) (null xs))
      nil
      (let ((posns (npositions x (cdr xs) :index (+ index 1))))
	(if (nmember x (car xs))
	    (cons index posns)
	    posns))))


(defun nmember (x xs)
  "Is x member of a nested list?"
  (cond ((not (listp xs)) (eq x xs))
	((null xs) nil)
	(t (or (nmember x (car xs))
	       (nmember x (cdr xs))))))

(defun nmin (xs)
  "Minimum of nested lists"
  (cond ((not (listp xs)) xs)
	((null xs) 0)
	((null (cdr xs)) (nmin (car xs)))
	(t (min (nmin (car xs)) (nmin (cdr xs))))))


(defun nmapcar (f xs)
  "Map function over nested lists"
  (cond ((not (listp xs)) (apply f (list xs)))
	((null xs) nil)
	(t (cons (nmapcar f (car xs))
		 (nmapcar f (cdr xs))))))

(defun nselectfirst (xs)
  "Pick first element of any list element"
  (flet ((pick1 (x) (if (listp x) (first x) x)))
    (mapcar #'pick1 xs)))


;;;===========================================================================
;;; Hash-tables
;;;===========================================================================

(defun hash-table->alist (hashtab)
  (let ((alist '()))
    (maphash #'(lambda (key value) (setq alist (cons (list key value) alist)))
             hashtab)
    alist))

(defun alist->hash-table (alist &key (test #'eql))
  (let ((hashtable (make-hash-table :test test)))
    (mapc #'(lambda (x) (setf (gethash (car x) hashtable) (cadr x)))
          alist)
    hashtable))

(defun hash-table->sorted-alist (ht sort-fn &optional &key (by :keys))
  (let ((sorted-entries nil))
    (maphash #'(lambda (k v) (push (cons k v) sorted-entries) (remhash k ht)) ht)
    (let ((sort-key (if (eql by :keys) #'car #'cdr)))
      (sort sorted-entries sort-fn :key sort-key))))

(defun csv->hash-table (path &key value-fun)
  "Reads csv file from <path> and uses it to create a hash-table which,
   for each line in the csv file, maps every field but the first field (keys)
   to the first field (values). Assumes that the csv file has no header.
   If a function <value-fun> is passed as an argument, then this function will 
   be applied to all values (not keys) before entry to the hash table."
  (let* ((lines (cl-csv:read-csv path))
	 (hash-table (make-hash-table :test #'equal)))
    (dolist (line lines hash-table)
      (let* ((value (car line))
	     (value (if (null value-fun)
			value
			(funcall value-fun value)))
	    (keys (cdr line)))
	(dolist (key keys)
	  (if (not (equal key ""))
	      (progn
		(if (nth-value 1 (gethash key hash-table))
		    (error (format nil "Attempted to add duplicate keys (~A) to hash table."
				   key)))
		(setf (gethash key hash-table) value))))))))


;;;===========================================================================
;;; File I/O 
;;;===========================================================================
                            
(defun write-object-to-file (object filename &optional (package :cl-user)
                             (fun #'prin1))
  (let ((gzipped-filename (add-file-suffix filename ".gz")))
    (if (probe-file gzipped-filename) (delete-file gzipped-filename))
    (with-open-file (s filename :direction :output :if-exists :overwrite
                       :if-does-not-exist :create)
      (with-standard-io-syntax
          (let ((*package* (find-package package)))
            (funcall fun object s))))
    (gzip filename)))
      
(defun read-object-from-file (filename &optional (package :cl-user))
  (let ((gzipped-filename (add-file-suffix filename ".gz")))
    (if (probe-file gzipped-filename)
        (read-object-from-gzipped-file gzipped-filename package)
        (if (probe-file filename) 
            (with-open-file (s filename :direction :input)
              (with-standard-io-syntax
                  (let ((*package* (find-package package)))
                    (read s nil))))
            (format t "~%~A does not exist." filename)))))

(defun file-exists (filename &key (suffix ".gz"))
  (or (probe-file filename)
      (probe-file (add-file-suffix filename suffix))))

(defun read-object-from-gzipped-file (filename &optional (package :cl-user))
  (let ((output-filename (remove-file-suffix filename)))
    (gunzip filename)
    (with-open-file (s output-filename :direction :input)
      (prog1
          (with-standard-io-syntax
              (let ((*package* (find-package package)))
                (read s nil)))
        (gzip output-filename)))))
       
(defun remove-file-suffix (filename &optional (suffix ".gz"))
  (reverse (subseq (reverse filename) (length suffix))))

(defun add-file-suffix (filename &optional (suffix ".gz"))
  (string-append filename suffix))

(defun gunzip (filename)
  #-win32 (shell-command "gunzip" (list filename)))

(defun gzip (filename)
  #-win32 (shell-command "gzip" (list filename)))

(defun md5-sum-of-list (list)
  "Returns the MD5SUM of the string representation of a list."
  (format nil "~{~X~}" (coerce (sb-md5:md5sum-string (format nil "~{~D,~}" list)) 'list)))

;; Taken from http://stackoverflow.com/questions/15796663/lisp-how-to-read-content-from-a-file-and-write-it-in-another-file
(defun copy-file (from-file to-file)
  "Copies a file from one location to another."
  (with-open-file (input-stream from-file
				:direction :input
				:element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
				   :direction :output
				   :if-exists :supersede
				   :if-does-not-exist :create
				   :element-type '(unsigned-byte 8))
      (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
    (loop for pos = (read-sequence buf input-stream)
       while (plusp pos)
       do (write-sequence buf output-stream :end pos))))))


;;;===========================================================================
;;; Pathnames
;;;===========================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'sb-posix) 
    (require 'sb-posix)))

(defun cd (&optional (dir (user-homedir-pathname)))
  (sb-posix:chdir dir)
  (let ((host (pathname-host dir))
        (name (pathname-name dir))
        (path (pathname-directory dir)))
    ;; allow dirs without ending delim "/tmp"
    (when name
      (setq path (append path (list name))))
    (setq *default-pathname-defaults*
          (make-pathname :host host :directory path))
    (namestring *default-pathname-defaults*)))

(defun pwd ()
  (namestring
   (make-pathname :host (pathname-host *default-pathname-defaults*)
                  :directory (pathname-directory
                              *default-pathname-defaults*))))

(defun ensure-directory (directory-name)
  (labels ((directory-p (pathname)
             (and (not (present-p (pathname-type pathname)))
                  (not (present-p (pathname-name pathname)))))
           (present-p (x)
             (and x (not (eq x :unspecific)))))
    (let ((pathname (pathname directory-name)))
      (if (not (directory-p directory-name))
          (make-pathname :directory (append (or (pathname-directory pathname) 
                                                (list :relative))
                                            (list (file-namestring pathname)))
                         :name nil
                         :type nil
                         :defaults pathname)
          pathname))))

(defun recursively-list-files (directory-name &key extensions (max-num-directories 100000))
  "Recursively lists all files present in <directory-name>, optionally filtered to 
  retain only files with extensions present in the list <extensions>.
  The number of directories to search is limited by <max-num-directories 100000>,
  which if exceeded causes an error to be thrown.
  Example usage: (recursively-list-files \"Users/\" :extensions '(\"krn\" \"jazz\"))"
  (labels ((directory-p (pathname)
             (and (not (present-p (pathname-type pathname)))
                  (not (present-p (pathname-name pathname)))))
           (present-p (x)
             (and x (not (eq x :unspecific))))
	   (fun (dirs-to-search files-found number-of-dirs-searched)
	     (if (null dirs-to-search) files-found
		 (let* ((new-dirs (remove-if-not
				   #'directory-p
				   (mapcan #'(lambda (dir) (directory (merge-pathnames
								       dir "*")))
						     dirs-to-search)))
			(new-files (mapcan #'uiop:directory-files dirs-to-search))
			(num-new-dirs (length new-dirs))
			;; (new-files-and-dirs (mapcan #'(lambda (dir)
			;; 				(directory dir))
			;; 			    dirs-to-search))
			;; (new-dirs (remove-if-not #'directory-p new-files-and-dirs))	
			;; (new-files (remove-if #'directory-p new-files-and-dirs))
			(new-files (if (null extensions) new-files
				       (remove-if-not #'(lambda (path)
							  (member (pathname-type path)
								  extensions
								  :test #'string=))
						      new-files))))
		   (if (> (+ num-new-dirs number-of-dirs-searched)
			  max-num-directories)
		       (error
			"Search did not terminate before the maximum number of directories were searched."))
		   (fun new-dirs (nconc new-files files-found) (+ num-new-dirs number-of-dirs-searched))))))
    (fun (list (ensure-directory directory-name)) nil 0)))


;;;===========================================================================
;;; Objects
;;;===========================================================================

(defun copy-instance (object &key (check-atomic nil))
  "Copies an instance <object>. Currently only supports the copying
of objects where all slots are atomic. If <check-atomic>, then
the function will check that all slots are atomic, and an error will
be thrown if any are not atomic; if not, no checks will be carried out,
and therefore non-atomic slots will still point to the data of the original
object." 
  (let* ((class (class-of object))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
      (when (slot-boundp object slot)
        (when (and check-atomic (not (atom (slot-value object slot))))
          (error "Tried to a copy a non-atomic slot using copy-instance."))
        (setf (slot-value copy slot) (slot-value object slot))))
    copy))

(Defun copy-slot-values (object1 object2)
  "Copy slot values from object1 to a copy of object2, wherever slot
names match, returning the copy of object2."
  (let* ((class (class-of object1))
         (copy2 (copy-instance object2)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
      (when (and (slot-boundp object1 slot) (slot-exists-p copy2 slot))
        (setf (slot-value copy2 slot) (slot-value object1 slot))))
    copy2))

(defun initialise-unbound-slots (object &optional value)
  (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of object))) object)
    (unless (slot-boundp object slot)
      (setf (slot-value object slot) value))))

;;;===========================================================================
;;; Portability 
;;;===========================================================================

(defun collect-garbage (&optional (full t))
  #+cmu (ext:gc :full full)
  #+sbcl (sb-ext:gc :full full)
  #+allegro (excl:gc full)
  )

(defun shell-command (command args) 
  #+cmu (ext:run-program command args)
  #+sbcl (sb-ext:run-program command args :search t)
  #+allegro (excl:run-shell-command 
             (apply #'string-append command 
                    (mapcar #'(lambda (x) (format nil " ~A" x)) args)))
  ) 
