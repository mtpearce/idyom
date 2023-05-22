;;;; ======================================================================
;;;; File:       preview.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2023-05-22 11:01:51 marcusp>
;;;; Time-stamp: <2023-05-22 12:29:26 marcusp>
;;;; ======================================================================

(cl:in-package #:idyom-db)

;; Paths to the user's timidy, musescore and pdf viewer executables
(defparameter *timidity-path* "/usr/local/bin/timidity")
(defparameter *musescore-path* "/Applications/MuseScore 4.app/Contents/MacOS/mscore")
(defparameter *pdf-viewer-path* "/Applications/Preview.app/Contents/MacOS/Preview")

(defmethod preview ((d idyom-db:mtp-dataset) &key (temp-dir "/tmp/idyom/")
					       (play-audio t) (display-score t))
  (let* ((compositions (dataset-compositions d))
	 (num-compositions (length compositions))
	 (counter 0))
    (utils:message (format nil "Previewing dataset (~A composition(s))."
			   num-compositions) :detail 1)
    (dolist (composition compositions)
      (utils:message (format nil "(~A/~A)" (incf counter) num-compositions)
		     :detail 1)
      (if (preview composition :temp-dir temp-dir
		   :play-audio play-audio :display-score display-score)
	  (return)))))

(defmethod preview ((c idyom-db:mtp-composition) &key (temp-dir "/tmp/idyom/")
						   (play-audio t) (display-score t))
  (utils:message (format nil "Previewing composition ~A."
			 (composition-description c))
		 :detail 1)
  (if (or play-audio display-score)
      (let* ((dir-path (ensure-directories-exist (utils:ensure-directory temp-dir)))
	     (midi-file-path (db2midi::export-data c :mid dir-path :filename "temp-audio.mid"))
	     (midi-file-path-string (namestring midi-file-path))
	     (pdf-file-path (merge-pathnames dir-path
					     (concatenate 'string "temp-score-"
							  (write-to-string
							   (get-internal-real-time))
							  ".pdf")))
	     (audio-process nil))
	(if display-score
	    (midi->pdf midi-file-path pdf-file-path :open-viewer t))
	(if play-audio
	    (setf audio-process (sb-ext:run-program *timidity-path*
						    (list midi-file-path-string)
						    :wait nil)))
	(utils:message
	 (format nil "Press enter to continue, or Q then enter to quit.~%")
	 :detail 1)
	(let ((char (read-char)))
	  (if (and (not (null audio-process))
		   (eql (sb-ext:process-status audio-process) :running))
	      (sb-ext:process-kill audio-process 15))
	  (if (eql char #\q) t nil)))))

(defun midi->pdf (input-file output-file &key open-viewer)
  (sb-ext:run-program *musescore-path*
		      (list (namestring input-file)
			    "-o" (namestring (ensure-directories-exist
					      output-file))))
  (if open-viewer
      (uiop:run-program (concatenate 'string "open "
					   (namestring output-file))))
  (namestring output-file))
