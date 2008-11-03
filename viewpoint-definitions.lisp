(cl:in-package #:viewpoints)

;;; Basic Viewpoints 

(define-basic-viewpoint onset (events)
  (amuse:timepoint (last-element events)))

(define-basic-viewpoint cpitch (events)
  (amuse:midi-pitch-number (last-element events)))

(define-basic-viewpoint dur (events)
  (amuse:duration (last-element events)))

(define-basic-viewpoint keysig (events)
  (amuse:key-signature-sharps 
   (car (amuse:get-applicable-key-signatures 
         (last-element events)
         nil))))

(define-basic-viewpoint mode (events)
  (amuse:key-signature-mode 
   (car (amuse:get-applicable-key-signatures 
         (last-element events)
         nil))))
  
(define-basic-viewpoint tempo (events)
  (amuse:bpm 
   (car (amuse:get-applicable-tempi
         (last-element events)
         nil))))

(define-basic-viewpoint pulses (events)
  (amuse:beat-units-per-bar
   (car (amuse:get-applicable-time-signatures
         (last-element events)
         nil))))

(define-basic-viewpoint barlength (events)
  (let* ((last-element (last-element events))
         (timesig (car (amuse:get-applicable-time-signatures last-element nil)))
         (timebase (* (amuse:duration (amuse:crotchet last-element)) 4)))
    (/ (* (amuse:beat-units-per-bar timesig) timebase)
       (amuse:beat-units timesig))))

(define-basic-viewpoint deltast (events)
  (let* ((last-element (last-element events))
         (penultimate-element (penultimate-element events)))
    (cond ((and last-element penultimate-element)
           (- (amuse:timepoint last-element)
              (+ (amuse:timepoint penultimate-element)
                 (amuse:duration penultimate-element))))
          ((and last-element (null penultimate-element))
           0)
          (t nil))))

(define-basic-viewpoint bioi (events)
  (let* ((last-element (last-element events))
         (penultimate-element (penultimate-element events)))
    (cond ((and last-element penultimate-element)
           (- (amuse:timepoint last-element)
              (amuse:timepoint penultimate-element)))
          ((and last-element (null penultimate-element))
           0)
          (t nil))))

(define-basic-viewpoint phrase (events) 
  (let ((after  (amuse-segmentation:ground-truth-segmenter-after (car events)))
        (before (amuse-segmentation:ground-truth-segmenter-before (car events)))
        (last-element (last-element events)))
    (cond ((= 1 (amuse-segmentation:boundary-strength after last-element nil))
           1)
          ((= 1 (amuse-segmentation:boundary-strength before last-element nil))
           -1)
          (t 0))))

(define-basic-viewpoint mpitch (events) 
  (+ (amuse:diatonic-pitch-mp
      (last-element events))
     12))

(define-basic-viewpoint accidental (events)
  (amuse:diatonic-pitch-accidental 
   (amuse:diatonic-pitch 
    (last-element events))))

;; TODO: (define-basic-viewpoint dyn)
;; TODO: (define-basic-viewpoint voice) 

;;; Derived Viewpoints 

;; Onset 

(define-viewpoint (ioi derived (onset))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((onset1 (onset (list e1)))
                        (onset2 (onset (list e2))))
                    (cond ((undefined-p onset1 onset2) +undefined+)
                          (t (- onset2 onset1))))))
  :function* (list (+ element (onset (list (penultimate-element events))))))

(define-viewpoint (posinbar derived (onset))
    (events element) 
  :function (let ((onset (onset events))
                  (barlength (barlength events)))
              (cond ((undefined-p onset barlength) +undefined+)
                    ((zerop barlength) +undefined+)
                    ((zerop onset) 0)
                    ((> onset 0) (mod onset barlength))
                    (t +undefined+)))
  ;; TODO: function*
  )

(define-viewpoint (fib test (onset))
    (events element) 
  :function (let ((posinbar (posinbar events)))
              (cond ((undefined-p posinbar) +undefined+)
                    ((= posinbar 0) 1)
                    (t 0)))
  ;; TODO: function* 
  )

(define-viewpoint (crotchet test (onset))
    (events element) 
  :function (let ((e1 (car events))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((onset1 (onset (list e1)))
                        (onset2 (onset (list e2))))
                    (if (undefined-p onset1 onset2) +undefined+
                        ;;this only works if crotchet == 24 
                        (if (zerop (mod (- onset2 onset1) 24)) 1 0)))))
  ;; TODO: function* 
  )

(define-viewpoint (tactus test (onset))
    (events element) 
  :function (let ((event (last events)))
              (if (null event) +undefined+
                  (let ((barlength (barlength event))
                        (pulses (pulses event))
                        (onset (onset event)))
                    (declare (type fixnum barlength pulses))
                    (if (or (undefined-p barlength pulses onset)
                            (zerop barlength)
                            (zerop pulses))
                        +undefined+
                        (if (zerop (mod onset (/ barlength pulses))) 1 0)))))
  ;; TODO: function* 
  )

(define-viewpoint (ioi-ratio derived (onset))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((ioi1 (ioi (list e1 e2)))
                        (ioi2 (ioi (list e2 e3))))
                    (declare (type fixnum ioi1 ioi2))
                    (if (undefined-p ioi1 ioi2) +undefined+
                        (/ ioi2 ioi1)))))
  :function* (let ((penultimate-element (list (penultimate-element events))))
               (list (+ (onset penultimate-element) 
                        (* element (ioi penultimate-element))))))
                    

;; dur

(define-viewpoint (dur-ratio derived (dur))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((dur1 (dur (list e1)))
                        (dur2 (dur (list e2))))
                    (declare (type fixnum dur1 dur2))
                    (if (undefined-p dur1 dur2) +undefined+
                        (/ dur2 dur1)))))
  :function* (list (* element (dur (list (penultimate-element events))))))


;; Keysig

(define-viewpoint (referent derived (keysig))
    (events element) 
  :function (let ((keysig (keysig events))
                  (mode (mode events)))
              (declare (type (integer -7 7) keysig) (type (integer 0 11) mode))
              (if (undefined-p keysig mode) +undefined+
                  (cond ((> keysig 0)
                         (mod (+ (* keysig 7) mode) 12))
                        ((< keysig 0)
                         (mod (+ (* (- keysig) 5) mode) 12))
                        (t mode))))
  :function* (viewpoint-alphabet (get-viewpoint 'keysig)))


;; Phrase

(define-viewpoint (fiph test (phrase))
    (events element) 
  :function (let ((phrase (phrase events)))
              (cond ((undefined-p phrase) +undefined+)
                    ((= phrase 1) 1)
                    (t 0)))
  :function* (if (= element 1) 1 (list -1 0)))

(define-viewpoint (liph test (phrase))
    (events element) 
  :function (let ((phrase (phrase events)))
              (cond ((undefined-p phrase) +undefined+)
                    ((= phrase -1) 1)
                    (t 0)))
  :function* (if (= element 1) -1 (list 1 0)))

(define-viewpoint (lphrase derived (phrase))
    (events element) 
  :function (let ((e2 (last-element events)))
              (if (null e2) +undefined+
                  (let* ((phrase (phrase (list e2)))
                         (e1 (case phrase
                               (0 +undefined+)
                               (1 (last-element 
                                   (strip-until-true (get-viewpoint 'fiph)
                                                     (butlast events))))
                               (-1 (last-element 
                                    (strip-until-true (get-viewpoint 'liph)
                                                      (butlast events)))))))
                    (cond ((undefined-p e1) +undefined+)
                          ((null e1) 0)
                          (t (ioi (list e1 e2)))))))
  ;; TODO: function* 
  )


;; Chromatic Pitch 

(define-viewpoint (cpint derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpitch1 (cpitch (list e1)))
                        (cpitch2 (cpitch (list e2))))
                    (if (undefined-p cpitch1 cpitch2) +undefined+
                        (- cpitch2 cpitch1)))))
  :function* (list (+ element (cpitch (list (penultimate-element events))))))

(define-viewpoint (cpint-size derived (cpitch))
    (events element) 
  :function (let ((cpint (cpint events)))
              (cond ((undefined-p cpint) +undefined+)
                    (t (abs cpint))))
  :function* (let ((pitch (cpitch (list (penultimate-element events)))))
               (remove-if-not #'(lambda (a) 
                                  (or (= (+ pitch element) a)
                                      (= (- pitch element) a)))
                              (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (contour derived (cpitch))
    (events element) 
  :function (let ((cpint (cpint events)))
              (cond ((undefined-p cpint) +undefined+)
                    (t (signum cpint))))
  :function* (let ((pitch (cpitch (list (penultimate-element events)))))
               (remove-if #'(lambda (a) (case element
                                          (-1 (>= a pitch))
                                          (0  (not (= a pitch)))
                                          (1  (<= a pitch))))
                          (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (newcontour derived (cpitch))
    (events element) 
  :function (let ((contour2 (contour events))
                  (contour1 (contour (reverse (cdr (reverse events))))))
              (cond ((undefined-p contour2 contour1)
                     +undefined+)
                    ((= contour1 contour2) 1)
                    (t 0)))
  ;; TODO: function* 
  )

(define-viewpoint (cpitch-class derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events)))
              (cond ((undefined-p cpitch) +undefined+)
                    (t (mod cpitch 12))))
  :function* (remove-if-not #'(lambda (e) (= (mod e 12) element)) 
                            (viewpoint-alphabet (get-viewpoint 'cpitch))))

(define-viewpoint (cpcint derived (cpitch))
    (events element) 
  :function (let* ((cpint (cpint events)))
              (if (or (null cpint) (undefined-p cpint)) +undefined+ 
                  (if (minusp cpint) 
                      (- (mod (abs cpint) 12))
                      (mod cpint 12))))
  :function* (let ((pitch (cpitch (list (penultimate-element events)))))
               (remove-if-not #'(lambda (e) 
                                  (let* ((cpint (- e pitch))
                                         (cpint (if (minusp cpint) 
                                                    (- (mod (abs cpint) 12))
                                                    (mod cpint 12))))
                                    (= element cpint)))
                              (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (cpcint-size derived (cpitch))
    (events element) 
  :function (let ((cpcint (cpcint events)))
              (cond ((undefined-p cpcint) +undefined+)
                    (t (abs cpcint))))
  ;; TODO: function*
  )

(define-viewpoint (cpcint-2 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 2)))))
  ;; TODO: function* 
  )

(define-viewpoint (cpcint-3 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 3)))))
  ;; TODO: function* 
  )

(define-viewpoint (cpcint-4 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 4)))))
  ;; TODO: function* 
  )
  
(define-viewpoint (cpcint-5 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 5)))))
  ;; TODO: function* 
  )
  
(define-viewpoint (cpcint-6 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 6)))))
  ;; TODO: function* 
  )

(define-viewpoint (cpintfref derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events))
                  (referent (referent events)))
              (cond ((undefined-p cpitch referent) +undefined+)
                    (t (mod (- cpitch referent) 12))))
  :function* (let* ((referent (referent events))
                    (pitch (mod (+ referent element) 12)))
               (remove-if-not #'(lambda (e) (= (mod e 12) pitch))
                              (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (cpintfip derived (cpitch))
    (events element) 
  :function (if (< (length events) 2) +undefined+ 
                ;; (if (= (length events) 1) 0
                (let ((cpitch1 (cpitch (list (car events))))
                      (cpitch2 (cpitch events)))
                  (if (undefined-p cpitch2 cpitch1) +undefined+
                      (- cpitch2 cpitch1))))
  :function* (list (+ element (cpitch (list (car events))))))


(define-viewpoint (cpintfiph derived (cpitch))
    (events element) 
  :function (if (= (fiph events) 1) +undefined+ 
                (let ((e1 (strip-until-true (get-viewpoint 'fiph) events)))
                  (if (null e1) +undefined+
                      (let ((cpitch1 (cpitch e1))
                            (cpitch2 (cpitch events)))
                        (if (undefined-p cpitch2 cpitch1) +undefined+
                            (- cpitch2 cpitch1))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fiph) events)))
               (list (+ element (cpitch e)))))

(define-viewpoint (cpintfib derived (cpitch))
    (events element) 
  :function (if (= (fib events) 1) +undefined+ ;; 0
                (let ((e1 (strip-until-true (get-viewpoint 'fib) events)))
                  (if (null e1) +undefined+
                      (let ((cpitch1 (cpitch e1))
                            (cpitch2 (cpitch events)))
                        (if (undefined-p cpitch2 cpitch1) +undefined+
                            (- cpitch2 cpitch1))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fib) events)))
               (list (+ element (cpitch e)))))
  
(define-viewpoint (inscale derived (cpitch))
    (events element) 
  :function (let ((cpitch-class (cpitch-class events))
                  (referent (referent events))
                  (mode (mode events)))
              (cond ((undefined-p cpitch-class referent mode) +undefined+)
                    ((member cpitch-class (diatonic-set referent mode) 
                             :test #'=)
                     1)
                    (t 0)))
  :function* (let* ((referent (referent events)) 
                    (mode (mode events))
                    (ds (diatonic-set referent mode)))
               (remove-if #'(lambda (e) (case element 
                                          (0 (member (mod e 12) ds))
                                          (1 (not (member (mod e 12) ds)))))
                          (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(defun diatonic-set (referent mode)
  (let* ((diatonic-set '(0 2 4 5 7 9 11))
         (start (position mode diatonic-set)))
    (mapcar #'(lambda (x) (mod (+ (- x mode) referent) 12))
            (append (subseq diatonic-set start)
                    (subseq diatonic-set 0 start)))))

(define-viewpoint (thrbar threaded (cpitch onset))
    (events element) 
  :function (let ((e1 (last-element (strip-until-true (get-viewpoint 'fib)
                                                    (butlast events))))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((fib1 (fib (list e1)))
              (fib2 (fib (list e2))))
                    (if (or (zerop fib1) (zerop fib2))
                        +undefined+
                        (list (cpint (list e1 e2)) (ioi (list e1 e2)))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fib) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))

(define-viewpoint (thrfiph threaded (cpitch onset))
    (events element) 
  :function (let ((e1 (last-element (strip-until-true (get-viewpoint 'fiph)
                                                    (butlast events))))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((fiph1 (fiph (list e1)))
                        (fiph2 (fiph (list e2))))
                    (if (or (zerop fiph1) (zerop fiph2))
                        +undefined+
                        (list (cpint (list e1 e2)) (ioi (list e1 e2)))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fiph) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))
  
(define-viewpoint (thrliph threaded (cpitch onset))
    (events element) 
  :function (let ((e1 (last-element (strip-until-true (get-viewpoint 'liph)
                                                    (butlast events))))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((liph1 (liph (list e1)))
                        (liph2 (liph (list e2))))
                    (if (or (zerop liph1) (zerop liph2))
                        +undefined+
                        (list (cpint (list e1 e2)) (ioi (list e1 e2)))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'liph) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))

(define-viewpoint (thrintfrefliph threaded (cpitch onset))
    (events element) 
  :function (let ((e (last-element events)))
              (if (null e) +undefined+
                  (let ((liph (liph (list e))))
                    (if (zerop liph)
                        +undefined+
                        (cpintfref (list e))))))
  ;; TODO: function* 
  ) 

(define-viewpoint (thrqu threaded (cpitch onset))
    (events element) 
  :function (let* ((events-1 (strip-until-true (get-viewpoint 'crotchet)
                                               (butlast events)))
                   (e1 (last-element events-1))
                   (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((crotchet1 (crotchet events-1))
                        (crotchet2 (crotchet events)))
                    (if (or (zerop crotchet1) (zerop crotchet2))
                        +undefined+
                        (list (cpint (list e1 e2)) (ioi (list e1 e2)))))))
  ;; TODO: function*
  )

(define-viewpoint (thrtactus threaded (cpitch onset))
    (events element) 
  :function (let* ((events-1 (strip-until-true (get-viewpoint 'tactus)
                                               (butlast events)))
                   (e1 (last-element events-1))
                   (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((tactus1 (tactus events-1))
                        (tactus2 (tactus events)))
                    (if (or (zerop tactus1) (zerop tactus2))
                        +undefined+
                        (list (cpint (list e1 e2)) (ioi (list e1 e2)))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'tactus) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))

(define-viewpoint (octave derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events)))
              (cond ((undefined-p cpitch) +undefined+)
                    (t (floor cpitch 12))))
  ;; TODO: function* 
  )

(define-viewpoint (tessitura derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events)))    
              (cond ((undefined-p cpitch) +undefined+)
                    ((< cpitch 66) 0) ; from chorales 
                    ((> cpitch 74) 2) ; from chorales 
                    (t 1)))
  :function* (remove-if #'(lambda (e) 
                            (case element
                              (0 (>= e 66))
                              (1 (not (<= 66 e 74)))
                              (2 (<= e 74))))
                        (viewpoint-alphabet (get-viewpoint 'cpitch))))


;; IR Principles

(defun large-interval (interval) (> (abs interval) 6))
(defun small-interval (interval) (< (abs interval) 6))
(defun same-direction (int1 int2) (= (signum int1) (signum int2)))
(defun different-direction (int1 int2) (not (same-direction int1 int2)))

(define-viewpoint (registral-direction derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (cond ((undefined-p implicative realised) +undefined+)
                          ((large-interval implicative)
                           (if (same-direction implicative realised) 0 1))
                          ((small-interval implicative)
                           (if (same-direction implicative realised) 1 0))
                          (t +undefined+)))))
  ;; TODO: function*
  )

(define-viewpoint (intervallic-difference derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (if (undefined-p implicative realised) +undefined+
                        (let ((margin (if (same-direction implicative realised) 3 2)))
                          (cond ((large-interval implicative)
                                 (if (>= (abs realised) (- (abs implicative) margin))
                                     0 1))
                                ((small-interval implicative)
                                 (if (and (>= (abs realised)
                                              (- (abs implicative) margin))
                                          (<= (abs realised)
                                              (+ (abs implicative) margin)))
                                     1 0))
                                (t +undefined+)))))))
  ;; TODO: function*
  )

(define-viewpoint (registral-return derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((cpitch1 (cpitch (list e1)))
                        (cpitch3 (cpitch (list e3)))
                        (implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (cond ((undefined-p implicative realised) +undefined+)
                          ((different-direction realised implicative)
                           (cond ((or (zerop implicative) (zerop realised)) 0)
                                 ((and (<= cpitch3 (+ cpitch1 2))
                                       (>= cpitch3 (- cpitch1 2)))
                                  (- 3 (abs (- cpitch1 cpitch3))))
                                 (t 0)))
                          (t 0)))))
  ;; TODO: function*
  )
                        
(define-viewpoint (proximity derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((realised (cpint (list e1 e2))))
                    (if (undefined-p realised) +undefined+
                        (let ((proximity (- 6 (abs realised))))
                          (if (< proximity 0) 0 proximity))))))
  ;; TODO: function*
  )
  
(define-viewpoint (closure derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (if (undefined-p implicative realised) +undefined+
                        (let ((condition1 (different-direction 
                                           implicative realised))
                              (condition2 (< (abs realised) 
                                             (- (abs implicative) 2)))
                              (score 0))
                          (when condition1 (incf score))
                          (when condition2 (incf score))
                          score)))))
  ;; TODO: function*
  )
 
                    
;; Morphetic pitch

(define-viewpoint (mpitch-class derived (mpitch))
    (events element) 
  :function (let ((mpitch (mpitch events)))
              (cond ((undefined-p mpitch) +undefined+)
                    (t (mod mpitch 7))))
  :function* (remove-if-not #'(lambda (e) (= (mod e 12) element)) 
                            (viewpoint-alphabet (get-viewpoint 'mpitch))))

