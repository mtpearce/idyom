Annotated examples generative modelling
=======================================

Features
-------------------

.. cl:package:: features

Classes
~~~~~~~

.. cl:type:: feature

.. cl:type:: normal

.. cl:type:: recursive

Creating features
~~~~~~~~~~~~~~~~~

FEATURE-instances are created with MAKE-NORMAL-FEATURE and MAKE-RECURSIVE-FEATURE. The cleanest way of defining a set of features is with the FEATURELET macro, but it is useful to understand the argument structure to MAKE-NORMAL-FEATURE and MAKE-RECURSIVE-FEATURE first.

.. cl:function:: make-normal

.. cl:function:: make-recursive

FEATURELET takes care of assigning feature-ids by using the variable names used when defining the feature. 

.. cl:macro:: featurelet

Generative models
-----------------

.. cl:package:: generative-models

Creating generative models
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. cl:function:: make-feature-graph


Annotated examples
------------------

One generative feature
^^^^^^^^^^^^^^^^^^^^^^

The example below defines a feature that takes no arguments and generates two symbols, defines a graphical model with MAKE-FEATURE-GRAPH, and generates the possible hidden states of the model after three events with MODEL-SEQUENCE.

::

    ;; f is the nickname for the features package
    (f:featurelet
        ;; Define a 'normal' (not recursive) feature 
        ;; with no horizontal and no vertical arguments.
        ((f (normal () ()
            ;; Below is the feature's function, which 
            ;; returms the list of "possible values" the
            ;; feature can assume given its horizontal
            ;; and vertical arguments. Since this feature 
            ;; takes no arguments it simply returns a list.
            '(a b))))
      ;; Add a sequential (ppm) model to f
      (f:add-model f)
      ;; Construct a 'feature-graph' containing just f.
      (let ((graph (gm:make-feature-graph f)))
        (multiple-value-bind (hidden-states evidence)
        ;; Model '(- - -) with graph.
        ;; No features are observed, so the contents of
        ;; the events are irrelevant.
        (gm:model-sequence graph '(- - -))
          ;; Report evidence
          ;; Probabilities may be logarithmic, depending
          ;; on whether probabilities:*log-space*
          ;; is true. probabilities:out converts them to
          ;; normal probabilities.
          (format t "~A~%" (probabilities:out evidence))
          ;; Iterate over hidden states remaining after 
          ;; observing events
          (dolist (state hidden-states)
        ;; Trace back reconstructs the values of 'f in the
        ;; parameter joint distribution parameter that a
        ;; hidden-state corresponds to.
        (format t "~A~%" (reverse (gm:trace-back state 'f)))))))

F can be made observable with MAKE-OBSERVABLE. A observation-function can be supplied that takes an event as its argument and returns the feature's observed value.

::

    (f:featurelet
        ((f (normal () () '(a b))))
      (f:add-model f)
      (f:make-observable f #'identity)
      (let ((graph (gm:make-feature-graph f)))
        (multiple-value-bind (hidden-states evidence)
        (gm:model-sequence graph '(a b b))
          (format t "~A~%" (probabilities:out evidence))
          (dolist (state hidden-states)
        (format t "~A~%" (reverse (gm:trace-back state 'f)))))))

Only one plausible hidden state remains. Notice that the evidence of the observation corresponds to the probability of the corresponding state of the joint distribution.

Two features
^^^^^^^^^^^^

::

    (f:featurelet 
        ((f (normal () () '(a b)))
         ;; f-eq has one *horizontal* argument f, refered to by 
         ;; 'previous-f' in its function and one *vertical*
         ;; argument, f.
         (f-eq (normal (f) (f) (when (eq previous-f f) (list 'e)))))
      (f:add-model f) (f:add-model f-eq)
      (let ((graph (gm:make-feature-graph f f-eq)))
        (multiple-value-bind (hidden-states)
        (gm:model-sequence graph '(0 1 2))
          (dolist (state hidden-states)
        ;; Now trace back the values of f and f-eq
        (format t "~A~%" (reverse (gm:trace-back state 'f 'f-eq)))))))

Notice that F-EQ is undefined in the first event.

Recursive features
^^^^^^^^^^^^^^^^^^

::

    (f:featurelet
        ;; F is defined with zero horizontal and zero vertical
        ;; arguments
        ;; however, it receives an implicit 
        ((f (recursive () ()
               (list (1+ previous-f))
               ;; The above part is the same as a normal feature's
               ;; definition.  The part below defines the function
               ;; that calculates the first value, the initialisation
               ;; function. 
               ;; Init function takes no arguments.
               () ()
               ;; Return a singleton containing zero.
               '(0))))
      (f:add-model f)
      (let ((graph (gm:make-feature-graph f)))
        (multiple-value-bind (hidden-states)
        (gm:model-sequence graph '(- - -))
          (dolist (state hidden-states)
        (format t "~A~%" (reverse (gm:trace-back state 'f)))))))

Easy enough? Here's a more complicated example.

::

    (f:featurelet 
        ((f (recursive () (f-eq) 
               (case f-eq 
                 (yes (list previous-f))
                 (no (case previous-f (a '(b)) (b '(a)))) 
                 (maybe '(a b)))
               () () '(a b)))
         (f-eq (normal () () '(yes no maybe))))
      (f:make-observable f-eq) (f:add-model f)
      (let ((graph (gm:make-feature-graph f f-eq)))
        (multiple-value-bind (hidden-states)
        (gm:model-sequence graph '(yes maybe no))
          (dolist (state hidden-states)
        (format t "~A~%" (reverse (gm:trace-back state 'f 'f-eq)))))))
    
Experiment with feeding difference sequences of yes, no, maybe into MODEL-SEQUENCE to get a feel for what this does.

An example of a feature that implements a delta feature. 
Let's generate all possible sequences of three events that can be generated by this model.

::

    (let ((f-alphabet '(0 1 2)))
      (f:featurelet 
          ((df (normal (f) ()
               (loop for next in f-alphabet
                  collect (- next previous-f))))
           (f (recursive () (df)
                 (list (+ previous-f df))
                 nil nil
                 f-alphabet)))
        (f:add-model df) (f:add-model f)
        (let ((graph (gm:make-feature-graph f df)))
          (multiple-value-bind (hidden-states)
          (gm:model-sequence graph '(- - -))
        (dolist (state hidden-states)
          (format t "~A~%" (reverse (gm:trace-back state 'f 'df))))))))

This specifies the delta feature generatively, which seems a bit counter-intuitive.
The following much more readable system is equivalent, but breaks with the idea of generating observations from latent features.

::

    (f:featurelet
        ((df (normal (f) (f) (list (- previous-f f))))
         (f (normal () () basic-alphabet)))
      ...)

A key inference model
^^^^^^^^^^^^^^^^^^^^^

Here's an example of a model that infers the key signature and mode of a melody.

Integration with music data is achieved by using music-data accessors as observation functions.

::

    (defun key-model (dataset)
      (let* ((octave 12)
         (modes '(0 9))
         (scale-degrees (loop for sd below octave collect sd))
         (keysigs (loop for ks below octave collect (- ks 5)))
         (pitches (viewpoints::unique-elements
               (viewpoints:get-viewpoint 'cpitch) dataset)))
        (f:featurelet
        ((mode (recursive () () (list previous-mode)
                  () () modes))
         (keysig (recursive () () (list previous-keysig)
                    () () keysigs))
         (tonic (normal () (keysig mode)
                (list (if (> keysig 0)
                      (mod (+ (* keysig 7) mode) octave)
                      (mod (+ (* (- keysig) 5) mode) octave)))))
         (scale-degree (normal () (mode)
                       (mapcar (lambda (sd) (cons sd mode)) 
                               scale-degrees)))
         (pitch (normal () (tonic scale-degree)
                (loop for pitch in pitches
                   if (eq (car scale-degree)
                          (mod (- pitch tonic)
                           octave))
                   collect pitch))))
          (values mode keysig tonic scale-degree pitch))))


This function generates a model given a dataset. The dataset is used to set the aphabet of the pitch feature.

The function below will return a trained key model that can be used to experiment on.

::

    (defun trained-key-model (dataset-id)
      (let ((dataset (md:get-event-sequences (list dataset-id))))
        (multiple-value-bind (mode keysig tonic scale-degree pitch)
        (key-model dataset)
          (let ((graph (gm:make-feature-graph mode keysig
                 tonic scale-degree pitch)))
        (f:make-observable keysig #'md:key-signature)
        (f:make-observable mode #'md:mode)
        (f:make-observable pitch #'md:chromatic-pitch)
        (f:add-model mode)
        (f:add-model scale-degree)
        (gm:model-dataset graph dataset :construct? t)
        (f:hide keysig mode)
        (f:make-observable pitch #'identity)
        (gm:flush-cache graph)
        (multiple-value-bind (states evidence)
            (gm:model-sequence graph '(60 62 64 67) :predict? t)
          (dolist (state (sort states #'> :key #'fourth))
            (let ((state (first (gm:trace-back state 'keysig 'mode)))
              (p (fourth state)))
              (format t "P~A: ~A~%" state
                  (probabilities:out 
                   (probabilities:div p evidence))))))))))
        (defun key-model (dataset)
          (let* ((octave 12)
             (modes '(0 9))
             (scale-degrees (loop for sd below octave collect sd))
             (keysigs (loop for ks below octave collect (- ks 5)))
             (pitches (viewpoints::unique-elements
                   (viewpoints:get-viewpoint 'cpitch) dataset)))
            (f:featurelet
            ((mode (recursive () () (list previous-mode)
                      () () modes))
             (keysig (recursive () () (list previous-keysig)
                        () () keysigs))
             (tonic (normal () (keysig mode)
                    (list (if (> keysig 0)
                          (mod (+ (* keysig 7) mode) octave)
                          (mod (+ (* (- keysig) 5) mode) octave)))))
             (scale-degree (normal () (mode)
                           (mapcar (lambda (sd) (cons sd mode)) 
                                   scale-degrees)))
             (pitch (normal () (tonic scale-degree)
                    (loop for pitch in pitches
                       if (eq (car scale-degree)
                              (mod (- pitch tonic)
                               octave))
                       collect pitch))))
              (values mode keysig tonic scale-degree pitch))))

Training takes a while because all mechanisms involved in prediction are also active during training.
