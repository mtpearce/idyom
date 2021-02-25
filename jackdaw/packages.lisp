(cl:defpackage #:jackdaw
  (:use #:common-lisp)
  (:export #:get-or-create-and-set
	   #:*log-level* #:cartesian-product
	   #:debugm #:logm #:infom
	   #:symbol-index-map #:dictionary
	   #:set-csv-field #:copy-hash-table)
  (:documentation "Utility functions."))

(cl:defpackage #:graphs 
  (:use #:common-lisp)
  (:export #:dag #:edge-function-dag #:edge-vector-graph
           #:order #:vertices #:edges #:roots #:leafs 
           #:edge-set #:ancestor-set #:depth #:distance #:topological-sort)
  (:documentation "Datatypes and methods for representing and reasoning about graphs."))

(cl:defpackage #:probabilities 
  (:use #:common-lisp)
  (:nicknames :probs)
  (:export #:*log-space* #:add #:mul #:div #:in #:out #:logarithm #:entropy)
  (:documentation "Arithmic operations for numbers that may be represented logarithmically."))

(cl:defpackage #:marginals 
  (:use #:common-lisp)
  (:export #:make #:parameters #:probability #:update #:marginal-parameter)
  (:documentation "Creating and evaluating discrete marginal probability distributions."))

(cl:defpackage #:features 
  (:use #:common-lisp #:jackdaw)
  (:nicknames :f)
  (:export
    #:feature #:normal #:recursive 
    #:make-normal #:make-recursive 
    #:observed? #:modeled? #:observable? #:representational? 
    #:identifier #:table #:model-accessor #:model-arguments-accessor
    #:horizontal-args #:vertical-args #:horizontal-init-args #:vertical-init-args
    #:model-args #:all-arguments
    #:f #:init-function #:models #:observation-function
    #:init-observation-function
    #:set-model #:make-representational #:hide #:observe
    #:get-model
    #:generate #:repeat-previous
    #:identifiers
    #:featurelet)
  (:documentation "Interface for defining probabilistic features."))

(cl:defpackage #:models
  (:use #:common-lisp #:jackdaw)
  (:export #:sequence-model #:ppm #:zeroth-order #:zeroth-order-once
	   #:next-location #:next-sequence #:last-event #:distribution
	   #:root-location
	   #:idyom-ppm #:zeroth-order #:zeroth-order-once #:phase-model)
  (:documentation "Provides probabilistic models that can be linked to features."))

(cl:defpackage #:generative-models 
  (:use #:common-lisp #:jackdaw)
  (:nicknames :gm)
  (:export #:*log-level* #:make-feature-graph
           #:model-dataset #:model-sequence #:model-event #:flush-cache
           #:possible-states #:trace-back
	   #:n)
  (:documentation "Interface for defining generative sequence models with probabilistic features."))

(cl:defpackage #:commands
  (:use :cl)
  (:import-from :unix-options
                :&parameters
                :&free
                :with-cli-options))
