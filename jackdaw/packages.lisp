(cl:defpackage #:graphs 
  (:use #:common-lisp #:utils)
  (:export #:dag #:edge-function-dag #:edge-vector-graph
           #:order #:vertices #:edges #:roots #:leafs 
           #:edge-set #:ancestor-set #:depth #:distance #:topological-sort)
  (:documentation "Datatypes and methods for representing and reasoning about graphs."))

(cl:defpackage #:probabilities 
  (:use #:common-lisp #:utils)
  (:export #:*log-space* #:add #:mul #:div #:in #:out)
  (:documentation "Arithmic operations for numbers that may be represented logarithmically."))

(cl:defpackage #:marginals 
  (:use #:common-lisp #:utils)
  (:export #:make #:parameters #:probability #:update)
  (:documentation "Creating and evaluating discrete marginal probability distributions."))

(cl:defpackage #:features 
  (:use #:common-lisp #:utils)
  (:nicknames :f)
  (:export
    #:normal #:recursive 
    #:make-normal #:make-recursive 
    #:observable? #:modeled? #:representational? 
    #:identifier #:table #:model-accessor
    #:horizontal-args #:vertical-args #:horizontal-init-args #:vertical-init-args
    #:f #:init-function #:models
    #:add-model #:make-representational #:make-observable #:hide #:observe
    #:generate #:repeat-previous
    #:identifiers
    #:featurelet)
  (:documentation "Interface for defining probabilistic features."))

(cl:defpackage #:generative-models 
  (:use #:common-lisp #:utils)
  (:nicknames :gm)
  (:export #:*log-level* #:make-feature-graph
           #:model-dataset #:model-sequence #:model-event #:flush-cache
           #:possible-states #:trace-back)
  (:documentation "Interface for defining generative sequence models with probabilistic features."))
