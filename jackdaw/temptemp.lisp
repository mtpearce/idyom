
(:PHASES 2 :METER-DOMAIN ((2 2)) :MODEL
 (:PHASE
  (:MODEL-KEYS (((2 2))) :MODELS
   ((:TYPE MODELS::PPM-PHASE-MODEL :DATA
     (:CONDITIONING ((2 2)) :LEAVES
      ((2
        #S(PPM-STAR::LEAF-RECORD
           :LABEL #S(PPM-STAR::LABEL
                     :LEFT #S(PPM-STAR::INDEX :S 0 :E 2)
                     :LENGTH NIL)
           :BROTHER NIL
           :COUNT0 1
           :COUNT1 1))
       (1
        #S(PPM-STAR::LEAF-RECORD
           :LABEL #S(PPM-STAR::LABEL
                     :LEFT #S(PPM-STAR::INDEX :S 0 :E 2)
                     :LENGTH NIL)
           :BROTHER NIL
           :COUNT0 1
           :COUNT1 1))
       (0
        #S(PPM-STAR::LEAF-RECORD
           :LABEL #S(PPM-STAR::LABEL
                     :LEFT #S(PPM-STAR::INDEX :S 0 :E 1)
                     :LENGTH NIL)
           :BROTHER #S(PPM-STAR::LEAF :INDEX 1)
           :COUNT0 1
           :COUNT1 1)))
      :BRANCHES
      ((2
        #S(PPM-STAR::BRANCH-RECORD
           :LABEL #S(PPM-STAR::LABEL
                     :LEFT #S(PPM-STAR::INDEX :S 0 :E 0)
                     :LENGTH 1)
           :BROTHER #S(PPM-STAR::LEAF :INDEX 2)
           :CHILD #S(PPM-STAR::LEAF :INDEX 0)
           :SLINK #S(PPM-STAR::BRANCH :INDEX 1)
           :DEPTH 1
           :COUNT0 2
           :COUNT1 2))
       (1
        #S(PPM-STAR::BRANCH-RECORD
           :LABEL #S(PPM-STAR::LABEL
                     :LEFT #S(PPM-STAR::INDEX :S 0 :E 0)
                     :LENGTH 0)
           :BROTHER NIL
           :CHILD #S(PPM-STAR::BRANCH :INDEX 2)
           :SLINK #S(PPM-STAR::BRANCH :INDEX 0)
           :DEPTH 0
           :COUNT0 2
           :COUNT1 1))
       (0
        #S(PPM-STAR::BRANCH-RECORD
           :LABEL NIL
           :BROTHER NIL
           :CHILD #S(PPM-STAR::BRANCH :INDEX 1)
           :SLINK NIL
           :DEPTH -1
           :COUNT0 0
           :COUNT1 0)))
      :DATASET ((0 ((2 :$) (1 0) (0 0)))) :ALPHABET (0 1) :ORDER-BOUND NIL
      :MIXTURES T :ESCAPE :C :UPDATE-EXCLUSION NIL :PHASES 2))))
  :METER
  (:MODEL-KEYS (NIL) :MODELS
   ((:TYPE MODELS::CATEGORICAL-ONCE :DATA
     (:CONDITIONING NIL :LAPLACE-SMOOTHING? NIL :OBSERVATION-COUNT 1
      :SYMBOL-COUNTS (((2 2) 1)) :INIT-UNIFORM? T)))))) 