See example models for some examples.

Models can be constructed by constructing a feature set with FEATURELET and making a model with MAKE-GRAPH.

The generative models package provides a number of interfaces for working with the models.
Individual sequences can be modeled with MODEL-SEQUENCE.
MODEL-SEQUENCE ensures that the graph state, model states and writers are updated appropriately.
MODEl-DATASET can be used to model sets of sequences; it simply calls MODEL-SEQUENCE on each sequence.

The result of MODEL-SEQUENCE is a VALUES list consisting of PLAUSIBLE-STATES, MODEL-LOCATIONS, and EVIDENCE.

    PLAUSIBLE-STATES is list of plausible states that remain after observing the last event.
    Each plausible state is a list containing BRANCH-IDS, ELEMENTS, and PROBABILITY.
        PROBABILITY is the probability of the state in the joint distribution.
        ELEMENTS is a HASH-TABLE containing the values of all features active in the current model-state.
        BRANCH-IDS is a HASH-TABLE containing a key into MODEL-LOCATIONS for each active modeled feature.active 


    MODEL-LOCATIONS is a HASH-TABLE that stores for each unique path and each active modeled feature a CONS whose CAR is a unique path through state space and whose CDR is a location object.

From the output of MODEL-SEQUENCE, only the total evidence, plausible states and unique paths through state space can be retrieved.
The function TRACE-BACK can be used as a shortcut for retrieving the unique path through state space associated with a plausible state.

Features that do not have higher order models associated with them are not visible in this output.
There are several options for obtaining more detailed output.
Most importantly, WRITERs.

A set WRITER objects may be passed to MODEL-SEQUENCE.
WRITER objects accumulate information that is generated while processing an individual event.
After some kind of processing (for example, calculating marginals or posteriors), the accumulated information is written to a given destination in csv format.
MODEL-SEQUENCE will call NEXT-SEQUENCE on the writer after each sequence, and NEXT-EVENT after each event.
During each event, SET-HORIZONTAL-WRITER-STATE is called for each (prior) plausible state.
Between calls to SET-HORIZONTAL-WRITER-STATE, ADD-POSSIBLE-STATE is called for each possible state along with an indicator of plausibility.
Finally, before the call to NEXT-EVENT, WRITE-EVENT is called which writes csv rows to (DESTINATION WRITER) pertaining to the current event.

A number of predefined writers are supplied: MARGINAL-WRITER outputs a marginal distribution over a given set of features, SEQUENCE-INDEX-WRITER outputs only the sequence index that was just processed, FULL-WRITER outputs all possible states along with an indication of plausibility.

TODO: posterior writer?


Features that have been marginalized out
