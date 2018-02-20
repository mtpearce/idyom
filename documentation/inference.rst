Inference
=========

Using abstract viewpoints and annotated training data, it is possible to use idiom for inferring certain kinds of latent variables from a musical surface.
Examples of latent variables which might underly a musical surface are *meter*, *key*, or *style*. 
The inference functionality in IDyOM supports the specification of a simple generative model involving one or more latent variables.
When this model is used to predict new sequences of events, the generative model is used to infer probability distributions defined over the state space of the latent variables.
Predictions are made by marginalizing out the latent variables.

Two entities are key in the specification of the generative model: the latent variables themselves and *interpretation*. 
Interpretation here refers to transforming the musical surface into a representation that depends on one (or more) of the latent variables.
Interpretation is implemented by *abstract viewpoints*: viewpoints that are a function of both a sequence of events as well as the state of one or more latent variables.
Latent variables represent independent probabilistic variables.
A latent variable definition defines a state-space for the latent variable, as well as methods for calculating the prior distribution.

A distinction is made between *categories* and *interpretations*.
For some latent variables, inference relies on (or can be enhanced by) learning different predictive models for different categories.
For example, in order to figure out the style of a piece, different predictive models are learned for different styles.
Inference relies on checking which predictive model most effectively predicts the current sequence of events.
In inferring meter, such techniques may be of use too: learning separate predictive models for each meter helps inferring meter.
However, in order to interpret a rhythm in a meter, it is also necessary to know how the meter aligns wiht the rhythm.
Not all latent variables 

The type generative models that can be defined are restricted in several ways: it is assumed that each latent variable has a single value for each composition.
Each composition that is part of the training set or the test set must, for example, be in a single meter and key for meter and key inference to work optimally.

Latent-variables and abstract viewpoints can be customly defined using macros in the latent-variables and viewpoints modules. 
The sections below describe how this works. 

Latent-variables can be defined with a macro in the latent-variables module.
Depending on the complexity of the custom latent variable, some methods may need to be specialized on its type.
The viewpoints module provides a macro for defining abstract viewpoints.
There are some constraints on the kind of generative models that can be specified.
Each latent variable is assumed to only have a single value for each piece.
Examples of such variables are key and meter for a repertory in which key and meter do not change.

Latent variables
----------------

Latent-variables, in a probabilistic sense, are variables that form part of a generative model but are not directly observable.
Latent-variables, as implemented in IDyOM, represent event attributes that can be inferred from the musical surface, represented by compositions consisting of sequences of events.

Latent variables are are defined by a set of *category parameters*, and a set of *interpretation parameters*.
Any instantiation of the category and interpretation parameters is called a *latent state*.
During inference, a posterior is derived over the latent states of each participating latent variables.

Latent variables may be defined either without category parameters or without interpretation parameters.
The effect defining a latent variable without interpretation parameters is that each unique latent state will use a different predictive model.
Defining a latent variable without category parameters will ensure that the same predictive model is used for each latent state, inference must rely entirely on the effects of interpretation through abstract viewpoints.

Category parameters are typically event attributes that will be hidden during inference.
Interpretation parameters may or may not be event attributes.
The set of interpretation of parameters and the set of category parameters should be disjoint.

During inference, a prior distribution will be estimated for each latent variable.
The prior distributions are based on a set of training items labelled with categories.
The parameters of the prior distribution correspond to the latent states of the latent variable.
By default, prior distributions are estimated by counting the relative frequency of each category in the training data, but custom behaviors may be defined by writing specialized GET-PRIOR-DISTRIBUTION methods.

Latent variables may be *linked* together, which means that their posterior distribution will be inferred jointly.
Linked latent variables are represented by their own class, on which methods may specialize.
A linked latent variable consists of a number of constituent links, which are simply the latent-variables that it links together.
A linked latent variable derives its category, interpretation, and latent-state parameters from its constituent links by concatenating their parameters and sorting them alphabetically.

The prior distribution of a linked latent variable is calculated by calculating the joint distribution of its constituent links.
By default, independence is assumed among these prior distribution, hence the paramaters of the joint distribution are derived from the cartesian product of the parameters of the constituent prior distributions.

During inference, any combination of latent variables and linked latent variables may be provided to the inference engine to be inferred from the musical surface.
It may be the case that latent variables become probabilistically dependent through linking. 
For example, latent variables A and B are linked and latent variables B and C are linked and both are specified to be inferred for prediction, A, B, and C will become probabilistically dependent the system will infer a joint distribution over a single linked latent variable consisting of constituents A, B, and C.

Abstract viewpoints
-------------------

Abstract viewpoints abstract away one or more attributes of the basic event representation.
They are defined as functions of both a sequence of events and the *latent-variable-state*.
The latent variable state is represented as a dynamic variable, \*LATENT-VARIABLE-STATE\*, which stores the current latent state of one or more latent variables.

The macro DEFINE-ABSTRACT-VIEWPOINT can be used to define abstract viewpoints.
Defining an abstract viewpoint is similar to defining derived viewpoints: a name, typeset, and event-type specializer are needed.
In addition, a set of *event attributes* and *parameters* is needed.
The event attributes specify which event attributes the viewpoint abstracts away from.
The attributes in this list can only be actual attributes of the chosen event type.
The parameters can be any other parameters that may be provided by latent-variable states.

The DEFINE-ABSTRACT-VIEWPOINT macro simply creates a normal viewpoint with the DEFINE-VIEWPOINT macro, but the function that is passed to this macro is wrapped in a function which extracts the values of the event attributes and remaining parameters from the current latent variable state, and passes the event attribute parameters as positional arguments and the additional parameters as keyword arguments to the viewpoint function.
Hence, the viewpoint function should be defined as a higher-order function which takes the event-attribute parameters followed by the additional parameters as positional arguments in the order in which they are given to the DEFINE-ABSTRACT-VIEWPOINT macro and returns a regular viewpoint function.
For the additional parameters, a default value should be specified which is used when the viewpoint is used in the training phase (see below).
For example, an abstract viewpoint function that performs metrical interpretation takes as arguments a metrical interpretation, say 6/8, and returns a function that interprets event sequences in 6/8.

Both latent variables and abstract viewpoints are defined by two disjoint sets of parameters.
In the case of latent variables, these are called category parameters and interpretation parameters, while in the case of abstract viewpoints they are called event attributes and parameters.
There are subtle differences between these pairs of parameter sets.
While category parameters are typically actual event attributes, they are not required to be. 
The event attributes of abstract viewpoints on the other hand necessarily need to be event attributes.
The parameters are any remaining parameters that are required for interpretation, but are not event attributes (for example the phase of a metrical interpretation).
The *raison d'etre* of these remaining parameters (such as phase of a metre), is that certain aspects that one may want to infer are implicitly encoded in the event representation.
The absolute onset times in the MELODY representation, for example, implicitly encode information about the possible presence of an anacrusis since they are defined such that time 0 corresponds to the downbeat of the first bar.
This distinction---wile it may seem a bit confusing---has a technical reason which is related to the automatic generation of *training viewpoints*.
When an abstract viewpoint is defined, a training viewpoint (whose name should be provided to the DEFINE-ABSTRACT-VIEWPOINT macro) is automatically created.
A training viewpoint is used to train the predictive model for an abstract viewpoint.
This can be done simply by applying the training viewpoint to a set of training sequences like one would with any normal viewpoint (although the training sequences should be limited to a specific category for which a predictive model is being trained) and learning a predictive model from the resulting sequences.
The training viewpoint calls the same function that is used by the abstract viewpoint, but rather than sourcing the values of the event attributes from the latent state, they are sourced directly from the event representation.
Since the remaining parameters represent features that are implicitly encoded in the event representation, they should have a default value that can be assumed during training.

Generative multiple viewpoint systems
-------------------------------------

Three additional classes of multiple viewpoint systems are defined to support inference.
The most central of these is the abstract multiple viewpoint system, ABSTRACT-MVS.
With some exceptions, an abstract multiple viewpoint system appears to to other functions and methods to behave exactly like a normal multiple viewpoint, but its behavior is dependent on the current \*LATENT-VARIABLE-STATE\*.

During inference, latent variables provided to be inferred for predictions are grouped together into independent *generative systems*. 
A generative system is a group of latent variables whose posterior distribution needs to be inferred jointly.
For example, if we specify latent variables $(A B)$, $C$, $(A C)$ and $D$ to be inferred (where variables grouped by brackets are linked together to be inferred jointly), two independent generative systems will be created:
One will jointly infer latent variables $A$, $B$ and $C$ by linking latent variables $A$, $B$, and $C$ together.
Another will infer latent variable $D$.
The user could of course have anticipated this transformation and have specified $(A B C)$ and $D$ to be inferred, but the system can take care of this reasoning as well.

ABSTRACT-MVS is initialized with a short-term model, a long-term model, a list of basic viewpoints, a list of viewpoints, a (possibly linked) latent variable and individual latent variables, one for each viewpoint.
ABSTRACT-MVS should be initialized with the MAKE-MVS function, which takes care of initializing its fields properly.
While a normal mvs stores long- and short-term models as a VECTOR of PPM models, one for each viewpoint, an abstract mvs needs to store considerably more models: per viewpoint, one model for each category needs to be stored.
This models are stored in the mvs-ltm and mvs-stm class slots, but instead of VECTORs, these slots hold hash tables where each model can be accessed by its latent-variable attribute and category.

An abstract mvs achieves its dependence on the latent state by overriding the MVS-LTM and MVS-STM slot acccessor methods.
The LTM and STM accessor methods return a vector of models, with one model for each viewpoint of the mvs.
However, in an abstract mvs, each viewpoint is associated with a latent variable.
Which models are returned depends on the latent category of each latent variable as encoded in the current latent state.
A generative system with latent variables

Prediction 

Inference and prediction
------------------------

Latent variable inference is fully integrated into the IDyOM top-level function.
In order to use it, 





