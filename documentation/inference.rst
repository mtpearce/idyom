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

Latent-variables, in a probabilistic sense, are variables that form part of a generative model but are not directly observed. Latent-variables, as implemented in IDyOM, represent constructs that can be inferred from the musical surface.

Latent variables are are defined by a set of *category parameters*, and a set of *interpretation parameters*. Any instantiation of the the category parameters and optionally the interpretation parameters is called a *latent state*.

The category parameters define which generative model is to be used: for each latent state in which the category parameters are the same, the same model is to be used.
Together with the category parameters, interpretation parameters define the parameters of the distribution that is to be inferred from the musical surface.


For instance, categories of the latent variable *metre*, are defined by the attributes :barlength and :pulses, while interpretations are defined by :phase. Interpretations are always generated after categories in the generative process. That is, an interpretation is never interpreted without a category.

The set of interpretation of parameters and the set of category parameters should be disjoint.

Abstract viewpoints
-------------------

Abstract viewpoints that abstract away one or more attributes of the basic event representation.
Abstract viewpoints are defined by a set of event-attributes and interpretation parameters. The event attributes 

Generative multiple viewpoint systems
-------------------------------------
