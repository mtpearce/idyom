Inference
=========

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
