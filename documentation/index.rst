.. IDyOM documentation master file, created by
   sphinx-quickstart on Thu Sep 28 19:57:12 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to IDyOM's documentation!
=================================

Information Dynamics Of Music or IDyOM (`Pearce, 2005`_): a framework for constructing multiple-viewpoint variable-order Markov models for predictive statistical modelling of musical structure (see `Conklin, 1990`_, `Conklin & Witten, 1995`_). The system generates a conditional probability distribution representing the estimated likelihood of each note in a melody, given the preceding musical context; it computes Shannon entropy as a measure of uncertainty about the next note and information content as a measure of the unexpectedness of the note that actually follows.

The IDyOM software is written in the programming language Common Lisp and is made available under the `GNU General Public License`_.

Downloads and documentation:

* For downloads, installation and usage, visit the `Wiki`_.
* Further scientific publications are available `here`_.

To cite the software in your research, please reference:

`Pearce, M. T. (2005). The Construction and Evaluation of Statistical Models of Melodic Structure in Music Perception and Composition. Doctoral Dissertation, Department of Computing, City University, London, UK.`_

I'd be interested to hear if you're using IDyOM in your research, so do drop me a note (marcus.pearce@qmul.ac.uk). If you would like to contribute to the development of IDyOM, please get in touch and see also the Development Roadmap for things currently in progress or planned.

| **Homepage:**
| http://webprojects.eecs.qmul.ac.uk/marcusp/

| **Subprojects:**
| IDyOM
   
.. _Pearce, 2005: http://webprojects.eecs.qmul.ac.uk/marcusp/papers/Pearce2005.pdf
.. _Pearce, M. T. (2005). The Construction and Evaluation of Statistical Models of Melodic Structure in Music Perception and Composition. Doctoral Dissertation, Department of Computing, City University, London, UK.: http://webprojects.eecs.qmul.ac.uk/marcusp/papers/Pearce2005.pdf
.. _Conklin, 1990: http://prism.ucalgary.ca/bitstream/1880/46518/2/1990-390-14.pdf
.. _Conklin & Witten, 1995: http://www.ehu.eus/cs-ikerbasque/conklin/papers/jnmr95.pdf
.. _GNU General Public License: https://code.soundsoftware.ac.uk/projects/idyom/repository/entry/COPYING
.. _here: https://code.soundsoftware.ac.uk/projects/idyom-project/wiki
.. _Wiki: https://code.soundsoftware.ac.uk/projects/idyom-project/wiki

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   getting-started
   database
   viewpoints
   prediction
   inference
   api_reference

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
