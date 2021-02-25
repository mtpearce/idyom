# Instructions for rendering the documentation locally

The documentation is generated using the python-based Sphinx project. To render documentation, you'll need python, [virtualenv](https://virtualenv.pypa.io/en/stable/), and [cl-launch](http://www.cliki.net/CL-Launch).

On ubuntu, this is a matter of

```bash
sudo apt-get install virtualenv cl-launch
```

To install the Sphinx and the common lisp extension, we'll first create a virtualenv. Navigate to a directory where you would like to store the virtualenv. It doesn't matter where that is but since a virtualenv is typically associated with a single project, I find it convenient to create a virtualenv in the top-level directory for that project. If you create a virtual env in idyom's top level directory with the name `env`, it will be ignored by git.

To create a virtualenv named env, run

```bash
virtualenv env
```

"Activate" the environment with: 

```bash
source env/bin/activate
```

You'll need to do this every time you open a new terminal session.

Navigate to the `documentation` folder in idyom. This folder contains a file `requirements.txt` that lists python projects to be installed by `pip`, a utility that installs things inside a virtualenv. Make sure the virtualenv is activated, and run

```bash
pip install -r requirements.txt
```

Still in the documentation folder, the file conf.py contains configuration settings for Sphinx. Sphinx has been configured to use the cl-domain and hyperspec (which converts every mention of a common lisp function or type into a link to the relevant hyperspec page) extensions. However, we still need to tell it where to find quicklisp and which lisp to use. Since these settings might vary from developer to developer, they are loaded from a separate file, `conf_local.py`. If on our particular machine quicklisp was located at `~/quicklisp` and we'd want to use SBCL, we'd create `conf_local.py` like so:

```bash
echo "from os.path import expandvars" > conf_local.py
echo "cl_quicklisp = expandvars('$home/quicklisp/')" >> conf_local.py
echo "cl_lisps = 'sbcl'" >> conf_local.py
```

## Tweaking cldomain

To obtain variable, macro, class, method, generic function and function definitions, cldomain will actually fire up the specified lisp implementation and load IDyOM. However, since IDyOM excepts a few global variables to set before it can be loaded, we'll need to tweak the code a bit to get it running with idyom. To do this, find the file cldomain.py in your virtualenv (it should be here: `env/src/sphinxcontrib-cldomain/sphinxcontrib/cldomain.py`). At the bottom of the file, in the function `cl_launch_args`, find the following bit of code:

```python
    quickload = """
(let ((*standard-output* *error-output*))
  (quicklisp:quickload '%s))
""" % package
```

and change it into:

```python
    quickload = """
(let ((*standard-output* *error-output*))
  (defvar *idyom-root* "/home/bastiaan/projects/idyom/")
  (defvar *idyom-code-root* "/home/bastiaan/projects/idyom/")
  (defvar *idyom-message-detail-level* 2)
  (push *idyom-root* asdf:*central-registry*)
  (quicklisp:quickload '%s))
""" % package
```

At this moment, there seems to be a bug in rendering warning messages when documenting methods. Rendering the warning message causes the build process to crash, even though the warning appears harmless. My crude solution is to simply not print the warning. In the function `find-method` of the class `CLDomain`, comment out the line:

```python
                env.warn_node('can\'t find method %s' % (name), node)
```

and perhaps replace it by a print statement

```python
                #env.warn_node('can\'t find method %s' % (name), node)
                print("can't find method %s" % name)
```

With these tweaks, everything seems to sort of work. Beware that the warning message for not finding a method specializer that you refer to in the documentation will also not work. Gotchas include having to add the package name with :: for symbols that are not external in the list of specializer arguments, and having to include common-lisp:t for all arguments that do not specialize on a specific class. See the example documentation for the viewpoints module at `documentation/api_reference.rst` for reference.

### Writing documentation

All files related to the documentation are located in `documentation/` in IDyOM's root directory. The documentation is written in `.md` (markdown) and `.rst` (reStructuredText) files placed directly in the documentation folder. The file `index.rst` represents the documentation front page. To generate html files, run

```bash
make html
```

from `documentation/`. The build will be placed in `documentation/_build/`.

Writing documentation is fairly straightforward. Have a peek at the [Sphinx tutorial](http://www.sphinx-doc.org/en/stable/tutorial.html), the [CLDomain documentation](http://cldomain.russellsim.org/) (a newer version seems to be hosted [here](http://40ants.com/cldomain/)), and the page sources for this documentation to get a feel for how everything works.

