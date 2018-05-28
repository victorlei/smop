
Installation guide
==================

The preferred option is installation from
the source using pip, because pip takes care
of everything -- finding the dowinload site,
installing the dependences (numpy and others), etc.
If you don't have pip, this is the perfect time
to install it::

    sudo apt-get install python-pip
    pip -V

On the flip side, pip is unexpectedly buggy.
So if you get weird messages from pip (such as
"missing version module", or "No module named
pip._internal"), your first guess is to reinstall
pip and pip3 as shown below.

The immediate suspect is the inconsistent
installation of python2, python3, and of the tools.
Unless your working environment is such that you
can work with one specific version of everything
you should learn to use virtualenv::

    virtualenv smop
    source bin/activate

We are done with the virtual environment. Now
the build::

    pip install -e .

If you use the --user option, don't forget to
put your .local/bin into the search path (unix only).

Run smop::
========

    smop foo.m bar.m bzz.m
    smop -h
    smop | more

Force a reinstall of pip::
========================

    curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
    python3 get-pip.py --force-reinstall

