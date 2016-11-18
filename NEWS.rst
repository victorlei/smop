=============
Release notes
=============
November 18, 2016
    Version 0.31 is out

    Changed naming of output files.
        If -o not specified, compiling foo.m
	now results in foo.py

    Using octave script library for testing
        $ wget ftp://ftp.gnu.org/ftp/octave/octave-4.0.2.tar.gz
	$ smop -a octave-4.0.2.tar.gz -g '*/scripts/*.m'
	$ ls -1 *.py | wc
	$ python -m py_compile *.py
	$ ls -1 *.pyc | wc

    New option -a --archive allows to get data from
        tar file, without extracting the contents
	to a temporary place.

    New option -g --glob-pattern allows to limit
        the amount of input data

    Removed option --ignore-errors.
        All errors are ignored now.

    
August 27, 2016
    Version 0.29 is out

    Test suite for regression testing is ready,
        with about 1000 m-scripts.  Scripts are translated
        to python, then the resulting py-files are loaded,
        but not yet run.
    
August 11, 2016
    Version 0.28 is out. 

    Line numbering information is included in the output.
        Enabled by default.  Disabled with ``--no-numbers``.

    Block comments are preserved now.
        Lines, containing anything but a comment, are
        preserved. Enabled by default.
        Disabled with ``--no-comments``.

    New command-line options
        ``--no-resolve,--no-backend`` useful mostly for
        debugging. New shortcuts -R -B -T -C -N
        
    Special ``%!`` comments are partially supported now.
        They are mostly useful in testing, and
        are used by Octave library and test suite. Disabled
        by default. Enabled with ``--testing-mode``.
        

June 19,2016
   After a year-long vacation I am back to active development.
   My first goal is adopting the Octave runtime and test suite.

October 23, 2014
   Downloaded ``mybench`` -- a collection of 20 or so
   micro-benchmarks originally meant to compare matlab and
   octave performance.  After succesfully running the first nine,
   the geometric mean of the speedup is 0.36,  which is cool.

    ==   ========   ======    ===========    =======
    //   name       octave    smop           speedup
    ==   ========   ======    ===========    =======
    1    rand       2.58      0.36           0.14
    2    randn      2.26      1.04           0.46
    3    primes     0.35      0.17           0.49
    4    fft2       2.75      1.13           0.41
    5    square     4.24      0              
    6    inv        4.38      2.26           0.53
    7    eig        17.95     9.09           0.51
    8    qr         3.06      1.83           0.60
    9    shur       5.98      2.31           0.39
    10   roots      8.31      2.02           0.24
    ==   ========   ======    ===========    =======

October 15, 2014
   Version 0.26.3 is available for beta testing.
   Next version 0.27 is planned to compile octave
   ``scripts`` library, which contains over 120 KLOC in
   almost 1,000 matlab files. There  are 13 compilation
   errors with smop 0.26.3 .

