``SMOP`` is Small Matlab and Octave to Python compiler.
    ``SMOP`` is used to translate to python legacy libraries
    containing algorithmic matlab code.  Despite the obvious
    similarities between matlab and numeric python, there
    are enough differences to make the manual translation of
    these libraries infeasible in real life.  ``SMOP``
    generates human-readable python, which also appears to
    be faster than octave.  How much faster?  Timing results
    for the "working example" below are summarized in
    Table 1. Additionally,
    compiling run-time library ``runtime.py``
    "as-is" with cython brings additional speedup.

        +-------------------------------------+--------+
        | octave-3.8.1                        | 190 ms |  
	+-------------------------------------+--------+
	| smop+python-2.7                     |  80 ms |
	+-------------------------------------+--------+
	| smop+cython-0.20.1                  |  40 ms |
	+-------------------------------------+--------+
        | Table 1. ``SMOP`` performance, measured on   |
        | fujitsu AH552 running linux 3.8.0-19         |
	+----------------------------------------------+

    There is a price, too. The generated sources are
    `matlabic`, rather than `pythonic`, which means that
    library maintainers must be fluent in both languages,
    and the old development environment must be kept around. 

    With less than five thousands lines of python code
    ``SMOP`` does not pretend to compete with such polished
    products as matlab or octave.  Yet, it is not a toy.
    There is an attempt to follow the original matlab
    semantics as close as possible.  Matlab language
    definition (never published afaik) is full of dark
    corners, and ``SMOP`` tries to follow matlab as
    precisely as possible.

Should the generated program be `pythonic` or `matlabic`? 
    For example should array indexing start with zero
    (`pythonic`) or with one (`matlabic`)?

    I beleive now that some matlabic accent is unavoidable
    in the generated python sources.  Imagine matlab program
    is using regular expressions, matlab style.  We are not
    going to translate them to python style, and that code
    will remain forever as a reminder of the program's
    matlab origin.

    Another example.  Matlab code opens a file; fopen
    returns -1 on error.  Pythonic code would raise
    exception, but we are not going to do `that`.   Instead,
    we will live with the accent, and smop takes this to the
    extreme --- the matlab program remains mostly unchanged.

    It turns out that generating `matlabic`` allows for
    moving much of the project complexity out of the
    compiler (which is already complicated enough) and into
    the runtime library, where there is almost no
    interaction between the library parts.

.. missing standard library and toolboxes
.. missing grapphics library

Working example: ``solver.m``
    We will translate ``solver.m`` to present a sample of
    smop features.  The program was borrowed from the
    matlab programming competition in 2004 (Moving
    Furniture). For the impatient, it is possible to compile
    and run the example without installing smop::

    $ tar zxvf smop-0.25.4.tar.gz
    $ cd smop-0.25.4/smop
    $ python main.py solver.m
    $ python go.py

To the left is ``solver.m``.  To the right is ``a.py`` --- its
translation to python.  Though only 30 lines long, this
example shows many of the complexities of converting matlab code
to python.

.. code:: matlab

  01   function mv = solver(ai,af,w)  01 def solver_(ai,af,w,nargout=1): 
  02   nBlocks = max(ai(:));          02     nBlocks=max_(ai[:]) 
  03   [m,n] = size(ai);              03     m,n=size_(ai,nargout=2)

====  ========================================================
  02  Matlab uses round brackets both for array indexing and
      for function calls. To figure out which is which,
      SMOP computes local use-def information, and then
      applies the following rule: undefined names are
      functions, while defined are arrays.
----  --------------------------------------------------------
  03  Matlab function ``size`` returns variable number of
      return values, which corresponds to returning a tuple
      in python.  Since python functions are unaware of the
      expected number of return values, their number must be
      explicitly passed in ``nargout``.
====  ========================================================

.. code:: matlab
                                                                                                        
  04   I = [0  1  0 -1];              04     I=matlabarray([0,1,0,- 1])
  05   J = [1  0 -1  0];              05     J=matlabarray([1,0,- 1,0])
  06   a = ai;                        06     a=copy_(ai)
  07   mv = [];                       07     mv=matlabarray([])

====  ========================================================
  04  Matlab array indexing starts with one; python indexing
      starts with zero.  New class ``matlabarray`` derives from
      ``ndarray``, but exposes matlab array behaviour.  For
      example, ``matlabarray`` instances always have at least
      two dimensions -- the shape of ``I`` and ``J`` is [1 4].
----  --------------------------------------------------------
  06  Matlab array assignment implies copying; python
      assignment implies data sharing.  We use explicit copy
      here.
----  --------------------------------------------------------
  07  Empty ``matlabarray`` object is created, and then
      extended at line 28.  Extending arrays by
      out-of-bounds assignment is deprecated in matlab, but
      is widely used never the less.  Python ``ndarray``
      can't be resized except in some special cases.
      Instances of ``matlabarray`` can be resized except
      where it is too expensive.
====  ========================================================

.. code:: matlab
                                                                                                        
  08   while ~isequal(af,a)           08     while not isequal_(af,a):
  09     bid = ceil(rand*nBlocks);    09         bid=ceil_(rand_() * nBlocks)
  10     [i,j] = find(a==bid);        10         i,j=find_(a == bid,nargout=2)
  11     r = ceil(rand*4);            11         r=ceil_(rand_() * 4)
  12     ni = i + I(r);               12         ni=i + I[r]
  13     nj = j + J(r);               13         nj=j + J[r]

====  ========================================================
  09  Matlab functions of zero arguments, such as
      ``rand``, can be used without parentheses.  In python,
      parentheses are required.  To detect such cases, used
      but undefined variables are assumed to be functions.
----  --------------------------------------------------------
  10  The expected number of return values from the matlab
      function ``find`` is explicitly passed in ``nargout``.
----  --------------------------------------------------------
  12  Variables I and J contain instances of the new class
      ``matlabarray``, which among other features uses one
      based array indexing.
====  ========================================================

.. code:: matlab

  14     if (ni<1) || (ni>m) ||       14         if (ni < 1) or (ni > m) or
                 (nj<1) || (nj>n)                            (nj < 1) or (nj > n):
  15         continue                 15             continue
  16     end                          16
  17     if a(ni,nj)>0                17         if a[ni,nj] > 0:
  18         continue                 18           continue
  19     end                          19
  20     [ti,tj] = find(af==bid);     20         ti,tj=find_(af == bid,nargout=2)
  21     d = (ti-i)^2 + (tj-j)^2;     21         d=(ti - i) ** 2 + (tj - j) ** 2
  22     dn = (ti-ni)^2 + (tj-nj)^2;  22         dn=(ti - ni) ** 2 + (tj - nj) ** 2
  23     if (d<dn) && (rand>0.05)     23         if (d < dn) and (rand_() > 0.05):
  24         continue                 24             continue
  25     end                          25
  26     a(ni,nj) = bid;              26         a[ni,nj]=bid
  27     a(i,j) = 0;                  27         a[i,j]=0
  28     mv(end+1,[1 2]) = [bid r];   28         mv[mv.shape[0] + 1,[1,2]]=[bid,r]
  29  end                             29
  30                                  30     return mv

Which one is faster --- python or octave?  I don't know.  
  Doing reliable performance measurements is notoriously
  hard, and is of low priority for me now.  Instead, I wrote
  a simple driver ``go.m`` and ``go.py`` and rewrote `rand`
  so that python and octave versions run the same code.
  Then I ran the above example on my laptop.  The results
  are twice as fast for the python version.   What does it
  mean?  Probably nothing. YMMV.

.. code:: matlab

    ai = zeros(10,10);
    af = ai;

    ai(1,1)=2;
    ai(2,2)=3;
    ai(3,3)=4;
    ai(4,4)=5;
    ai(5,5)=1;

    af(9,9)=1;
    af(8,8)=2;
    af(7,7)=3;
    af(6,6)=4;
    af(10,10)=5;

    tic;
    mv = solver(ai,af,0);
    toc
---------------------------------------------------------------------

Work in progress below this line
================================


Running the test suite::

     $ cd smop
     $ make check

Command-line options
--------------------

.. code:: sh

    lei@dilbert ~/smop-github/smop $ python main.py -h
    SMOP compiler version 0.25.1
    Usage: smop [options] file-list
        Options:
        -V --version
        -X --exclude=FILES      Ignore files listed in comma-separated list FILES
        -d --dot=REGEX          For functions whose names match REGEX, save debugging
                                information in "dot" format (see www.graphviz.org).
                                You need an installation of graphviz to use --dot
                                option.  Use "dot" utility to create a pdf file.
                                For example: 
                                    $ python main.py fastsolver.m -d "solver|cbest"
                                    $ dot -Tpdf -o resolve_solver.pdf resolve_solver.dot
        -h --help
        -o --output=FILENAME    By default create file named a.py
        -o- --output=-          Use standard output
        -s --strict             Stop on the first error
        -v --verbose

---------------------------------------------------------------------


+-----------------------------------------+-------+-------+-------+
|                                         |matlab |fortran|python |
+=========================================+=======+=======+=======+
|                                         |       |       |       |
|   A. Base-one indexing                  |  yes  | yes   |  no   |
+-----------------------------------------+-------+-------+-------+
|                                         |       |       |       |
|   B. Columns-first data layout          |  yes  | yes   |  no   |
+-----------------------------------------+-------+-------+-------+
|   C. Auto-expanding arrays              |  yes  | no *  |  yes  |
+-----------------------------------------+-------+-------+-------+
|   D. Update to create                   |  yes  | no *  |  yes  |
+-----------------------------------------+-------+-------+-------+
|   E. Assignment as copy                 |  yes  |  yes  |   no  |
+-----------------------------------------+-------+-------+-------+


+-----------------------------------------+-------+-------+-------+
|                                         |matlab |fortran|python |
+=========================================+=======+=======+=======+
|   F. Matrices everywhere                |  yes  |  no   |   no  |
+-----------------------------------------+-------+-------+-------+
|   G. Single subscript implies ravel     |  yes  |       |       |
+-----------------------------------------+-------+-------+-------+
|   H. Broadcast                          |       |       |       |
+-----------------------------------------+-------+-------+-------+
|   I. Boolean indexing                   |       |       |       |
+-----------------------------------------+-------+-------+-------+
|   J. Type and rank must be known        |  no   | yes   |  no   |
|      in compile time                    |       |       |       |
+-----------------------------------------+-------+-------+-------+

+-----------------------------------------+-------+-------+-------+
|                                         |matlab |fortran|python |
+=========================================+=======+=======+=======+
|   K. Garbage collection                 |  yes  | no *  |  yes  |
+-----------------------------------------+-------+-------+-------+
|   L. All uppercase                      |  no   | yes   |  no   |
+-----------------------------------------+-------+-------+-------+
|   M. Structs                            |       |       |       |
+-----------------------------------------+-------+-------+-------+
|   N. Interpreted                        |  yes  | no    |  yes  |
+-----------------------------------------+-------+-------+-------+
|   P. Strings are arrays of chars        |  yes  | no    |  yes  |
+-----------------------------------------+-------+-------+-------+


Base-one indexing
   Following fortran tradition, matlab starts array indexing with one,
   not zero.  Correspondingly, the last element of a N-element array is
   N, not N-1.

C_CONTIGUOUS and F_CONTIGUOUS data layout
  Matlab matrix elements are ordered in columns-first, aka
  F_CONTIGUOUS order.  Numpy arrays are C_CONTIGUOUS by default, with
  some support for F_CONTIGUOUS arrays.  Instances of matlabarray are
  F_CONTIGUOUS except if created empty, in which case they are
  C_CONTIGUOUS.

Auto-expanding arrays
  Matlab arrays are auto-magically resized on out-of-bounds update.
  Though deprecated, this feature is widely used in legacy code.
  Supporting this feature is one of the main reasons behind creation
  of the dedicated ``matlabarray`` class.  If we chose the `pythonic`
  option --- smop arrays directly mapped to ndarrays --- any array
  update that could not be proven to be safe, should have been
  enclosed in try-except-resize-retry.  It would not look any better.
  
  In fortran, the pattern should be somehow (how exactly?) detected in
  compile-time.  In python ``__setitem__`` hides ``try-catch``, with
  ``resize`` called inside ``catch``.  Is try-catch in fortran?

  In numpy out-of-bounds assignment is an error.  In smop,
  out-of-bounds assignment is supported for row and column matrices
  and their generalizations having shape

      [1 1 ... N ... 1]

  These arrays may be resized along their only non-singular dimension.
  For other matrices, new columns can be added to F_CONTIGUOUS arrays,
  and new rows can be added to C_CONTIGUOUS arrays.

  Finally, scalar array of any dimension, having shape

      [1 1 ... 1]

  can be resized along any dimension.

Update to create
  In matlab, arrays may be created by  updating a non existent array,
  as in the example::

      >>> clear a
      >>> a(17)=42

  This unique feature is not supported by smop, but can be worked
  around by inserting assignments into the original matlab code::

      >>> a=[]
      >>> a(17_=42

-------------------------------------

SMOP assumes that the input is syntactically correct  and
passes some test suite. 

.. code:: matlab
 
  01   ok = 0                         01 def solver_(c):                  
  02   if c                           02     if c:
  03      ok = f00                    03         ok = f00()
    
    .. code:: matlab

     

.. vim: tw=60
