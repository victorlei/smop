====
SMOP
====

SMOP stands for Small Matlab/Octave to Python compiler.  It
converts matlab and octave programs to python.  The
resulting ``.py`` program imports ``runtime.py``, which
contains the necessary runtime support -- several class
definitions and about 30 small everyday functions, such
as ``length``.

Example: ``solver.m``
    The matlab program ``solver.m`` was taken from the  matlab
    programming competition in 2004 (Moving Furniture).  To the
    right is ``a.py`` -- its SMOP translation to python.
    Though only 30 lines long, this example shows many of 
    the complexities of converting matlab code to python.


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

.. vim: tw=60
