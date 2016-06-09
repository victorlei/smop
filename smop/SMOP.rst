
Class matlabarray
=================

Matlab arrays differ from numpy arrays in many ways, and class
matlabarray captures the following differences:

A. Base-one indexing
    Following Fortran tradition, matlab starts array indexing with
    one, not with zero. Correspondingly, the last element of a
    N-element array is N, not N-1.

B. C_CONTIGUOUS and F_CONTIGUOUS data layout
    Matlab matrix elements are ordered in columns-first, aka
    F_CONTIGUOUS order.  By default, numpy arrays are C_CONTIGUOUS.
    Instances of matlabarray are F_CONTIGUOUS, except if created
    empty, in which case they are C_CONTIGUOUS.
    
    +-----------------------+--------------------------------------+
    | matlab                | numpy                                |
    +=======================+======================================+
    |::                     |::                                    |
    |                       |                                      |
    |  > reshape(1:4,[2 2]) |   >>> a=matlabarray([1,2,3,4])       |
    |  1 3                  |   >>> a.reshape(2,2,order="F")       |
    |  2 4                  |   1 3                                |
    |                       |   2 4                                |
    |                       |                                      |
    |                       |   >>> a.reshape(2,2,order="C")       |
    |                       |   1 2                                |
    |                       |   3 4                                |
    +-----------------------+--------------------------------------+

    >>> a=matlabarray([1,2,3,4])
    >>> a.flags.f_contiguous
    True
    >>> a.flags.c_contiguous
    False
  
    >>> a=matlabarray()
    >>> a.flags.c_contiguous
    True
    >>> a.flags.f_contiguous
    False

C. Auto-expanding arrays
    Arrays are auto-expanded on out-of-bound assignment. Deprecated,
    this feature is widely used in legacy code.  In smop, out-of-bound
    assignment is fully supported for row and column vectors, and for
    their generalizations having shape
    
        [1 1 ... N ... 1 1 1]

    These arrays may be resized along their only non-singular
    dimension.  For other arrays, new columns can be added to
    F_CONTIGUOUS arrays, and new rows can be added to C_CONTIGUOUS
    arrays.

    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    |::                          |::                                |
    |                            |                                  |
    |  > a=[]                    |   >>> a=matlabarray()            |
    |  > a(1)=123                |   >>> a[1]=123                   |
    |  > a                       |   >>> a                          |
    |  123                       |   123                            |
    |                            |                                  |
    +----------------------------+----------------------------------+

D. Create by update
    In matlab, arrays can be created by updating a non-existent array,
    as in the following example:

    >>> clear a
    >>> a(17) = 42

    This unique feature is not yet supported by smop, but can be
    worked around by inserting assignments into the original matlab
    code:

    >>> a = []
    >>> a(17) = 42

E. Assignment as copy
    Array data is not shared by copying or slice indexing. Instead
    there is copy-on-write.

F. Everything is a matrix
    There are no zero or one-dimensional arrays. Scalars are
    two-dimensional rather than zero-dimensional as in numpy.

G. Single subscript implies ravel.
    TBD

H. Broadcasting
    Boadcasting rules are different

I. Boolean indexing
    TBD

----------------------------------------------------------------------

Data structures
===============

#. Empty vector [], empty string "", and empty cellarray {}
    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    | ::                         | ::                               |
    |                            |                                  |
    |   > size([])               |   >>> matlabarray().shape        |
    |   0 0                      |   (0, 0)                         |
    |                            |                                  |
    |   > size('')               |   >>> char().shape               |
    |   0 0                      |   (0, 0)                         |
    |                            |                                  |
    |   > size({})               |   >>> cellarray().shape          |
    |   0 0                      |   (0, 0)                         |
    +----------------------------+----------------------------------+
    

#. Scalars are 1x1 matrices
    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    | ::                         | ::                               |
    |                            |                                  |
    |   > a=17                   |   >>> a=matlabarray(17)          |
    |   > size(a)                |   >>> a.shape                    |
    |   1 1                      |   1 1                            |
    |                            |                                  |
    +----------------------------+----------------------------------+

#. Rectangular char arrays
    Class char inherits from class matlabarray the usual matlab array
    behaviour -- base-1 indexing, Fortran data order, auto-expand on
    out-of-bound assignment, etc.

    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    | ::                         | ::                               |
    |                            |                                  |
    |   > s='helloworld'         |   >>> s=char('helloworld')       |
    |   > size(s)                |   >>> print size_(s)             |
    |   1 10                     |   (1,10)                         |
    |   > s(1:5)='HELLO'         |   >>> s[1:5]='HELLO'             |
    |   > s                      |   >>> print s                    |
    |   HELLOworld               |   HELLOworld                     |
    |   > resize(s,[2 5])        |   >>> print resize_(s,[2,5])     |
    |   HELLO                    |   HELLO                          |
    |   world                    |   world                          |
    +----------------------------+----------------------------------+

#. Row vector
    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    | ::                         | ::                               |
    |                            |                                  |
    |  > s=[1 2 3]               |   >>> s=matlabarray([1,2,3])     |
    |                            |                                  |
    +----------------------------+----------------------------------+


#. Column vector
    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    |::                          |::                                |
    |                            |                                  |
    |  > a=[1;2;3]               |   >>> a=matlabarray([[1],        |
    |                            |                      [2],        |
    |                            |                      [2]])       |
    |  > size(a)                 |   >>> a.shape                    |
    |  3 1                       |   (3, 1)                         |
    +----------------------------+----------------------------------+


#. Cell arrays
    Cell arrays subclass matlabarray and inherit the usual matlab
    array behaviour -- base-1 indexing, Fortran data order, expand on
    out-of-bound assignment, etc. Unlike matlabarray, each element of
    cellarray holds a python object.

    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    |::                          |::                                |
    |                            |                                  |
    |  > a = { 'abc', 123 }      |   >>> a=cellarray(['abc',123])   |
    |  > a{1}                    |   >>> a[1]                       |
    |  abc                       |   abc                            |
    +----------------------------+----------------------------------+

#. Cell arrays of strings
    In matlab, cellstrings are cell arrays, where each cell contains a
    char object.  In numpy, class cellstring derives from matlabarray,
    and each cell contains a native python string (not a char
    instance).

    +----------------------------+----------------------------------+
    | matlab                     | numpy                            |
    +============================+==================================+
    |::                          |::                                |
    |                            |                                  |
    |  > a = { 'abc', 'hello' }  |   >>> a=cellstring(['abc',       |
    |                            |                     'hello'])    |
    |  > a{1}                    |   >>> a[1]                       |
    |  abc                       |   abc                            |
    +----------------------------+----------------------------------+

----------------------------------------------------------------------

Data structures
    All matlab data structures subclass from matlabarray

Structs
    TBD

Function pointers
    Handles @

String concatenation
    Array concatenation not implemented

    >>> ['hello' 'world']
    helloworld

.. vim: tw=70:sw=2
