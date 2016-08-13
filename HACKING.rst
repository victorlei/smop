=========
HACKING
=========

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

C. Auto-expanding arrays
  Matlab arrays are auto-magically resized on out-of-bounds
  update.  Deprecated, this feature is widely used in legacy code.
  Supporting this feature is hard both in python and in fortran.  
  
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

D. Update to create
  In matlab, arrays may be created by  updating a non existent array,
  as in the example::

      >>> clear a
      >>> a(17)=42

  This unique feature is not supported by smop, but can be worked
  around by inserting assignments into the original matlab code::

      >>> a=[]
      >>> a(17_=42

-------------------------------------

.. vim:tw=70
