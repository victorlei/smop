subscripts = "square" # "round"

"""Row vectors in Matlab can be expressed in Fortran as
   either one or two dimensional arrays"""
row_vector_ndim   = 1 # 2

"""Given Matlab code such as [a b c]=size(X) we heuristically
   decide that X is 3-dimensional."""
rank_guess_from_size = True

"""Given false(1,n), deduce that n is a scalar"""
rank_backward_propagate=1

"""0=not even constants
   1=char constants supported
"""
has_char_constants = 0

do_allocate = 0
do_resolve  = 1
do_rewrite  = 0
do_rename   = 0 # SSA
do_typeof   = 0
do_listing  = 0

debug = False

uppercase=True # active only with f90

line_numbering=True #True # uses either # or ! or %

syntax_errors = []
verbose = 0
filename = ""
