/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_MArray_defs_h)
#define octave_MArray_defs_h 1

#include "mx-inlines.cc"

// Instantiate the OP= operators.
#define MARRAY_OP_ASSIGN_DEFS(A_T, E_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS (A_T, E_T, template, API, , RHS_T)

#define MARRAY_OP_ASSIGN_DEFS1(A_T, E_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS1 (A_T, E_T, template, API, , RHS_T)

// Instantiate the unary operators.
#define MARRAY_UNOP_DEFS(A_T, E_T, API) \
  MARRAY_UNOP_DECLS (A_T, E_T, template, API, )

// Instantiate the binary operators.
#define MARRAY_BINOP_DEFS(A_T, E_T, API) \
  MARRAY_BINOP_DECLS (A_T, E_T, template, API, , A_T<E_T>, E_T) \
  MARRAY_BINOP_DECLS (A_T, E_T, template, API, , E_T, A_T<E_T>) \
  MARRAY_AA_BINOP_DECLS (A_T, E_T, template, API, )

#define MDIAGARRAY2_BINOP_DEFS(A_T, E_T, API) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, E_T, template, API, , A_T<E_T>, E_T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, E_T, template, API, , E_T, A_T<E_T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, E_T, template, API, )

// The following macros are for external use.

// Instantiate all the MArray friends for MArray element type T.
#define INSTANTIATE_MARRAY_FRIENDS(T, API) \
  MARRAY_OP_ASSIGN_DEFS1 (MArray, T, T, API) \
  MARRAY_OP_ASSIGN_DEFS (MArray, T, MArray<T>, API) \
  MARRAY_UNOP_DEFS (MArray, T, API) \
  MARRAY_BINOP_DEFS (MArray, T, API)

// Instantiate all the MDiagArray2 friends for MDiagArray2 element type T.
#define INSTANTIATE_MDIAGARRAY2_FRIENDS(T, API) \
  MARRAY_UNOP_DEFS (MDiagArray2, T, API) \
  MDIAGARRAY2_BINOP_DEFS (MDiagArray2, T, API)

// Now we have all the definitions we need.

#endif
