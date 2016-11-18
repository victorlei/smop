/*

Copyright (C) 1996-2015 John W. Eaton

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

#if !defined (octave_MArray_decl_h)
#define octave_MArray_decl_h 1

// A macro that can be used to declare and instantiate OP= operators.
#define MARRAY_OP_ASSIGN_DECL(A_T, E_T, OP, PFX, API, LTGT, RHS_T) \
  PFX API A_T<E_T>& \
  operator OP LTGT (A_T<E_T>&, const RHS_T&)

#define MARRAY_OP_ASSIGN_DECLX(A_T, E_T, OP, PFX, API, LTGT, RHS_T) \
  PFX API A_T<E_T>& \
  OP LTGT (A_T<E_T>&, const RHS_T&)

// All the OP= operators that we care about.
#define MARRAY_OP_ASSIGN_DECLS(A_T, E_T, PFX, API, LTGT, RHS_T) \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, +=, PFX, API, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, -=, PFX, API, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECLX (A_T, E_T, product_eq, PFX, API, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECLX (A_T, E_T, quotient_eq, PFX, API, LTGT, RHS_T);

#define MARRAY_OP_ASSIGN_DECLS1(A_T, E_T, PFX, API, LTGT, RHS_T) \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, +=, PFX, API, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, -=, PFX, API, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, *=, PFX, API, LTGT, RHS_T); \
  MARRAY_OP_ASSIGN_DECL (A_T, E_T, /=, PFX, API, LTGT, RHS_T);

// Generate forward declarations for OP= operators.
#define MARRAY_OP_ASSIGN_FWD_DECLS(A_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS (A_T, T, template <typename T>, API, , RHS_T)

#define MARRAY_OP_ASSIGN_FWD_DECLS1(A_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS1 (A_T, T, template <typename T>, API, , RHS_T)

// Generate friend declarations for the OP= operators.
#define MARRAY_OP_ASSIGN_FRIENDS(A_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS (A_T, T, friend, API, <>, RHS_T)

#define MARRAY_OP_ASSIGN_FRIENDS1(A_T, RHS_T, API) \
  MARRAY_OP_ASSIGN_DECLS1 (A_T, T, friend, API, <>, RHS_T)

// A function that can be used to forward OP= operations from derived
// classes back to us.
#define MARRAY_OP_ASSIGN_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T) \
  inline R \
  F (X_T& x, const Y_T& y) \
  { \
    return R (F (C_X (x), C_Y (y))); \
  }

// All the OP= operators that we care about forwarding.
#define MARRAY_OP_ASSIGN_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator +=, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator -=, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, product_eq, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, quotient_eq, T, C_X, X_T, C_Y, Y_T)

#define MARRAY_OP_ASSIGN_FWD_DEFS1(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator +=, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator -=, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator *=, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_OP_ASSIGN_FWD_FCN (R, operator /=, T, C_X, X_T, C_Y, Y_T)

// A macro that can be used to declare and instantiate unary operators.
#define MARRAY_UNOP(A_T, E_T, F, PFX, API, LTGT) \
  PFX API A_T<E_T> \
  F LTGT (const A_T<E_T>&)

// All the unary operators that we care about.
#define MARRAY_UNOP_DECLS(A_T, E_T, PFX, API, LTGT) \
  MARRAY_UNOP (A_T, E_T, operator +, PFX, API, LTGT); \
  MARRAY_UNOP (A_T, E_T, operator -, PFX, API, LTGT);

// Generate forward declarations for unary operators.
#define MARRAY_UNOP_FWD_DECLS(A_T, API) \
  MARRAY_UNOP_DECLS (A_T, T, template <typename T>, API, )

// Generate friend declarations for the unary operators.
#define MARRAY_UNOP_FRIENDS(A_T, API) \
  MARRAY_UNOP_DECLS (A_T, T, friend, API, <>)

// A function that can be used to forward unary operations from derived
// classes back to us.
#define MARRAY_UNOP_FWD_FCN(R, F, T, C_X, X_T) \
  inline R \
  F (const X_T& x) \
  { \
    return R (F (C_X (x))); \
  }

// All the unary operators that we care about forwarding.
#define MARRAY_UNOP_FWD_DEFS(R, T, C_X, X_T) \
  MARRAY_UNOP_FWD_FCN (R, operator +, T, C_X, X_T) \
  MARRAY_UNOP_FWD_FCN (R, operator -, T, C_X, X_T)

// A macro that can be used to declare and instantiate binary operators.
#define MARRAY_BINOP_DECL(A_T, E_T, F, PFX, API, LTGT, X_T, Y_T) \
  PFX API A_T<E_T> \
  F LTGT (const X_T&, const Y_T&)

// All the binary operators that we care about.  We have two
// sets of macros since the MArray OP MArray operations use functions
// (product and quotient) instead of operators (*, /).
#define MARRAY_BINOP_DECLS(A_T, E_T, PFX, API, LTGT, X_T, Y_T) \
  MARRAY_BINOP_DECL (A_T, E_T, operator +, PFX, API, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator -, PFX, API, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator *, PFX, API, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator /, PFX, API, LTGT, X_T, Y_T);

#define MARRAY_AA_BINOP_DECLS(A_T, E_T, PFX, API, LTGT) \
  MARRAY_BINOP_DECL (A_T, E_T, operator +, PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, operator -, PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, quotient,   PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, product,    PFX, API, LTGT, A_T<E_T>, A_T<E_T>);

#define MDIAGARRAY2_DAS_BINOP_DECLS(A_T, E_T, PFX, API, LTGT, X_T, Y_T) \
  MARRAY_BINOP_DECL (A_T, E_T, operator *, PFX, API, LTGT, X_T, Y_T); \
  MARRAY_BINOP_DECL (A_T, E_T, operator /, PFX, API, LTGT, X_T, Y_T);

#define MDIAGARRAY2_SDA_BINOP_DECLS(A_T, E_T, PFX, API, LTGT, X_T, Y_T) \
  MARRAY_BINOP_DECL (A_T, E_T, operator *, PFX, API, LTGT, X_T, Y_T);

#define MDIAGARRAY2_DADA_BINOP_DECLS(A_T, E_T, PFX, API, LTGT) \
  MARRAY_BINOP_DECL (A_T, E_T, operator +, PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, operator -, PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  MARRAY_BINOP_DECL (A_T, E_T, product,    PFX, API, LTGT, A_T<E_T>, A_T<E_T>);

// Generate forward declarations for binary operators.
#define MARRAY_BINOP_FWD_DECLS(A_T, API) \
  MARRAY_BINOP_DECLS (A_T, T, template <typename T>, API, , A_T<T>, T) \
  MARRAY_BINOP_DECLS (A_T, T, template <typename T>, API, , T, A_T<T>) \
  MARRAY_AA_BINOP_DECLS (A_T, T, template <typename T>, API, )

#define MDIAGARRAY2_BINOP_FWD_DECLS(A_T, API) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, T, template <typename T>, API, , A_T<T>, T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, T, template <typename T>, API, , T, A_T<T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, T, template <typename T>, API, )

// Generate friend declarations for the binary operators.
#define MARRAY_BINOP_FRIENDS(A_T, API) \
  MARRAY_BINOP_DECLS (A_T, T, friend, API, <>, A_T<T>, T) \
  MARRAY_BINOP_DECLS (A_T, T, friend, API, <>, T, A_T<T>) \
  MARRAY_AA_BINOP_DECLS (A_T, T, friend, API, <>)

#define MDIAGARRAY2_BINOP_FRIENDS(A_T, API) \
  MDIAGARRAY2_DAS_BINOP_DECLS (A_T, T, friend, API, <>, A_T<T>, T) \
  MDIAGARRAY2_SDA_BINOP_DECLS (A_T, T, friend, API, <>, T, A_T<T>) \
  MDIAGARRAY2_DADA_BINOP_DECLS (A_T, T, friend, API, <>)

// A function that can be used to forward binary operations from derived
// classes back to us.
#define MARRAY_BINOP_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T) \
  inline R \
  F (const X_T& x, const Y_T& y) \
  { \
    return R (F (C_X (x), C_Y (y))); \
  }

// The binary operators that we care about forwarding.  We have two
// sets of macros since the MArray OP MArray operations use functions
// (product and quotient) instead of operators (*, /).
#define MARRAY_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator /, T, C_X, X_T, C_Y, Y_T)

#define MARRAY_AA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, product,    T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, quotient,   T, C_X, X_T, C_Y, Y_T)

#define MDIAGARRAY2_DAS_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator /, T, C_X, X_T, C_Y, Y_T)

#define MDIAGARRAY2_SDA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T)

#define MDIAGARRAY2_DADA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  MARRAY_BINOP_FWD_FCN (R, product,    T, C_X, X_T, C_Y, Y_T)

// Forward declarations for the MArray operators.
#define MARRAY_OPS_FORWARD_DECLS(A_T, API) \
  template <class T> \
  class A_T; \
 \
  MARRAY_OP_ASSIGN_FWD_DECLS1 (A_T, T, API) \
  MARRAY_OP_ASSIGN_FWD_DECLS (A_T, A_T<T>, API) \
  MARRAY_UNOP_FWD_DECLS (A_T, API) \
  MARRAY_BINOP_FWD_DECLS (A_T, API)

#define MDIAGARRAY2_OPS_FORWARD_DECLS(A_T, API) \
  template <class T> \
  class A_T; \
 \
  MARRAY_UNOP_FWD_DECLS (A_T, API) \
  MDIAGARRAY2_BINOP_FWD_DECLS (A_T, API)

// Friend declarations for the MArray operators.
#define MARRAY_OPS_FRIEND_DECLS(A_T, API) \
  MARRAY_OP_ASSIGN_FRIENDS1 (A_T, T, API) \
  MARRAY_OP_ASSIGN_FRIENDS (A_T, A_T<T>, API) \
  MARRAY_UNOP_FRIENDS (A_T, API) \
  MARRAY_BINOP_FRIENDS (A_T, API)

#define MDIAGARRAY2_OPS_FRIEND_DECLS(A_T, API) \
  MARRAY_UNOP_FRIENDS (A_T, API) \
  MDIAGARRAY2_BINOP_FRIENDS (A_T, API)

// Define all the MArray forwarding functions for return type R and
// MArray element type T
#define MARRAY_FORWARD_DEFS(B, R, T) \
  MARRAY_OP_ASSIGN_FWD_DEFS1 \
    (R, T, dynamic_cast<B<T>&>, R, , T) \
 \
  MARRAY_OP_ASSIGN_FWD_DEFS \
    (R, T, \
     dynamic_cast<B<T>&>, R, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_UNOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, , T) \
 \
  MARRAY_BINOP_FWD_DEFS \
    (R, T, , T, dynamic_cast<const B<T>&>, R) \
 \
  MARRAY_AA_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, dynamic_cast<const B<T>&>, R)

#define MDIAGARRAY2_FORWARD_DEFS(B, R, T) \
  MARRAY_UNOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R) \
 \
  MDIAGARRAY2_DAS_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, , T) \
 \
  MDIAGARRAY2_SDA_BINOP_FWD_DEFS \
    (R, T, , T, dynamic_cast<const B<T>&>, R) \
 \
  MDIAGARRAY2_DADA_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, dynamic_cast<const B<T>&>, R)

#endif
