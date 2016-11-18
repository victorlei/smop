/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#if !defined (octave_MSparse_defs_h)
#define octave_MSparse_defs_h 1

// Nothing like a little CPP abuse to brighten everyone's day.

// A macro that can be used to declare and instantiate OP= operators.
#define SPARSE_OP_ASSIGN_DECL(A_T, E_T, OP, PFX, API, LTGT, RHS_T) \
  PFX API A_T<E_T>& \
  operator OP LTGT (A_T<E_T>&, const RHS_T&)

// All the OP= operators that we care about.
#define SPARSE_OP_ASSIGN_DECLS(A_T, E_T, PFX, API, LTGT, RHS_T) \
  SPARSE_OP_ASSIGN_DECL (A_T, E_T, +=, PFX, API, LTGT, RHS_T); \
  SPARSE_OP_ASSIGN_DECL (A_T, E_T, -=, PFX, API, LTGT, RHS_T);

// Generate forward declarations for OP= operators.
#define SPARSE_OP_ASSIGN_FWD_DECLS(A_T, RHS_T, API) \
  SPARSE_OP_ASSIGN_DECLS (A_T, T, template <typename T>, API, , RHS_T)

// Generate friend declarations for the OP= operators.
#define SPARSE_OP_ASSIGN_FRIENDS(A_T, RHS_T, API) \
  SPARSE_OP_ASSIGN_DECLS (A_T, T, friend, API, <>, RHS_T)

// Instantiate the OP= operators.
#define SPARSE_OP_ASSIGN_DEFS(A_T, E_T, RHS_T, API) \
  SPARSE_OP_ASSIGN_DECLS (A_T, E_T, template, API, , RHS_T)

// A function that can be used to forward OP= operations from derived
// classes back to us.
#define SPARSE_OP_ASSIGN_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T) \
  inline R \
  F (X_T& x, const Y_T& y) \
  { \
    return R (F (C_X (x), C_Y (y))); \
  }

// All the OP= operators that we care about forwarding.
#define SPARSE_OP_ASSIGN_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_OP_ASSIGN_FWD_FCN (R, operator +=, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_OP_ASSIGN_FWD_FCN (R, operator -=, T, C_X, X_T, C_Y, Y_T)

// A macro that can be used to declare and instantiate unary operators.
#define SPARSE_UNOP(A_T, E_T, F, PFX, API, LTGT) \
  PFX API A_T<E_T> \
  F LTGT (const A_T<E_T>&)

// All the unary operators that we care about.
#define SPARSE_UNOP_DECLS(A_T, E_T, PFX, API, LTGT) \
  SPARSE_UNOP (A_T, E_T, operator +, PFX, API, LTGT); \
  SPARSE_UNOP (A_T, E_T, operator -, PFX, API, LTGT);

// Generate forward declarations for unary operators.
#define SPARSE_UNOP_FWD_DECLS(A_T, API) \
  SPARSE_UNOP_DECLS (A_T, T, template <typename T>, API, )

// Generate friend declarations for the unary operators.
#define SPARSE_UNOP_FRIENDS(A_T, API) \
  SPARSE_UNOP_DECLS (A_T, T, friend, API, <>)

// Instantiate the unary operators.
#define SPARSE_UNOP_DEFS(A_T, E_T, API) \
  SPARSE_UNOP_DECLS (A_T, E_T, template, API, )

// A function that can be used to forward unary operations from derived
// classes back to us.
#define SPARSE_UNOP_FWD_FCN(R, F, T, C_X, X_T) \
  inline R \
  F (const X_T& x) \
  { \
    return R (F (C_X (x))); \
  }

// All the unary operators that we care about forwarding.
#define SPARSE_UNOP_FWD_DEFS(R, T, C_X, X_T) \
  SPARSE_UNOP_FWD_FCN (R, operator +, T, C_X, X_T) \
  SPARSE_UNOP_FWD_FCN (R, operator -, T, C_X, X_T)

// A macro that can be used to declare and instantiate binary operators.
#define SPARSE_BINOP_DECL(A_T, E_T, F, PFX, API, LTGT, X_T, Y_T) \
  PFX API A_T<E_T> \
  F LTGT (const X_T&, const Y_T&)

// All the binary operators that we care about.  We have two
// sets of macros since the MArray OP MArray operations use functions
// (product and quotient) instead of operators (*, /).
#define SPARSE_BINOP_DECLS(A_T, F_T, E_T, PFX, API, LTGT, X_T, Y_T)      \
  SPARSE_BINOP_DECL (F_T, E_T, operator +, PFX, API, LTGT, X_T, Y_T); \
  SPARSE_BINOP_DECL (F_T, E_T, operator -, PFX, API, LTGT, X_T, Y_T); \
  SPARSE_BINOP_DECL (A_T, E_T, operator *, PFX, API, LTGT, X_T, Y_T); \
  SPARSE_BINOP_DECL (A_T, E_T, operator /, PFX, API, LTGT, X_T, Y_T);

#define SPARSE_AA_BINOP_DECLS(A_T, E_T, PFX, API, LTGT) \
  SPARSE_BINOP_DECL (A_T, E_T, operator +, PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  SPARSE_BINOP_DECL (A_T, E_T, operator -, PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  SPARSE_BINOP_DECL (A_T, E_T, quotient,   PFX, API, LTGT, A_T<E_T>, A_T<E_T>); \
  SPARSE_BINOP_DECL (A_T, E_T, product,    PFX, API, LTGT, A_T<E_T>, A_T<E_T>);

// Generate forward declarations for binary operators.
#define SPARSE_BINOP_FWD_DECLS(A_T, F_T, API) \
  SPARSE_BINOP_DECLS (A_T, F_T, T, template <typename T>, API, , A_T<T>, T)     \
  SPARSE_BINOP_DECLS (A_T, F_T, T, template <typename T>, API, , T, A_T<T>) \
  SPARSE_AA_BINOP_DECLS (A_T, T, template <typename T>, API, )

// Generate friend declarations for the binary operators.
#define SPARSE_BINOP_FRIENDS(A_T, F_T, API)                  \
  SPARSE_BINOP_DECLS (A_T, F_T, T, friend, API, <>, A_T<T>, T)  \
  SPARSE_BINOP_DECLS (A_T, F_T, T, friend, API, <>, T, A_T<T>)  \
  SPARSE_AA_BINOP_DECLS (A_T, T, friend, API, <>)

// Instantiate the binary operators.
#define SPARSE_BINOP_DEFS(A_T, F_T, E_T, API) \
  SPARSE_BINOP_DECLS (A_T, F_T, E_T, template, API, , A_T<E_T>, E_T)    \
  SPARSE_BINOP_DECLS (A_T, F_T, E_T, template, API, , E_T, A_T<E_T>)    \
  SPARSE_AA_BINOP_DECLS (A_T, E_T, template, API, )

// A function that can be used to forward binary operations from derived
// classes back to us.
#define SPARSE_BINOP_FWD_FCN(R, F, T, C_X, X_T, C_Y, Y_T) \
  inline R \
  F (const X_T& x, const Y_T& y) \
  { \
    return R (F (C_X (x), C_Y (y))); \
  }

// The binary operators that we care about forwarding.  We have two
// sets of macros since the MSparse OP MSparse operations use functions
// (product and quotient) instead of operators (*, /).
#define SPARSE_BINOP_FWD_DEFS(R, F, T, C_X, X_T, C_Y, Y_T)     \
  SPARSE_BINOP_FWD_FCN (F, operator +, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (F, operator -, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (R, operator *, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (R, operator /, T, C_X, X_T, C_Y, Y_T)

#define SPARSE_AA_BINOP_FWD_DEFS(R, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (R, operator +, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (R, operator -, T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (R, product,    T, C_X, X_T, C_Y, Y_T) \
  SPARSE_BINOP_FWD_FCN (R, quotient,   T, C_X, X_T, C_Y, Y_T)

// Forward declarations for the MSparse operators.
#define SPARSE_OPS_FORWARD_DECLS(A_T, F_T, API) \
  template <class T> \
  class A_T; \
 \
  /* SPARSE_OP_ASSIGN_FWD_DECLS (A_T, T) */ \
  SPARSE_OP_ASSIGN_FWD_DECLS (A_T, A_T<T>, API) \
  SPARSE_UNOP_FWD_DECLS (A_T, API) \
  SPARSE_BINOP_FWD_DECLS (A_T, F_T, API)

// Friend declarations for the MSparse operators.
#define SPARSE_OPS_FRIEND_DECLS(A_T, F_T, API)  \
  /* SPARSE_OP_ASSIGN_FRIENDS (A_T, T) */ \
  SPARSE_OP_ASSIGN_FRIENDS (A_T, A_T<T>, API) \
  SPARSE_UNOP_FRIENDS (A_T, API) \
    SPARSE_BINOP_FRIENDS (A_T, F_T, API)

// The following macros are for external use.

// Instantiate all the MSparse friends for MSparse element type T.
#define INSTANTIATE_SPARSE_FRIENDS(T, API) \
  /* SPARSE_OP_ASSIGN_DEFS (MSparse, T, T) */ \
  SPARSE_OP_ASSIGN_DEFS (MSparse, T, MSparse<T>, API) \
  SPARSE_UNOP_DEFS (MSparse, T, API) \
  SPARSE_BINOP_DEFS (MSparse, MArray, T, API)

// Define all the MSparse forwarding functions for return type R and
// MSparse element type T
#define SPARSE_FORWARD_DEFS(B, R, F, T)         \
  /* SPARSE_OP_ASSIGN_FWD_DEFS  */ \
  /* (R, T, dynamic_cast<B<T>&>, R, , T) */     \
 \
  SPARSE_OP_ASSIGN_FWD_DEFS \
    (R, T, \
     dynamic_cast<B<T>&>, R, dynamic_cast<const B<T>&>, R) \
 \
  SPARSE_UNOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R) \
 \
  SPARSE_BINOP_FWD_DEFS \
    (R, F, T, dynamic_cast<const B<T>&>, R, , T) \
 \
  SPARSE_BINOP_FWD_DEFS \
    (R, F, T, , T, dynamic_cast<const B<T>&>, R)        \
 \
  SPARSE_AA_BINOP_FWD_DEFS \
    (R, T, dynamic_cast<const B<T>&>, R, dynamic_cast<const B<T>&>, R)

// Now we have all the definitions we need.

#endif
