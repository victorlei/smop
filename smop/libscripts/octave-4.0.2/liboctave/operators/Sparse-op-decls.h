/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler
Copyright (C) 2008 Jaroslav Hajek

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

#if !defined (octave_Sparse_op_decls_h)
#define octave_Sparse_op_decls_h 1

class SparseBoolMatrix;

#define SPARSE_BIN_OP_DECL(R, OP, X, Y, API) \
  extern API R OP (const X&, const Y&)

#define SPARSE_CMP_OP_DECL(OP, X, Y, API) \
  extern API SparseBoolMatrix OP (const X&, const Y&)

#define SPARSE_BOOL_OP_DECL(OP, X, Y, API) \
  extern API SparseBoolMatrix OP (const X&, const Y&)

// sparse matrix by scalar operations.

#define SPARSE_SMS_BIN_OP_DECLS(R1, R2, M, S, API)  \
  SPARSE_BIN_OP_DECL (R1, operator +, M, S, API); \
  SPARSE_BIN_OP_DECL (R1, operator -, M, S, API); \
  SPARSE_BIN_OP_DECL (R2, operator *, M, S, API); \
  SPARSE_BIN_OP_DECL (R2, operator /, M, S, API);

#define SPARSE_SMS_CMP_OP_DECLS(M, S, API) \
  SPARSE_CMP_OP_DECL (mx_el_lt, M, S, API); \
  SPARSE_CMP_OP_DECL (mx_el_le, M, S, API); \
  SPARSE_CMP_OP_DECL (mx_el_ge, M, S, API); \
  SPARSE_CMP_OP_DECL (mx_el_gt, M, S, API); \
  SPARSE_CMP_OP_DECL (mx_el_eq, M, S, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M, S, API);

#define SPARSE_SMS_EQNE_OP_DECLS(M, S, API) \
  SPARSE_CMP_OP_DECL (mx_el_eq, M, S, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M, S, API);

#define SPARSE_SMS_BOOL_OP_DECLS(M, S, API) \
  SPARSE_BOOL_OP_DECL (mx_el_and, M, S, API); \
  SPARSE_BOOL_OP_DECL (mx_el_or,  M, S, API);

#define SPARSE_SMS_OP_DECLS(R1, R2, M, S, API) \
  SPARSE_SMS_BIN_OP_DECLS (R1, R2, M, S, API)    \
  SPARSE_SMS_CMP_OP_DECLS (M, S, API) \
  SPARSE_SMS_BOOL_OP_DECLS (M, S, API)

// scalar by sparse matrix operations.

#define SPARSE_SSM_BIN_OP_DECLS(R1, R2, S, M, API)    \
  SPARSE_BIN_OP_DECL (R1, operator +, S, M, API); \
  SPARSE_BIN_OP_DECL (R1, operator -, S, M, API); \
  SPARSE_BIN_OP_DECL (R2, operator *, S, M, API); \
  SPARSE_BIN_OP_DECL (R2, operator /, S, M, API);

#define SPARSE_SSM_CMP_OP_DECLS(S, M, API) \
  SPARSE_CMP_OP_DECL (mx_el_lt, S, M, API); \
  SPARSE_CMP_OP_DECL (mx_el_le, S, M, API); \
  SPARSE_CMP_OP_DECL (mx_el_ge, S, M, API); \
  SPARSE_CMP_OP_DECL (mx_el_gt, S, M, API); \
  SPARSE_CMP_OP_DECL (mx_el_eq, S, M, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, S, M, API);

#define SPARSE_SSM_EQNE_OP_DECLS(S, M, API) \
  SPARSE_CMP_OP_DECL (mx_el_eq, S, M, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, S, M, API);

#define SPARSE_SSM_BOOL_OP_DECLS(S, M, API) \
  SPARSE_BOOL_OP_DECL (mx_el_and, S, M, API); \
  SPARSE_BOOL_OP_DECL (mx_el_or,  S, M, API); \

#define SPARSE_SSM_OP_DECLS(R1, R2, S, M, API) \
  SPARSE_SSM_BIN_OP_DECLS (R1, R2, S, M, API)    \
  SPARSE_SSM_CMP_OP_DECLS (S, M, API) \
  SPARSE_SSM_BOOL_OP_DECLS (S, M, API) \

// sparse matrix by sparse matrix operations.

#define SPARSE_SMSM_BIN_OP_DECLS(R1, R2, M1, M2, API)   \
  SPARSE_BIN_OP_DECL (R1, operator +, M1, M2, API); \
  SPARSE_BIN_OP_DECL (R1, operator -, M1, M2, API); \
  SPARSE_BIN_OP_DECL (R2, product,    M1, M2, API); \
  SPARSE_BIN_OP_DECL (R2, quotient,   M1, M2, API);

#define SPARSE_SMSM_CMP_OP_DECLS(M1, M2, API) \
  SPARSE_CMP_OP_DECL (mx_el_lt, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_le, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ge, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_gt, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define SPARSE_SMSM_EQNE_OP_DECLS(M1, M2, API) \
  SPARSE_CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define SPARSE_SMSM_BOOL_OP_DECLS(M1, M2, API) \
  SPARSE_BOOL_OP_DECL (mx_el_and, M1, M2, API); \
  SPARSE_BOOL_OP_DECL (mx_el_or,  M1, M2, API);

#define SPARSE_SMSM_OP_DECLS(R1, R2, M1, M2, API) \
  SPARSE_SMSM_BIN_OP_DECLS (R1, R2, M1, M2, API) \
  SPARSE_SMSM_CMP_OP_DECLS (M1, M2, API) \
  SPARSE_SMSM_BOOL_OP_DECLS (M1, M2, API)

// matrix by sparse matrix operations.

#define SPARSE_MSM_BIN_OP_DECLS(R1, R2, M1, M2, API)    \
  SPARSE_BIN_OP_DECL (R1, operator +, M1, M2, API); \
  SPARSE_BIN_OP_DECL (R1, operator -, M1, M2, API); \
  SPARSE_BIN_OP_DECL (R2, product,    M1, M2, API); \
  SPARSE_BIN_OP_DECL (R2, quotient,   M1, M2, API);

#define SPARSE_MSM_CMP_OP_DECLS(M1, M2, API) \
  SPARSE_CMP_OP_DECL (mx_el_lt, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_le, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ge, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_gt, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define SPARSE_MSM_EQNE_OP_DECLS(M1, M2, API) \
  SPARSE_CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define SPARSE_MSM_BOOL_OP_DECLS(M1, M2, API) \
  SPARSE_BOOL_OP_DECL (mx_el_and, M1, M2, API); \
  SPARSE_BOOL_OP_DECL (mx_el_or,  M1, M2, API);

#define SPARSE_MSM_OP_DECLS(R1, R2, M1, M2, API) \
  SPARSE_MSM_BIN_OP_DECLS (R1, R2, M1, M2, API) \
  SPARSE_MSM_CMP_OP_DECLS (M1, M2, API) \
  SPARSE_MSM_BOOL_OP_DECLS (M1, M2, API)

// sparse matrix by matrix operations.

#define SPARSE_SMM_BIN_OP_DECLS(R1, R2, M1, M2, API)    \
  SPARSE_BIN_OP_DECL (R1, operator +, M1, M2, API); \
  SPARSE_BIN_OP_DECL (R1, operator -, M1, M2, API); \
  SPARSE_BIN_OP_DECL (R2, product,    M1, M2, API); \
  SPARSE_BIN_OP_DECL (R2, quotient,   M1, M2, API);

#define SPARSE_SMM_CMP_OP_DECLS(M1, M2, API) \
  SPARSE_CMP_OP_DECL (mx_el_lt, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_le, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ge, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_gt, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define SPARSE_SMM_EQNE_OP_DECLS(M1, M2, API) \
  SPARSE_CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  SPARSE_CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define SPARSE_SMM_BOOL_OP_DECLS(M1, M2, API) \
  SPARSE_BOOL_OP_DECL (mx_el_and, M1, M2, API); \
  SPARSE_BOOL_OP_DECL (mx_el_or,  M1, M2, API);

#define SPARSE_SMM_OP_DECLS(R1, R2, M1, M2, API) \
  SPARSE_SMM_BIN_OP_DECLS (R1, R2, M1, M2, API) \
  SPARSE_SMM_CMP_OP_DECLS (M1, M2, API) \
  SPARSE_SMM_BOOL_OP_DECLS (M1, M2, API)

#endif
