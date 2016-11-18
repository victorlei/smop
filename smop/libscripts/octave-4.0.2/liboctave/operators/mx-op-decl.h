/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
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

#if !defined (octave_mx_op_decl_h)
#define octave_mx_op_decl_h 1

#define BIN_OP_DECL(R, OP, X, Y, API) \
  extern API R OP (const X&, const Y&)

class boolMatrix;
class boolNDArray;

#define CMP_OP_DECL(OP, X, Y, API) \
  extern API boolMatrix OP (const X&, const Y&)

#define NDCMP_OP_DECL(OP, X, Y, API) \
  extern API boolNDArray OP (const X&, const Y&)

#define BOOL_OP_DECL(OP, X, Y, API) \
  extern API boolMatrix OP (const X&, const Y&)

#define NDBOOL_OP_DECL(OP, X, Y, API) \
  extern API boolNDArray OP (const X&, const Y&)

// vector by scalar operations.

#define VS_BIN_OP_DECLS(R, V, S, API) \
  BIN_OP_DECL (R, operator +, V, S, API); \
  BIN_OP_DECL (R, operator -, V, S, API); \
  BIN_OP_DECL (R, operator *, V, S, API); \
  BIN_OP_DECL (R, operator /, V, S, API);

#define VS_OP_DECLS(R, V, S, API) \
  VS_BIN_OP_DECLS(R, V, S, API)

// scalar by vector by operations.

#define SV_BIN_OP_DECLS(R, S, V, API) \
  BIN_OP_DECL (R, operator +, S, V, API); \
  BIN_OP_DECL (R, operator -, S, V, API); \
  BIN_OP_DECL (R, operator *, S, V, API); \
  BIN_OP_DECL (R, operator /, S, V, API);

#define SV_OP_DECLS(R, S, V, API) \
  SV_BIN_OP_DECLS(R, S, V, API)

// vector by vector operations.

#define VV_BIN_OP_DECLS(R, V1, V2, API) \
  BIN_OP_DECL (R, operator +, V1, V2, API); \
  BIN_OP_DECL (R, operator -, V1, V2, API); \
  BIN_OP_DECL (R, product,    V1, V2, API); \
  BIN_OP_DECL (R, quotient,   V1, V2, API);

#define VV_OP_DECLS(R, V1, V2, API) \
  VV_BIN_OP_DECLS(R, V1, V2, API)

// matrix by scalar operations.

#define MS_BIN_OP_DECLS(R, M, S, API) \
  BIN_OP_DECL (R, operator +, M, S, API); \
  BIN_OP_DECL (R, operator -, M, S, API); \
  BIN_OP_DECL (R, operator *, M, S, API); \
  BIN_OP_DECL (R, operator /, M, S, API);

#define MS_CMP_OP_DECLS(M, S, API) \
  CMP_OP_DECL (mx_el_lt, M, S, API); \
  CMP_OP_DECL (mx_el_le, M, S, API); \
  CMP_OP_DECL (mx_el_ge, M, S, API); \
  CMP_OP_DECL (mx_el_gt, M, S, API); \
  CMP_OP_DECL (mx_el_eq, M, S, API); \
  CMP_OP_DECL (mx_el_ne, M, S, API);

#define MS_BOOL_OP_DECLS(M, S, API) \
  BOOL_OP_DECL (mx_el_and, M, S, API); \
  BOOL_OP_DECL (mx_el_or,  M, S, API); \

#define MS_OP_DECLS(R, M, S, API) \
  MS_BIN_OP_DECLS (R, M, S, API) \
  MS_CMP_OP_DECLS (M, S, API) \
  MS_BOOL_OP_DECLS (M, S, API) \

// scalar by matrix operations.

#define SM_BIN_OP_DECLS(R, S, M, API) \
  BIN_OP_DECL (R, operator +, S, M, API); \
  BIN_OP_DECL (R, operator -, S, M, API); \
  BIN_OP_DECL (R, operator *, S, M, API); \
  BIN_OP_DECL (R, operator /, S, M, API);

#define SM_CMP_OP_DECLS(S, M, API) \
  CMP_OP_DECL (mx_el_lt, S, M, API); \
  CMP_OP_DECL (mx_el_le, S, M, API); \
  CMP_OP_DECL (mx_el_ge, S, M, API); \
  CMP_OP_DECL (mx_el_gt, S, M, API); \
  CMP_OP_DECL (mx_el_eq, S, M, API); \
  CMP_OP_DECL (mx_el_ne, S, M, API);

#define SM_BOOL_OP_DECLS(S, M, API) \
  BOOL_OP_DECL (mx_el_and, S, M, API); \
  BOOL_OP_DECL (mx_el_or,  S, M, API); \

#define SM_OP_DECLS(R, S, M, API) \
  SM_BIN_OP_DECLS (R, S, M, API) \
  SM_CMP_OP_DECLS (S, M, API) \
  SM_BOOL_OP_DECLS (S, M, API) \

// matrix by matrix operations.

#define MM_BIN_OP_DECLS(R, M1, M2, API) \
  BIN_OP_DECL (R, operator +, M1, M2, API); \
  BIN_OP_DECL (R, operator -, M1, M2, API); \
  BIN_OP_DECL (R, product,    M1, M2, API); \
  BIN_OP_DECL (R, quotient,   M1, M2, API);

#define MM_CMP_OP_DECLS(M1, M2, API) \
  CMP_OP_DECL (mx_el_lt, M1, M2, API); \
  CMP_OP_DECL (mx_el_le, M1, M2, API); \
  CMP_OP_DECL (mx_el_ge, M1, M2, API); \
  CMP_OP_DECL (mx_el_gt, M1, M2, API); \
  CMP_OP_DECL (mx_el_eq, M1, M2, API); \
  CMP_OP_DECL (mx_el_ne, M1, M2, API);

#define MM_BOOL_OP_DECLS(M1, M2, API) \
  BOOL_OP_DECL (mx_el_and, M1, M2, API); \
  BOOL_OP_DECL (mx_el_or,  M1, M2, API);

#define MM_OP_DECLS(R, M1, M2, API) \
  MM_BIN_OP_DECLS (R, M1, M2, API) \
  MM_CMP_OP_DECLS (M1, M2, API) \
  MM_BOOL_OP_DECLS (M1, M2, API)

// N-d matrix by scalar operations.

#define NDS_BIN_OP_DECLS(R, ND, S, API) \
  BIN_OP_DECL (R, operator +, ND, S, API); \
  BIN_OP_DECL (R, operator -, ND, S, API); \
  BIN_OP_DECL (R, operator *, ND, S, API); \
  BIN_OP_DECL (R, operator /, ND, S, API);

#define NDS_CMP_OP_DECLS(ND, S, API) \
  NDCMP_OP_DECL (mx_el_lt, ND, S, API); \
  NDCMP_OP_DECL (mx_el_le, ND, S, API); \
  NDCMP_OP_DECL (mx_el_ge, ND, S, API); \
  NDCMP_OP_DECL (mx_el_gt, ND, S, API); \
  NDCMP_OP_DECL (mx_el_eq, ND, S, API); \
  NDCMP_OP_DECL (mx_el_ne, ND, S, API);

#define NDS_BOOL_OP_DECLS(ND, S, API) \
  NDBOOL_OP_DECL (mx_el_and, ND, S, API); \
  NDBOOL_OP_DECL (mx_el_or,  ND, S, API); \
  NDBOOL_OP_DECL (mx_el_not_and, ND, S, API); \
  NDBOOL_OP_DECL (mx_el_not_or,  ND, S, API);

#define NDS_OP_DECLS(R, ND, S, API) \
  NDS_BIN_OP_DECLS (R, ND, S, API) \
  NDS_CMP_OP_DECLS (ND, S, API) \
  NDS_BOOL_OP_DECLS (ND, S, API)

// scalar by N-d matrix operations.

#define SND_BIN_OP_DECLS(R, S, ND, API) \
  BIN_OP_DECL (R, operator +, S, ND, API); \
  BIN_OP_DECL (R, operator -, S, ND, API); \
  BIN_OP_DECL (R, operator *, S, ND, API); \
  BIN_OP_DECL (R, operator /, S, ND, API);

#define SND_CMP_OP_DECLS(S, ND, API) \
  NDCMP_OP_DECL (mx_el_lt, S, ND, API); \
  NDCMP_OP_DECL (mx_el_le, S, ND, API); \
  NDCMP_OP_DECL (mx_el_ge, S, ND, API); \
  NDCMP_OP_DECL (mx_el_gt, S, ND, API); \
  NDCMP_OP_DECL (mx_el_eq, S, ND, API); \
  NDCMP_OP_DECL (mx_el_ne, S, ND, API);

#define SND_BOOL_OP_DECLS(S, ND, API) \
  NDBOOL_OP_DECL (mx_el_and, S, ND, API); \
  NDBOOL_OP_DECL (mx_el_or,  S, ND, API); \
  NDBOOL_OP_DECL (mx_el_and_not, S, ND, API); \
  NDBOOL_OP_DECL (mx_el_or_not,  S, ND, API);

#define SND_OP_DECLS(R, S, ND, API) \
  SND_BIN_OP_DECLS (R, S, ND, API) \
  SND_CMP_OP_DECLS (S, ND, API) \
  SND_BOOL_OP_DECLS (S, ND, API)

// N-d matrix by N-d matrix operations.

#define NDND_BIN_OP_DECLS(R, ND1, ND2, API) \
  BIN_OP_DECL (R, operator +, ND1, ND2, API); \
  BIN_OP_DECL (R, operator -, ND1, ND2, API); \
  BIN_OP_DECL (R, product,    ND1, ND2, API); \
  BIN_OP_DECL (R, quotient,   ND1, ND2, API);

#define NDND_CMP_OP_DECLS(ND1, ND2, API) \
  NDCMP_OP_DECL (mx_el_lt, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_le, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_ge, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_gt, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_eq, ND1, ND2, API); \
  NDCMP_OP_DECL (mx_el_ne, ND1, ND2, API);

#define NDND_BOOL_OP_DECLS(ND1, ND2, API) \
  NDBOOL_OP_DECL (mx_el_and, ND1, ND2, API); \
  NDBOOL_OP_DECL (mx_el_or,  ND1, ND2, API); \
  NDBOOL_OP_DECL (mx_el_and_not, ND1, ND2, API); \
  NDBOOL_OP_DECL (mx_el_or_not,  ND1, ND2, API); \
  NDBOOL_OP_DECL (mx_el_not_and, ND1, ND2, API); \
  NDBOOL_OP_DECL (mx_el_not_or,  ND1, ND2, API);

#define NDND_OP_DECLS(R, ND1, ND2, API) \
  NDND_BIN_OP_DECLS (R, ND1, ND2, API) \
  NDND_CMP_OP_DECLS (ND1, ND2, API) \
  NDND_BOOL_OP_DECLS (ND1, ND2, API)

// scalar by diagonal matrix operations.

#define SDM_BIN_OP_DECLS(R, S, DM, API) \
  BIN_OP_DECL (R, operator *, S, DM, API); \

#define SDM_OP_DECLS(R, S, DM, API) \
  SDM_BIN_OP_DECLS(R, S, DM, API)

// diagonal matrix by scalar operations.

#define DMS_BIN_OP_DECLS(R, DM, S, API) \
  BIN_OP_DECL (R, operator *, DM, S, API); \
  BIN_OP_DECL (R, operator /, DM, S, API);

#define DMS_OP_DECLS(R, DM, S, API) \
  DMS_BIN_OP_DECLS(R, DM, S, API)

// matrix by diagonal matrix operations.

#define MDM_BIN_OP_DECLS(R, M, DM, API) \
  BIN_OP_DECL (R, operator +, M, DM, API); \
  BIN_OP_DECL (R, operator -, M, DM, API); \
  BIN_OP_DECL (R, operator *, M, DM, API);

#define MDM_OP_DECLS(R, M, DM, API) \
  MDM_BIN_OP_DECLS(R, M, DM, API)

// diagonal matrix by matrix operations.

#define DMM_BIN_OP_DECLS(R, DM, M, API) \
  BIN_OP_DECL (R, operator +, DM, M, API); \
  BIN_OP_DECL (R, operator -, DM, M, API); \
  BIN_OP_DECL (R, operator *, DM, M, API);

#define DMM_OP_DECLS(R, DM, M, API) \
  DMM_BIN_OP_DECLS(R, DM, M, API)

// diagonal matrix by diagonal matrix operations.

#define DMDM_BIN_OP_DECLS(R, DM1, DM2, API) \
  BIN_OP_DECL (R, operator +, DM1, DM2, API); \
  BIN_OP_DECL (R, operator -, DM1, DM2, API); \
  BIN_OP_DECL (R, product, DM1, DM2, API);

#define DMDM_OP_DECLS(R, DM1, DM2, API) \
  DMDM_BIN_OP_DECLS (R, DM1, DM2, API)

// scalar by N-d array min/max ops

#define MINMAX_DECLS(T, S, API) \
  extern API T min (S d, const T& m); \
  extern API T min (const T& m, S d); \
  extern API T min (const T& a, const T& b); \
  extern API T max (S d, const T& m); \
  extern API T max (const T& m, S d); \
  extern API T max (const T& a, const T& b);

// permutation matrix by matrix ops and vice versa

#define PMM_BIN_OP_DECLS(R, PM, M, API) \
  BIN_OP_DECL (R, operator *, PM, M, API);

#define MPM_BIN_OP_DECLS(R, M, PM, API) \
  BIN_OP_DECL (R, operator *, M, PM, API);

#endif

