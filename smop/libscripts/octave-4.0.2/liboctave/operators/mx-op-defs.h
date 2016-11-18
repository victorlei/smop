/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009-2010 VZLU Prague, a.s.

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

#if !defined (octave_mx_op_defs_h)
#define octave_mx_op_defs_h 1

#include "lo-array-gripes.h"
#include "mx-op-decl.h"
#include "mx-inlines.cc"

#define SNANCHK(s) \
  if (xisnan (s)) \
    gripe_nan_to_logical_conversion ()

#define MNANCHK(m, MT) \
  if (do_mx_check (m, mx_inline_any_nan<MT>)) \
    gripe_nan_to_logical_conversion ()

// vector by scalar operations.

#define VS_BIN_OP(R, F, OP, V, S) \
  R \
  F (const V& v, const S& s) \
  { \
    return do_ms_binary_op<R::element_type, V::element_type, S> (v, s, OP); \
  }

#define VS_BIN_OPS(R, V, S) \
  VS_BIN_OP (R, operator +, mx_inline_add, V, S) \
  VS_BIN_OP (R, operator -, mx_inline_sub, V, S) \
  VS_BIN_OP (R, operator *, mx_inline_mul, V, S) \
  VS_BIN_OP (R, operator /, mx_inline_div, V, S)

// scalar by vector by operations.

#define SV_BIN_OP(R, F, OP, S, V) \
  R \
  F (const S& s, const V& v) \
  { \
    return do_sm_binary_op<R::element_type, S, V::element_type> (s, v, OP); \
  }

#define SV_BIN_OPS(R, S, V) \
  SV_BIN_OP (R, operator +, mx_inline_add, S, V) \
  SV_BIN_OP (R, operator -, mx_inline_sub, S, V) \
  SV_BIN_OP (R, operator *, mx_inline_mul, S, V) \
  SV_BIN_OP (R, operator /, mx_inline_div, S, V)

// vector by vector operations.

#define VV_BIN_OP(R, F, OP, V1, V2) \
  R \
  F (const V1& v1, const V2& v2) \
  { \
    return do_mm_binary_op<R::element_type, V1::element_type, V2::element_type> (v1, v2, OP, OP, OP, #F); \
  }

#define VV_BIN_OPS(R, V1, V2) \
  VV_BIN_OP (R, operator +, mx_inline_add, V1, V2) \
  VV_BIN_OP (R, operator -, mx_inline_sub, V1, V2) \
  VV_BIN_OP (R, product,    mx_inline_mul, V1, V2) \
  VV_BIN_OP (R, quotient,   mx_inline_div, V1, V2)

// matrix by scalar operations.

#define MS_BIN_OP(R, OP, M, S, F) \
  R \
  OP (const M& m, const S& s) \
  { \
    return do_ms_binary_op<R::element_type, M::element_type, S> (m, s, F); \
  }

#define MS_BIN_OPS(R, M, S) \
  MS_BIN_OP (R, operator +, M, S, mx_inline_add) \
  MS_BIN_OP (R, operator -, M, S, mx_inline_sub) \
  MS_BIN_OP (R, operator *, M, S, mx_inline_mul) \
  MS_BIN_OP (R, operator /, M, S, mx_inline_div)

#define MS_CMP_OP(F, OP, M, S) \
  boolMatrix \
  F (const M& m, const S& s) \
  { \
    return do_ms_binary_op<bool, M::element_type, S> (m, s, OP); \
  }

#define MS_CMP_OPS(M, S) \
  MS_CMP_OP (mx_el_lt, mx_inline_lt, M, S) \
  MS_CMP_OP (mx_el_le, mx_inline_le, M, S) \
  MS_CMP_OP (mx_el_ge, mx_inline_ge, M, S) \
  MS_CMP_OP (mx_el_gt, mx_inline_gt, M, S) \
  MS_CMP_OP (mx_el_eq, mx_inline_eq, M, S) \
  MS_CMP_OP (mx_el_ne, mx_inline_ne, M, S)

#define MS_BOOL_OP(F, OP, M, S) \
  boolMatrix \
  F (const M& m, const S& s) \
  { \
    MNANCHK (m, M::element_type); \
    SNANCHK (s); \
    return do_ms_binary_op<bool, M::element_type, S> (m, s, OP); \
  }

#define MS_BOOL_OPS(M, S) \
  MS_BOOL_OP (mx_el_and, mx_inline_and, M, S) \
  MS_BOOL_OP (mx_el_or,  mx_inline_or,  M, S)

// scalar by matrix operations.

#define SM_BIN_OP(R, OP, S, M, F) \
  R \
  OP (const S& s, const M& m) \
  { \
    return do_sm_binary_op<R::element_type, S, M::element_type> (s, m, F); \
  }

#define SM_BIN_OPS(R, S, M) \
  SM_BIN_OP (R, operator +, S, M, mx_inline_add) \
  SM_BIN_OP (R, operator -, S, M, mx_inline_sub) \
  SM_BIN_OP (R, operator *, S, M, mx_inline_mul) \
  SM_BIN_OP (R, operator /, S, M, mx_inline_div)

#define SM_CMP_OP(F, OP, S, M) \
  boolMatrix \
  F (const S& s, const M& m) \
  { \
    return do_sm_binary_op<bool, S, M::element_type> (s, m, OP); \
  }

#define SM_CMP_OPS(S, M) \
  SM_CMP_OP (mx_el_lt, mx_inline_lt, S, M) \
  SM_CMP_OP (mx_el_le, mx_inline_le, S, M) \
  SM_CMP_OP (mx_el_ge, mx_inline_ge, S, M) \
  SM_CMP_OP (mx_el_gt, mx_inline_gt, S, M) \
  SM_CMP_OP (mx_el_eq, mx_inline_eq, S, M) \
  SM_CMP_OP (mx_el_ne, mx_inline_ne, S, M)

#define SM_BOOL_OP(F, OP, S, M) \
  boolMatrix \
  F (const S& s, const M& m) \
  { \
    SNANCHK (s); \
    MNANCHK (m, M::element_type); \
    return do_sm_binary_op<bool, S, M::element_type> (s, m, OP); \
  }

#define SM_BOOL_OPS(S, M) \
  SM_BOOL_OP (mx_el_and, mx_inline_and, S, M) \
  SM_BOOL_OP (mx_el_or,  mx_inline_or,  S, M)

// matrix by matrix operations.

#define MM_BIN_OP(R, OP, M1, M2, F) \
  R \
  OP (const M1& m1, const M2& m2) \
  { \
    return do_mm_binary_op<R::element_type, M1::element_type, M2::element_type> (m1, m2, F, F, F, #OP); \
  }

#define MM_BIN_OPS(R, M1, M2) \
  MM_BIN_OP (R, operator +, M1, M2, mx_inline_add) \
  MM_BIN_OP (R, operator -, M1, M2, mx_inline_sub) \
  MM_BIN_OP (R, product,    M1, M2, mx_inline_mul) \
  MM_BIN_OP (R, quotient,   M1, M2, mx_inline_div)

#define MM_CMP_OP(F, OP, M1, M2) \
  boolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    return do_mm_binary_op<bool, M1::element_type, M2::element_type> (m1, m2, OP, OP, OP, #F); \
  }

#define MM_CMP_OPS(M1, M2) \
  MM_CMP_OP (mx_el_lt, mx_inline_lt, M1, M2) \
  MM_CMP_OP (mx_el_le, mx_inline_le, M1, M2) \
  MM_CMP_OP (mx_el_ge, mx_inline_ge, M1, M2) \
  MM_CMP_OP (mx_el_gt, mx_inline_gt, M1, M2) \
  MM_CMP_OP (mx_el_eq, mx_inline_eq, M1, M2) \
  MM_CMP_OP (mx_el_ne, mx_inline_ne, M1, M2)

#define MM_BOOL_OP(F, OP, M1, M2) \
  boolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    MNANCHK (m1, M1::element_type); \
    MNANCHK (m2, M2::element_type); \
    return do_mm_binary_op<bool, M1::element_type, M2::element_type> (m1, m2, OP, OP, OP, #F); \
  }

#define MM_BOOL_OPS(M1, M2) \
  MM_BOOL_OP (mx_el_and, mx_inline_and, M1, M2) \
  MM_BOOL_OP (mx_el_or,  mx_inline_or,  M1, M2)

// N-d matrix by scalar operations.

#define NDS_BIN_OP(R, OP, ND, S, F) \
  R \
  OP (const ND& m, const S& s) \
  { \
    return do_ms_binary_op<R::element_type, ND::element_type, S> (m, s, F); \
  }

#define NDS_BIN_OPS(R, ND, S) \
  NDS_BIN_OP (R, operator +, ND, S, mx_inline_add) \
  NDS_BIN_OP (R, operator -, ND, S, mx_inline_sub) \
  NDS_BIN_OP (R, operator *, ND, S, mx_inline_mul) \
  NDS_BIN_OP (R, operator /, ND, S, mx_inline_div)

#define NDS_CMP_OP(F, OP, ND, S) \
  boolNDArray \
  F (const ND& m, const S& s) \
  { \
    return do_ms_binary_op<bool, ND::element_type, S> (m, s, OP); \
  }

#define NDS_CMP_OPS(ND, S) \
  NDS_CMP_OP (mx_el_lt, mx_inline_lt, ND, S) \
  NDS_CMP_OP (mx_el_le, mx_inline_le, ND, S) \
  NDS_CMP_OP (mx_el_ge, mx_inline_ge, ND, S) \
  NDS_CMP_OP (mx_el_gt, mx_inline_gt, ND, S) \
  NDS_CMP_OP (mx_el_eq, mx_inline_eq, ND, S) \
  NDS_CMP_OP (mx_el_ne, mx_inline_ne, ND, S)

#define NDS_BOOL_OP(F, OP, ND, S) \
  boolNDArray \
  F (const ND& m, const S& s) \
  { \
    MNANCHK (m, ND::element_type); \
    SNANCHK (s); \
    return do_ms_binary_op<bool, ND::element_type, S> (m, s, OP); \
  }

#define NDS_BOOL_OPS(ND, S) \
  NDS_BOOL_OP (mx_el_and,     mx_inline_and,     ND, S) \
  NDS_BOOL_OP (mx_el_or,      mx_inline_or,      ND, S) \
  NDS_BOOL_OP (mx_el_not_and, mx_inline_not_and, ND, S) \
  NDS_BOOL_OP (mx_el_not_or,  mx_inline_not_or,  ND, S) \
  NDS_BOOL_OP (mx_el_and_not, mx_inline_and_not, ND, S) \
  NDS_BOOL_OP (mx_el_or_not,  mx_inline_or_not,  ND, S)

// scalar by N-d matrix operations.

#define SND_BIN_OP(R, OP, S, ND, F) \
  R \
  OP (const S& s, const ND& m) \
  { \
    return do_sm_binary_op<R::element_type, S, ND::element_type> (s, m, F); \
  }

#define SND_BIN_OPS(R, S, ND) \
  SND_BIN_OP (R, operator +, S, ND, mx_inline_add) \
  SND_BIN_OP (R, operator -, S, ND, mx_inline_sub) \
  SND_BIN_OP (R, operator *, S, ND, mx_inline_mul) \
  SND_BIN_OP (R, operator /, S, ND, mx_inline_div)

#define SND_CMP_OP(F, OP, S, ND) \
  boolNDArray \
  F (const S& s, const ND& m) \
  { \
    return do_sm_binary_op<bool, S, ND::element_type> (s, m, OP); \
  }

#define SND_CMP_OPS(S, ND) \
  SND_CMP_OP (mx_el_lt, mx_inline_lt, S, ND) \
  SND_CMP_OP (mx_el_le, mx_inline_le, S, ND) \
  SND_CMP_OP (mx_el_ge, mx_inline_ge, S, ND) \
  SND_CMP_OP (mx_el_gt, mx_inline_gt, S, ND) \
  SND_CMP_OP (mx_el_eq, mx_inline_eq, S, ND) \
  SND_CMP_OP (mx_el_ne, mx_inline_ne, S, ND)

#define SND_BOOL_OP(F, OP, S, ND) \
  boolNDArray \
  F (const S& s, const ND& m) \
  { \
    SNANCHK (s); \
    MNANCHK (m, ND::element_type); \
    return do_sm_binary_op<bool, S, ND::element_type> (s, m, OP); \
  }

#define SND_BOOL_OPS(S, ND) \
  SND_BOOL_OP (mx_el_and,     mx_inline_and,     S, ND) \
  SND_BOOL_OP (mx_el_or,      mx_inline_or,      S, ND) \
  SND_BOOL_OP (mx_el_not_and, mx_inline_not_and, S, ND) \
  SND_BOOL_OP (mx_el_not_or,  mx_inline_not_or,  S, ND) \
  SND_BOOL_OP (mx_el_and_not, mx_inline_and_not, S, ND) \
  SND_BOOL_OP (mx_el_or_not,  mx_inline_or_not,  S, ND)

// N-d matrix by N-d matrix operations.

#define NDND_BIN_OP(R, OP, ND1, ND2, F) \
  R \
  OP (const ND1& m1, const ND2& m2) \
  { \
    return do_mm_binary_op<R::element_type, ND1::element_type, ND2::element_type> (m1, m2, F, F, F, #OP); \
  }

#define NDND_BIN_OPS(R, ND1, ND2) \
  NDND_BIN_OP (R, operator +, ND1, ND2, mx_inline_add) \
  NDND_BIN_OP (R, operator -, ND1, ND2, mx_inline_sub) \
  NDND_BIN_OP (R, product,    ND1, ND2, mx_inline_mul) \
  NDND_BIN_OP (R, quotient,   ND1, ND2, mx_inline_div)

#define NDND_CMP_OP(F, OP, ND1, ND2) \
  boolNDArray \
  F (const ND1& m1, const ND2& m2) \
  { \
    return do_mm_binary_op<bool, ND1::element_type, ND2::element_type> (m1, m2, OP, OP, OP, #F); \
  }

#define NDND_CMP_OPS(ND1, ND2) \
  NDND_CMP_OP (mx_el_lt, mx_inline_lt, ND1, ND2) \
  NDND_CMP_OP (mx_el_le, mx_inline_le, ND1, ND2) \
  NDND_CMP_OP (mx_el_ge, mx_inline_ge, ND1, ND2) \
  NDND_CMP_OP (mx_el_gt, mx_inline_gt, ND1, ND2) \
  NDND_CMP_OP (mx_el_eq, mx_inline_eq, ND1, ND2) \
  NDND_CMP_OP (mx_el_ne, mx_inline_ne, ND1, ND2)

#define NDND_BOOL_OP(F, OP, ND1, ND2) \
  boolNDArray \
  F (const ND1& m1, const ND2& m2) \
  { \
    MNANCHK (m1, ND1::element_type); \
    MNANCHK (m2, ND2::element_type); \
    return do_mm_binary_op<bool, ND1::element_type, ND2::element_type> (m1, m2, OP, OP, OP, #F); \
  }

#define NDND_BOOL_OPS(ND1, ND2) \
  NDND_BOOL_OP (mx_el_and,     mx_inline_and,     ND1, ND2) \
  NDND_BOOL_OP (mx_el_or,      mx_inline_or,      ND1, ND2) \
  NDND_BOOL_OP (mx_el_not_and, mx_inline_not_and, ND1, ND2) \
  NDND_BOOL_OP (mx_el_not_or,  mx_inline_not_or,  ND1, ND2) \
  NDND_BOOL_OP (mx_el_and_not, mx_inline_and_not, ND1, ND2) \
  NDND_BOOL_OP (mx_el_or_not,  mx_inline_or_not,  ND1, ND2)

// scalar by diagonal matrix operations.

#define SDM_BIN_OP(R, OP, S, DM) \
  R \
  operator OP (const S& s, const DM& dm) \
  { \
    R r (dm.rows (), dm.cols ()); \
 \
    for (octave_idx_type i = 0; i < dm.length (); i++) \
      r.dgxelem (i) = s OP dm.dgelem (i); \
 \
    return r; \
}

#define SDM_BIN_OPS(R, S, DM) \
  SDM_BIN_OP (R, *, S, DM)

// diagonal matrix by scalar operations.

#define DMS_BIN_OP(R, OP, DM, S) \
  R \
  operator OP (const DM& dm, const S& s) \
  { \
    R r (dm.rows (), dm.cols ()); \
 \
    for (octave_idx_type i = 0; i < dm.length (); i++) \
      r.dgxelem (i) = dm.dgelem (i) OP s; \
 \
    return r; \
  }

#define DMS_BIN_OPS(R, DM, S) \
  DMS_BIN_OP (R, *, DM, S) \
  DMS_BIN_OP (R, /, DM, S)

// matrix by diagonal matrix operations.

#define MDM_BIN_OP(R, OP, M, DM, OPEQ) \
R \
OP (const M& m, const DM& dm) \
{ \
  R r; \
 \
  octave_idx_type m_nr = m.rows (); \
  octave_idx_type m_nc = m.cols (); \
 \
  octave_idx_type dm_nr = dm.rows (); \
  octave_idx_type dm_nc = dm.cols (); \
 \
  if (m_nr != dm_nr || m_nc != dm_nc) \
    gripe_nonconformant (#OP, m_nr, m_nc, dm_nr, dm_nc); \
  else \
    { \
      r.resize (m_nr, m_nc); \
 \
      if (m_nr > 0 && m_nc > 0) \
        { \
          r = R (m); \
 \
          octave_idx_type len = dm.length (); \
 \
          for (octave_idx_type i = 0; i < len; i++) \
            r.elem (i, i) OPEQ dm.elem (i, i); \
        } \
    } \
 \
  return r; \
}

#define MDM_MULTIPLY_OP(R, M, DM, R_ZERO) \
R \
operator * (const M& m, const DM& dm) \
{ \
  R r; \
 \
  octave_idx_type m_nr = m.rows (); \
  octave_idx_type m_nc = m.cols (); \
 \
  octave_idx_type dm_nr = dm.rows (); \
  octave_idx_type dm_nc = dm.cols (); \
 \
  if (m_nc != dm_nr) \
    gripe_nonconformant ("operator *", m_nr, m_nc, dm_nr, dm_nc); \
  else \
    { \
      r = R (m_nr, dm_nc); \
      R::element_type *rd = r.fortran_vec (); \
      const M::element_type *md = m.data (); \
      const DM::element_type *dd = dm.data (); \
 \
      octave_idx_type len = dm.length (); \
      for (octave_idx_type i = 0; i < len; i++) \
        { \
          mx_inline_mul (m_nr, rd, md, dd[i]); \
          rd += m_nr; md += m_nr; \
        } \
      mx_inline_fill (m_nr * (dm_nc - len), rd, R_ZERO); \
    } \
 \
  return r; \
}

#define MDM_BIN_OPS(R, M, DM, R_ZERO) \
  MDM_BIN_OP (R, operator +, M, DM, +=) \
  MDM_BIN_OP (R, operator -, M, DM, -=) \
  MDM_MULTIPLY_OP (R, M, DM, R_ZERO)

// diagonal matrix by matrix operations.

#define DMM_BIN_OP(R, OP, DM, M, OPEQ, PREOP) \
R \
OP (const DM& dm, const M& m) \
{ \
  R r; \
 \
  octave_idx_type dm_nr = dm.rows (); \
  octave_idx_type dm_nc = dm.cols (); \
 \
  octave_idx_type m_nr = m.rows (); \
  octave_idx_type m_nc = m.cols (); \
 \
  if (dm_nr != m_nr || dm_nc != m_nc) \
    gripe_nonconformant (#OP, dm_nr, dm_nc, m_nr, m_nc); \
  else \
    { \
      if (m_nr > 0 && m_nc > 0) \
        { \
          r = R (PREOP m); \
 \
          octave_idx_type len = dm.length (); \
 \
          for (octave_idx_type i = 0; i < len; i++) \
            r.elem (i, i) OPEQ dm.elem (i, i); \
        } \
      else \
        r.resize (m_nr, m_nc); \
    } \
 \
  return r; \
}

#define DMM_MULTIPLY_OP(R, DM, M, R_ZERO) \
R \
operator * (const DM& dm, const M& m) \
{ \
  R r; \
 \
  octave_idx_type dm_nr = dm.rows (); \
  octave_idx_type dm_nc = dm.cols (); \
 \
  octave_idx_type m_nr = m.rows (); \
  octave_idx_type m_nc = m.cols (); \
 \
  if (dm_nc != m_nr) \
    gripe_nonconformant ("operator *", dm_nr, dm_nc, m_nr, m_nc); \
  else \
    { \
      r = R (dm_nr, m_nc); \
      R::element_type *rd = r.fortran_vec (); \
      const M::element_type *md = m.data (); \
      const DM::element_type *dd = dm.data (); \
 \
      octave_idx_type len = dm.length (); \
      for (octave_idx_type i = 0; i < m_nc; i++) \
        { \
          mx_inline_mul (len, rd, md, dd); \
          rd += len; md += m_nr; \
          mx_inline_fill (dm_nr - len, rd, R_ZERO); \
          rd += dm_nr - len; \
        } \
    } \
 \
  return r; \
}

#define DMM_BIN_OPS(R, DM, M, R_ZERO) \
  DMM_BIN_OP (R, operator +, DM, M, +=, ) \
  DMM_BIN_OP (R, operator -, DM, M, +=, -) \
  DMM_MULTIPLY_OP (R, DM, M, R_ZERO)

// diagonal matrix by diagonal matrix operations.

#define DMDM_BIN_OP(R, OP, DM1, DM2, F) \
  R \
  OP (const DM1& dm1, const DM2& dm2) \
  { \
    R r; \
 \
    octave_idx_type dm1_nr = dm1.rows (); \
    octave_idx_type dm1_nc = dm1.cols (); \
 \
    octave_idx_type dm2_nr = dm2.rows (); \
    octave_idx_type dm2_nc = dm2.cols (); \
 \
    if (dm1_nr != dm2_nr || dm1_nc != dm2_nc) \
      gripe_nonconformant (#OP, dm1_nr, dm1_nc, dm2_nr, dm2_nc); \
    else \
      { \
        r.resize (dm1_nr, dm1_nc); \
 \
        if (dm1_nr > 0 && dm1_nc > 0) \
          F (dm1.length (), r.fortran_vec (), dm1.data (), dm2.data ()); \
      } \
 \
    return r; \
  }

#define DMDM_BIN_OPS(R, DM1, DM2) \
  DMDM_BIN_OP (R, operator +, DM1, DM2, mx_inline_add) \
  DMDM_BIN_OP (R, operator -, DM1, DM2, mx_inline_sub) \
  DMDM_BIN_OP (R, product,    DM1, DM2, mx_inline_mul)

// scalar by N-d array min/max ops

#define SND_MINMAX_FCN(FCN, OP, T, S) \
T \
FCN (S d, const T& m) \
{ \
  return do_sm_binary_op<T::element_type, S, T::element_type> (d, m, mx_inline_x##FCN); \
}

#define NDS_MINMAX_FCN(FCN, OP, T, S) \
T \
FCN (const T& m, S d) \
{ \
  return do_ms_binary_op<T::element_type, T::element_type, S> (m, d, mx_inline_x##FCN); \
}

#define NDND_MINMAX_FCN(FCN, OP, T, S) \
T \
FCN (const T& a, const T& b) \
{ \
  return do_mm_binary_op<T::element_type, T::element_type, T::element_type> (a, b, mx_inline_x##FCN, mx_inline_x##FCN, mx_inline_x##FCN, #FCN); \
}

#define MINMAX_FCNS(T, S) \
  SND_MINMAX_FCN (min, <, T, S) \
  NDS_MINMAX_FCN (min, <, T, S) \
  NDND_MINMAX_FCN (min, <, T, S) \
  SND_MINMAX_FCN (max, >, T, S) \
  NDS_MINMAX_FCN (max, >, T, S) \
  NDND_MINMAX_FCN (max, >, T, S)

// permutation matrix by matrix ops and vice versa

#define PMM_MULTIPLY_OP(PM, M) \
M operator * (const PM& p, const M& x) \
{ \
  octave_idx_type nr = x.rows (); \
  octave_idx_type nc = x.columns (); \
  M result; \
  if (p.columns () != nr) \
    gripe_nonconformant ("operator *", p.rows (), p.columns (), nr, nc); \
  else \
    { \
      result = M (nr, nc); \
      result.assign (p.col_perm_vec (), idx_vector::colon, x); \
    } \
  \
  return result; \
}

#define MPM_MULTIPLY_OP(M, PM) \
M operator * (const M& x, const PM& p) \
{ \
  octave_idx_type nr = x.rows (); \
  octave_idx_type nc = x.columns (); \
  M result; \
  if (p.rows () != nc) \
    gripe_nonconformant ("operator *", nr, nc, p.rows (), p.columns ()); \
  else \
    result = x.index (idx_vector::colon, p.col_perm_vec ()); \
  \
  return result; \
}

#define PMM_BIN_OPS(R, PM, M) \
  PMM_MULTIPLY_OP(PM, M);

#define MPM_BIN_OPS(R, M, PM) \
  MPM_MULTIPLY_OP(M, PM);

#define NDND_MAPPER_BODY(R, NAME) \
  R retval (dims ()); \
  octave_idx_type n = numel (); \
  for (octave_idx_type i = 0; i < n; i++) \
    retval.xelem (i) = NAME (elem (i)); \
  return retval;

#endif
