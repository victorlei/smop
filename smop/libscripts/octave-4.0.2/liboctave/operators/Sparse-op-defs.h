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

#if !defined (octave_Sparse_op_defs_h)
#define octave_Sparse_op_defs_h 1

#include "Array-util.h"
#include "oct-locbuf.h"
#include "mx-inlines.cc"

// sparse matrix by scalar operations.

#define SPARSE_SMS_BIN_OP_1(R, F, OP, M, S)     \
  R \
  F (const M& m, const S& s) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
 \
    R r (nr, nc, (0.0 OP s)); \
 \
    for (octave_idx_type j = 0; j < nc; j++) \
      for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
        r.xelem (m.ridx (i), j) = m.data (i) OP s; \
    return r; \
  }

#define SPARSE_SMS_BIN_OP_2(R, F, OP, M, S)     \
  R \
  F (const M& m, const S& s) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
    octave_idx_type nz = m.nnz (); \
 \
    R r (nr, nc, nz); \
 \
    for (octave_idx_type i = 0; i < nz; i++) \
      { \
        r.xdata (i) = m.data (i) OP s; \
        r.xridx (i) = m.ridx (i); \
      } \
    for (octave_idx_type i = 0; i < nc + 1; i++) \
      r.xcidx (i) = m.cidx (i); \
    \
    r.maybe_compress (true); \
    return r; \
  }

#define SPARSE_SMS_BIN_OPS(R1, R2, M, S) \
  SPARSE_SMS_BIN_OP_1 (R1, operator +, +, M, S) \
  SPARSE_SMS_BIN_OP_1 (R1, operator -, -, M, S) \
  SPARSE_SMS_BIN_OP_2 (R2, operator *, *, M, S) \
  SPARSE_SMS_BIN_OP_2 (R2, operator /, /, M, S)

#define SPARSE_SMS_CMP_OP(F, OP, M, MZ, MC, S, SZ, SC)  \
  SparseBoolMatrix \
  F (const M& m, const S& s) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
    SparseBoolMatrix r; \
    \
    if (MC (MZ) OP SC (s)) \
      { \
        r = SparseBoolMatrix (nr, nc, true); \
        for (octave_idx_type j = 0; j < nc; j++) \
          for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
            if (! (MC (m.data (i)) OP SC (s))) \
              r.data (m.ridx (i) + j * nr) = false; \
        r.maybe_compress (true); \
      } \
    else \
      { \
        r = SparseBoolMatrix (nr, nc, m.nnz ()); \
        r.cidx (0) = static_cast<octave_idx_type> (0); \
        octave_idx_type nel = 0; \
        for (octave_idx_type j = 0; j < nc; j++) \
          { \
            for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
              if (MC (m.data (i)) OP SC (s)) \
                { \
                  r.ridx (nel) = m.ridx (i); \
                  r.data (nel++) = true; \
                } \
            r.cidx (j + 1) = nel; \
          } \
        r.maybe_compress (false); \
      } \
    return r; \
  }

#define SPARSE_SMS_CMP_OPS(M, MZ, CM, S, SZ, CS)        \
  SPARSE_SMS_CMP_OP (mx_el_lt, <,  M, MZ,   , S, SZ,   )        \
  SPARSE_SMS_CMP_OP (mx_el_le, <=, M, MZ,   , S, SZ,   )        \
  SPARSE_SMS_CMP_OP (mx_el_ge, >=, M, MZ,   , S, SZ,   )        \
  SPARSE_SMS_CMP_OP (mx_el_gt, >,  M, MZ,   , S, SZ,   )        \
  SPARSE_SMS_CMP_OP (mx_el_eq, ==, M, MZ,   , S, SZ,   )        \
  SPARSE_SMS_CMP_OP (mx_el_ne, !=, M, MZ,   , S, SZ,   )

#define SPARSE_SMS_EQNE_OPS(M, MZ, CM, S, SZ, CS)       \
  SPARSE_SMS_CMP_OP (mx_el_eq, ==, M, MZ,   , S, SZ,   )        \
  SPARSE_SMS_CMP_OP (mx_el_ne, !=, M, MZ,   , S, SZ,   )

#define SPARSE_SMS_BOOL_OP(F, OP, M, S, LHS_ZERO, RHS_ZERO) \
  SparseBoolMatrix \
  F (const M& m, const S& s) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
    SparseBoolMatrix r; \
    \
    if (nr > 0 && nc > 0) \
      { \
        if (LHS_ZERO OP (s != RHS_ZERO)) \
          { \
            r = SparseBoolMatrix (nr, nc, true); \
            for (octave_idx_type j = 0; j < nc; j++) \
              for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
                if (! ((m.data (i) != LHS_ZERO) OP (s != RHS_ZERO))) \
                  r.data (m.ridx (i) + j * nr) = false; \
            r.maybe_compress (true); \
          } \
        else \
          { \
            r = SparseBoolMatrix (nr, nc, m.nnz ()); \
            r.cidx (0) = static_cast<octave_idx_type> (0); \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < nc; j++) \
              { \
                for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
                  if ((m.data (i) != LHS_ZERO) OP (s != RHS_ZERO)) \
                    { \
                      r.ridx (nel) = m.ridx (i); \
                      r.data (nel++) = true; \
                    } \
                r.cidx (j + 1) = nel; \
              } \
            r.maybe_compress (false); \
          } \
      } \
    return r; \
  }

#define SPARSE_SMS_BOOL_OPS2(M, S, LHS_ZERO, RHS_ZERO) \
  SPARSE_SMS_BOOL_OP (mx_el_and, &&, M, S, LHS_ZERO, RHS_ZERO) \
  SPARSE_SMS_BOOL_OP (mx_el_or,  ||, M, S, LHS_ZERO, RHS_ZERO)

#define SPARSE_SMS_BOOL_OPS(M, S, ZERO) \
  SPARSE_SMS_BOOL_OPS2(M, S, ZERO, ZERO)

// scalar by sparse matrix operations.

#define SPARSE_SSM_BIN_OP_1(R, F, OP, S, M) \
  R \
  F (const S& s, const M& m) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
 \
    R r (nr, nc, (s OP 0.0)); \
 \
    for (octave_idx_type j = 0; j < nc; j++) \
      for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
        r.xelem (m.ridx (i), j) = s OP m.data (i); \
 \
    return r; \
  }

#define SPARSE_SSM_BIN_OP_2(R, F, OP, S, M) \
  R \
  F (const S& s, const M& m) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
    octave_idx_type nz = m.nnz (); \
 \
    R r (nr, nc, nz); \
 \
    for (octave_idx_type i = 0; i < nz; i++) \
      { \
        r.xdata (i) = s OP m.data (i); \
        r.xridx (i) = m.ridx (i); \
      } \
    for (octave_idx_type i = 0; i < nc + 1; i++) \
      r.xcidx (i) = m.cidx (i); \
 \
    r.maybe_compress(true); \
    return r; \
  }

#define SPARSE_SSM_BIN_OPS(R1, R2, S, M) \
  SPARSE_SSM_BIN_OP_1 (R1, operator +, +, S, M) \
  SPARSE_SSM_BIN_OP_1 (R1, operator -, -, S, M) \
  SPARSE_SSM_BIN_OP_2 (R2, operator *, *, S, M) \
  SPARSE_SSM_BIN_OP_2 (R2, operator /, /, S, M)

#define SPARSE_SSM_CMP_OP(F, OP, S, SZ, SC, M, MZ, MC)  \
  SparseBoolMatrix \
  F (const S& s, const M& m) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
    SparseBoolMatrix r; \
    \
    if (SC (s) OP SC (MZ)) \
      { \
        r = SparseBoolMatrix (nr, nc, true); \
        for (octave_idx_type j = 0; j < nc; j++) \
          for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
            if (! (SC (s) OP MC (m.data (i)))) \
              r.data (m.ridx (i) + j * nr) = false; \
        r.maybe_compress (true); \
      } \
    else \
      { \
        r = SparseBoolMatrix (nr, nc, m.nnz ()); \
        r.cidx (0) = static_cast<octave_idx_type> (0); \
        octave_idx_type nel = 0; \
        for (octave_idx_type j = 0; j < nc; j++) \
          { \
            for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
              if (SC (s) OP MC (m.data (i))) \
                { \
                  r.ridx (nel) = m.ridx (i); \
                  r.data (nel++) = true; \
                } \
            r.cidx (j + 1) = nel; \
          } \
        r.maybe_compress (false); \
      } \
    return r; \
  }

#define SPARSE_SSM_CMP_OPS(S, SZ, SC, M, MZ, MC)        \
  SPARSE_SSM_CMP_OP (mx_el_lt, <,  S, SZ,   , M, MZ,   )        \
  SPARSE_SSM_CMP_OP (mx_el_le, <=, S, SZ,   , M, MZ,   )        \
  SPARSE_SSM_CMP_OP (mx_el_ge, >=, S, SZ,   , M, MZ,   )        \
  SPARSE_SSM_CMP_OP (mx_el_gt, >,  S, SZ,   , M, MZ,   )        \
  SPARSE_SSM_CMP_OP (mx_el_eq, ==, S, SZ,   , M, MZ,   )        \
  SPARSE_SSM_CMP_OP (mx_el_ne, !=, S, SZ,   , M, MZ,   )

#define SPARSE_SSM_EQNE_OPS(S, SZ, SC, M, MZ, MC)       \
  SPARSE_SSM_CMP_OP (mx_el_eq, ==, S, SZ,   , M, MZ,   )        \
  SPARSE_SSM_CMP_OP (mx_el_ne, !=, S, SZ,   , M, MZ,   )

#define SPARSE_SSM_BOOL_OP(F, OP, S, M, LHS_ZERO, RHS_ZERO) \
  SparseBoolMatrix \
  F (const S& s, const M& m) \
  { \
    octave_idx_type nr = m.rows (); \
    octave_idx_type nc = m.cols (); \
    SparseBoolMatrix r; \
    \
    if (nr > 0 && nc > 0) \
      { \
        if ((s != LHS_ZERO) OP RHS_ZERO) \
          { \
            r = SparseBoolMatrix (nr, nc, true); \
            for (octave_idx_type j = 0; j < nc; j++) \
              for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
                if (! ((s != LHS_ZERO) OP (m.data (i) != RHS_ZERO))) \
                  r.data (m.ridx (i) + j * nr) = false; \
            r.maybe_compress (true); \
          } \
        else \
          { \
            r = SparseBoolMatrix (nr, nc, m.nnz ()); \
            r.cidx (0) = static_cast<octave_idx_type> (0); \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < nc; j++) \
              { \
                for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++) \
                  if ((s != LHS_ZERO) OP (m.data (i) != RHS_ZERO)) \
                    { \
                      r.ridx (nel) = m.ridx (i); \
                      r.data (nel++) = true; \
                    } \
                r.cidx (j + 1) = nel; \
              } \
            r.maybe_compress (false); \
          } \
      } \
    return r; \
  }

#define SPARSE_SSM_BOOL_OPS2(S, M, LHS_ZERO, RHS_ZERO) \
  SPARSE_SSM_BOOL_OP (mx_el_and, &&, S, M, LHS_ZERO, RHS_ZERO) \
  SPARSE_SSM_BOOL_OP (mx_el_or,  ||, S, M, LHS_ZERO, RHS_ZERO)

#define SPARSE_SSM_BOOL_OPS(S, M, ZERO) \
  SPARSE_SSM_BOOL_OPS2(S, M, ZERO, ZERO)

// sparse matrix by sparse matrix operations.

#define SPARSE_SMSM_BIN_OP_1(R, F, OP, M1, M2)  \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m1_nr == 1 && m1_nc == 1) \
      { \
        if (m1.elem (0,0) == 0.) \
          r = OP R (m2); \
        else \
          { \
            r = R (m2_nr, m2_nc, m1.data (0) OP 0.); \
            \
            for (octave_idx_type j = 0 ; j < m2_nc ; j++) \
              { \
                octave_quit (); \
                octave_idx_type idxj = j * m2_nr; \
                for (octave_idx_type i = m2.cidx (j) ; i < m2.cidx (j+1) ; i++) \
                  { \
                    octave_quit (); \
                    r.data (idxj + m2.ridx (i)) = m1.data (0) OP m2.data (i); \
                  } \
              } \
            r.maybe_compress (); \
          } \
      } \
    else if (m2_nr == 1 && m2_nc == 1) \
      { \
        if (m2.elem (0,0) == 0.) \
          r = R (m1); \
        else \
          { \
            r = R (m1_nr, m1_nc, 0. OP m2.data (0)); \
            \
            for (octave_idx_type j = 0 ; j < m1_nc ; j++) \
              { \
                octave_quit (); \
                octave_idx_type idxj = j * m1_nr; \
                for (octave_idx_type i = m1.cidx (j) ; i < m1.cidx (j+1) ; i++) \
                  { \
                    octave_quit (); \
                    r.data (idxj + m1.ridx (i)) = m1.data (i) OP m2.data (0); \
                  } \
              } \
            r.maybe_compress (); \
          } \
      } \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
        r = R (m1_nr, m1_nc, (m1.nnz () + m2.nnz ())); \
        \
        octave_idx_type jx = 0; \
        r.cidx (0) = 0; \
        for (octave_idx_type i = 0 ; i < m1_nc ; i++) \
          { \
            octave_idx_type  ja = m1.cidx (i); \
            octave_idx_type  ja_max = m1.cidx (i+1); \
            bool ja_lt_max= ja < ja_max; \
            \
            octave_idx_type  jb = m2.cidx (i); \
            octave_idx_type  jb_max = m2.cidx (i+1); \
            bool jb_lt_max = jb < jb_max; \
            \
            while (ja_lt_max || jb_lt_max) \
              { \
                octave_quit (); \
                if ((! jb_lt_max) || \
                      (ja_lt_max && (m1.ridx (ja) < m2.ridx (jb)))) \
                  { \
                    r.ridx (jx) = m1.ridx (ja); \
                    r.data (jx) = m1.data (ja) OP 0.; \
                    jx++; \
                    ja++; \
                    ja_lt_max= ja < ja_max; \
                  } \
                else if ((! ja_lt_max) || \
                     (jb_lt_max && (m2.ridx (jb) < m1.ridx (ja)))) \
                  { \
                    r.ridx (jx) = m2.ridx (jb); \
                    r.data (jx) = 0. OP m2.data (jb); \
                    jx++; \
                    jb++; \
                    jb_lt_max= jb < jb_max; \
                  } \
                else \
                  { \
                     if ((m1.data (ja) OP m2.data (jb)) != 0.) \
                       { \
                          r.data (jx) = m1.data (ja) OP m2.data (jb); \
                          r.ridx (jx) = m1.ridx (ja); \
                          jx++; \
                       } \
                     ja++; \
                     ja_lt_max= ja < ja_max; \
                     jb++; \
                     jb_lt_max= jb < jb_max; \
                  } \
              } \
            r.cidx (i+1) = jx; \
          } \
        \
        r.maybe_compress (); \
      } \
 \
    return r; \
  }

#define SPARSE_SMSM_BIN_OP_2(R, F, OP, M1, M2)  \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m1_nr == 1 && m1_nc == 1) \
      { \
        if (m1.elem (0,0) == 0.) \
          r = R (m2_nr, m2_nc); \
        else \
          { \
            r = R (m2); \
            octave_idx_type m2_nnz = m2.nnz (); \
            \
            for (octave_idx_type i = 0 ; i < m2_nnz ; i++) \
              { \
                octave_quit (); \
                r.data (i) = m1.data (0) OP r.data (i); \
              } \
            r.maybe_compress (); \
          } \
      } \
    else if (m2_nr == 1 && m2_nc == 1) \
      { \
        if (m2.elem (0,0) == 0.) \
          r = R (m1_nr, m1_nc); \
        else \
          { \
            r = R (m1); \
            octave_idx_type m1_nnz = m1.nnz (); \
            \
            for (octave_idx_type i = 0 ; i < m1_nnz ; i++) \
              { \
                octave_quit (); \
                r.data (i) = r.data (i) OP m2.data (0); \
              } \
            r.maybe_compress (); \
          } \
      } \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
        r = R (m1_nr, m1_nc, (m1.nnz () > m2.nnz () ? m1.nnz () : m2.nnz ())); \
        \
        octave_idx_type jx = 0; \
        r.cidx (0) = 0; \
        for (octave_idx_type i = 0 ; i < m1_nc ; i++) \
          { \
            octave_idx_type  ja = m1.cidx (i); \
            octave_idx_type  ja_max = m1.cidx (i+1); \
            bool ja_lt_max= ja < ja_max; \
            \
            octave_idx_type  jb = m2.cidx (i); \
            octave_idx_type  jb_max = m2.cidx (i+1); \
            bool jb_lt_max = jb < jb_max; \
            \
            while (ja_lt_max || jb_lt_max) \
              { \
                octave_quit (); \
                if ((! jb_lt_max) || \
                      (ja_lt_max && (m1.ridx (ja) < m2.ridx (jb)))) \
                  { \
                     ja++; ja_lt_max= ja < ja_max; \
                  } \
                else if ((! ja_lt_max) || \
                     (jb_lt_max && (m2.ridx (jb) < m1.ridx (ja)))) \
                  { \
                     jb++; jb_lt_max= jb < jb_max; \
                  } \
                else \
                  { \
                     if ((m1.data (ja) OP m2.data (jb)) != 0.) \
                       { \
                          r.data (jx) = m1.data (ja) OP m2.data (jb); \
                          r.ridx (jx) = m1.ridx (ja); \
                          jx++; \
                       } \
                     ja++; ja_lt_max= ja < ja_max; \
                     jb++; jb_lt_max= jb < jb_max; \
                  } \
              } \
            r.cidx (i+1) = jx; \
          } \
        \
        r.maybe_compress (); \
      } \
 \
    return r; \
  }

#define SPARSE_SMSM_BIN_OP_3(R, F, OP, M1, M2)  \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m1_nr == 1 && m1_nc == 1) \
      { \
        if ((m1.elem (0,0) OP Complex ()) == Complex ()) \
          { \
            octave_idx_type m2_nnz = m2.nnz (); \
            r = R (m2); \
            for (octave_idx_type i = 0 ; i < m2_nnz ; i++) \
              r.data (i) = m1.elem (0,0) OP r.data (i); \
            r.maybe_compress (); \
          } \
        else \
          { \
            r = R (m2_nr, m2_nc, m1.elem (0,0) OP Complex ()); \
            for (octave_idx_type j = 0 ; j < m2_nc ; j++) \
              { \
                octave_quit (); \
                octave_idx_type idxj = j * m2_nr; \
                for (octave_idx_type i = m2.cidx (j) ; i < m2.cidx (j+1) ; i++) \
                  { \
                    octave_quit (); \
                    r.data (idxj + m2.ridx (i)) = m1.elem (0,0) OP m2.data (i); \
                  } \
              } \
            r.maybe_compress (); \
          } \
      } \
    else if (m2_nr == 1 && m2_nc == 1) \
      { \
        if ((Complex () OP m1.elem (0,0)) == Complex ()) \
          { \
            octave_idx_type m1_nnz = m1.nnz (); \
            r = R (m1); \
            for (octave_idx_type i = 0 ; i < m1_nnz ; i++) \
              r.data (i) = r.data (i) OP m2.elem (0,0); \
            r.maybe_compress (); \
          } \
        else \
          { \
            r = R (m1_nr, m1_nc, Complex () OP m2.elem (0,0)); \
            for (octave_idx_type j = 0 ; j < m1_nc ; j++) \
              { \
                octave_quit (); \
                octave_idx_type idxj = j * m1_nr; \
                for (octave_idx_type i = m1.cidx (j) ; i < m1.cidx (j+1) ; i++) \
                  { \
                    octave_quit (); \
                    r.data (idxj + m1.ridx (i)) = m1.data (i) OP m2.elem (0,0); \
                  } \
              } \
            r.maybe_compress (); \
          } \
      } \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
 \
        /* FIXME: Kludge... Always double/Complex, so Complex () */ \
        r = R (m1_nr, m1_nc, (Complex () OP Complex ())); \
        \
        for (octave_idx_type i = 0 ; i < m1_nc ; i++) \
          { \
            octave_idx_type  ja = m1.cidx (i); \
            octave_idx_type  ja_max = m1.cidx (i+1); \
            bool ja_lt_max= ja < ja_max; \
            \
            octave_idx_type  jb = m2.cidx (i); \
            octave_idx_type  jb_max = m2.cidx (i+1); \
            bool jb_lt_max = jb < jb_max; \
            \
            while (ja_lt_max || jb_lt_max) \
              { \
                octave_quit (); \
                if ((! jb_lt_max) || \
                      (ja_lt_max && (m1.ridx (ja) < m2.ridx (jb)))) \
                  { \
                    /* keep those kludges coming */ \
                    r.elem (m1.ridx (ja),i) = m1.data (ja) OP Complex (); \
                    ja++; \
                    ja_lt_max= ja < ja_max; \
                  } \
                else if ((! ja_lt_max) || \
                     (jb_lt_max && (m2.ridx (jb) < m1.ridx (ja)))) \
                  { \
                    /* keep those kludges coming */ \
                    r.elem (m2.ridx (jb),i) = Complex () OP m2.data (jb);  \
                    jb++; \
                    jb_lt_max= jb < jb_max; \
                  } \
                else \
                  { \
                    r.elem (m1.ridx (ja),i) = m1.data (ja) OP m2.data (jb); \
                    ja++; \
                    ja_lt_max= ja < ja_max; \
                    jb++; \
                    jb_lt_max= jb < jb_max; \
                  } \
              } \
          } \
        r.maybe_compress (true); \
      } \
 \
    return r; \
  }

// Note that SM ./ SM needs to take into account the NaN and Inf values
// implied by the division by zero.
// FIXME: Are the NaNs double(NaN) or Complex(NaN,Nan) in the complex case?
#define SPARSE_SMSM_BIN_OPS(R1, R2, M1, M2)  \
  SPARSE_SMSM_BIN_OP_1 (R1, operator +,  +, M1, M2) \
  SPARSE_SMSM_BIN_OP_1 (R1, operator -,  -, M1, M2) \
  SPARSE_SMSM_BIN_OP_2 (R2, product,     *, M1, M2) \
  SPARSE_SMSM_BIN_OP_3 (R2, quotient,    /, M1, M2)

// FIXME: this macro duplicates the bodies of the template functions
// defined in the SPARSE_SSM_CMP_OP and SPARSE_SMS_CMP_OP macros.

#define SPARSE_SMSM_CMP_OP(F, OP, M1, Z1, C1, M2, Z2, C2)       \
  SparseBoolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    SparseBoolMatrix r; \
    \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
    \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
    \
    if (m1_nr == 1 && m1_nc == 1) \
      { \
    if (C1 (m1.elem (0,0)) OP C2 (Z2)) \
          { \
            r = SparseBoolMatrix (m2_nr, m2_nc, true); \
            for (octave_idx_type j = 0; j < m2_nc; j++) \
              for (octave_idx_type i = m2.cidx (j); i < m2.cidx (j+1); i++) \
                if (! (C1 (m1.elem (0,0)) OP C2 (m2.data (i)))) \
                  r.data (m2.ridx (i) + j * m2_nr) = false; \
            r.maybe_compress (true); \
          } \
        else \
          { \
            r = SparseBoolMatrix (m2_nr, m2_nc, m2.nnz ()); \
            r.cidx (0) = static_cast<octave_idx_type> (0); \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m2_nc; j++) \
              { \
                for (octave_idx_type i = m2.cidx (j); i < m2.cidx (j+1); i++) \
                  if (C1 (m1.elem (0,0)) OP C2 (m2.data (i))) \
                    { \
                      r.ridx (nel) = m2.ridx (i); \
                      r.data (nel++) = true; \
                    } \
                r.cidx (j + 1) = nel; \
              } \
            r.maybe_compress (false); \
          } \
      } \
    else if (m2_nr == 1 && m2_nc == 1) \
      { \
        if (C1 (Z1) OP C2 (m2.elem (0,0))) \
          { \
            r = SparseBoolMatrix (m1_nr, m1_nc, true); \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              for (octave_idx_type i = m1.cidx (j); i < m1.cidx (j+1); i++) \
                if (! (C1 (m1.data (i)) OP C2 (m2.elem (0,0)))) \
                  r.data (m1.ridx (i) + j * m1_nr) = false; \
            r.maybe_compress (true); \
          } \
        else \
          { \
            r = SparseBoolMatrix (m1_nr, m1_nc, m1.nnz ()); \
            r.cidx (0) = static_cast<octave_idx_type> (0); \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              { \
                for (octave_idx_type i = m1.cidx (j); i < m1.cidx (j+1); i++) \
                  if (C1 (m1.data (i)) OP C2 (m2.elem (0,0))) \
                    { \
                      r.ridx (nel) = m1.ridx (i); \
                      r.data (nel++) = true; \
                    } \
                r.cidx (j + 1) = nel; \
              } \
            r.maybe_compress (false); \
          } \
      } \
    else if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
        if (m1_nr != 0 || m1_nc != 0) \
          { \
            if (C1 (Z1) OP C2 (Z2)) \
              { \
                r = SparseBoolMatrix (m1_nr, m1_nc, true); \
                for (octave_idx_type j = 0; j < m1_nc; j++) \
                  { \
                     octave_idx_type i1 = m1.cidx (j); \
                     octave_idx_type e1 = m1.cidx (j+1); \
                     octave_idx_type i2 = m2.cidx (j); \
                     octave_idx_type e2 = m2.cidx (j+1); \
                     while (i1 < e1 || i2 < e2) \
                       { \
                         if (i1 == e1 || (i2 < e2 && m1.ridx (i1) > m2.ridx (i2))) \
                           { \
                             if (! (C1 (Z1) OP C2 (m2.data (i2)))) \
                               r.data (m2.ridx (i2) + j * m1_nr) = false; \
                             i2++; \
                           } \
                         else if (i2 == e2 || m1.ridx (i1) < m2.ridx (i2)) \
                           { \
                             if (! (C1 (m1.data (i1)) OP C2 (Z2))) \
                               r.data (m1.ridx (i1) + j * m1_nr) = false; \
                             i1++; \
                           } \
                         else \
                           { \
                             if (! (C1 (m1.data (i1)) OP C2 (m2.data (i2)))) \
                               r.data (m1.ridx (i1) + j * m1_nr) = false; \
                             i1++; \
                             i2++; \
                           } \
                       } \
                  } \
                r.maybe_compress (true); \
              } \
            else \
              { \
                r = SparseBoolMatrix (m1_nr, m1_nc, m1.nnz () + m2.nnz ()); \
                r.cidx (0) = static_cast<octave_idx_type> (0); \
                octave_idx_type nel = 0; \
                for (octave_idx_type j = 0; j < m1_nc; j++) \
                  { \
                     octave_idx_type i1 = m1.cidx (j); \
                     octave_idx_type e1 = m1.cidx (j+1); \
                     octave_idx_type i2 = m2.cidx (j); \
                     octave_idx_type e2 = m2.cidx (j+1); \
                     while (i1 < e1 || i2 < e2) \
                       { \
                         if (i1 == e1 || (i2 < e2 && m1.ridx (i1) > m2.ridx (i2))) \
                           { \
                             if (C1 (Z1) OP C2 (m2.data (i2))) \
                               { \
                                 r.ridx (nel) = m2.ridx (i2); \
                                 r.data (nel++) = true; \
                               } \
                             i2++; \
                           } \
                         else if (i2 == e2 || m1.ridx (i1) < m2.ridx (i2)) \
                           { \
                             if (C1 (m1.data (i1)) OP C2 (Z2)) \
                               { \
                                 r.ridx (nel) = m1.ridx (i1); \
                                 r.data (nel++) = true; \
                               } \
                             i1++; \
                           } \
                         else \
                           { \
                             if (C1 (m1.data (i1)) OP C2 (m2.data (i2))) \
                               { \
                                 r.ridx (nel) = m1.ridx (i1); \
                                 r.data (nel++) = true; \
                               } \
                             i1++; \
                             i2++; \
                           } \
                       } \
                     r.cidx (j + 1) = nel; \
                  } \
                r.maybe_compress (false); \
              } \
          } \
      }       \
    else \
      { \
        if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
          gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
    return r; \
  }

#define SPARSE_SMSM_CMP_OPS(M1, Z1, C1, M2, Z2, C2)  \
  SPARSE_SMSM_CMP_OP (mx_el_lt, <,  M1, Z1,   , M2, Z2,   ) \
  SPARSE_SMSM_CMP_OP (mx_el_le, <=, M1, Z1,   , M2, Z2,   ) \
  SPARSE_SMSM_CMP_OP (mx_el_ge, >=, M1, Z1,   , M2, Z2,   ) \
  SPARSE_SMSM_CMP_OP (mx_el_gt, >,  M1, Z1,   , M2, Z2,   ) \
  SPARSE_SMSM_CMP_OP (mx_el_eq, ==, M1, Z1,   , M2, Z2,   ) \
  SPARSE_SMSM_CMP_OP (mx_el_ne, !=, M1, Z1,   , M2, Z2,   )

#define SPARSE_SMSM_EQNE_OPS(M1, Z1, C1, M2, Z2, C2)  \
  SPARSE_SMSM_CMP_OP (mx_el_eq, ==, M1, Z1,   , M2, Z2,   ) \
  SPARSE_SMSM_CMP_OP (mx_el_ne, !=, M1, Z1,   , M2, Z2,   )

// FIXME: this macro duplicates the bodies of the template functions
// defined in the SPARSE_SSM_BOOL_OP and SPARSE_SMS_BOOL_OP macros.

#define SPARSE_SMSM_BOOL_OP(F, OP, M1, M2, LHS_ZERO, RHS_ZERO) \
  SparseBoolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    SparseBoolMatrix r; \
    \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
    \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
    \
    if (m1_nr == 1 && m1_nc == 1) \
      { \
        if (m2_nr > 0 && m2_nc > 0) \
          { \
            if ((m1.elem (0,0) != LHS_ZERO) OP RHS_ZERO) \
              { \
                r = SparseBoolMatrix (m2_nr, m2_nc, true); \
                for (octave_idx_type j = 0; j < m2_nc; j++) \
                  for (octave_idx_type i = m2.cidx (j); i < m2.cidx (j+1); i++) \
                    if (! ((m1.elem (0,0) != LHS_ZERO) OP (m2.data (i) != RHS_ZERO))) \
                      r.data (m2.ridx (i) + j * m2_nr) = false; \
                r.maybe_compress (true); \
              } \
            else \
              { \
                r = SparseBoolMatrix (m2_nr, m2_nc, m2.nnz ()); \
                r.cidx (0) = static_cast<octave_idx_type> (0); \
                octave_idx_type nel = 0; \
                for (octave_idx_type j = 0; j < m2_nc; j++) \
                  { \
                    for (octave_idx_type i = m2.cidx (j); i < m2.cidx (j+1); i++) \
                      if ((m1.elem (0,0) != LHS_ZERO) OP (m2.data (i) != RHS_ZERO)) \
                        { \
                          r.ridx (nel) = m2.ridx (i); \
                          r.data (nel++) = true; \
                        } \
                    r.cidx (j + 1) = nel; \
                  } \
                r.maybe_compress (false); \
              } \
          } \
      } \
    else if (m2_nr == 1 && m2_nc == 1) \
      { \
        if (m1_nr > 0 && m1_nc > 0) \
          { \
            if (LHS_ZERO OP (m2.elem (0,0) != RHS_ZERO)) \
              { \
                r = SparseBoolMatrix (m1_nr, m1_nc, true); \
                for (octave_idx_type j = 0; j < m1_nc; j++) \
                  for (octave_idx_type i = m1.cidx (j); i < m1.cidx (j+1); i++) \
                    if (! ((m1.data (i) != LHS_ZERO) OP (m2.elem (0,0) != RHS_ZERO))) \
                      r.data (m1.ridx (i) + j * m1_nr) = false; \
                r.maybe_compress (true); \
              } \
            else \
              { \
                r = SparseBoolMatrix (m1_nr, m1_nc, m1.nnz ()); \
                r.cidx (0) = static_cast<octave_idx_type> (0); \
                octave_idx_type nel = 0; \
                for (octave_idx_type j = 0; j < m1_nc; j++) \
                  { \
                    for (octave_idx_type i = m1.cidx (j); i < m1.cidx (j+1); i++) \
                      if ((m1.data (i) != LHS_ZERO) OP (m2.elem (0,0) != RHS_ZERO)) \
                        { \
                          r.ridx (nel) = m1.ridx (i); \
                          r.data (nel++) = true; \
                        } \
                    r.cidx (j + 1) = nel; \
                  } \
                r.maybe_compress (false); \
              } \
          } \
      } \
    else if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
        if (m1_nr != 0 || m1_nc != 0) \
          { \
            r = SparseBoolMatrix (m1_nr, m1_nc, m1.nnz () + m2.nnz ()); \
            r.cidx (0) = static_cast<octave_idx_type> (0); \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              { \
                octave_idx_type i1 = m1.cidx (j); \
                octave_idx_type e1 = m1.cidx (j+1); \
                octave_idx_type i2 = m2.cidx (j); \
                octave_idx_type e2 = m2.cidx (j+1); \
                while (i1 < e1 || i2 < e2) \
                  { \
                    if (i1 == e1 || (i2 < e2 && m1.ridx (i1) > m2.ridx (i2))) \
                      { \
                        if (LHS_ZERO OP m2.data (i2) != RHS_ZERO) \
                          { \
                            r.ridx (nel) = m2.ridx (i2); \
                            r.data (nel++) = true; \
                          } \
                        i2++; \
                      } \
                    else if (i2 == e2 || m1.ridx (i1) < m2.ridx (i2)) \
                      { \
                        if (m1.data (i1) != LHS_ZERO OP RHS_ZERO) \
                          { \
                            r.ridx (nel) = m1.ridx (i1); \
                            r.data (nel++) = true; \
                          } \
                        i1++; \
                      } \
                    else \
                      { \
                        if (m1.data (i1) != LHS_ZERO OP m2.data (i2) != RHS_ZERO) \
                          { \
                            r.ridx (nel) = m1.ridx (i1); \
                            r.data (nel++) = true; \
                          } \
                        i1++; \
                        i2++; \
                      } \
                  } \
                r.cidx (j + 1) = nel; \
              } \
            r.maybe_compress (false); \
          } \
      }       \
    else \
      { \
        if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
          gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
    return r; \
  }

#define SPARSE_SMSM_BOOL_OPS2(M1, M2, LHS_ZERO, RHS_ZERO) \
  SPARSE_SMSM_BOOL_OP (mx_el_and, &&, M1, M2, LHS_ZERO, RHS_ZERO) \
  SPARSE_SMSM_BOOL_OP (mx_el_or,  ||, M1, M2, LHS_ZERO, RHS_ZERO)

#define SPARSE_SMSM_BOOL_OPS(M1, M2, ZERO) \
  SPARSE_SMSM_BOOL_OPS2(M1, M2, ZERO, ZERO)

// matrix by sparse matrix operations.

#define SPARSE_MSM_BIN_OP_1(R, F, OP, M1, M2)   \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m2_nr == 1 && m2_nc == 1) \
      r = R (m1 OP m2.elem (0,0)); \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
        r = R (F (m1, m2.matrix_value ())); \
      } \
    return r; \
  }

#define SPARSE_MSM_BIN_OP_2(R, F, OP, M1, M2) \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m2_nr == 1 && m2_nc == 1) \
      r = R (m1 OP m2.elem (0,0)); \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
        if (do_mx_check (m1, mx_inline_all_finite<M1::element_type>)) \
          { \
            /* Sparsity pattern is preserved. */ \
            octave_idx_type m2_nz = m2.nnz (); \
            r = R (m2_nr, m2_nc, m2_nz); \
            for (octave_idx_type j = 0, k = 0; j < m2_nc; j++) \
              { \
                octave_quit (); \
                for (octave_idx_type i = m2.cidx (j); i < m2.cidx (j+1); i++) \
                  { \
                    octave_idx_type mri = m2.ridx (i); \
                    R::element_type x = m1(mri, j) OP m2.data (i); \
                    if (x != 0.0) \
                      { \
                        r.xdata (k) = x; \
                        r.xridx (k) = m2.ridx (i); \
                        k++; \
                      } \
                  } \
                r.xcidx (j+1) = k; \
              } \
            r.maybe_compress (false); \
            return r; \
          } \
        else \
          r = R (F (m1, m2.matrix_value ())); \
      } \
 \
    return r; \
  }

// FIXME: Pass a specific ZERO value
#define SPARSE_MSM_BIN_OPS(R1, R2, M1, M2) \
  SPARSE_MSM_BIN_OP_1 (R1, operator +,  +, M1, M2) \
  SPARSE_MSM_BIN_OP_1 (R1, operator -,  -, M1, M2) \
  SPARSE_MSM_BIN_OP_2 (R2, product,     *, M1, M2) \
  SPARSE_MSM_BIN_OP_1 (R2, quotient,    /, M1, M2)

#define SPARSE_MSM_CMP_OP(F, OP, M1, C1, M2, C2)        \
  SparseBoolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    SparseBoolMatrix r; \
    \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
    \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
    \
    if (m2_nr == 1 && m2_nc == 1) \
      r = SparseBoolMatrix (F (m1, m2.elem (0,0))); \
    else if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
        if (m1_nr != 0 || m1_nc != 0) \
          { \
            /* Count num of nonzero elements */ \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              for (octave_idx_type i = 0; i < m1_nr; i++) \
                if (C1 (m1.elem (i, j)) OP C2 (m2.elem (i, j))) \
                  nel++; \
            \
            r = SparseBoolMatrix (m1_nr, m1_nc, nel); \
            \
            octave_idx_type ii = 0; \
            r.cidx (0) = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              { \
                for (octave_idx_type i = 0; i < m1_nr; i++) \
                  { \
                    bool el = C1 (m1.elem (i, j)) OP C2 (m2.elem (i, j)); \
                    if (el) \
                      { \
                        r.data (ii) = el; \
                        r.ridx (ii++) = i; \
                      } \
                  } \
                r.cidx (j+1) = ii; \
              } \
          } \
      }       \
    else \
      { \
        if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
          gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
    return r; \
  }

#define SPARSE_MSM_CMP_OPS(M1, Z1, C1, M2, Z2, C2)  \
  SPARSE_MSM_CMP_OP (mx_el_lt, <,  M1,   , M2,   ) \
  SPARSE_MSM_CMP_OP (mx_el_le, <=, M1,   , M2,   ) \
  SPARSE_MSM_CMP_OP (mx_el_ge, >=, M1,   , M2,   ) \
  SPARSE_MSM_CMP_OP (mx_el_gt, >,  M1,   , M2,   ) \
  SPARSE_MSM_CMP_OP (mx_el_eq, ==, M1,   , M2,   ) \
  SPARSE_MSM_CMP_OP (mx_el_ne, !=, M1,   , M2,   )

#define SPARSE_MSM_EQNE_OPS(M1, Z1, C1, M2, Z2, C2)  \
  SPARSE_MSM_CMP_OP (mx_el_eq, ==, M1,   , M2,   ) \
  SPARSE_MSM_CMP_OP (mx_el_ne, !=, M1,   , M2,   )

#define SPARSE_MSM_BOOL_OP(F, OP, M1, M2, LHS_ZERO, RHS_ZERO) \
  SparseBoolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    SparseBoolMatrix r; \
    \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
    \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
    \
    if (m2_nr == 1 && m2_nc == 1) \
      r = SparseBoolMatrix (F (m1, m2.elem (0,0))); \
    else if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
        if (m1_nr != 0 || m1_nc != 0) \
          { \
            /* Count num of nonzero elements */ \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              for (octave_idx_type i = 0; i < m1_nr; i++) \
                if ((m1.elem (i, j) != LHS_ZERO) \
                    OP (m2.elem (i, j) != RHS_ZERO)) \
                  nel++; \
            \
            r = SparseBoolMatrix (m1_nr, m1_nc, nel); \
            \
            octave_idx_type ii = 0; \
            r.cidx (0) = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              { \
                for (octave_idx_type i = 0; i < m1_nr; i++) \
                  { \
                    bool el = (m1.elem (i, j) != LHS_ZERO) \
                      OP (m2.elem (i, j) != RHS_ZERO);     \
                    if (el) \
                      { \
                        r.data (ii) = el; \
                        r.ridx (ii++) = i; \
                      } \
                  } \
                r.cidx (j+1) = ii; \
              } \
          } \
      }       \
    else \
      { \
        if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
          gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
    return r; \
  }

#define SPARSE_MSM_BOOL_OPS2(M1, M2, LHS_ZERO, RHS_ZERO) \
  SPARSE_MSM_BOOL_OP (mx_el_and, &&, M1, M2, LHS_ZERO, RHS_ZERO) \
  SPARSE_MSM_BOOL_OP (mx_el_or,  ||, M1, M2, LHS_ZERO, RHS_ZERO)

#define SPARSE_MSM_BOOL_OPS(M1, M2, ZERO) \
  SPARSE_MSM_BOOL_OPS2(M1, M2, ZERO, ZERO)

// sparse matrix by matrix operations.

#define SPARSE_SMM_BIN_OP_1(R, F, OP, M1, M2)   \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m1_nr == 1 && m1_nc == 1) \
      r = R (m1.elem (0,0) OP m2); \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
        r = R (m1.matrix_value () OP m2); \
      } \
    return r; \
  }

// sm .* m preserves sparsity if m contains no Infs nor Nans.
#define SPARSE_SMM_BIN_OP_2_CHECK_product(ET) \
  do_mx_check (m2, mx_inline_all_finite<ET>)

// sm ./ m preserves sparsity if m contains no NaNs or zeros.
#define SPARSE_SMM_BIN_OP_2_CHECK_quotient(ET) \
  ! do_mx_check (m2, mx_inline_any_nan<ET>) && m2.nnz () == m2.numel ()

#define SPARSE_SMM_BIN_OP_2(R, F, OP, M1, M2) \
  R \
  F (const M1& m1, const M2& m2) \
  { \
    R r; \
 \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
 \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
 \
    if (m1_nr == 1 && m1_nc == 1) \
      r = R (m1.elem (0,0) OP m2); \
    else if (m1_nr != m2_nr || m1_nc != m2_nc) \
      gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
    else \
      { \
        if (SPARSE_SMM_BIN_OP_2_CHECK_ ## F(M2::element_type)) \
          { \
            /* Sparsity pattern is preserved. */ \
            octave_idx_type m1_nz = m1.nnz (); \
            r = R (m1_nr, m1_nc, m1_nz); \
            for (octave_idx_type j = 0, k = 0; j < m1_nc; j++) \
              { \
                octave_quit (); \
                for (octave_idx_type i = m1.cidx (j); i < m1.cidx (j+1); i++) \
                  { \
                    octave_idx_type mri = m1.ridx (i); \
                    R::element_type x = m1.data (i) OP m2 (mri, j); \
                    if (x != 0.0) \
                      { \
                        r.xdata (k) = x; \
                        r.xridx (k) = m1.ridx (i); \
                        k++; \
                      } \
                  } \
                r.xcidx (j+1) = k; \
              } \
            r.maybe_compress (false); \
            return r; \
          } \
        else \
          r = R (F (m1.matrix_value (), m2)); \
      } \
 \
    return r; \
  }

#define SPARSE_SMM_BIN_OPS(R1, R2, M1, M2) \
  SPARSE_SMM_BIN_OP_1 (R1, operator +,  +, M1, M2) \
  SPARSE_SMM_BIN_OP_1 (R1, operator -,  -, M1, M2) \
  SPARSE_SMM_BIN_OP_2 (R2, product,     *, M1, M2) \
  SPARSE_SMM_BIN_OP_2 (R2, quotient,    /, M1, M2)

#define SPARSE_SMM_CMP_OP(F, OP, M1, C1, M2, C2)        \
  SparseBoolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    SparseBoolMatrix r; \
    \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
    \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
    \
    if (m1_nr == 1 && m1_nc == 1) \
      r = SparseBoolMatrix (F (m1.elem (0,0), m2)); \
    else if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
        if (m1_nr != 0 || m1_nc != 0) \
          { \
            /* Count num of nonzero elements */ \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              for (octave_idx_type i = 0; i < m1_nr; i++) \
                if (C1 (m1.elem (i, j)) OP C2 (m2.elem (i, j))) \
                  nel++; \
            \
            r = SparseBoolMatrix (m1_nr, m1_nc, nel); \
            \
            octave_idx_type ii = 0; \
            r.cidx (0) = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              { \
                for (octave_idx_type i = 0; i < m1_nr; i++) \
                  { \
                    bool el = C1 (m1.elem (i, j)) OP C2 (m2.elem (i, j)); \
                    if (el) \
                      { \
                        r.data (ii) = el; \
                        r.ridx (ii++) = i; \
                      } \
                  } \
                r.cidx (j+1) = ii; \
              } \
          } \
      }       \
    else \
      { \
        if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
          gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
    return r; \
  }

#define SPARSE_SMM_CMP_OPS(M1, Z1, C1, M2, Z2, C2)  \
  SPARSE_SMM_CMP_OP (mx_el_lt, <,  M1,   , M2,   ) \
  SPARSE_SMM_CMP_OP (mx_el_le, <=, M1,   , M2,   ) \
  SPARSE_SMM_CMP_OP (mx_el_ge, >=, M1,   , M2,   ) \
  SPARSE_SMM_CMP_OP (mx_el_gt, >,  M1,   , M2,   ) \
  SPARSE_SMM_CMP_OP (mx_el_eq, ==, M1,   , M2,   ) \
  SPARSE_SMM_CMP_OP (mx_el_ne, !=, M1,   , M2,   )

#define SPARSE_SMM_EQNE_OPS(M1, Z1, C1, M2, Z2, C2)  \
  SPARSE_SMM_CMP_OP (mx_el_eq, ==, M1,   , M2,   ) \
  SPARSE_SMM_CMP_OP (mx_el_ne, !=, M1,   , M2,   )

#define SPARSE_SMM_BOOL_OP(F, OP, M1, M2, LHS_ZERO, RHS_ZERO) \
  SparseBoolMatrix \
  F (const M1& m1, const M2& m2) \
  { \
    SparseBoolMatrix r; \
    \
    octave_idx_type m1_nr = m1.rows (); \
    octave_idx_type m1_nc = m1.cols (); \
    \
    octave_idx_type m2_nr = m2.rows (); \
    octave_idx_type m2_nc = m2.cols (); \
    \
    if (m1_nr == 1 && m1_nc == 1) \
      r = SparseBoolMatrix (F (m1.elem (0,0), m2)); \
    else if (m1_nr == m2_nr && m1_nc == m2_nc) \
      { \
        if (m1_nr != 0 || m1_nc != 0) \
          { \
            /* Count num of nonzero elements */ \
            octave_idx_type nel = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              for (octave_idx_type i = 0; i < m1_nr; i++) \
                if ((m1.elem (i, j) != LHS_ZERO) \
                    OP (m2.elem (i, j) != RHS_ZERO)) \
                  nel++; \
            \
            r = SparseBoolMatrix (m1_nr, m1_nc, nel); \
            \
            octave_idx_type ii = 0; \
            r.cidx (0) = 0; \
            for (octave_idx_type j = 0; j < m1_nc; j++) \
              { \
                for (octave_idx_type i = 0; i < m1_nr; i++) \
                  { \
                    bool el = (m1.elem (i, j) != LHS_ZERO) \
                      OP (m2.elem (i, j) != RHS_ZERO);     \
                    if (el) \
                      { \
                        r.data (ii) = el; \
                        r.ridx (ii++) = i; \
                      } \
                  } \
                r.cidx (j+1) = ii; \
              } \
          } \
      }       \
    else \
      { \
        if ((m1_nr != 0 || m1_nc != 0) && (m2_nr != 0 || m2_nc != 0)) \
          gripe_nonconformant (#F, m1_nr, m1_nc, m2_nr, m2_nc); \
      } \
    return r; \
  }

#define SPARSE_SMM_BOOL_OPS2(M1, M2, LHS_ZERO, RHS_ZERO) \
  SPARSE_SMM_BOOL_OP (mx_el_and, &&, M1, M2, LHS_ZERO, RHS_ZERO) \
  SPARSE_SMM_BOOL_OP (mx_el_or,  ||, M1, M2, LHS_ZERO, RHS_ZERO)

#define SPARSE_SMM_BOOL_OPS(M1, M2, ZERO) \
  SPARSE_SMM_BOOL_OPS2(M1, M2, ZERO, ZERO)

// Avoid some code duplication.  Maybe we should use templates.

#define SPARSE_CUMSUM(RET_TYPE, ELT_TYPE, FCN)  \
 \
  octave_idx_type nr = rows (); \
  octave_idx_type nc = cols (); \
 \
  RET_TYPE retval; \
 \
  if (nr > 0 && nc > 0) \
    { \
      if ((nr == 1 && dim == -1) || dim == 1) \
        /* Ugly!! Is there a better way? */ \
        retval = transpose (). FCN (0) .transpose (); \
      else \
        { \
          octave_idx_type nel = 0; \
          for (octave_idx_type i = 0; i < nc; i++) \
            { \
              ELT_TYPE t = ELT_TYPE (); \
              for (octave_idx_type j = cidx (i); j < cidx (i+1); j++)   \
                { \
                  t += data (j); \
                  if (t != ELT_TYPE ()) \
                    { \
                      if (j == cidx (i+1) - 1) \
                        nel += nr - ridx (j);  \
                      else \
                        nel += ridx (j+1) - ridx (j); \
                    } \
                } \
            } \
          retval = RET_TYPE (nr, nc, nel); \
          retval.cidx (0) = 0; \
          octave_idx_type ii = 0; \
          for (octave_idx_type i = 0; i < nc; i++) \
            { \
              ELT_TYPE t = ELT_TYPE (); \
              for (octave_idx_type j = cidx (i); j < cidx (i+1); j++)   \
                { \
                  t += data (j); \
                  if (t != ELT_TYPE ()) \
                    { \
                      if (j == cidx (i+1) - 1) \
                        { \
                          for (octave_idx_type k = ridx (j); k < nr; k++) \
                            { \
                               retval.data (ii) = t; \
                               retval.ridx (ii++) = k; \
                            } \
                        } \
                      else \
                        { \
                          for (octave_idx_type k = ridx (j); k < ridx (j+1); k++) \
                            { \
                               retval.data (ii) = t; \
                               retval.ridx (ii++) = k; \
                            } \
                        } \
                    } \
                } \
              retval.cidx (i+1) = ii; \
            } \
        } \
    } \
  else \
    retval = RET_TYPE (nr,nc); \
 \
  return retval


#define SPARSE_CUMPROD(RET_TYPE, ELT_TYPE, FCN) \
 \
  octave_idx_type nr = rows (); \
  octave_idx_type nc = cols (); \
 \
  RET_TYPE retval; \
 \
  if (nr > 0 && nc > 0) \
    { \
      if ((nr == 1 && dim == -1) || dim == 1) \
        /* Ugly!! Is there a better way? */ \
        retval = transpose (). FCN (0) .transpose (); \
      else \
        { \
          octave_idx_type nel = 0; \
          for (octave_idx_type i = 0; i < nc; i++) \
            { \
              octave_idx_type jj = 0; \
              for (octave_idx_type j = cidx (i); j < cidx (i+1); j++) \
                { \
                  if (jj == ridx (j)) \
                    { \
                      nel++; \
                      jj++; \
                    } \
                  else \
                    break; \
                } \
            } \
          retval = RET_TYPE (nr, nc, nel); \
          retval.cidx (0) = 0; \
          octave_idx_type ii = 0; \
          for (octave_idx_type i = 0; i < nc; i++) \
            { \
              ELT_TYPE t = ELT_TYPE (1.); \
              octave_idx_type jj = 0; \
              for (octave_idx_type j = cidx (i); j < cidx (i+1); j++) \
                { \
                  if (jj == ridx (j)) \
                    { \
                      t *= data (j); \
                      retval.data (ii) = t; \
                      retval.ridx (ii++) = jj++; \
                    } \
                  else \
                    break; \
                } \
              retval.cidx (i+1) = ii; \
            } \
        } \
    } \
  else \
    retval = RET_TYPE (nr,nc); \
 \
  return retval

#define SPARSE_BASE_REDUCTION_OP(RET_TYPE, EL_TYPE, ROW_EXPR, COL_EXPR, \
                                 INIT_VAL, MT_RESULT) \
 \
  octave_idx_type nr = rows (); \
  octave_idx_type nc = cols (); \
 \
  RET_TYPE retval; \
 \
  if (nr > 0 && nc > 0) \
    { \
      if ((nr == 1 && dim == -1) || dim == 1) \
        { \
          /* Define j here to allow fancy definition for prod method */ \
          octave_idx_type j = 0; \
          OCTAVE_LOCAL_BUFFER (EL_TYPE, tmp, nr); \
          \
          for (octave_idx_type i = 0; i < nr; i++) \
            tmp[i] = INIT_VAL; \
          for (j = 0; j < nc; j++) \
            { \
              for (octave_idx_type i = cidx (j); i < cidx (j + 1); i++) \
                { \
                  ROW_EXPR; \
                } \
            } \
          octave_idx_type nel = 0; \
          for (octave_idx_type i = 0; i < nr; i++) \
            if (tmp[i] != EL_TYPE ())  \
              nel++ ; \
          retval = RET_TYPE (nr, static_cast<octave_idx_type> (1), nel); \
          retval.cidx (0) = 0; \
          retval.cidx (1) = nel; \
          nel = 0; \
          for (octave_idx_type i = 0; i < nr; i++) \
            if (tmp[i] != EL_TYPE ())  \
              { \
                retval.data (nel) = tmp[i]; \
                retval.ridx (nel++) = i; \
              } \
        } \
      else \
        { \
          OCTAVE_LOCAL_BUFFER (EL_TYPE, tmp, nc); \
          \
          for (octave_idx_type j = 0; j < nc; j++) \
            { \
              tmp[j] = INIT_VAL; \
              for (octave_idx_type i = cidx (j); i < cidx (j + 1); i++) \
                { \
                  COL_EXPR; \
                } \
            } \
          octave_idx_type nel = 0; \
          for (octave_idx_type i = 0; i < nc; i++) \
            if (tmp[i] != EL_TYPE ())  \
              nel++ ; \
          retval = RET_TYPE (static_cast<octave_idx_type> (1), nc, nel); \
          retval.cidx (0) = 0; \
          nel = 0; \
          for (octave_idx_type i = 0; i < nc; i++) \
            if (tmp[i] != EL_TYPE ())  \
              { \
                retval.data (nel) = tmp[i]; \
                retval.ridx (nel++) = 0; \
                retval.cidx (i+1) = retval.cidx (i) + 1; \
              } \
            else \
              retval.cidx (i+1) = retval.cidx (i); \
        } \
    } \
  else if (nc == 0 && (nr == 0 || (nr == 1 && dim == -1))) \
    { \
      if (MT_RESULT) \
        { \
          retval = RET_TYPE (static_cast<octave_idx_type> (1), \
                             static_cast<octave_idx_type> (1), \
                             static_cast<octave_idx_type> (1)); \
          retval.cidx (0) = 0; \
          retval.cidx (1) = 1; \
          retval.ridx (0) = 0; \
          retval.data (0) = MT_RESULT; \
        } \
      else \
          retval = RET_TYPE (static_cast<octave_idx_type> (1), \
                             static_cast<octave_idx_type> (1), \
                             static_cast<octave_idx_type> (0)); \
    } \
  else if (nr == 0 && (dim == 0 || dim == -1)) \
    { \
      if (MT_RESULT) \
        { \
          retval = RET_TYPE (static_cast<octave_idx_type> (1), nc, nc); \
          retval.cidx (0) = 0; \
          for (octave_idx_type i = 0; i < nc ; i++) \
            { \
              retval.ridx (i) = 0; \
              retval.cidx (i+1) = i+1; \
              retval.data (i) = MT_RESULT; \
            } \
        } \
      else \
        retval = RET_TYPE (static_cast<octave_idx_type> (1), nc, \
                           static_cast<octave_idx_type> (0)); \
    } \
  else if (nc == 0 && dim == 1) \
    { \
      if (MT_RESULT) \
        { \
          retval = RET_TYPE (nr, static_cast<octave_idx_type> (1), nr); \
          retval.cidx (0) = 0; \
          retval.cidx (1) = nr; \
          for (octave_idx_type i = 0; i < nr; i++) \
            { \
              retval.ridx (i) = i; \
              retval.data (i) = MT_RESULT; \
            } \
        } \
      else \
        retval = RET_TYPE (nr, static_cast<octave_idx_type> (1), \
                           static_cast<octave_idx_type> (0)); \
    } \
  else \
    retval.resize (nr > 0, nc > 0); \
 \
  return retval

#define SPARSE_REDUCTION_OP_ROW_EXPR(OP) \
  tmp[ridx (i)] OP data (i)

#define SPARSE_REDUCTION_OP_COL_EXPR(OP) \
  tmp[j] OP data (i)

#define SPARSE_REDUCTION_OP(RET_TYPE, EL_TYPE, OP, INIT_VAL, MT_RESULT) \
  SPARSE_BASE_REDUCTION_OP (RET_TYPE, EL_TYPE, \
                        SPARSE_REDUCTION_OP_ROW_EXPR (OP), \
                        SPARSE_REDUCTION_OP_COL_EXPR (OP), \
                        INIT_VAL, MT_RESULT)


// Don't break from this loop if the test succeeds because
// we are looping over the rows and not the columns in the inner loop.
#define SPARSE_ANY_ALL_OP_ROW_CODE(TEST_OP, TEST_TRUE_VAL) \
  if (data (i) TEST_OP 0.0) \
    tmp[ridx (i)] = TEST_TRUE_VAL;

#define SPARSE_ANY_ALL_OP_COL_CODE(TEST_OP, TEST_TRUE_VAL) \
  if (data (i) TEST_OP 0.0) \
    { \
      tmp[j] = TEST_TRUE_VAL; \
      break; \
    }

#define SPARSE_ANY_ALL_OP(DIM, INIT_VAL, MT_RESULT, TEST_OP, TEST_TRUE_VAL) \
  SPARSE_BASE_REDUCTION_OP (SparseBoolMatrix, char, \
                        SPARSE_ANY_ALL_OP_ROW_CODE (TEST_OP, TEST_TRUE_VAL), \
                        SPARSE_ANY_ALL_OP_COL_CODE (TEST_OP, TEST_TRUE_VAL), \
                        INIT_VAL, MT_RESULT)

#define SPARSE_ALL_OP(DIM) \
  if ((rows () == 1 && dim == -1) || dim == 1) \
    return transpose (). all (0). transpose (); \
  else \
    { \
      SPARSE_ANY_ALL_OP (DIM, (cidx (j+1) - cidx (j) < nr ? false : true), \
                         true, ==, false); \
    }

#define SPARSE_ANY_OP(DIM) SPARSE_ANY_ALL_OP (DIM, false, false, !=, true)

#define SPARSE_SPARSE_MUL(RET_TYPE, RET_EL_TYPE, EL_TYPE) \
  octave_idx_type nr = m.rows (); \
  octave_idx_type nc = m.cols (); \
  \
  octave_idx_type a_nr = a.rows (); \
  octave_idx_type a_nc = a.cols (); \
  \
  if (nr == 1 && nc == 1) \
   { \
     RET_EL_TYPE s = m.elem (0,0); \
     octave_idx_type nz = a.nnz (); \
     RET_TYPE r (a_nr, a_nc, nz); \
     \
     for (octave_idx_type i = 0; i < nz; i++) \
       { \
         octave_quit (); \
         r.data (i) = s * a.data (i); \
         r.ridx (i) = a.ridx (i); \
       } \
     for (octave_idx_type i = 0; i < a_nc + 1; i++) \
       { \
         octave_quit (); \
         r.cidx (i) = a.cidx (i); \
       } \
     \
     r.maybe_compress (true); \
     return r; \
   } \
  else if (a_nr == 1 && a_nc == 1) \
   { \
     RET_EL_TYPE s = a.elem (0,0); \
     octave_idx_type nz = m.nnz (); \
     RET_TYPE r (nr, nc, nz); \
     \
     for (octave_idx_type i = 0; i < nz; i++) \
       { \
         octave_quit (); \
         r.data (i) = m.data (i) * s; \
         r.ridx (i) = m.ridx (i); \
       } \
     for (octave_idx_type i = 0; i < nc + 1; i++) \
       { \
         octave_quit (); \
         r.cidx (i) = m.cidx (i); \
       } \
     \
     r.maybe_compress (true); \
     return r; \
   } \
  else if (nc != a_nr) \
    { \
      gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc); \
      return RET_TYPE (); \
    } \
  else \
    { \
      OCTAVE_LOCAL_BUFFER (octave_idx_type, w, nr); \
      RET_TYPE retval (nr, a_nc, static_cast<octave_idx_type> (0)); \
      for (octave_idx_type i = 0; i < nr; i++) \
        w[i] = 0; \
      retval.xcidx (0) = 0; \
      \
      octave_idx_type nel = 0; \
      \
      for (octave_idx_type i = 0; i < a_nc; i++) \
        { \
          for (octave_idx_type j = a.cidx (i); j < a.cidx (i+1); j++) \
            { \
              octave_idx_type  col = a.ridx (j); \
              for (octave_idx_type k = m.cidx (col) ; k < m.cidx (col+1); k++) \
                { \
                  if (w[m.ridx (k)] < i + 1) \
                    { \
                      w[m.ridx (k)] = i + 1; \
                      nel++; \
                    } \
                  octave_quit (); \
                } \
            } \
          retval.xcidx (i+1) = nel; \
        } \
      \
      if (nel == 0) \
        return RET_TYPE (nr, a_nc); \
      else \
        {  \
          for (octave_idx_type i = 0; i < nr; i++) \
            w[i] = 0; \
          \
          OCTAVE_LOCAL_BUFFER (RET_EL_TYPE, Xcol, nr); \
          \
          retval.change_capacity (nel); \
          /* The optimal break-point as estimated from simulations */ \
          /* Note that Mergesort is O(nz log(nz)) while searching all */ \
          /* values is O(nr), where nz here is nonzero per row of */ \
          /* length nr. The test itself was then derived from the */ \
          /* simulation with random square matrices and the observation */ \
          /* of the number of nonzero elements in the output matrix */ \
          /* it was found that the breakpoints were */ \
          /*   nr: 500  1000  2000  5000 10000 */ \
          /*   nz:   6    25    97   585  2202 */ \
          /* The below is a simplication of the 'polyfit'-ed parameters */ \
          /* to these breakpoints */ \
          octave_idx_type n_per_col = (a_nc > 43000 ? 43000 : \
                                        (a_nc * a_nc) / 43000); \
          octave_idx_type ii = 0; \
          octave_idx_type *ri = retval.xridx (); \
          octave_sort<octave_idx_type> sort; \
          \
          for (octave_idx_type i = 0; i < a_nc ; i++) \
            { \
              if (retval.xcidx (i+1) - retval.xcidx (i) > n_per_col) \
                { \
                  for (octave_idx_type j = a.cidx (i); j < a.cidx (i+1); j++) \
                    { \
                      octave_idx_type col = a.ridx (j); \
                      EL_TYPE tmpval = a.data (j); \
                      for (octave_idx_type k = m.cidx (col) ; \
                           k < m.cidx (col+1); k++) \
                        { \
                          octave_quit (); \
                          octave_idx_type row = m.ridx (k); \
                          if (w[row] < i + 1) \
                            { \
                              w[row] = i + 1; \
                              Xcol[row] = tmpval * m.data (k); \
                            } \
                          else \
                            Xcol[row] += tmpval * m.data (k); \
                        } \
                    } \
                  for (octave_idx_type k = 0; k < nr; k++) \
                    if (w[k] == i + 1) \
                      { \
                        retval.xdata (ii) = Xcol[k]; \
                        retval.xridx (ii++) = k; \
                      } \
                } \
              else \
                { \
                  for (octave_idx_type j = a.cidx (i); j < a.cidx (i+1); j++) \
                    { \
                      octave_idx_type col = a.ridx (j); \
                      EL_TYPE tmpval = a.data (j); \
                      for (octave_idx_type k = m.cidx (col) ; \
                          k < m.cidx (col+1); k++) \
                        { \
                          octave_quit (); \
                          octave_idx_type row = m.ridx (k); \
                          if (w[row] < i + 1) \
                            { \
                              w[row] = i + 1; \
                              retval.xridx (ii++) = row;\
                              Xcol[row] = tmpval * m.data (k); \
                            } \
                          else \
                            Xcol[row] += tmpval * m.data (k); \
                        } \
                    } \
                  sort.sort (ri + retval.xcidx (i), ii - retval.xcidx (i)); \
                  for (octave_idx_type k = retval.xcidx (i); k < ii; k++) \
                    retval.xdata (k) = Xcol[retval.xridx (k)]; \
                }  \
            } \
          retval.maybe_compress (true);\
          return retval; \
        } \
    }

#define SPARSE_FULL_MUL(RET_TYPE, EL_TYPE, ZERO) \
  octave_idx_type nr = m.rows (); \
  octave_idx_type nc = m.cols (); \
  \
  octave_idx_type a_nr = a.rows (); \
  octave_idx_type a_nc = a.cols (); \
  \
  if (nr == 1 && nc == 1) \
    { \
      RET_TYPE retval = m.elem (0,0) * a; \
      return retval; \
    } \
  else if (nc != a_nr) \
    { \
      gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc); \
      return RET_TYPE (); \
    } \
  else \
    { \
      RET_TYPE retval (nr, a_nc, ZERO); \
      \
      for (octave_idx_type i = 0; i < a_nc ; i++) \
        { \
          for (octave_idx_type j = 0; j < a_nr; j++) \
            { \
              octave_quit (); \
              \
              EL_TYPE tmpval = a.elem (j,i); \
              for (octave_idx_type k = m.cidx (j) ; k < m.cidx (j+1); k++) \
                retval.elem (m.ridx (k),i) += tmpval * m.data (k); \
            } \
        } \
      return retval; \
    }

#define SPARSE_FULL_TRANS_MUL(RET_TYPE, EL_TYPE, ZERO, CONJ_OP) \
  octave_idx_type nr = m.rows (); \
  octave_idx_type nc = m.cols (); \
  \
  octave_idx_type a_nr = a.rows (); \
  octave_idx_type a_nc = a.cols (); \
  \
  if (nr == 1 && nc == 1) \
    { \
      RET_TYPE retval = CONJ_OP (m.elem (0,0)) * a; \
      return retval; \
    } \
  else if (nr != a_nr) \
    { \
      gripe_nonconformant ("operator *", nc, nr, a_nr, a_nc); \
      return RET_TYPE (); \
    } \
  else \
    { \
      RET_TYPE retval (nc, a_nc); \
      \
      for (octave_idx_type i = 0; i < a_nc ; i++) \
        { \
          for (octave_idx_type j = 0; j < nc; j++) \
            { \
              octave_quit (); \
              \
              EL_TYPE acc = ZERO; \
              for (octave_idx_type k = m.cidx (j) ; k < m.cidx (j+1); k++) \
                acc += a.elem (m.ridx (k),i) * CONJ_OP (m.data (k)); \
              retval.xelem (j,i) = acc; \
            } \
        } \
      return retval; \
    }

#define FULL_SPARSE_MUL(RET_TYPE, EL_TYPE, ZERO) \
  octave_idx_type nr = m.rows (); \
  octave_idx_type nc = m.cols (); \
  \
  octave_idx_type a_nr = a.rows (); \
  octave_idx_type a_nc = a.cols (); \
  \
  if (a_nr == 1 && a_nc == 1) \
    { \
      RET_TYPE retval = m * a.elem (0,0); \
      return retval; \
    } \
  else if (nc != a_nr) \
    { \
      gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc); \
      return RET_TYPE (); \
    } \
  else \
    { \
      RET_TYPE retval (nr, a_nc, ZERO); \
      \
      for (octave_idx_type i = 0; i < a_nc ; i++) \
        { \
          octave_quit (); \
          for (octave_idx_type j = a.cidx (i); j < a.cidx (i+1); j++) \
            { \
              octave_idx_type col = a.ridx (j); \
              EL_TYPE tmpval = a.data (j); \
              \
              for (octave_idx_type k = 0 ; k < nr; k++) \
                retval.xelem (k,i) += tmpval * m.elem (k,col); \
            } \
        } \
      return retval; \
    }

#define FULL_SPARSE_MUL_TRANS(RET_TYPE, EL_TYPE, ZERO, CONJ_OP) \
  octave_idx_type nr = m.rows (); \
  octave_idx_type nc = m.cols (); \
  \
  octave_idx_type a_nr = a.rows (); \
  octave_idx_type a_nc = a.cols (); \
  \
  if (a_nr == 1 && a_nc == 1) \
    { \
      RET_TYPE retval = m * CONJ_OP (a.elem (0,0)); \
      return retval; \
    } \
  else if (nc != a_nc) \
    { \
      gripe_nonconformant ("operator *", nr, nc, a_nc, a_nr); \
      return RET_TYPE (); \
    } \
  else \
    { \
      RET_TYPE retval (nr, a_nr, ZERO); \
      \
      for (octave_idx_type i = 0; i < a_nc ; i++) \
        { \
          octave_quit (); \
          for (octave_idx_type j = a.cidx (i); j < a.cidx (i+1); j++) \
            { \
              octave_idx_type col = a.ridx (j); \
              EL_TYPE tmpval = CONJ_OP (a.data (j)); \
              for (octave_idx_type k = 0 ; k < nr; k++) \
                retval.xelem (k,col) += tmpval * m.elem (k,i); \
            } \
        } \
      return retval; \
    }

#endif
