/*

Copyright (C) 2006-2015 David Bateman

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>

#include "MArray.h"
#include "MSparse.h"
#include "SparseQR.h"
#include "SparseCmplxQR.h"
#include "MatrixType.h"
#include "oct-sort.h"
#include "oct-locbuf.h"
#include "oct-inttypes.h"

template <class T>
static MSparse<T>
dmsolve_extract (const MSparse<T> &A, const octave_idx_type *Pinv,
                 const octave_idx_type *Q, octave_idx_type rst,
                 octave_idx_type rend, octave_idx_type cst,
                 octave_idx_type cend, octave_idx_type maxnz = -1,
                 bool lazy = false)
{
  octave_idx_type nr = rend - rst;
  octave_idx_type nc = cend - cst;
  maxnz = (maxnz < 0 ? A.nnz () : maxnz);
  octave_idx_type nz;

  // Cast to uint64 to handle overflow in this multiplication
  if (octave_uint64 (nr)*octave_uint64 (nc) < octave_uint64 (maxnz))
    nz = nr*nc;
  else
    nz = maxnz;

  MSparse<T> B (nr, nc, (nz < maxnz ? nz : maxnz));
  // Some sparse functions can support lazy indexing (where elements
  // in the row are in no particular order), even though octave in
  // general can't. For those functions that can using it is a big
  // win here in terms of speed.
  if (lazy)
    {
      nz = 0;
      for (octave_idx_type j = cst ; j < cend ; j++)
        {
          octave_idx_type qq = (Q ? Q[j] : j);
          B.xcidx (j - cst) = nz;
          for (octave_idx_type p = A.cidx (qq) ; p < A.cidx (qq+1) ; p++)
            {
              octave_quit ();
              octave_idx_type r = (Pinv ? Pinv[A.ridx (p)] : A.ridx (p));
              if (r >= rst && r < rend)
                {
                  B.xdata (nz) = A.data (p);
                  B.xridx (nz++) = r - rst ;
                }
            }
        }
      B.xcidx (cend - cst) = nz ;
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (T, X, rend - rst);
      octave_sort<octave_idx_type> sort;
      octave_idx_type *ri = B.xridx ();
      nz = 0;
      for (octave_idx_type j = cst ; j < cend ; j++)
        {
          octave_idx_type qq = (Q ? Q[j] : j);
          B.xcidx (j - cst) = nz;
          for (octave_idx_type p = A.cidx (qq) ; p < A.cidx (qq+1) ; p++)
            {
              octave_quit ();
              octave_idx_type r = (Pinv ? Pinv[A.ridx (p)] : A.ridx (p));
              if (r >= rst && r < rend)
                {
                  X[r-rst] = A.data (p);
                  B.xridx (nz++) = r - rst ;
                }
            }
          sort.sort (ri + B.xcidx (j - cst), nz - B.xcidx (j - cst));
          for (octave_idx_type p = B.cidx (j - cst); p < nz; p++)
            B.xdata (p) = X[B.xridx (p)];
        }
      B.xcidx (cend - cst) = nz ;
    }

  return B;
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static MSparse<double>
dmsolve_extract (const MSparse<double> &A, const octave_idx_type *Pinv,
                 const octave_idx_type *Q, octave_idx_type rst,
                 octave_idx_type rend, octave_idx_type cst,
                 octave_idx_type cend, octave_idx_type maxnz,
                 bool lazy);

static MSparse<Complex>
dmsolve_extract (const MSparse<Complex> &A, const octave_idx_type *Pinv,
                 const octave_idx_type *Q, octave_idx_type rst,
                 octave_idx_type rend, octave_idx_type cst,
                 octave_idx_type cend, octave_idx_type maxnz,
                 bool lazy);
#endif

template <class T>
static MArray<T>
dmsolve_extract (const MArray<T> &m, const octave_idx_type *,
                 const octave_idx_type *, octave_idx_type r1,
                 octave_idx_type r2, octave_idx_type c1,
                 octave_idx_type c2)
{
  r2 -= 1;
  c2 -= 1;
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  octave_idx_type new_r = r2 - r1 + 1;
  octave_idx_type new_c = c2 - c1 + 1;

  MArray<T> result (dim_vector (new_r, new_c));

  for (octave_idx_type j = 0; j < new_c; j++)
    for (octave_idx_type i = 0; i < new_r; i++)
      result.xelem (i, j) = m.elem (r1+i, c1+j);

  return result;
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static MArray<double>
dmsolve_extract (const MArray<double> &m, const octave_idx_type *,
                 const octave_idx_type *, octave_idx_type r1,
                 octave_idx_type r2, octave_idx_type c1,
                 octave_idx_type c2)

static MArray<Complex>
dmsolve_extract (const MArray<Complex> &m, const octave_idx_type *,
                 const octave_idx_type *, octave_idx_type r1,
                 octave_idx_type r2, octave_idx_type c1,
                 octave_idx_type c2)
#endif

template <class T>
static void
dmsolve_insert (MArray<T> &a, const MArray<T> &b, const octave_idx_type *Q,
                octave_idx_type r, octave_idx_type c)
{
  T *ax = a.fortran_vec ();
  const T *bx = b.fortran_vec ();
  octave_idx_type anr = a.rows ();
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();
  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_idx_type aoff = (c + j) * anr;
      octave_idx_type boff = j * nr;
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          ax[Q[r + i] + aoff] = bx[i + boff];
        }
    }
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static void
dmsolve_insert (MArray<double> &a, const MArray<double> &b,
                const octave_idx_type *Q, octave_idx_type r, octave_idx_type c);

static void
dmsolve_insert (MArray<Complex> &a, const MArray<Complex> &b,
                const octave_idx_type *Q, octave_idx_type r, octave_idx_type c);
#endif

template <class T>
static void
dmsolve_insert (MSparse<T> &a, const MSparse<T> &b, const octave_idx_type *Q,
                octave_idx_type r, octave_idx_type c)
{
  octave_idx_type b_rows = b.rows ();
  octave_idx_type b_cols = b.cols ();
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  OCTAVE_LOCAL_BUFFER (octave_idx_type, Qinv, nr);
  for (octave_idx_type i = 0; i < nr; i++)
    Qinv[Q[i]] = i;

  // First count the number of elements in the final array
  octave_idx_type nel = a.xcidx (c) + b.nnz ();

  if (c + b_cols < nc)
    nel += a.xcidx (nc) - a.xcidx (c + b_cols);

  for (octave_idx_type i = c; i < c + b_cols; i++)
    for (octave_idx_type j = a.xcidx (i); j < a.xcidx (i+1); j++)
      if (Qinv[a.xridx (j)] < r || Qinv[a.xridx (j)] >= r + b_rows)
        nel++;

  OCTAVE_LOCAL_BUFFER (T, X, nr);
  octave_sort<octave_idx_type> sort;
  MSparse<T> tmp (a);
  a = MSparse<T> (nr, nc, nel);
  octave_idx_type *ri = a.xridx ();

  for (octave_idx_type i = 0; i < tmp.cidx (c); i++)
    {
      a.xdata (i) = tmp.xdata (i);
      a.xridx (i) = tmp.xridx (i);
    }
  for (octave_idx_type i = 0; i < c + 1; i++)
    a.xcidx (i) = tmp.xcidx (i);

  octave_idx_type ii = a.xcidx (c);

  for (octave_idx_type i = c; i < c + b_cols; i++)
    {
      octave_quit ();

      for (octave_idx_type j = tmp.xcidx (i); j < tmp.xcidx (i+1); j++)
        if (Qinv[tmp.xridx (j)] < r ||  Qinv[tmp.xridx (j)] >= r + b_rows)
          {
            X[tmp.xridx (j)] = tmp.xdata (j);
            a.xridx (ii++) = tmp.xridx (j);
          }

      octave_quit ();

      for (octave_idx_type j = b.cidx (i-c); j < b.cidx (i-c+1); j++)
        {
          X[Q[r + b.ridx (j)]] = b.data (j);
          a.xridx (ii++) = Q[r + b.ridx (j)];
        }

      sort.sort (ri + a.xcidx (i), ii - a.xcidx (i));
      for (octave_idx_type p = a.xcidx (i); p < ii; p++)
        a.xdata (p) = X[a.xridx (p)];
      a.xcidx (i+1) = ii;
    }

  for (octave_idx_type i = c + b_cols; i < nc; i++)
    {
      for (octave_idx_type j = tmp.xcidx (i); j < tmp.cidx (i+1); j++)
        {
          a.xdata (ii) = tmp.xdata (j);
          a.xridx (ii++) = tmp.xridx (j);
        }
      a.xcidx (i+1) = ii;
    }
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static void
dmsolve_insert (MSparse<double> &a, const SparseMatrix &b,
                const octave_idx_type *Q, octave_idx_type r, octave_idx_type c);

static void
dmsolve_insert (MSparse<Complex> &a, const MSparse<Complex> &b,
                const octave_idx_type *Q, octave_idx_type r, octave_idx_type c);
#endif

template <class T, class RT>
static void
dmsolve_permute (MArray<RT> &a, const MArray<T>& b, const octave_idx_type *p)
{
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  const T *Bx = b.fortran_vec ();
  a.resize (dim_vector (b_nr, b_nc));
  RT *Btx = a.fortran_vec ();
  for (octave_idx_type j = 0; j < b_nc; j++)
    {
      octave_idx_type off = j * b_nr;
      for (octave_idx_type i = 0; i < b_nr; i++)
        {
          octave_quit ();
          Btx[p[i] + off] = Bx[ i + off];
        }
    }
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static void
dmsolve_permute (MArray<double> &a, const MArray<double>& b,
                 const octave_idx_type *p);

static void
dmsolve_permute (MArray<Complex> &a, const MArray<double>& b,
                 const octave_idx_type *p);

static void
dmsolve_permute (MArray<Complex> &a, const MArray<Complex>& b,
                 const octave_idx_type *p);
#endif

template <class T, class RT>
static void
dmsolve_permute (MSparse<RT> &a, const MSparse<T>& b, const octave_idx_type *p)
{
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nz = b.nnz ();
  octave_idx_type nz = 0;
  a = MSparse<RT> (b_nr, b_nc, b_nz);
  octave_sort<octave_idx_type> sort;
  octave_idx_type *ri = a.xridx ();
  OCTAVE_LOCAL_BUFFER (RT, X, b_nr);
  a.xcidx (0) = 0;
  for (octave_idx_type j = 0; j < b_nc; j++)
    {
      for (octave_idx_type i = b.cidx (j); i < b.cidx (j+1); i++)
        {
          octave_quit ();
          octave_idx_type r = p[b.ridx (i)];
          X[r] = b.data (i);
          a.xridx (nz++) = p[b.ridx (i)];
        }
      sort.sort (ri + a.xcidx (j), nz - a.xcidx (j));
      for (octave_idx_type i = a.cidx (j); i < nz; i++)
        {
          octave_quit ();
          a.xdata (i) = X[a.xridx (i)];
        }
      a.xcidx (j+1) = nz;
    }
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
static void
dmsolve_permute (MSparse<double> &a, const MSparse<double>& b,
                 const octave_idx_type *p);

static void
dmsolve_permute (MSparse<Complex> &a, const MSparse<double>& b,
                 const octave_idx_type *p);

static void
dmsolve_permute (MSparse<Complex> &a, const MSparse<Complex>& b,
                 const octave_idx_type *p);
#endif

static void
solve_singularity_warning (double)
{
  // Dummy singularity handler so that LU solver doesn't flag
  // an error for numerically rank defficient matrices
}

template <class RT, class ST, class T>
RT
dmsolve (const ST &a, const T &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  RT retval;

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr == 0 || nc == 0 || b_nc == 0)
    retval = RT (nc, b_nc, 0.0);
  else
    {
      octave_idx_type nnz_remaining = a.nnz ();
      CXSPARSE_DNAME () csm;
      csm.m = nr;
      csm.n = nc;
      csm.x = 0;
      csm.nz = -1;
      csm.nzmax = a.nnz ();
      // Cast away const on A, with full knowledge that CSparse won't touch it.
      // Prevents the methods below making a copy of the data.
      csm.p = const_cast<octave_idx_type *>(a.cidx ());
      csm.i = const_cast<octave_idx_type *>(a.ridx ());

#if defined (CS_VER) && (CS_VER >= 2)
      CXSPARSE_DNAME (d) *dm = CXSPARSE_DNAME(_dmperm) (&csm, 0);
      octave_idx_type *p = dm->p;
      octave_idx_type *q = dm->q;
#else
      CXSPARSE_DNAME (d) *dm = CXSPARSE_DNAME(_dmperm) (&csm);
      octave_idx_type *p = dm->P;
      octave_idx_type *q = dm->Q;
#endif
      OCTAVE_LOCAL_BUFFER (octave_idx_type, pinv, nr);
      for (octave_idx_type i = 0; i < nr; i++)
        pinv[p[i]] = i;
      RT btmp;
      dmsolve_permute (btmp, b, pinv);
      info = 0;
      retval.resize (nc, b_nc);

      // Leading over-determined block
      if (dm->rr[2] < nr && dm->cc[3] < nc)
        {
          ST m = dmsolve_extract (a, pinv, q, dm->rr[2], nr, dm->cc[3], nc,
                                  nnz_remaining, true);
          nnz_remaining -= m.nnz ();
          RT mtmp =
            qrsolve (m, dmsolve_extract (btmp, 0, 0, dm->rr[2], b_nr, 0,
                                         b_nc), info);
          dmsolve_insert (retval, mtmp, q, dm->cc[3], 0);
          if (dm->rr[2] > 0 && !info)
            {
              m = dmsolve_extract (a, pinv, q, 0, dm->rr[2],
                                   dm->cc[3], nc, nnz_remaining, true);
              nnz_remaining -= m.nnz ();
              RT ctmp = dmsolve_extract (btmp, 0, 0, 0,
                                         dm->rr[2], 0, b_nc);
              btmp.insert (ctmp - m * mtmp, 0, 0);
            }
        }

      // Structurally non-singular blocks
      // FIXME: Should use fine Dulmange-Mendelsohn decomposition here.
      if (dm->rr[1] < dm->rr[2] && dm->cc[2] < dm->cc[3] && !info)
        {
          ST m = dmsolve_extract (a, pinv, q, dm->rr[1], dm->rr[2],
                                  dm->cc[2], dm->cc[3], nnz_remaining, false);
          nnz_remaining -= m.nnz ();
          RT btmp2 = dmsolve_extract (btmp, 0, 0, dm->rr[1], dm->rr[2],
                                      0, b_nc);
          double rcond = 0.0;
          MatrixType mtyp (MatrixType::Full);
          RT mtmp = m.solve (mtyp, btmp2, info, rcond,
                             solve_singularity_warning, false);
          if (info != 0)
            {
              info = 0;
              mtmp = qrsolve (m, btmp2, info);
            }

          dmsolve_insert (retval, mtmp, q, dm->cc[2], 0);
          if (dm->rr[1] > 0 && !info)
            {
              m = dmsolve_extract (a, pinv, q, 0, dm->rr[1], dm->cc[2],
                                   dm->cc[3], nnz_remaining, true);
              nnz_remaining -= m.nnz ();
              RT ctmp = dmsolve_extract (btmp, 0, 0, 0,
                                         dm->rr[1], 0, b_nc);
              btmp.insert (ctmp - m * mtmp, 0, 0);
            }
        }

      // Trailing under-determined block
      if (dm->rr[1] > 0 && dm->cc[2] > 0 && !info)
        {
          ST m = dmsolve_extract (a, pinv, q, 0, dm->rr[1], 0,
                                  dm->cc[2], nnz_remaining, true);
          RT mtmp =
            qrsolve (m, dmsolve_extract (btmp, 0, 0, 0, dm->rr[1] , 0,
                                         b_nc), info);
          dmsolve_insert (retval, mtmp, q, 0, 0);
        }

      CXSPARSE_DNAME (_dfree) (dm);
    }
  return retval;
#else
  (*current_liboctave_error_handler)
    ("CXSPARSE unavailable; cannot solve minimum norm problem");
  return RT ();
#endif
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern Matrix
dmsolve (const SparseMatrix &a, const Matrix &b,
         octave_idx_type &info);

extern ComplexMatrix
dmsolve (const SparseMatrix &a, const ComplexMatrix &b,
         octave_idx_type &info);

extern ComplexMatrix
dmsolve (const SparseComplexMatrix &a, const Matrix &b,
         octave_idx_type &info);

extern ComplexMatrix
dmsolve (const SparseComplexMatrix &a, const ComplexMatrix &b,
         octave_idx_type &info);

extern SparseMatrix
dmsolve (const SparseMatrix &a, const SparseMatrix &b,
         octave_idx_type &info);

extern SparseComplexMatrix
dmsolve (const SparseMatrix &a, const SparseComplexMatrix &b,
         octave_idx_type &info);

extern SparseComplexMatrix
dmsolve (const SparseComplexMatrix &a, const SparseMatrix &b,
         octave_idx_type &info);

extern SparseComplexMatrix
dmsolve (const SparseComplexMatrix &a, const SparseComplexMatrix &b,
         octave_idx_type &info);
#endif
