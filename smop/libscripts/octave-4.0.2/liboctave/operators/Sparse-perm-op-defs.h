/* -*- C++ -*-

Copyright (C) 2009-2015 Jason Riedy

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

#if !defined (octave_Sparse_perm_op_defs_h)
#define octave_Sparse_perm_op_defs_h 1

// Matrix multiplication

template <typename SM>
SM octinternal_do_mul_colpm_sm (const octave_idx_type *pcol, const SM& a)
// Relabel the rows according to pcol.
{
  const octave_idx_type nr = a.rows ();
  const octave_idx_type nc = a.cols ();
  const octave_idx_type nent = a.nnz ();
  SM r (nr, nc, nent);

  octave_sort<octave_idx_type> sort;

  for (octave_idx_type j = 0; j <= nc; ++j)
    r.xcidx (j) = a.cidx (j);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_quit ();

      OCTAVE_LOCAL_BUFFER (octave_idx_type, sidx, r.xcidx (j+1) - r.xcidx (j));
      for (octave_idx_type i = r.xcidx (j), ii = 0; i < r.xcidx (j+1); i++)
        {
          sidx[ii++]=i;
          r.xridx (i) = pcol[a.ridx (i)];
        }
      sort.sort (r.xridx () + r.xcidx (j), sidx, r.xcidx (j+1) - r.xcidx (j));
      for (octave_idx_type i = r.xcidx (j), ii = 0; i < r.xcidx (j+1); i++)
        r.xdata (i) = a.data (sidx[ii++]);
    }

  return r;
}

template <typename SM>
SM octinternal_do_mul_pm_sm (const PermMatrix& p, const SM& a)
{
  const octave_idx_type nr = a.rows ();
  if (p.cols () != nr)
    {
      gripe_nonconformant ("operator *", p.rows (), p.cols (), a.rows (), a.cols ());
      return SM ();
    }

  return octinternal_do_mul_colpm_sm (p.col_perm_vec ().data (), a);
}

template <typename SM>
SM octinternal_do_mul_sm_rowpm (const SM& a, const octave_idx_type *prow)
// For a row permutation, iterate across the source a and stuff the
// results into the correct destination column in r.
{
  const octave_idx_type nr = a.rows ();
  const octave_idx_type nc = a.cols ();
  const octave_idx_type nent = a.nnz ();
  SM r (nr, nc, nent);

  for (octave_idx_type j_src = 0; j_src < nc; ++j_src)
    r.xcidx (prow[j_src]) = a.cidx (j_src+1) - a.cidx (j_src);
  octave_idx_type k = 0;
  for (octave_idx_type j = 0; j < nc; ++j)
    {
      const octave_idx_type tmp = r.xcidx (j);
      r.xcidx (j) = k;
      k += tmp;
    }
  r.xcidx (nc) = nent;

  octave_idx_type k_src = 0;
  for (octave_idx_type j_src = 0; j_src < nc; ++j_src)
    {
      octave_quit ();
      const octave_idx_type j = prow[j_src];
      const octave_idx_type kend_src = a.cidx (j_src + 1);
      for (k = r.xcidx (j); k_src < kend_src; ++k, ++k_src)
        {
          r.xridx (k) = a.ridx (k_src);
          r.xdata (k) = a.data (k_src);
        }
    }
  assert (k_src == nent);

  return r;
}

template <typename SM>
SM octinternal_do_mul_sm_colpm (const SM& a, const octave_idx_type *pcol)
// For a column permutation, iterate across the destination r and pull
// data from the correct column of a.
{
  const octave_idx_type nr = a.rows ();
  const octave_idx_type nc = a.cols ();
  const octave_idx_type nent = a.nnz ();
  SM r (nr, nc, nent);

  for (octave_idx_type j = 0; j < nc; ++j)
    {
      const octave_idx_type j_src = pcol[j];
      r.xcidx (j+1) = r.xcidx (j) + (a.cidx (j_src+1) - a.cidx (j_src));
    }
  assert (r.xcidx (nc) == nent);

  octave_idx_type k = 0;
  for (octave_idx_type j = 0; j < nc; ++j)
    {
      octave_quit ();
      const octave_idx_type j_src = pcol[j];
      octave_idx_type k_src;
      const octave_idx_type kend_src = a.cidx (j_src + 1);
      for (k_src = a.cidx (j_src); k_src < kend_src; ++k_src, ++k)
        {
          r.xridx (k) = a.ridx (k_src);
          r.xdata (k) = a.data (k_src);
        }
    }
  assert (k == nent);

  return r;
}

template <typename SM>
SM octinternal_do_mul_sm_pm (const SM& a, const PermMatrix& p)
{
  const octave_idx_type nc = a.cols ();
  if (p.rows () != nc)
    {
      gripe_nonconformant ("operator *", a.rows (), a.cols (), p.rows (), p.cols ());
      return SM ();
    }

  return octinternal_do_mul_sm_colpm (a, p.col_perm_vec ().data ());
}

#endif // octave_Sparse_perm_op_defs_h
