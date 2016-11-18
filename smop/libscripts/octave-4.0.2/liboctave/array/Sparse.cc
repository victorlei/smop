// Template sparse array class
/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler
Copyright (C) 2010 VZLU Prague

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

#include <cassert>

#include <algorithm>
#include <iostream>
#include <limits>
#include <sstream>
#include <vector>

#include "Array.h"
#include "MArray.h"
#include "Array-util.h"
#include "Range.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "quit.h"
#include "oct-locbuf.h"

#include "Sparse.h"
#include "sparse-sort.h"
#include "sparse-util.h"
#include "oct-spparms.h"
#include "mx-inlines.cc"

#include "PermMatrix.h"

template <class T>
typename Sparse<T>::SparseRep *
Sparse<T>::nil_rep (void)
{
  static typename Sparse<T>::SparseRep nr;
  return &nr;
}

template <class T>
Sparse<T>::Sparse (const PermMatrix& a)
  : rep (new typename Sparse<T>::SparseRep (a.rows (), a.cols (), a.rows ())),
    dimensions (dim_vector (a.rows (), a.cols ()))
{
  octave_idx_type n = a.rows ();
  for (octave_idx_type i = 0; i <= n; i++)
    cidx (i) = i;

  const Array<octave_idx_type> pv = a.col_perm_vec ();

  for (octave_idx_type i = 0; i < n; i++)
    ridx (i) = pv(i);

  for (octave_idx_type i = 0; i < n; i++)
    data (i) = 1.0;
}

template <class T>
T&
Sparse<T>::SparseRep::elem (octave_idx_type _r, octave_idx_type _c)
{
  octave_idx_type i;

  if (nzmx > 0)
    {
      for (i = c[_c]; i < c[_c + 1]; i++)
        if (r[i] == _r)
          return d[i];
        else if (r[i] > _r)
          break;

      // Ok, If we've gotten here, we're in trouble.. Have to create a
      // new element in the sparse array. This' gonna be slow!!!
      if (c[ncols] == nzmx)
        {
          (*current_liboctave_error_handler)
            ("Sparse::SparseRep::elem (octave_idx_type, octave_idx_type): sparse matrix filled");
          return *d;
        }

      octave_idx_type to_move = c[ncols] - i;
      if (to_move != 0)
        {
          for (octave_idx_type j = c[ncols]; j > i; j--)
            {
              d[j] = d[j-1];
              r[j] = r[j-1];
            }
        }

      for (octave_idx_type j = _c + 1; j < ncols + 1; j++)
        c[j] = c[j] + 1;

      d[i] = 0.;
      r[i] = _r;

      return d[i];
    }
  else
    {
      (*current_liboctave_error_handler)
        ("Sparse::SparseRep::elem (octave_idx_type, octave_idx_type): sparse matrix filled");
      return *d;
    }
}

template <class T>
T
Sparse<T>::SparseRep::celem (octave_idx_type _r, octave_idx_type _c) const
{
  if (nzmx > 0)
    for (octave_idx_type i = c[_c]; i < c[_c + 1]; i++)
      if (r[i] == _r)
        return d[i];
  return T ();
}

template <class T>
void
Sparse<T>::SparseRep::maybe_compress (bool remove_zeros)
{
  if (remove_zeros)
    {
      octave_idx_type i = 0;
      octave_idx_type k = 0;
      for (octave_idx_type j = 1; j <= ncols; j++)
        {
          octave_idx_type u = c[j];
          for (i = i; i < u; i++)
            if (d[i] != T ())
              {
                d[k] = d[i];
                r[k++] = r[i];
              }
          c[j] = k;
        }
    }

  change_length (c[ncols]);
}

template <class T>
void
Sparse<T>::SparseRep::change_length (octave_idx_type nz)
{
  for (octave_idx_type j = ncols; j > 0 && c[j] > nz; j--)
    c[j] = nz;

  // We shall skip reallocation if we have less than 1/frac extra elements to
  // discard.
  static const int frac = 5;
  if (nz > nzmx || nz < nzmx - nzmx/frac)
    {
      // Reallocate.
      octave_idx_type min_nzmx = std::min (nz, nzmx);

      octave_idx_type * new_ridx = new octave_idx_type [nz];
      std::copy (r, r + min_nzmx, new_ridx);

      delete [] r;
      r = new_ridx;

      T * new_data = new T [nz];
      std::copy (d, d + min_nzmx, new_data);

      delete [] d;
      d = new_data;

      nzmx = nz;
    }
}

template <class T>
bool
Sparse<T>::SparseRep::indices_ok (void) const
{
  return sparse_indices_ok (r, c, nrows, ncols, nnz ());
}

template <class T>
Sparse<T>::Sparse (octave_idx_type nr, octave_idx_type nc, T val)
  : rep (0), dimensions (dim_vector (nr, nc))
{
  if (val != T ())
    {
      rep = new typename Sparse<T>::SparseRep (nr, nc, dimensions.safe_numel ());

      octave_idx_type ii = 0;
      xcidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              xdata (ii) = val;
              xridx (ii++) = i;
            }
          xcidx (j+1) = ii;
        }
    }
  else
    {
      rep = new typename Sparse<T>::SparseRep (nr, nc, 0);
      for (octave_idx_type j = 0; j < nc+1; j++)
        xcidx (j) = 0;
    }
}

template <class T>
Sparse<T>::Sparse (const dim_vector& dv)
  : rep (0), dimensions (dv)
{
  if (dv.length () != 2)
    (*current_liboctave_error_handler)
      ("Sparse::Sparse (const dim_vector&): dimension mismatch");
  else
    rep = new typename Sparse<T>::SparseRep (dv(0), dv(1), 0);
}

template <class T>
Sparse<T>::Sparse (const Sparse<T>& a, const dim_vector& dv)
  : rep (0), dimensions (dv)
{

  // Work in unsigned long long to avoid overflow issues with numel
  unsigned long long a_nel = static_cast<unsigned long long>(a.rows ()) *
                             static_cast<unsigned long long>(a.cols ());
  unsigned long long dv_nel = static_cast<unsigned long long>(dv (0)) *
                              static_cast<unsigned long long>(dv (1));

  if (a_nel != dv_nel)
    (*current_liboctave_error_handler)
      ("Sparse::Sparse (const Sparse&, const dim_vector&): dimension mismatch");
  else
    {
      dim_vector old_dims = a.dims ();
      octave_idx_type new_nzmx = a.nnz ();
      octave_idx_type new_nr = dv (0);
      octave_idx_type new_nc = dv (1);
      octave_idx_type old_nr = old_dims (0);
      octave_idx_type old_nc = old_dims (1);

      rep = new typename Sparse<T>::SparseRep (new_nr, new_nc, new_nzmx);

      octave_idx_type kk = 0;
      xcidx (0) = 0;
      for (octave_idx_type i = 0; i < old_nc; i++)
        for (octave_idx_type j = a.cidx (i); j < a.cidx (i+1); j++)
          {
            octave_idx_type tmp = i * old_nr + a.ridx (j);
            octave_idx_type ii = tmp % new_nr;
            octave_idx_type jj = (tmp - ii) / new_nr;
            for (octave_idx_type k = kk; k < jj; k++)
              xcidx (k+1) = j;
            kk = jj;
            xdata (j) = a.data (j);
            xridx (j) = ii;
          }
      for (octave_idx_type k = kk; k < new_nc; k++)
        xcidx (k+1) = new_nzmx;
    }
}

template <class T>
Sparse<T>::Sparse (const Array<T>& a, const idx_vector& r,
                   const idx_vector& c, octave_idx_type nr,
                   octave_idx_type nc, bool sum_terms,
                   octave_idx_type nzm)
  : rep (0), dimensions ()
{
  if (nr < 0)
    nr = r.extent (0);
  else if (r.extent (nr) > nr)
    (*current_liboctave_error_handler) ("sparse: row index %d out of bound %d",
                                        r.extent (nr), nr);

  if (nc < 0)
    nc = c.extent (0);
  else if (c.extent (nc) > nc)
    (*current_liboctave_error_handler)
      ("sparse: column index %d out of bound %d", r.extent (nc), nc);

  dimensions = dim_vector (nr, nc);

  octave_idx_type n = a.numel ();
  octave_idx_type rl = r.length (nr);
  octave_idx_type cl = c.length (nc);
  bool a_scalar = n == 1;
  if (a_scalar)
    {
      if (rl != 1)
        n = rl;
      else if (cl != 1)
        n = cl;
    }

  if ((rl != 1 && rl != n) || (cl != 1 && cl != n))
    (*current_liboctave_error_handler) ("sparse: dimension mismatch");

  // Only create rep after input validation to avoid memory leak.
  rep = new typename Sparse<T>::SparseRep (nr, nc, (nzm > 0 ? nzm : 0));

  if (rl <= 1 && cl <= 1)
    {
      if (n == 1 && a(0) != T ())
        {
          change_capacity (nzm > 1 ? nzm : 1);
          xcidx (0) = 0;
          xridx (0) = r(0);
          xdata (0) = a(0);

          for (octave_idx_type j = 0; j < nc; j++)
            xcidx (j+1) = j >= c(0);
        }
    }
  else if (a_scalar)
    {
      // This is completely specialized, because the sorts can be simplified.
      T a0 = a(0);
      if (a0 == T ())
        {
          // Do nothing, it's an empty matrix.
        }
      else if (cl == 1)
        {
          // Sparse column vector. Sort row indices.
          idx_vector rs = r.sorted ();

          octave_quit ();

          const octave_idx_type *rd = rs.raw ();
          // Count unique indices.
          octave_idx_type new_nz = 1;
          for (octave_idx_type i = 1; i < n; i++)
            new_nz += rd[i-1] != rd[i];
          // Allocate result.
          change_capacity (nzm > new_nz ? nzm : new_nz);
          xcidx (0) = 0;
          xcidx (1) = new_nz;
          octave_idx_type *rri = ridx ();
          T *rrd = data ();

          octave_quit ();

          octave_idx_type k = -1;
          octave_idx_type l = -1;

          if (sum_terms)
            {
              // Sum repeated indices.
              for (octave_idx_type i = 0; i < n; i++)
                {
                  if (rd[i] != l)
                    {
                      l = rd[i];
                      rri[++k] = rd[i];
                      rrd[k] = a0;
                    }
                  else
                    rrd[k] += a0;
                }
            }
          else
            {
              // Pick the last one.
              for (octave_idx_type i = 0; i < n; i++)
                {
                  if (rd[i] != l)
                    {
                      l = rd[i];
                      rri[++k] = rd[i];
                      rrd[k] = a0;
                    }
                }
            }

        }
      else
        {
          idx_vector rr = r;
          idx_vector cc = c;
          const octave_idx_type *rd = rr.raw ();
          const octave_idx_type *cd = cc.raw ();
          OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, ci, nc+1, 0);
          ci[0] = 0;
          // Bin counts of column indices.
          for (octave_idx_type i = 0; i < n; i++)
            ci[cd[i]+1]++;
          // Make them cumulative, shifted one to right.
          for (octave_idx_type i = 1, s = 0; i <= nc; i++)
            {
              octave_idx_type s1 = s + ci[i];
              ci[i] = s;
              s = s1;
            }

          octave_quit ();

          // Bucket sort.
          OCTAVE_LOCAL_BUFFER (octave_idx_type, sidx, n);
          for (octave_idx_type i = 0; i < n; i++)
            if (rl == 1)
              sidx[ci[cd[i]+1]++] = rd[0];
            else
              sidx[ci[cd[i]+1]++] = rd[i];

          // Subsorts. We don't need a stable sort, all values are equal.
          xcidx (0) = 0;
          for (octave_idx_type j = 0; j < nc; j++)
            {
              std::sort (sidx + ci[j], sidx + ci[j+1]);
              octave_idx_type l = -1;
              octave_idx_type nzj = 0;
              // Count.
              for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                {
                  octave_idx_type k = sidx[i];
                  if (k != l)
                    {
                      l = k;
                      nzj++;
                    }
                }
              // Set column pointer.
              xcidx (j+1) = xcidx (j) + nzj;
            }

          change_capacity (nzm > xcidx (nc) ? nzm : xcidx (nc));
          octave_idx_type *rri = ridx ();
          T *rrd = data ();

          // Fill-in data.
          for (octave_idx_type j = 0, jj = -1; j < nc; j++)
            {
              octave_quit ();
              octave_idx_type l = -1;
              if (sum_terms)
                {
                  // Sum adjacent terms.
                  for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                    {
                      octave_idx_type k = sidx[i];
                      if (k != l)
                        {
                          l = k;
                          rrd[++jj] = a0;
                          rri[jj] = k;
                        }
                      else
                        rrd[jj] += a0;
                    }
                }
              else
                {
                  // Use the last one.
                  for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                    {
                      octave_idx_type k = sidx[i];
                      if (k != l)
                        {
                          l = k;
                          rrd[++jj] = a0;
                          rri[jj] = k;
                        }
                    }
                }
            }
        }
    }
  else if (cl == 1)
    {
      // Sparse column vector. Sort row indices.
      Array<octave_idx_type> rsi;
      idx_vector rs = r.sorted (rsi);

      octave_quit ();

      const octave_idx_type *rd = rs.raw ();
      const octave_idx_type *rdi = rsi.data ();
      // Count unique indices.
      octave_idx_type new_nz = 1;
      for (octave_idx_type i = 1; i < n; i++)
        new_nz += rd[i-1] != rd[i];
      // Allocate result.
      change_capacity (nzm > new_nz ? nzm : new_nz);
      xcidx (0) = 0;
      xcidx (1) = new_nz;
      octave_idx_type *rri = ridx ();
      T *rrd = data ();

      octave_quit ();

      octave_idx_type k = 0;
      rri[k] = rd[0];
      rrd[k] = a(rdi[0]);

      if (sum_terms)
        {
          // Sum repeated indices.
          for (octave_idx_type i = 1; i < n; i++)
            {
              if (rd[i] != rd[i-1])
                {
                  rri[++k] = rd[i];
                  rrd[k] = a(rdi[i]);
                }
              else
                rrd[k] += a(rdi[i]);
            }
        }
      else
        {
          // Pick the last one.
          for (octave_idx_type i = 1; i < n; i++)
            {
              if (rd[i] != rd[i-1])
                rri[++k] = rd[i];
              rrd[k] = a(rdi[i]);
            }
        }

      maybe_compress (true);
    }
  else
    {
      idx_vector rr = r;
      idx_vector cc = c;
      const octave_idx_type *rd = rr.raw ();
      const octave_idx_type *cd = cc.raw ();
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, ci, nc+1, 0);
      ci[0] = 0;
      // Bin counts of column indices.
      for (octave_idx_type i = 0; i < n; i++)
        ci[cd[i]+1]++;
      // Make them cumulative, shifted one to right.
      for (octave_idx_type i = 1, s = 0; i <= nc; i++)
        {
          octave_idx_type s1 = s + ci[i];
          ci[i] = s;
          s = s1;
        }

      octave_quit ();

      typedef std::pair<octave_idx_type, octave_idx_type> idx_pair;
      // Bucket sort.
      OCTAVE_LOCAL_BUFFER (idx_pair, spairs, n);
      for (octave_idx_type i = 0; i < n; i++)
        {
          idx_pair& p = spairs[ci[cd[i]+1]++];
          if (rl == 1)
            p.first = rd[0];
          else
            p.first = rd[i];
          p.second = i;
        }

      // Subsorts. We don't need a stable sort, the second index stabilizes it.
      xcidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
        {
          std::sort (spairs + ci[j], spairs + ci[j+1]);
          octave_idx_type l = -1;
          octave_idx_type nzj = 0;
          // Count.
          for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
            {
              octave_idx_type k = spairs[i].first;
              if (k != l)
                {
                  l = k;
                  nzj++;
                }
            }
          // Set column pointer.
          xcidx (j+1) = xcidx (j) + nzj;
        }

      change_capacity (nzm > xcidx (nc) ? nzm : xcidx (nc));
      octave_idx_type *rri = ridx ();
      T *rrd = data ();

      // Fill-in data.
      for (octave_idx_type j = 0, jj = -1; j < nc; j++)
        {
          octave_quit ();
          octave_idx_type l = -1;
          if (sum_terms)
            {
              // Sum adjacent terms.
              for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                {
                  octave_idx_type k = spairs[i].first;
                  if (k != l)
                    {
                      l = k;
                      rrd[++jj] = a(spairs[i].second);
                      rri[jj] = k;
                    }
                  else
                    rrd[jj] += a(spairs[i].second);
                }
            }
          else
            {
              // Use the last one.
              for (octave_idx_type i = ci[j]; i < ci[j+1]; i++)
                {
                  octave_idx_type k = spairs[i].first;
                  if (k != l)
                    {
                      l = k;
                      rri[++jj] = k;
                    }
                  rrd[jj] = a(spairs[i].second);
                }
            }
        }

      maybe_compress (true);
    }
}

template <class T>
Sparse<T>::Sparse (const Array<T>& a)
  : rep (0), dimensions (a.dims ())
{
  if (dimensions.length () > 2)
    (*current_liboctave_error_handler)
      ("Sparse::Sparse (const Array<T>&): dimension mismatch");
  else
    {
      octave_idx_type nr = rows ();
      octave_idx_type nc = cols ();
      octave_idx_type len = a.length ();
      octave_idx_type new_nzmx = 0;

      // First count the number of nonzero terms
      for (octave_idx_type i = 0; i < len; i++)
        if (a(i) != T ())
          new_nzmx++;

      rep = new typename Sparse<T>::SparseRep (nr, nc, new_nzmx);

      octave_idx_type ii = 0;
      xcidx (0) = 0;
      for (octave_idx_type j = 0; j < nc; j++)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            if (a.elem (i,j) != T ())
              {
                xdata (ii) = a.elem (i,j);
                xridx (ii++) = i;
              }
          xcidx (j+1) = ii;
        }
    }
}

template <class T>
Sparse<T>::~Sparse (void)
{
  if (--rep->count == 0)
    delete rep;
}

template <class T>
Sparse<T>&
Sparse<T>::operator = (const Sparse<T>& a)
{
  if (this != &a)
    {
      if (--rep->count == 0)
        delete rep;

      rep = a.rep;
      rep->count++;

      dimensions = a.dimensions;
    }

  return *this;
}

template <class T>
octave_idx_type
Sparse<T>::compute_index (const Array<octave_idx_type>& ra_idx) const
{
  octave_idx_type retval = -1;

  octave_idx_type n = dimensions.length ();

  if (n > 0 && n == ra_idx.length ())
    {
      retval = ra_idx(--n);

      while (--n >= 0)
        {
          retval *= dimensions(n);
          retval += ra_idx(n);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("Sparse<T>::compute_index: invalid ra_idxing operation");

  return retval;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, octave_idx_type n) const
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, octave_idx_type n)
{
  (*current_liboctave_error_handler) ("%s (%d): range error", fcn, n);
  static T foo;
  return foo;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn, octave_idx_type i,
                        octave_idx_type j) const
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, octave_idx_type i, octave_idx_type j)
{
  (*current_liboctave_error_handler)
    ("%s (%d, %d): range error", fcn, i, j);
  static T foo;
  return foo;
}

template <class T>
T
Sparse<T>::range_error (const char *fcn,
                        const Array<octave_idx_type>& ra_idx) const
{
  std::ostringstream buf;

  buf << fcn << " (";

  octave_idx_type n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (octave_idx_type i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  std::string buf_str = buf.str ();

  (*current_liboctave_error_handler) (buf_str.c_str ());

  return T ();
}

template <class T>
T&
Sparse<T>::range_error (const char *fcn, const Array<octave_idx_type>& ra_idx)
{
  std::ostringstream buf;

  buf << fcn << " (";

  octave_idx_type n = ra_idx.length ();

  if (n > 0)
    buf << ra_idx(0);

  for (octave_idx_type i = 1; i < n; i++)
    buf << ", " << ra_idx(i);

  buf << "): range error";

  std::string buf_str = buf.str ();

  (*current_liboctave_error_handler) (buf_str.c_str ());

  static T foo;
  return foo;
}

template <class T>
Sparse<T>
Sparse<T>::reshape (const dim_vector& new_dims) const
{
  Sparse<T> retval;
  dim_vector dims2 = new_dims;

  if (dims2.length () > 2)
    {
      (*current_liboctave_warning_with_id_handler)
        ("Octave:reshape-smashes-dims",
         "reshape: sparse reshape to N-d array smashes dims");

      for (octave_idx_type i = 2; i < dims2.length (); i++)
        dims2(1) *= dims2(i);

      dims2.resize (2);
    }

  if (dimensions != dims2)
    {
      if (dimensions.numel () == dims2.numel ())
        {
          octave_idx_type new_nnz = nnz ();
          octave_idx_type new_nr = dims2 (0);
          octave_idx_type new_nc = dims2 (1);
          octave_idx_type old_nr = rows ();
          octave_idx_type old_nc = cols ();
          retval = Sparse<T> (new_nr, new_nc, new_nnz);

          octave_idx_type kk = 0;
          retval.xcidx (0) = 0;
          // quotient and remainder of  i * old_nr divided by new_nr
          // Track them individually to avoid overflow (bug #42850).
          octave_idx_type i_old_qu = 0;
          octave_idx_type i_old_rm = static_cast<octave_idx_type> (-old_nr);
          for (octave_idx_type i = 0; i < old_nc; i++)
            {
              i_old_rm += old_nr;
              if (i_old_rm >= new_nr)
                {
                  i_old_qu += i_old_rm / new_nr;
                  i_old_rm = i_old_rm % new_nr;
                }
              for (octave_idx_type j = cidx (i); j < cidx (i+1); j++)
                {
                  octave_idx_type ii, jj;
                  ii = (i_old_rm + ridx (j)) % new_nr;
                  jj = i_old_qu + (i_old_rm + ridx (j)) / new_nr;

                  // Original calculation subject to overflow
                  // ii = (i*old_nr + ridx (j)) % new_nr
                  // jj = (i*old_nr + ridx (j)) / new_nr

                  for (octave_idx_type k = kk; k < jj; k++)
                    retval.xcidx (k+1) = j;
                  kk = jj;
                  retval.xdata (j) = data (j);
                  retval.xridx (j) = ii;
                }
            }
          for (octave_idx_type k = kk; k < new_nc; k++)
            retval.xcidx (k+1) = new_nnz;
        }
      else
        {
          std::string dimensions_str = dimensions.str ();
          std::string new_dims_str = new_dims.str ();

          (*current_liboctave_error_handler)
            ("reshape: can't reshape %s array to %s array",
             dimensions_str.c_str (), new_dims_str.c_str ());
        }
    }
  else
    retval = *this;

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::permute (const Array<octave_idx_type>& perm_vec, bool) const
{
  // The only valid permutations of a sparse array are [1, 2] and [2, 1].

  bool fail = false;
  bool trans = false;

  if (perm_vec.length () == 2)
    {
      if (perm_vec(0) == 0 && perm_vec(1) == 1)
        /* do nothing */;
      else if (perm_vec(0) == 1 && perm_vec(1) == 0)
        trans = true;
      else
        fail = true;
    }
  else
    fail = true;

  if (fail)
    (*current_liboctave_error_handler)
      ("permutation vector contains an invalid element");

  return trans ? this->transpose () : *this;
}

template <class T>
void
Sparse<T>::resize1 (octave_idx_type n)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 0)
    resize (1, std::max (nc, n));
  else if (nc == 0)
    resize (nr, (n + nr - 1) / nr); // Ain't it wicked?
  else if (nr == 1)
    resize (1, n);
  else if (nc == 1)
    resize (n, 1);
  else
    gripe_invalid_resize ();
}

template <class T>
void
Sparse<T>::resize (const dim_vector& dv)
{
  octave_idx_type n = dv.length ();

  if (n != 2)
    {
      (*current_liboctave_error_handler) ("sparse array must be 2-D");
      return;
    }

  resize (dv(0), dv(1));
}

template <class T>
void
Sparse<T>::resize (octave_idx_type r, octave_idx_type c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
        ("can't resize to negative dimension");
      return;
    }

  if (r == dim1 () && c == dim2 ())
    return;

  // This wouldn't be necessary for r >= rows () if nrows wasn't part of the
  // Sparse rep. It is not good for anything in there.
  make_unique ();

  if (r < rows ())
    {
      octave_idx_type i = 0;
      octave_idx_type k = 0;
      for (octave_idx_type j = 1; j <= rep->ncols; j++)
        {
          octave_idx_type u = xcidx (j);
          for (i = i; i < u; i++)
            if (xridx (i) < r)
              {
                xdata (k) = xdata (i);
                xridx (k++) = xridx (i);
              }
          xcidx (j) = k;
        }
    }

  rep->nrows = dimensions(0) = r;

  if (c != rep->ncols)
    {
      octave_idx_type *new_cidx = new octave_idx_type [c+1];
      std::copy (rep->c, rep->c + std::min (c, rep->ncols) + 1, new_cidx);
      delete [] rep->c;
      rep->c = new_cidx;

      if (c > rep->ncols)
        std::fill_n (rep->c + rep->ncols + 1, c - rep->ncols,
                     rep->c[rep->ncols]);
    }

  rep->ncols = dimensions(1) = c;

  rep->change_length (rep->nnz ());
}

template <class T>
Sparse<T>&
Sparse<T>::insert (const Sparse<T>& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_rows = a.rows ();
  octave_idx_type a_cols = a.cols ();
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r < 0 || r + a_rows > rows () || c < 0 || c + a_cols > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  // First count the number of elements in the final array
  octave_idx_type nel = cidx (c) + a.nnz ();

  if (c + a_cols < nc)
    nel += cidx (nc) - cidx (c + a_cols);

  for (octave_idx_type i = c; i < c + a_cols; i++)
    for (octave_idx_type j = cidx (i); j < cidx (i+1); j++)
      if (ridx (j) < r || ridx (j) >= r + a_rows)
        nel++;

  Sparse<T> tmp (*this);
  --rep->count;
  rep = new typename Sparse<T>::SparseRep (nr, nc, nel);

  for (octave_idx_type i = 0; i < tmp.cidx (c); i++)
    {
      data (i) = tmp.data (i);
      ridx (i) = tmp.ridx (i);
    }
  for (octave_idx_type i = 0; i < c + 1; i++)
    cidx (i) = tmp.cidx (i);

  octave_idx_type ii = cidx (c);

  for (octave_idx_type i = c; i < c + a_cols; i++)
    {
      octave_quit ();

      for (octave_idx_type j = tmp.cidx (i); j < tmp.cidx (i+1); j++)
        if (tmp.ridx (j) < r)
          {
            data (ii) = tmp.data (j);
            ridx (ii++) = tmp.ridx (j);
          }

      octave_quit ();

      for (octave_idx_type j = a.cidx (i-c); j < a.cidx (i-c+1); j++)
        {
          data (ii) = a.data (j);
          ridx (ii++) = r + a.ridx (j);
        }

      octave_quit ();

      for (octave_idx_type j = tmp.cidx (i); j < tmp.cidx (i+1); j++)
        if (tmp.ridx (j) >= r + a_rows)
          {
            data (ii) = tmp.data (j);
            ridx (ii++) = tmp.ridx (j);
          }

      cidx (i+1) = ii;
    }

  for (octave_idx_type i = c + a_cols; i < nc; i++)
    {
      for (octave_idx_type j = tmp.cidx (i); j < tmp.cidx (i+1); j++)
        {
          data (ii) = tmp.data (j);
          ridx (ii++) = tmp.ridx (j);
        }
      cidx (i+1) = ii;
    }

  return *this;
}

template <class T>
Sparse<T>&
Sparse<T>::insert (const Sparse<T>& a, const Array<octave_idx_type>& ra_idx)
{

  if (ra_idx.length () != 2)
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  return insert (a, ra_idx (0), ra_idx (1));
}

template <class T>
Sparse<T>
Sparse<T>::transpose (void) const
{
  assert (ndims () == 2);

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  octave_idx_type nz = nnz ();
  Sparse<T> retval (nc, nr, nz);

  for (octave_idx_type i = 0; i < nz; i++)
    retval.xcidx (ridx (i) + 1)++;
  // retval.xcidx[1:nr] holds the row degrees for rows 0:(nr-1)
  nz = 0;
  for (octave_idx_type i = 1; i <= nr; i++)
    {
      const octave_idx_type tmp = retval.xcidx (i);
      retval.xcidx (i) = nz;
      nz += tmp;
    }
  // retval.xcidx[1:nr] holds row entry *start* offsets for rows 0:(nr-1)

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type k = cidx (j); k < cidx (j+1); k++)
      {
        octave_idx_type q = retval.xcidx (ridx (k) + 1)++;
        retval.xridx (q) = j;
        retval.xdata (q) = data (k);
      }
  assert (nnz () == retval.xcidx (nr));
  // retval.xcidx[1:nr] holds row entry *end* offsets for rows 0:(nr-1)
  // and retval.xcidx[0:(nr-1)] holds their row entry *start* offsets

  return retval;
}

// Lower bound lookup. Could also use octave_sort, but that has upper bound
// semantics, so requires some manipulation to set right. Uses a plain loop for
// small columns.
static octave_idx_type
lblookup (const octave_idx_type *ridx, octave_idx_type nr,
          octave_idx_type ri)
{
  if (nr <= 8)
    {
      octave_idx_type l;
      for (l = 0; l < nr; l++)
        if (ridx[l] >= ri)
          break;
      return l;
    }
  else
    return std::lower_bound (ridx, ridx + nr, ri) - ridx;
}

template <class T>
void
Sparse<T>::delete_elements (const idx_vector& idx)
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type nel = numel (); // Can throw.

  const dim_vector idx_dims = idx.orig_dimensions ();

  if (idx.extent (nel) > nel)
    gripe_del_index_out_of_range (true, idx.extent (nel), nel);
  else if (nc == 1)
    {
      // Sparse column vector.
      const Sparse<T> tmp = *this; // constant copy to prevent COW.

      octave_idx_type lb, ub;

      if (idx.is_cont_range (nel, lb, ub))
        {
          // Special-case a contiguous range.
          // Look-up indices first.
          octave_idx_type li = lblookup (tmp.ridx (), nz, lb);
          octave_idx_type ui = lblookup (tmp.ridx (), nz, ub);
          // Copy data and adjust indices.
          octave_idx_type nz_new = nz - (ui - li);
          *this = Sparse<T> (nr - (ub - lb), 1, nz_new);
          std::copy (tmp.data (), tmp.data () + li, data ());
          std::copy (tmp.ridx (), tmp.ridx () + li, xridx ());
          std::copy (tmp.data () + ui, tmp.data () + nz, xdata () + li);
          mx_inline_sub (nz - ui, xridx () + li, tmp.ridx () + ui, ub - lb);
          xcidx (1) = nz_new;
        }
      else
        {
          OCTAVE_LOCAL_BUFFER (octave_idx_type, ridx_new, nz);
          OCTAVE_LOCAL_BUFFER (T, data_new, nz);
          idx_vector sidx = idx.sorted (true);
          const octave_idx_type *sj = sidx.raw ();
          octave_idx_type sl = sidx.length (nel);
          octave_idx_type nz_new = 0;
          octave_idx_type j = 0;
          for (octave_idx_type i = 0; i < nz; i++)
            {
              octave_idx_type r = tmp.ridx (i);
              for (; j < sl && sj[j] < r; j++) ;
              if (j == sl || sj[j] > r)
                {
                  data_new[nz_new] = tmp.data (i);
                  ridx_new[nz_new++] = r - j;
                }
            }

          *this = Sparse<T> (nr - sl, 1, nz_new);
          std::copy (ridx_new, ridx_new + nz_new, ridx ());
          std::copy (data_new, data_new + nz_new, xdata ());
          xcidx (1) = nz_new;
        }
    }
  else if (nr == 1)
    {
      // Sparse row vector.
      octave_idx_type lb, ub;
      if (idx.is_cont_range (nc, lb, ub))
        {
          const Sparse<T> tmp = *this;
          octave_idx_type lbi = tmp.cidx (lb);
          octave_idx_type ubi = tmp.cidx (ub);
          octave_idx_type new_nz = nz - (ubi - lbi);
          *this = Sparse<T> (1, nc - (ub - lb), new_nz);
          std::copy (tmp.data (), tmp.data () + lbi, data ());
          std::copy (tmp.data () + ubi, tmp.data () + nz , xdata () + lbi);
          std::fill_n (ridx (), new_nz, static_cast<octave_idx_type> (0));
          std::copy (tmp.cidx () + 1, tmp.cidx () + 1 + lb, cidx () + 1);
          mx_inline_sub (nc - ub, xcidx () + 1, tmp.cidx () + ub + 1,
                         ubi - lbi);
        }
      else
        *this = index (idx.complement (nc));
    }
  else if (idx.length (nel) != 0)
    {
      if (idx.is_colon_equiv (nel))
        *this = Sparse<T> ();
      else
        {
          *this = index (idx_vector::colon);
          delete_elements (idx);
          *this = transpose (); // We want a row vector.
        }
    }
}

template <class T>
void
Sparse<T>::delete_elements (const idx_vector& idx_i, const idx_vector& idx_j)
{
  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  if (idx_i.is_colon ())
    {
      // Deleting columns.
      octave_idx_type lb, ub;
      if (idx_j.extent (nc) > nc)
        gripe_del_index_out_of_range (false, idx_j.extent (nc), nc);
      else if (idx_j.is_cont_range (nc, lb, ub))
        {
          if (lb == 0 && ub == nc)
            {
              // Delete all rows and columns.
              *this = Sparse<T> (nr, 0);
            }
          else if (nz == 0)
            {
              // No elements to preserve; adjust dimensions.
              *this = Sparse<T> (nr, nc - (ub - lb));
            }
          else
            {
              const Sparse<T> tmp = *this;
              octave_idx_type lbi = tmp.cidx (lb);
              octave_idx_type ubi = tmp.cidx (ub);
              octave_idx_type new_nz = nz - (ubi - lbi);

              *this = Sparse<T> (nr, nc - (ub - lb), new_nz);
              std::copy (tmp.data (), tmp.data () + lbi, data ());
              std::copy (tmp.ridx (), tmp.ridx () + lbi, ridx ());
              std::copy (tmp.data () + ubi, tmp.data () + nz, xdata () + lbi);
              std::copy (tmp.ridx () + ubi, tmp.ridx () + nz, xridx () + lbi);
              std::copy (tmp.cidx () + 1, tmp.cidx () + 1 + lb, cidx () + 1);
              mx_inline_sub (nc - ub, xcidx () + lb + 1,
                             tmp.cidx () + ub + 1, ubi - lbi);
            }
        }
      else
        *this = index (idx_i, idx_j.complement (nc));
    }
  else if (idx_j.is_colon ())
    {
      // Deleting rows.
      octave_idx_type lb, ub;
      if (idx_i.extent (nr) > nr)
        gripe_del_index_out_of_range (false, idx_i.extent (nr), nr);
      else if (idx_i.is_cont_range (nr, lb, ub))
        {
          if (lb == 0 && ub == nr)
            {
              // Delete all rows and columns.
              *this = Sparse<T> (0, nc);
            }
          else if (nz == 0)
            {
              // No elements to preserve; adjust dimensions.
              *this = Sparse<T> (nr - (ub - lb), nc);
            }
          else
            {
              // This is more memory-efficient than the approach below.
              const Sparse<T> tmpl = index (idx_vector (0, lb), idx_j);
              const Sparse<T> tmpu = index (idx_vector (ub, nr), idx_j);
              *this = Sparse<T> (nr - (ub - lb), nc,
                                 tmpl.nnz () + tmpu.nnz ());
              for (octave_idx_type j = 0, k = 0; j < nc; j++)
                {
                  for (octave_idx_type i = tmpl.cidx (j); i < tmpl.cidx (j+1);
                       i++)
                    {
                      xdata (k) = tmpl.data (i);
                      xridx (k++) = tmpl.ridx (i);
                    }
                  for (octave_idx_type i = tmpu.cidx (j); i < tmpu.cidx (j+1);
                       i++)
                    {
                      xdata (k) = tmpu.data (i);
                      xridx (k++) = tmpu.ridx (i) + lb;
                    }

                  xcidx (j+1) = k;
                }
            }
        }
      else
        {
          // This is done by transposing, deleting columns, then transposing
          // again.
          Sparse<T> tmp = transpose ();
          tmp.delete_elements (idx_j, idx_i);
          *this = tmp.transpose ();
        }
    }
  else
    {
      // Empty assignment (no elements to delete) is OK if there is at
      // least one zero-length index and at most one other index that is
      // non-colon (or equivalent) index.  Since we only have two
      // indices, we just need to check that we have at least one zero
      // length index.  Matlab considers "[]" to be an empty index but
      // not "false".  We accept both.

      bool empty_assignment
        = (idx_i.length (nr) == 0 || idx_j.length (nc) == 0);

      if (! empty_assignment)
        (*current_liboctave_error_handler)
          ("a null assignment can only have one non-colon index");
    }
}

template <class T>
void
Sparse<T>::delete_elements (int dim, const idx_vector& idx)
{
  if (dim == 0)
    delete_elements (idx, idx_vector::colon);
  else if (dim == 1)
    delete_elements (idx_vector::colon, idx);
  else
    {
      (*current_liboctave_error_handler)
        ("invalid dimension in delete_elements");
      return;
    }
}

template <class T>
Sparse<T>
Sparse<T>::index (const idx_vector& idx, bool resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type nel = numel (); // Can throw.

  const dim_vector idx_dims = idx.orig_dimensions ().redim (2);

  if (idx.is_colon ())
    {
      if (nc == 1)
        retval = *this;
      else
        {
          // Fast magic colon processing.
          retval = Sparse<T> (nel, 1, nz);

          for (octave_idx_type i = 0; i < nc; i++)
            {
              for (octave_idx_type j = cidx (i); j < cidx (i+1); j++)
                {
                  retval.xdata (j) = data (j);
                  retval.xridx (j) = ridx (j) + i * nr;
                }
            }

          retval.xcidx (0) = 0;
          retval.xcidx (1) = nz;
        }
    }
  else if (idx.extent (nel) > nel)
    {
      // resize_ok is completely handled here.
      if (resize_ok)
        {
          octave_idx_type ext = idx.extent (nel);
          Sparse<T> tmp = *this;
          tmp.resize1 (ext);
          retval = tmp.index (idx);
        }
      else
        gripe_index_out_of_range (1, 1, idx.extent (nel), nel);
    }
  else if (nr == 1 && nc == 1)
    {
      // You have to be pretty sick to get to this bit of code,
      // since you have a scalar stored as a sparse matrix, and
      // then want to make a dense matrix with sparse
      // representation. Ok, we'll do it, but you deserve what
      // you get!!
      retval = Sparse<T> (idx_dims(0), idx_dims(1), nz ? data (0) : T ());
    }
  else if (nc == 1)
    {
      // Sparse column vector.
      octave_idx_type lb, ub;

      if (idx.is_scalar ())
        {
          // Scalar index - just a binary lookup.
          octave_idx_type i = lblookup (ridx (), nz, idx(0));
          if (i < nz && ridx (i) == idx(0))
            retval = Sparse (1, 1, data (i));
          else
            retval = Sparse (1, 1);
        }
      else if (idx.is_cont_range (nel, lb, ub))
        {
          // Special-case a contiguous range.
          // Look-up indices first.
          octave_idx_type li = lblookup (ridx (), nz, lb);
          octave_idx_type ui = lblookup (ridx (), nz, ub);
          // Copy data and adjust indices.
          octave_idx_type nz_new = ui - li;
          retval = Sparse<T> (ub - lb, 1, nz_new);
          std::copy (data () + li, data () + li + nz_new, retval.data ());
          mx_inline_sub (nz_new, retval.xridx (), ridx () + li, lb);
          retval.xcidx (1) = nz_new;
        }
      else if (idx.is_permutation (nel) && idx.is_vector ())
        {
          if (idx.is_range () && idx.increment () == -1)
            {
              retval = Sparse<T> (nr, 1, nz);

              for (octave_idx_type j = 0; j < nz; j++)
                retval.ridx (j) = nr - ridx (nz - j - 1) - 1;

              std::copy (cidx (), cidx () + 2, retval.cidx ());
              std::reverse_copy (data (), data () + nz, retval.data ());
            }
          else
            {
              Array<T> tmp = array_value ();
              tmp = tmp.index (idx);
              retval = Sparse<T> (tmp);
            }
        }
      else
        {
          // If indexing a sparse column vector by a vector, the result is a
          // sparse column vector, otherwise it inherits the shape of index.
          // Vector transpose is cheap, so do it right here.

          Array<octave_idx_type> tmp_idx = idx.as_array ().as_matrix ();

          const Array<octave_idx_type> idxa = (idx_dims(0) == 1
                                               ? tmp_idx.transpose ()
                                               : tmp_idx);

          octave_idx_type new_nr = idxa.rows ();
          octave_idx_type new_nc = idxa.cols ();

          // Lookup.
          // FIXME: Could specialize for sorted idx?
          NoAlias< Array<octave_idx_type> > lidx (dim_vector (new_nr, new_nc));
          for (octave_idx_type i = 0; i < new_nr*new_nc; i++)
            lidx(i) = lblookup (ridx (), nz, idxa(i));

          // Count matches.
          retval = Sparse<T> (idxa.rows (), idxa.cols ());
          for (octave_idx_type j = 0; j < new_nc; j++)
            {
              octave_idx_type nzj = 0;
              for (octave_idx_type i = 0; i < new_nr; i++)
                {
                  octave_idx_type l = lidx(i, j);
                  if (l < nz && ridx (l) == idxa(i, j))
                    nzj++;
                  else
                    lidx(i, j) = nz;
                }
              retval.xcidx (j+1) = retval.xcidx (j) + nzj;
            }

          retval.change_capacity (retval.xcidx (new_nc));

          // Copy data and set row indices.
          octave_idx_type k = 0;
          for (octave_idx_type j = 0; j < new_nc; j++)
            for (octave_idx_type i = 0; i < new_nr; i++)
              {
                octave_idx_type l = lidx(i, j);
                if (l < nz)
                  {
                    retval.data (k) = data (l);
                    retval.xridx (k++) = i;
                  }
              }
        }
    }
  else if (nr == 1)
    {
      octave_idx_type lb, ub;
      if (idx.is_scalar ())
        retval = Sparse<T> (1, 1, elem (0, idx(0)));
      else if (idx.is_cont_range (nel, lb, ub))
        {
          // Special-case a contiguous range.
          octave_idx_type lbi = cidx (lb);
          octave_idx_type ubi = cidx (ub);
          octave_idx_type new_nz = ubi - lbi;
          retval = Sparse<T> (1, ub - lb, new_nz);
          std::copy (data () + lbi, data () + lbi + new_nz, retval.data ());
          std::fill_n (retval.ridx (), new_nz, static_cast<octave_idx_type> (0));
          mx_inline_sub (ub - lb + 1, retval.cidx (), cidx () + lb, lbi);
        }
      else
        {
          // Sparse row vectors occupy O(nr) storage anyway, so let's just
          // convert the matrix to full, index, and sparsify the result.
          retval = Sparse<T> (array_value ().index (idx));
        }
    }
  else
    {
      if (nr != 0 && idx.is_scalar ())
        retval = Sparse<T> (1, 1, elem (idx(0) % nr, idx(0) / nr));
      else
        {
          // Indexing a non-vector sparse matrix by linear indexing.
          // I suppose this is rare (and it may easily overflow), so let's take
          // the easy way, and reshape first to column vector, which is already
          // handled above.
          retval = index (idx_vector::colon).index (idx);
          // In this case we're supposed to always inherit the shape, but
          // column(row) doesn't do it, so we'll do it instead.
          if (idx_dims(0) == 1 && idx_dims(1) != 1)
            retval = retval.transpose ();
        }
    }

  return retval;
}

template <class T>
Sparse<T>
Sparse<T>::index (const idx_vector& idx_i, const idx_vector& idx_j,
                  bool resize_ok) const
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  octave_idx_type n = idx_i.length (nr);
  octave_idx_type m = idx_j.length (nc);

  octave_idx_type lb, ub;

  if (idx_i.extent (nr) > nr || idx_j.extent (nc) > nc)
    {
      // resize_ok is completely handled here.
      if (resize_ok)
        {
          octave_idx_type ext_i = idx_i.extent (nr);
          octave_idx_type ext_j = idx_j.extent (nc);
          Sparse<T> tmp = *this;
          tmp.resize (ext_i, ext_j);
          retval = tmp.index (idx_i, idx_j);
        }
      else if (idx_i.extent (nr) > nr)
        gripe_index_out_of_range (2, 1, idx_i.extent (nr), nr);
      else
        gripe_index_out_of_range (2, 2, idx_j.extent (nc), nc);
    }
  else if (nr == 1 && nc == 1)
    {
      // Scalars stored as sparse matrices occupy more memory than
      // a scalar, so let's just convert the matrix to full, index,
      // and sparsify the result.

      retval = Sparse<T> (array_value ().index (idx_i, idx_j));
    }
  else if (idx_i.is_colon ())
    {
      // Great, we're just manipulating columns. This is going to be quite
      // efficient, because the columns can stay compressed as they are.
      if (idx_j.is_colon ())
        retval = *this; // Shallow copy.
      else if (idx_j.is_cont_range (nc, lb, ub))
        {
          // Special-case a contiguous range.
          octave_idx_type lbi = cidx (lb);
          octave_idx_type ubi = cidx (ub);
          octave_idx_type new_nz = ubi - lbi;
          retval = Sparse<T> (nr, ub - lb, new_nz);
          std::copy (data () + lbi, data () + lbi + new_nz, retval.data ());
          std::copy (ridx () + lbi, ridx () + lbi + new_nz, retval.ridx ());
          mx_inline_sub (ub - lb + 1, retval.cidx (), cidx () + lb, lbi);
        }
      else
        {
          // Count new nonzero elements.
          retval = Sparse<T> (nr, m);
          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_idx_type jj = idx_j(j);
              retval.xcidx (j+1) = retval.xcidx (j) + (cidx (jj+1) - cidx (jj));
            }

          retval.change_capacity (retval.xcidx (m));

          // Copy data & indices.
          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_idx_type ljj = cidx (idx_j(j));
              octave_idx_type lj = retval.xcidx (j);
              octave_idx_type nzj = retval.xcidx (j+1) - lj;

              std::copy (data () + ljj, data () + ljj + nzj, retval.data () + lj);
              std::copy (ridx () + ljj, ridx () + ljj + nzj, retval.ridx () + lj);
            }
        }
    }
  else if (nc == 1 && idx_j.is_colon_equiv (nc) && idx_i.is_vector ())
    {
      // It's actually vector indexing. The 1D index is specialized for that.
      retval = index (idx_i);

      // If nr == 1 then the vector indexing will return a column vector!!
      if (nr == 1)
        retval.transpose ();
    }
  else if (idx_i.is_scalar ())
    {
      octave_idx_type ii = idx_i(0);
      retval = Sparse<T> (1, m);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ij, m);
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_quit ();
          octave_idx_type jj = idx_j(j);
          octave_idx_type lj = cidx (jj);
          octave_idx_type nzj = cidx (jj+1) - cidx (jj);

          // Scalar index - just a binary lookup.
          octave_idx_type i = lblookup (ridx () + lj, nzj, ii);
          if (i < nzj && ridx (i+lj) == ii)
            {
              ij[j] = i + lj;
              retval.xcidx (j+1) = retval.xcidx (j) + 1;
            }
          else
            retval.xcidx (j+1) = retval.xcidx (j);
        }

      retval.change_capacity (retval.xcidx (m));

      // Copy data, adjust row indices.
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_idx_type i = retval.xcidx (j);
          if (retval.xcidx (j+1) > i)
            {
              retval.xridx (i) = 0;
              retval.xdata (i) = data (ij[j]);
            }
        }
    }
  else if (idx_i.is_cont_range (nr, lb, ub))
    {
      retval = Sparse<T> (n, m);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, li, m);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ui, m);
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_quit ();
          octave_idx_type jj = idx_j(j);
          octave_idx_type lj = cidx (jj);
          octave_idx_type nzj = cidx (jj+1) - cidx (jj);
          octave_idx_type lij, uij;

          // Lookup indices.
          li[j] = lij = lblookup (ridx () + lj, nzj, lb) + lj;
          ui[j] = uij = lblookup (ridx () + lj, nzj, ub) + lj;
          retval.xcidx (j+1) = retval.xcidx (j) + ui[j] - li[j];
        }

      retval.change_capacity (retval.xcidx (m));

      // Copy data, adjust row indices.
      for (octave_idx_type j = 0, k = 0; j < m; j++)
        {
          octave_quit ();
          for (octave_idx_type i = li[j]; i < ui[j]; i++)
            {
              retval.xdata (k) = data (i);
              retval.xridx (k++) = ridx (i) - lb;
            }
        }
    }
  else if (idx_i.is_permutation (nr))
    {
      // The columns preserve their length, just need to renumber and sort them.
      // Count new nonzero elements.
      retval = Sparse<T> (nr, m);
      for (octave_idx_type j = 0; j < m; j++)
        {
          octave_idx_type jj = idx_j(j);
          retval.xcidx (j+1) = retval.xcidx (j) + (cidx (jj+1) - cidx (jj));
        }

      retval.change_capacity (retval.xcidx (m));

      octave_quit ();

      if (idx_i.is_range () && idx_i.increment () == -1)
        {
          // It's nr:-1:1. Just flip all columns.
          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_quit ();
              octave_idx_type jj = idx_j(j);
              octave_idx_type lj = cidx (jj);
              octave_idx_type nzj = cidx (jj+1) - cidx (jj);
              octave_idx_type li = retval.xcidx (j);
              octave_idx_type uj = lj + nzj - 1;
              for (octave_idx_type i = 0; i < nzj; i++)
                {
                  retval.xdata (li + i) = data (uj - i); // Copy in reverse order.
                  retval.xridx (li + i) = nr - 1 - ridx (uj - i); // Ditto with transform.
                }
            }
        }
      else
        {
          // Get inverse permutation.
          idx_vector idx_iinv = idx_i.inverse_permutation (nr);
          const octave_idx_type *iinv = idx_iinv.raw ();

          // Scatter buffer.
          OCTAVE_LOCAL_BUFFER (T, scb, nr);
          octave_idx_type *rri = retval.ridx ();

          for (octave_idx_type j = 0; j < m; j++)
            {
              octave_quit ();
              octave_idx_type jj = idx_j(j);
              octave_idx_type lj = cidx (jj);
              octave_idx_type nzj = cidx (jj+1) - cidx (jj);
              octave_idx_type li = retval.xcidx (j);
              // Scatter the column, transform indices.
              for (octave_idx_type i = 0; i < nzj; i++)
                scb[rri[li + i] = iinv[ridx (lj + i)]] = data (lj + i);

              octave_quit ();

              // Sort them.
              std::sort (rri + li, rri + li + nzj);

              // Gather.
              for (octave_idx_type i = 0; i < nzj; i++)
                retval.xdata (li + i) = scb[rri[li + i]];
            }
        }

    }
  else if (idx_j.is_colon ())
    {
      // This requires  uncompressing columns, which is generally costly,
      // so we rely on the efficient transpose to handle this.
      // It may still make sense to optimize some cases here.
      retval = transpose ();
      retval = retval.index (idx_vector::colon, idx_i);
      retval = retval.transpose ();
    }
  else
    {
      // A(I, J) is decomposed into A(:, J)(I, :).
      retval = index (idx_vector::colon, idx_j);
      retval = retval.index (idx_i, idx_vector::colon);
    }

  return retval;
}

template <class T>
void
Sparse<T>::assign (const idx_vector& idx, const Sparse<T>& rhs)
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type n = numel (); // Can throw.

  octave_idx_type rhl = rhs.numel ();

  if (idx.length (n) == rhl)
    {
      if (rhl == 0)
        return;

      octave_idx_type nx = idx.extent (n);
      // Try to resize first if necessary.
      if (nx != n)
        {
          resize1 (nx);
          n = numel ();
          nr = rows ();
          nc = cols ();
          // nz is preserved.
        }

      if (idx.is_colon ())
        {
          *this = rhs.reshape (dimensions);
        }
      else if (nc == 1 && rhs.cols () == 1)
        {
          // Sparse column vector to sparse column vector assignment.

          octave_idx_type lb, ub;
          if (idx.is_cont_range (nr, lb, ub))
            {
              // Special-case a contiguous range.
              // Look-up indices first.
              octave_idx_type li = lblookup (ridx (), nz, lb);
              octave_idx_type ui = lblookup (ridx (), nz, ub);
              octave_idx_type rnz = rhs.nnz ();
              octave_idx_type new_nz = nz - (ui - li) + rnz;

              if (new_nz >= nz && new_nz <= capacity ())
                {
                  // Adding/overwriting elements, enough capacity allocated.

                  if (new_nz > nz)
                    {
                      // Make room first.
                      std::copy_backward (data () + ui, data () + nz,
                                          data () + nz + rnz);
                      std::copy_backward (ridx () + ui, ridx () + nz,
                                          ridx () + nz + rnz);
                    }

                  // Copy data and adjust indices from rhs.
                  std::copy (rhs.data (), rhs.data () + rnz, data () + li);
                  mx_inline_add (rnz, ridx () + li, rhs.ridx (), lb);
                }
              else
                {
                  // Clearing elements or exceeding capacity, allocate afresh
                  // and paste pieces.
                  const Sparse<T> tmp = *this;
                  *this = Sparse<T> (nr, 1, new_nz);

                  // Head ...
                  std::copy (tmp.data (), tmp.data () + li, data ());
                  std::copy (tmp.ridx (), tmp.ridx () + li, ridx ());

                  // new stuff ...
                  std::copy (rhs.data (), rhs.data () + rnz, data () + li);
                  mx_inline_add (rnz, ridx () + li, rhs.ridx (), lb);

                  // ...tail
                  std::copy (tmp.data () + ui, tmp.data () + nz,
                             data () + li + rnz);
                  std::copy (tmp.ridx () + ui, tmp.ridx () + nz,
                             ridx () + li + rnz);
                }

              cidx (1) = new_nz;
            }
          else if (idx.is_range () && idx.increment () == -1)
            {
              // It's s(u:-1:l) = r. Reverse the assignment.
              assign (idx.sorted (), rhs.index (idx_vector (rhl - 1, 0, -1)));
            }
          else if (idx.is_permutation (n))
            {
              *this = rhs.index (idx.inverse_permutation (n));
            }
          else if (rhs.nnz () == 0)
            {
              // Elements are being zeroed.
              octave_idx_type *ri = ridx ();
              for (octave_idx_type i = 0; i < rhl; i++)
                {
                  octave_idx_type iidx = idx(i);
                  octave_idx_type li = lblookup (ri, nz, iidx);
                  if (li != nz && ri[li] == iidx)
                    xdata (li) = T ();
                }

              maybe_compress (true);
            }
          else
            {
              const Sparse<T> tmp = *this;
              octave_idx_type new_nz = nz + rhl;
              // Disassembly our matrix...
              Array<octave_idx_type> new_ri (dim_vector (new_nz, 1));
              Array<T> new_data (dim_vector (new_nz, 1));
              std::copy (tmp.ridx (), tmp.ridx () + nz, new_ri.fortran_vec ());
              std::copy (tmp.data (), tmp.data () + nz, new_data.fortran_vec ());
              // ... insert new data (densified) ...
              idx.copy_data (new_ri.fortran_vec () + nz);
              new_data.assign (idx_vector (nz, new_nz), rhs.array_value ());
              // ... reassembly.
              *this = Sparse<T> (new_data, new_ri,
                                 static_cast<octave_idx_type> (0),
                                 nr, nc, false);
            }
        }
      else
        {
          dim_vector save_dims = dimensions;
          *this = index (idx_vector::colon);
          assign (idx, rhs.index (idx_vector::colon));
          *this = reshape (save_dims);
        }
    }
  else if (rhl == 1)
    {
      rhl = idx.length (n);
      if (rhs.nnz () != 0)
        assign (idx, Sparse<T> (rhl, 1, rhs.data (0)));
      else
        assign (idx, Sparse<T> (rhl, 1));
    }
  else
    gripe_invalid_assignment_size ();
}

template <class T>
void
Sparse<T>::assign (const idx_vector& idx_i,
                   const idx_vector& idx_j, const Sparse<T>& rhs)
{
  Sparse<T> retval;

  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();
  octave_idx_type nz = nnz ();

  octave_idx_type n = rhs.rows ();
  octave_idx_type m = rhs.columns ();

  // FIXME: this should probably be written more like the
  // Array<T>::assign function...

  bool orig_zero_by_zero = (nr == 0 && nc == 0);

  if (orig_zero_by_zero || (idx_i.length (nr) == n && idx_j.length (nc) == m))
    {
      octave_idx_type nrx;
      octave_idx_type ncx;

      if (orig_zero_by_zero)
        {
          if (idx_i.is_colon ())
            {
              nrx = n;

              if (idx_j.is_colon ())
                ncx = m;
              else
                ncx = idx_j.extent (nc);
            }
          else if (idx_j.is_colon ())
            {
              nrx = idx_i.extent (nr);
              ncx = m;
            }
          else
            {
              nrx = idx_i.extent (nr);
              ncx = idx_j.extent (nc);
            }
        }
      else
        {
          nrx = idx_i.extent (nr);
          ncx = idx_j.extent (nc);
        }

      // Try to resize first if necessary.
      if (nrx != nr || ncx != nc)
        {
          resize (nrx, ncx);
          nr = rows ();
          nc = cols ();
          // nz is preserved.
        }

      if (n == 0 || m == 0)
        return;

      if (idx_i.is_colon ())
        {
          octave_idx_type lb, ub;
          // Great, we're just manipulating columns. This is going to be quite
          // efficient, because the columns can stay compressed as they are.
          if (idx_j.is_colon ())
            *this = rhs; // Shallow copy.
          else if (idx_j.is_cont_range (nc, lb, ub))
            {
              // Special-case a contiguous range.
              octave_idx_type li = cidx (lb);
              octave_idx_type ui = cidx (ub);
              octave_idx_type rnz = rhs.nnz ();
              octave_idx_type new_nz = nz - (ui - li) + rnz;

              if (new_nz >= nz && new_nz <= capacity ())
                {
                  // Adding/overwriting elements, enough capacity allocated.

                  if (new_nz > nz)
                    {
                      // Make room first.
                      std::copy_backward (data () + ui, data () + nz,
                                          data () + new_nz);
                      std::copy_backward (ridx () + ui, ridx () + nz,
                                          ridx () + new_nz);
                      mx_inline_add2 (nc - ub, cidx () + ub + 1, new_nz - nz);
                    }

                  // Copy data and indices from rhs.
                  std::copy (rhs.data (), rhs.data () + rnz, data () + li);
                  std::copy (rhs.ridx (), rhs.ridx () + rnz, ridx () + li);
                  mx_inline_add (ub - lb, cidx () + lb + 1, rhs.cidx () + 1,
                                 li);

                  assert (nnz () == new_nz);
                }
              else
                {
                  // Clearing elements or exceeding capacity, allocate afresh
                  // and paste pieces.
                  const Sparse<T> tmp = *this;
                  *this = Sparse<T> (nr, nc, new_nz);

                  // Head...
                  std::copy (tmp.data (), tmp.data () + li, data ());
                  std::copy (tmp.ridx (), tmp.ridx () + li, ridx ());
                  std::copy (tmp.cidx () + 1, tmp.cidx () + 1 + lb, cidx () + 1);

                  // new stuff...
                  std::copy (rhs.data (), rhs.data () + rnz, data () + li);
                  std::copy (rhs.ridx (), rhs.ridx () + rnz, ridx () + li);
                  mx_inline_add (ub - lb, cidx () + lb + 1, rhs.cidx () + 1,
                                 li);

                  // ...tail.
                  std::copy (tmp.data () + ui, tmp.data () + nz,
                             data () + li + rnz);
                  std::copy (tmp.ridx () + ui, tmp.ridx () + nz,
                             ridx () + li + rnz);
                  mx_inline_add (nc - ub, cidx () + ub + 1,
                                 tmp.cidx () + ub + 1, new_nz - nz);

                  assert (nnz () == new_nz);
                }
            }
          else if (idx_j.is_range () && idx_j.increment () == -1)
            {
              // It's s(:,u:-1:l) = r. Reverse the assignment.
              assign (idx_i, idx_j.sorted (),
                      rhs.index (idx_i, idx_vector (m - 1, 0, -1)));
            }
          else if (idx_j.is_permutation (nc))
            {
              *this = rhs.index (idx_i, idx_j.inverse_permutation (nc));
            }
          else
            {
              const Sparse<T> tmp = *this;
              *this = Sparse<T> (nr, nc);
              OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, jsav, nc, -1);

              // Assemble column lengths.
              for (octave_idx_type i = 0; i < nc; i++)
                xcidx (i+1) = tmp.cidx (i+1) - tmp.cidx (i);

              for (octave_idx_type i = 0; i < m; i++)
                {
                  octave_idx_type j =idx_j(i);
                  jsav[j] = i;
                  xcidx (j+1) = rhs.cidx (i+1) - rhs.cidx (i);
                }

              // Make cumulative.
              for (octave_idx_type i = 0; i < nc; i++)
                xcidx (i+1) += xcidx (i);

              change_capacity (nnz ());

              // Merge columns.
              for (octave_idx_type i = 0; i < nc; i++)
                {
                  octave_idx_type l = xcidx (i);
                  octave_idx_type u = xcidx (i+1);
                  octave_idx_type j = jsav[i];
                  if (j >= 0)
                    {
                      // from rhs
                      octave_idx_type k = rhs.cidx (j);
                      std::copy (rhs.data () + k, rhs.data () + k + u - l,
                                 xdata () + l);
                      std::copy (rhs.ridx () + k, rhs.ridx () + k + u - l,
                                 xridx () + l);
                    }
                  else
                    {
                      // original
                      octave_idx_type k = tmp.cidx (i);
                      std::copy (tmp.data () + k, tmp.data () + k + u - l,
                                 xdata () + l);
                      std::copy (tmp.ridx () + k, tmp.ridx () + k + u - l,
                                 xridx () + l);
                    }
                }

            }
        }
      else if (nc == 1 && idx_j.is_colon_equiv (nc) && idx_i.is_vector ())
        {
          // It's just vector indexing.  The 1D assign is specialized for that.
          assign (idx_i, rhs);
        }
      else if (idx_j.is_colon ())
        {
          if (idx_i.is_permutation (nr))
            {
              *this = rhs.index (idx_i.inverse_permutation (nr), idx_j);
            }
          else
            {
              // FIXME: optimize more special cases?
              // In general this requires unpacking the columns, which is slow,
              // especially for many small columns. OTOH, transpose is an
              // efficient O(nr+nc+nnz) operation.
              *this = transpose ();
              assign (idx_vector::colon, idx_i, rhs.transpose ());
              *this = transpose ();
            }
        }
      else
        {
          // Split it into 2 assignments and one indexing.
          Sparse<T> tmp = index (idx_vector::colon, idx_j);
          tmp.assign (idx_i, idx_vector::colon, rhs);
          assign (idx_vector::colon, idx_j, tmp);
        }
    }
  else if (m == 1 && n == 1)
    {
      n = idx_i.length (nr);
      m = idx_j.length (nc);
      if (rhs.nnz () != 0)
        assign (idx_i, idx_j, Sparse<T> (n, m, rhs.data (0)));
      else
        assign (idx_i, idx_j, Sparse<T> (n, m));
    }
  else
    gripe_assignment_dimension_mismatch ();
}

// Can't use versions of these in Array.cc due to duplication of the
// instantiations for Array<double and Sparse<double>, etc
template <class T>
bool
sparse_ascending_compare (typename ref_param<T>::type a,
                          typename ref_param<T>::type b)
{
  return (a < b);
}

template <class T>
bool
sparse_descending_compare (typename ref_param<T>::type a,
                           typename ref_param<T>::type b)
{
  return (a > b);
}

template <class T>
Sparse<T>
Sparse<T>::sort (octave_idx_type dim, sortmode mode) const
{
  Sparse<T> m = *this;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (m.length () < 1 || dim > 1)
    return m;

  if (dim > 0)
    {
      m = m.transpose ();
      nr = m.rows ();
      nc = m.columns ();
    }

  octave_sort<T> lsort;

  if (mode == ASCENDING)
    lsort.set_compare (sparse_ascending_compare<T>);
  else if (mode == DESCENDING)
    lsort.set_compare (sparse_descending_compare<T>);
  else
    abort ();

  T *v = m.data ();
  octave_idx_type *mcidx = m.cidx ();
  octave_idx_type *mridx = m.ridx ();

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_idx_type ns = mcidx[j + 1] - mcidx[j];
      lsort.sort (v, ns);

      octave_idx_type i;
      if (mode == ASCENDING)
        {
          for (i = 0; i < ns; i++)
            if (sparse_ascending_compare<T> (static_cast<T> (0), v[i]))
              break;
        }
      else
        {
          for (i = 0; i < ns; i++)
            if (sparse_descending_compare<T> (static_cast<T> (0), v[i]))
              break;
        }
      for (octave_idx_type k = 0; k < i; k++)
        mridx[k] = k;
      for (octave_idx_type k = i; k < ns; k++)
        mridx[k] = k - ns + nr;

      v += ns;
      mridx += ns;
    }

  if (dim > 0)
    m = m.transpose ();

  return m;
}

template <class T>
Sparse<T>
Sparse<T>::sort (Array<octave_idx_type> &sidx, octave_idx_type dim,
                 sortmode mode) const
{
  Sparse<T> m = *this;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (m.length () < 1 || dim > 1)
    {
      sidx = Array<octave_idx_type> (dim_vector (nr, nc), 1);
      return m;
    }

  if (dim > 0)
    {
      m = m.transpose ();
      nr = m.rows ();
      nc = m.columns ();
    }

  octave_sort<T> indexed_sort;

  if (mode == ASCENDING)
    indexed_sort.set_compare (sparse_ascending_compare<T>);
  else if (mode == DESCENDING)
    indexed_sort.set_compare (sparse_descending_compare<T>);
  else
    abort ();

  T *v = m.data ();
  octave_idx_type *mcidx = m.cidx ();
  octave_idx_type *mridx = m.ridx ();

  sidx = Array<octave_idx_type> (dim_vector (nr, nc));
  OCTAVE_LOCAL_BUFFER (octave_idx_type, vi, nr);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      octave_idx_type ns = mcidx[j + 1] - mcidx[j];
      octave_idx_type offset = j * nr;

      if (ns == 0)
        {
          for (octave_idx_type k = 0; k < nr; k++)
            sidx (offset + k) = k;
        }
      else
        {
          for (octave_idx_type i = 0; i < ns; i++)
            vi[i] = mridx[i];

          indexed_sort.sort (v, vi, ns);

          octave_idx_type i;
          if (mode == ASCENDING)
            {
              for (i = 0; i < ns; i++)
                if (sparse_ascending_compare<T> (static_cast<T> (0), v[i]))
                  break;
            }
          else
            {
              for (i = 0; i < ns; i++)
                if (sparse_descending_compare<T> (static_cast<T> (0), v[i]))
                  break;
            }

          octave_idx_type ii = 0;
          octave_idx_type jj = i;
          for (octave_idx_type k = 0; k < nr; k++)
            {
              if (ii < ns && mridx[ii] == k)
                ii++;
              else
                sidx (offset + jj++) = k;
            }

          for (octave_idx_type k = 0; k < i; k++)
            {
              sidx (k + offset) = vi[k];
              mridx[k] = k;
            }

          for (octave_idx_type k = i; k < ns; k++)
            {
              sidx (k - ns + nr + offset) = vi[k];
              mridx[k] = k - ns + nr;
            }

          v += ns;
          mridx += ns;
        }
    }

  if (dim > 0)
    {
      m = m.transpose ();
      sidx = sidx.transpose ();
    }

  return m;
}

template <class T>
Sparse<T>
Sparse<T>::diag (octave_idx_type k) const
{
  octave_idx_type nnr = rows ();
  octave_idx_type nnc = cols ();
  Sparse<T> d;

  if (nnr == 0 || nnc == 0)
    ; // do nothing
  else if (nnr != 1 && nnc != 1)
    {
      if (k > 0)
        nnc -= k;
      else if (k < 0)
        nnr += k;

      if (nnr > 0 && nnc > 0)
        {
          octave_idx_type ndiag = (nnr < nnc) ? nnr : nnc;

          // Count the number of nonzero elements
          octave_idx_type nel = 0;
          if (k > 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                if (elem (i, i+k) != 0.)
                  nel++;
            }
          else if (k < 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                if (elem (i-k, i) != 0.)
                  nel++;
            }
          else
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                if (elem (i, i) != 0.)
                  nel++;
            }

          d = Sparse<T> (ndiag, 1, nel);
          d.xcidx (0) = 0;
          d.xcidx (1) = nel;

          octave_idx_type ii = 0;
          if (k > 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                {
                  T tmp = elem (i, i+k);
                  if (tmp != 0.)
                    {
                      d.xdata (ii) = tmp;
                      d.xridx (ii++) = i;
                    }
                }
            }
          else if (k < 0)
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                {
                  T tmp = elem (i-k, i);
                  if (tmp != 0.)
                    {
                      d.xdata (ii) = tmp;
                      d.xridx (ii++) = i;
                    }
                }
            }
          else
            {
              for (octave_idx_type i = 0; i < ndiag; i++)
                {
                  T tmp = elem (i, i);
                  if (tmp != 0.)
                    {
                      d.xdata (ii) = tmp;
                      d.xridx (ii++) = i;
                    }
                }
            }
        }
      else
        {
          // Matlab returns [] 0x1 for out-of-range diagonal

          octave_idx_type nr = 0;
          octave_idx_type nc = 1;
          octave_idx_type nz = 0;

          d = Sparse<T> (nr, nc, nz);
        }
    }
  else if (nnr != 0 && nnc != 0)
    {
      octave_idx_type roff = 0;
      octave_idx_type coff = 0;
      if (k > 0)
        {
          roff = 0;
          coff = k;
        }
      else if (k < 0)
        {
          roff = -k;
          coff = 0;
        }

      if (nnr == 1)
        {
          octave_idx_type n = nnc + std::abs (k);
          octave_idx_type nz = nnz ();

          d = Sparse<T> (n, n, nz);

          if (nnz () > 0)
            {
              for (octave_idx_type i = 0; i < coff+1; i++)
                d.xcidx (i) = 0;

              for (octave_idx_type j = 0; j < nnc; j++)
                {
                  for (octave_idx_type i = cidx (j); i < cidx (j+1); i++)
                    {
                      d.xdata (i) = data (i);
                      d.xridx (i) = j + roff;
                    }
                  d.xcidx (j + coff + 1) = cidx (j+1);
                }

              for (octave_idx_type i = nnc + coff + 1; i < n + 1; i++)
                d.xcidx (i) = nz;
            }
        }
      else
        {
          octave_idx_type n = nnr + std::abs (k);
          octave_idx_type nz = nnz ();

          d = Sparse<T> (n, n, nz);

          if (nnz () > 0)
            {
              octave_idx_type ii = 0;
              octave_idx_type ir = ridx (0);

              for (octave_idx_type i = 0; i < coff+1; i++)
                d.xcidx (i) = 0;

              for (octave_idx_type i = 0; i < nnr; i++)
                {
                  if (ir == i)
                    {
                      d.xdata (ii) = data (ii);
                      d.xridx (ii++) = ir + roff;

                      if (ii != nz)
                        ir = ridx (ii);
                    }
                  d.xcidx (i + coff + 1) = ii;
                }

              for (octave_idx_type i = nnr + coff + 1; i < n+1; i++)
                d.xcidx (i) = nz;
            }
        }
    }

  return d;
}

template <class T>
Sparse<T>
Sparse<T>::cat (int dim, octave_idx_type n, const Sparse<T> *sparse_list)
{
  // Default concatenation.
  bool (dim_vector::*concat_rule) (const dim_vector&, int) = &dim_vector::concat;

  if (dim == -1 || dim == -2)
    {
      concat_rule = &dim_vector::hvcat;
      dim = -dim - 1;
    }
  else if (dim < 0)
    (*current_liboctave_error_handler)
      ("cat: invalid dimension");

  dim_vector dv;
  octave_idx_type total_nz = 0;
  if (dim == 0 || dim == 1)
    {
      if (n == 1)
        return sparse_list[0];

      for (octave_idx_type i = 0; i < n; i++)
        {
          if (! (dv.*concat_rule) (sparse_list[i].dims (), dim))
            (*current_liboctave_error_handler)
              ("cat: dimension mismatch");
          total_nz += sparse_list[i].nnz ();
        }
    }
  else
    (*current_liboctave_error_handler)
      ("cat: invalid dimension for sparse concatenation");

  Sparse<T> retval (dv, total_nz);

  if (retval.is_empty ())
    return retval;

  switch (dim)
    {
    case 0:
      {
        // sparse vertcat.  This is not efficiently handled by assignment,
        // so we'll do it directly.
        octave_idx_type l = 0;
        for (octave_idx_type j = 0; j < dv(1); j++)
          {
            octave_quit ();

            octave_idx_type rcum = 0;
            for (octave_idx_type i = 0; i < n; i++)
              {
                const Sparse<T>& spi = sparse_list[i];
                // Skipping empty matrices. See the comment in Array.cc.
                if (spi.is_empty ())
                  continue;

                octave_idx_type kl = spi.cidx (j);
                octave_idx_type ku = spi.cidx (j+1);
                for (octave_idx_type k = kl; k < ku; k++, l++)
                  {
                    retval.xridx (l) = spi.ridx (k) + rcum;
                    retval.xdata (l) = spi.data (k);
                  }

                rcum += spi.rows ();
              }

            retval.xcidx (j+1) = l;
          }

        break;
      }
    case 1:
      {
        octave_idx_type l = 0;
        for (octave_idx_type i = 0; i < n; i++)
          {
            octave_quit ();

            // Skipping empty matrices. See the comment in Array.cc.
            if (sparse_list[i].is_empty ())
              continue;

            octave_idx_type u = l + sparse_list[i].columns ();
            retval.assign (idx_vector::colon, idx_vector (l, u),
                           sparse_list[i]);
            l = u;
          }

        break;
      }
    default:
      assert (false);
    }

  return retval;
}

template <class T>
Array<T>
Sparse<T>::array_value () const
{
  NoAlias< Array<T> > retval (dims (), T ());
  if (rows () == 1)
    {
      octave_idx_type i = 0;
      for (octave_idx_type j = 0, nc = cols (); j < nc; j++)
        {
          if (cidx (j+1) > i)
            retval(j) = data (i++);
        }
    }
  else
    {
      for (octave_idx_type j = 0, nc = cols (); j < nc; j++)
        for (octave_idx_type i = cidx (j), iu = cidx (j+1); i < iu; i++)
          retval(ridx (i), j) = data (i);
    }

  return retval;
}

/*
 * Tests
 *

%!function x = set_slice (x, dim, slice, arg)
%!  switch (dim)
%!    case 11
%!      x(slice) = 2;
%!    case 21
%!      x(slice, :) = 2;
%!    case 22
%!      x(:, slice) = 2;
%!    otherwise
%!      error ("invalid dim, '%d'", dim);
%!  endswitch
%!endfunction

%!function x = set_slice2 (x, dim, slice)
%!  switch (dim)
%!    case 11
%!      x(slice) = 2 * ones (size (slice));
%!    case 21
%!      x(slice, :) = 2 * ones (length (slice), columns (x));
%!    case 22
%!      x(:, slice) = 2 * ones (rows (x), length (slice));
%!    otherwise
%!      error ("invalid dim, '%d'", dim);
%!  endswitch
%!endfunction

%!function test_sparse_slice (size, dim, slice)
%!  x = ones (size);
%!  s = set_slice (sparse (x), dim, slice);
%!  f = set_slice (x, dim, slice);
%!  assert (nnz (s), nnz (f));
%!  assert (full (s), f);
%!  s = set_slice2 (sparse (x), dim, slice);
%!  f = set_slice2 (x, dim, slice);
%!  assert (nnz (s), nnz (f));
%!  assert (full (s), f);
%!endfunction

#### 1d indexing

## size = [2 0]
%!test test_sparse_slice ([2 0], 11, []);
%!assert (set_slice (sparse (ones ([2 0])), 11, 1), sparse ([2 0]'))  # sparse different from full
%!assert (set_slice (sparse (ones ([2 0])), 11, 2), sparse ([0 2]'))  # sparse different from full
%!assert (set_slice (sparse (ones ([2 0])), 11, 3), sparse ([0 0; 2 0]'))  # sparse different from full
%!assert (set_slice (sparse (ones ([2 0])), 11, 4), sparse ([0 0; 0 2]'))  # sparse different from full

## size = [0 2]
%!test test_sparse_slice ([0 2], 11, []);
%!assert (set_slice (sparse (ones ([0 2])), 11, 1), sparse ([2 0]))  # sparse different from full
%!test test_sparse_slice ([0 2], 11, 2);
%!test test_sparse_slice ([0 2], 11, 3);
%!test test_sparse_slice ([0 2], 11, 4);
%!test test_sparse_slice ([0 2], 11, [4, 4]);

## size = [2 1]
%!test test_sparse_slice ([2 1], 11, []);
%!test test_sparse_slice ([2 1], 11, 1);
%!test test_sparse_slice ([2 1], 11, 2);
%!test test_sparse_slice ([2 1], 11, 3);
%!test test_sparse_slice ([2 1], 11, 4);
%!test test_sparse_slice ([2 1], 11, [4, 4]);

## size = [1 2]
%!test test_sparse_slice ([1 2], 11, []);
%!test test_sparse_slice ([1 2], 11, 1);
%!test test_sparse_slice ([1 2], 11, 2);
%!test test_sparse_slice ([1 2], 11, 3);
%!test test_sparse_slice ([1 2], 11, 4);
%!test test_sparse_slice ([1 2], 11, [4, 4]);

## size = [2 2]
%!test test_sparse_slice ([2 2], 11, []);
%!test test_sparse_slice ([2 2], 11, 1);
%!test test_sparse_slice ([2 2], 11, 2);
%!test test_sparse_slice ([2 2], 11, 3);
%!test test_sparse_slice ([2 2], 11, 4);
%!test test_sparse_slice ([2 2], 11, [4, 4]);
# These 2 errors are the same as in the full case
%!error id=Octave:invalid-resize set_slice (sparse (ones ([2 2])), 11, 5)
%!error id=Octave:invalid-resize set_slice (sparse (ones ([2 2])), 11, 6)


#### 2d indexing

## size = [2 0]
%!test test_sparse_slice ([2 0], 21, []);
%!test test_sparse_slice ([2 0], 21, 1);
%!test test_sparse_slice ([2 0], 21, 2);
%!test test_sparse_slice ([2 0], 21, [2,2]);
%!assert (set_slice (sparse (ones ([2 0])), 21, 3), sparse (3,0))
%!assert (set_slice (sparse (ones ([2 0])), 21, 4), sparse (4,0))
%!test test_sparse_slice ([2 0], 22, []);
%!test test_sparse_slice ([2 0], 22, 1);
%!test test_sparse_slice ([2 0], 22, 2);
%!test test_sparse_slice ([2 0], 22, [2,2]);
%!assert (set_slice (sparse (ones ([2 0])), 22, 3), sparse ([0 0 2;0 0 2]))  # sparse different from full
%!assert (set_slice (sparse (ones ([2 0])), 22, 4), sparse ([0 0 0 2;0 0 0 2]))  # sparse different from full

## size = [0 2]
%!test test_sparse_slice ([0 2], 21, []);
%!test test_sparse_slice ([0 2], 21, 1);
%!test test_sparse_slice ([0 2], 21, 2);
%!test test_sparse_slice ([0 2], 21, [2,2]);
%!assert (set_slice (sparse (ones ([0 2])), 21, 3), sparse ([0 0;0 0;2 2]))  # sparse different from full
%!assert (set_slice (sparse (ones ([0 2])), 21, 4), sparse ([0 0;0 0;0 0;2 2]))  # sparse different from full
%!test test_sparse_slice ([0 2], 22, []);
%!test test_sparse_slice ([0 2], 22, 1);
%!test test_sparse_slice ([0 2], 22, 2);
%!test test_sparse_slice ([0 2], 22, [2,2]);
%!assert (set_slice (sparse (ones ([0 2])), 22, 3), sparse (0,3))
%!assert (set_slice (sparse (ones ([0 2])), 22, 4), sparse (0,4))

## size = [2 1]
%!test test_sparse_slice ([2 1], 21, []);
%!test test_sparse_slice ([2 1], 21, 1);
%!test test_sparse_slice ([2 1], 21, 2);
%!test test_sparse_slice ([2 1], 21, [2,2]);
%!test test_sparse_slice ([2 1], 21, 3);
%!test test_sparse_slice ([2 1], 21, 4);
%!test test_sparse_slice ([2 1], 22, []);
%!test test_sparse_slice ([2 1], 22, 1);
%!test test_sparse_slice ([2 1], 22, 2);
%!test test_sparse_slice ([2 1], 22, [2,2]);
%!test test_sparse_slice ([2 1], 22, 3);
%!test test_sparse_slice ([2 1], 22, 4);

## size = [1 2]
%!test test_sparse_slice ([1 2], 21, []);
%!test test_sparse_slice ([1 2], 21, 1);
%!test test_sparse_slice ([1 2], 21, 2);
%!test test_sparse_slice ([1 2], 21, [2,2]);
%!test test_sparse_slice ([1 2], 21, 3);
%!test test_sparse_slice ([1 2], 21, 4);
%!test test_sparse_slice ([1 2], 22, []);
%!test test_sparse_slice ([1 2], 22, 1);
%!test test_sparse_slice ([1 2], 22, 2);
%!test test_sparse_slice ([1 2], 22, [2,2]);
%!test test_sparse_slice ([1 2], 22, 3);
%!test test_sparse_slice ([1 2], 22, 4);

## size = [2 2]
%!test test_sparse_slice ([2 2], 21, []);
%!test test_sparse_slice ([2 2], 21, 1);
%!test test_sparse_slice ([2 2], 21, 2);
%!test test_sparse_slice ([2 2], 21, [2,2]);
%!test test_sparse_slice ([2 2], 21, 3);
%!test test_sparse_slice ([2 2], 21, 4);
%!test test_sparse_slice ([2 2], 22, []);
%!test test_sparse_slice ([2 2], 22, 1);
%!test test_sparse_slice ([2 2], 22, 2);
%!test test_sparse_slice ([2 2], 22, [2,2]);
%!test test_sparse_slice ([2 2], 22, 3);
%!test test_sparse_slice ([2 2], 22, 4);

bug #35570:

%!assert (speye (3,1)(3:-1:1), sparse ([0; 0; 1]))

## Test removing columns (bug #36656)

%!test
%! s = sparse (magic (5));
%! s(:,2:4) = [];
%! assert (s, sparse (magic (5)(:, [1,5])));

%!test
%! s = sparse ([], [], [], 1, 1);
%! s(1,:) = [];
%! assert (s, sparse ([], [], [], 0, 1));

## Test (bug #37321)
%!test a=sparse (0,0); assert (all (a) == sparse ([1]));
%!test a=sparse (0,1); assert (all (a) == sparse ([1]));
%!test a=sparse (1,0); assert (all (a) == sparse ([1]));
%!test a=sparse (1,0); assert (all (a,2) == sparse ([1]));
%!test a=sparse (1,0); assert (size (all (a,1)), [1 0]);
%!test a=sparse (1,1);
%! assert (all (a) == sparse ([0]));
%! assert (size (all (a)), [1 1]);
%!test a=sparse (2,1);
%! assert (all (a) == sparse ([0]));
%! assert (size (all (a)), [1 1]);
%!test a=sparse (1,2);
%! assert (all (a) == sparse ([0]));
%! assert (size (all (a)), [1 1]);
%!test a=sparse (2,2); assert (isequal (all (a), sparse ([0 0])));

*/

template <class T>
void
Sparse<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << "\n"
     << prefix << "rep->nzmx:   " << rep->nzmx  << "\n"
     << prefix << "rep->nrows:  " << rep->nrows << "\n"
     << prefix << "rep->ncols:  " << rep->ncols << "\n"
     << prefix << "rep->data:   " << static_cast<void *> (rep->d) << "\n"
     << prefix << "rep->ridx:   " << static_cast<void *> (rep->r) << "\n"
     << prefix << "rep->cidx:   " << static_cast<void *> (rep->c) << "\n"
     << prefix << "rep->count:  " << rep->count << "\n";
}

#define INSTANTIATE_SPARSE(T, API) \
  template class API Sparse<T>;

