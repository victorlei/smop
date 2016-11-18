// Template array classes
/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague

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

#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <new>

#include "Array.h"
#include "Array-util.h"
#include "idx-vector.h"
#include "lo-error.h"
#include "oct-locbuf.h"

// One dimensional array class.  Handles the reference counting for
// all the derived classes.

template <class T>
typename Array<T>::ArrayRep *
Array<T>::nil_rep (void)
{
  static ArrayRep nr;
  return &nr;
}

template <class T>
Array<T>::Array (const Array<T>& a, const dim_vector& dv)
  : dimensions (dv), rep (a.rep),
    slice_data (a.slice_data), slice_len (a.slice_len)
{
  if (dimensions.safe_numel () != a.numel ())
    {
      std::string dimensions_str = a.dimensions.str ();
      std::string new_dims_str = dimensions.str ();

      (*current_liboctave_error_handler)
        ("reshape: can't reshape %s array to %s array",
         dimensions_str.c_str (), new_dims_str.c_str ());
    }

  // This goes here because if an exception is thrown by the above,
  // destructor will be never called.
  rep->count++;
  dimensions.chop_trailing_singletons ();
}

template <class T>
void
Array<T>::fill (const T& val)
{
  if (rep->count > 1)
    {
      --rep->count;
      rep = new ArrayRep (length (), val);
      slice_data = rep->data;
    }
  else
    std::fill_n (slice_data, slice_len, val);
}

template <class T>
void
Array<T>::clear (void)
{
  if (--rep->count == 0)
    delete rep;

  rep = nil_rep ();
  rep->count++;
  slice_data = rep->data;
  slice_len = rep->len;

  dimensions = dim_vector ();
}

template <class T>
void
Array<T>::clear (const dim_vector& dv)
{
  if (--rep->count == 0)
    delete rep;

  rep = new ArrayRep (dv.safe_numel ());
  slice_data = rep->data;
  slice_len = rep->len;

  dimensions = dv;
  dimensions.chop_trailing_singletons ();
}

template <class T>
Array<T>
Array<T>::squeeze (void) const
{
  Array<T> retval = *this;

  if (ndims () > 2)
    {
      bool dims_changed = false;

      dim_vector new_dimensions = dimensions;

      int k = 0;

      for (int i = 0; i < ndims (); i++)
        {
          if (dimensions(i) == 1)
            dims_changed = true;
          else
            new_dimensions(k++) = dimensions(i);
        }

      if (dims_changed)
        {
          switch (k)
            {
            case 0:
              new_dimensions = dim_vector (1, 1);
              break;

            case 1:
              {
                octave_idx_type tmp = new_dimensions(0);

                new_dimensions.resize (2);

                new_dimensions(0) = tmp;
                new_dimensions(1) = 1;
              }
              break;

            default:
              new_dimensions.resize (k);
              break;
            }
        }

      retval = Array<T> (*this, new_dimensions);
    }

  return retval;
}

template <class T>
octave_idx_type
Array<T>::compute_index (octave_idx_type i, octave_idx_type j) const
{
  return ::compute_index (i, j, dimensions);
}

template <class T>
octave_idx_type
Array<T>::compute_index (octave_idx_type i, octave_idx_type j,
                         octave_idx_type k) const
{
  return ::compute_index (i, j, k, dimensions);
}

template <class T>
octave_idx_type
Array<T>::compute_index (const Array<octave_idx_type>& ra_idx) const
{
  return ::compute_index (ra_idx, dimensions);
}

template <class T>
T&
Array<T>::checkelem (octave_idx_type n)
{
  // Do checks directly to avoid recomputing slice_len.
  if (n < 0)
    gripe_invalid_index ();
  if (n >= slice_len)
    gripe_index_out_of_range (1, 1, n+1, slice_len);

  return elem (n);
}

template <class T>
T&
Array<T>::checkelem (octave_idx_type i, octave_idx_type j)
{
  return elem (compute_index (i, j));
}

template <class T>
T&
Array<T>::checkelem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
{
  return elem (compute_index (i, j, k));
}

template <class T>
T&
Array<T>::checkelem (const Array<octave_idx_type>& ra_idx)
{
  return elem (compute_index (ra_idx));
}

template <class T>
typename Array<T>::crefT
Array<T>::checkelem (octave_idx_type n) const
{
  // Do checks directly to avoid recomputing slice_len.
  if (n < 0)
    gripe_invalid_index ();
  if (n >= slice_len)
    gripe_index_out_of_range (1, 1, n+1, slice_len);

  return elem (n);
}

template <class T>
typename Array<T>::crefT
Array<T>::checkelem (octave_idx_type i, octave_idx_type j) const
{
  return elem (compute_index (i, j));
}

template <class T>
typename Array<T>::crefT
Array<T>::checkelem (octave_idx_type i, octave_idx_type j,
                     octave_idx_type k) const
{
  return elem (compute_index (i, j, k));
}

template <class T>
typename Array<T>::crefT
Array<T>::checkelem (const Array<octave_idx_type>& ra_idx) const
{
  return elem (compute_index (ra_idx));
}

template <class T>
Array<T>
Array<T>::column (octave_idx_type k) const
{
  octave_idx_type r = dimensions(0);
#ifdef BOUNDS_CHECKING
  if (k < 0 || k > dimensions.numel (1))
    gripe_index_out_of_range (2, 2, k+1, dimensions.numel (1));
#endif

  return Array<T> (*this, dim_vector (r, 1), k*r, k*r + r);
}

template <class T>
Array<T>
Array<T>::page (octave_idx_type k) const
{
  octave_idx_type r = dimensions(0);
  octave_idx_type c = dimensions(1);
  octave_idx_type p = r*c;
#ifdef BOUNDS_CHECKING
  if (k < 0 || k > dimensions.numel (2))
    gripe_index_out_of_range (3, 3, k+1, dimensions.numel (2));
#endif

  return Array<T> (*this, dim_vector (r, c), k*p, k*p + p);
}

template <class T>
Array<T>
Array<T>::linear_slice (octave_idx_type lo, octave_idx_type up) const
{
#ifdef BOUNDS_CHECKING
  if (lo < 0)
    gripe_index_out_of_range (1, 1, lo+1, numel ());
  if (up > numel ())
    gripe_index_out_of_range (1, 1, up, numel ());
#endif
  if (up < lo) up = lo;
  return Array<T> (*this, dim_vector (up - lo, 1), lo, up);
}

// Helper class for multi-d dimension permuting (generalized transpose).
class rec_permute_helper
{
  // STRIDE occupies the last half of the space allocated for dim to
  // avoid a double allocation.

  int n;
  int top;
  octave_idx_type *dim;
  octave_idx_type *stride;
  bool use_blk;

public:
  rec_permute_helper (const dim_vector& dv, const Array<octave_idx_type>& perm)

    : n (dv.length ()), top (0), dim (new octave_idx_type [2*n]),
      stride (dim + n), use_blk (false)
  {
    assert (n == perm.length ());

    // Get cumulative dimensions.
    OCTAVE_LOCAL_BUFFER (octave_idx_type, cdim, n+1);
    cdim[0] = 1;
    for (int i = 1; i < n+1; i++) cdim[i] = cdim[i-1] * dv(i-1);

    // Setup the permuted strides.
    for (int k = 0; k < n; k++)
      {
        int kk = perm(k);
        dim[k] = dv(kk);
        stride[k] = cdim[kk];
      }

    // Reduce contiguous runs.
    for (int k = 1; k < n; k++)
      {
        if (stride[k] == stride[top]*dim[top])
          dim[top] *= dim[k];
        else
          {
            top++;
            dim[top] = dim[k];
            stride[top] = stride[k];
          }
      }

    // Determine whether we can use block transposes.
    use_blk = top >= 1 && stride[1] == 1 && stride[0] == dim[1];

  }

  ~rec_permute_helper (void) { delete [] dim; }

  // Helper method for fast blocked transpose.
  template <class T>
  static T *
  blk_trans (const T *src, T *dest, octave_idx_type nr, octave_idx_type nc)
  {
    static const octave_idx_type m = 8;
    OCTAVE_LOCAL_BUFFER (T, blk, m*m);
    for (octave_idx_type kr = 0; kr < nr; kr += m)
      for (octave_idx_type kc = 0; kc < nc; kc += m)
        {
          octave_idx_type lr = std::min (m, nr - kr);
          octave_idx_type lc = std::min (m, nc - kc);
          if (lr == m && lc == m)
            {
              const T *ss = src + kc * nr + kr;
              for (octave_idx_type j = 0; j < m; j++)
                for (octave_idx_type i = 0; i < m; i++)
                  blk[j*m+i] = ss[j*nr + i];
              T *dd = dest + kr * nc + kc;
              for (octave_idx_type j = 0; j < m; j++)
                for (octave_idx_type i = 0; i < m; i++)
                  dd[j*nc+i] = blk[i*m+j];
            }
          else
            {
              const T *ss = src + kc * nr + kr;
              for (octave_idx_type j = 0; j < lc; j++)
                for (octave_idx_type i = 0; i < lr; i++)
                  blk[j*m+i] = ss[j*nr + i];
              T *dd = dest + kr * nc + kc;
              for (octave_idx_type j = 0; j < lr; j++)
                for (octave_idx_type i = 0; i < lc; i++)
                  dd[j*nc+i] = blk[i*m+j];
            }
        }

    return dest + nr*nc;
  }

private:

  // Recursive N-d generalized transpose
  template <class T>
  T *do_permute (const T *src, T *dest, int lev) const
  {
    if (lev == 0)
      {
        octave_idx_type step = stride[0];
        octave_idx_type len = dim[0];
        if (step == 1)
          {
            std::copy (src, src + len, dest);
            dest += len;
          }
        else
          {
            for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
              dest[i] = src[j];

            dest += len;
          }
      }
    else if (use_blk && lev == 1)
      dest = blk_trans (src, dest, dim[1], dim[0]);
    else
      {
        octave_idx_type step = stride[lev];
        octave_idx_type len = dim[lev];
        for (octave_idx_type i = 0, j = 0; i < len; i++, j+= step)
          dest = do_permute (src + i * step, dest, lev-1);
      }

    return dest;
  }

  // No copying!

  rec_permute_helper (const rec_permute_helper&);

  rec_permute_helper& operator = (const rec_permute_helper&);

public:

  template <class T>
  void permute (const T *src, T *dest) const { do_permute (src, dest, top); }
};


template <class T>
Array<T>
Array<T>::permute (const Array<octave_idx_type>& perm_vec_arg, bool inv) const
{
  Array<T> retval;

  Array<octave_idx_type> perm_vec = perm_vec_arg;

  dim_vector dv = dims ();

  int perm_vec_len = perm_vec_arg.length ();

  if (perm_vec_len < dv.length ())
    (*current_liboctave_error_handler)
      ("%s: invalid permutation vector", inv ? "ipermute" : "permute");

  dim_vector dv_new = dim_vector::alloc (perm_vec_len);

  // Append singleton dimensions as needed.
  dv.resize (perm_vec_len, 1);

  // Need this array to check for identical elements in permutation array.
  OCTAVE_LOCAL_BUFFER_INIT (bool, checked, perm_vec_len, false);

  bool identity = true;

  // Find dimension vector of permuted array.
  for (int i = 0; i < perm_vec_len; i++)
    {
      octave_idx_type perm_elt = perm_vec.elem (i);
      if (perm_elt >= perm_vec_len || perm_elt < 0)
        {
          (*current_liboctave_error_handler)
            ("%s: permutation vector contains an invalid element",
             inv ? "ipermute" : "permute");

          return retval;
        }

      if (checked[perm_elt])
        {
          (*current_liboctave_error_handler)
            ("%s: permutation vector cannot contain identical elements",
             inv ? "ipermute" : "permute");

          return retval;
        }
      else
        {
          checked[perm_elt] = true;
          identity = identity && perm_elt == i;
        }
    }

  if (identity)
    return *this;

  if (inv)
    {
      for (int i = 0; i < perm_vec_len; i++)
        perm_vec(perm_vec_arg(i)) = i;
    }

  for (int i = 0; i < perm_vec_len; i++)
    dv_new(i) = dv(perm_vec(i));

  retval = Array<T> (dv_new);

  if (numel () > 0)
    {
      rec_permute_helper rh (dv, perm_vec);
      rh.permute (data (), retval.fortran_vec ());
    }

  return retval;
}

// Helper class for multi-d index reduction and recursive
// indexing/indexed assignment.  Rationale: we could avoid recursion
// using a state machine instead.  However, using recursion is much
// more amenable to possible parallelization in the future.
// Also, the recursion solution is cleaner and more understandable.

class rec_index_helper
{
  // CDIM occupies the last half of the space allocated for dim to
  // avoid a double allocation.

  int n;
  int top;
  octave_idx_type *dim;
  octave_idx_type *cdim;
  idx_vector *idx;

public:
  rec_index_helper (const dim_vector& dv, const Array<idx_vector>& ia)
    : n (ia.length ()), top (0), dim (new octave_idx_type [2*n]),
      cdim (dim + n), idx (new idx_vector [n])
  {
    assert (n > 0 && (dv.length () == std::max (n, 2)));

    dim[0] = dv(0);
    cdim[0] = 1;
    idx[0] = ia(0);

    for (int i = 1; i < n; i++)
      {
        // Try reduction...
        if (idx[top].maybe_reduce (dim[top], ia(i), dv(i)))
          {
            // Reduction successful, fold dimensions.
            dim[top] *= dv(i);
          }
        else
          {
            // Unsuccessful, store index & cumulative dim.
            top++;
            idx[top] = ia(i);
            dim[top] = dv(i);
            cdim[top] = cdim[top-1] * dim[top-1];
          }
      }
  }

  ~rec_index_helper (void) { delete [] idx; delete [] dim; }

private:

  // Recursive N-d indexing
  template <class T>
  T *do_index (const T *src, T *dest, int lev) const
  {
    if (lev == 0)
      dest += idx[0].index (src, dim[0], dest);
    else
      {
        octave_idx_type nn = idx[lev].length (dim[lev]);
        octave_idx_type d = cdim[lev];
        for (octave_idx_type i = 0; i < nn; i++)
          dest = do_index (src + d*idx[lev].xelem (i), dest, lev-1);
      }

    return dest;
  }

  // Recursive N-d indexed assignment
  template <class T>
  const T *do_assign (const T *src, T *dest, int lev) const
  {
    if (lev == 0)
      src += idx[0].assign (src, dim[0], dest);
    else
      {
        octave_idx_type nn = idx[lev].length (dim[lev]);
        octave_idx_type d = cdim[lev];
        for (octave_idx_type i = 0; i < nn; i++)
          src = do_assign (src, dest + d*idx[lev].xelem (i), lev-1);
      }

    return src;
  }

  // Recursive N-d indexed assignment
  template <class T>
  void do_fill (const T& val, T *dest, int lev) const
  {
    if (lev == 0)
      idx[0].fill (val, dim[0], dest);
    else
      {
        octave_idx_type nn = idx[lev].length (dim[lev]);
        octave_idx_type d = cdim[lev];
        for (octave_idx_type i = 0; i < nn; i++)
          do_fill (val, dest + d*idx[lev].xelem (i), lev-1);
      }
  }

  // No copying!

  rec_index_helper (const rec_index_helper&);

  rec_index_helper& operator = (const rec_index_helper&);

public:

  template <class T>
  void index (const T *src, T *dest) const { do_index (src, dest, top); }

  template <class T>
  void assign (const T *src, T *dest) const { do_assign (src, dest, top); }

  template <class T>
  void fill (const T& val, T *dest) const { do_fill (val, dest, top); }

  bool is_cont_range (octave_idx_type& l,
                      octave_idx_type& u) const
  {
    return top == 0 && idx[0].is_cont_range (dim[0], l, u);
  }
};

// Helper class for multi-d recursive resizing
// This handles resize () in an efficient manner, touching memory only
// once (apart from reinitialization)
class rec_resize_helper
{
  octave_idx_type *cext;
  octave_idx_type *sext;
  octave_idx_type *dext;
  int n;

public:
  rec_resize_helper (const dim_vector& ndv, const dim_vector& odv)
    : cext (0), sext (0), dext (0), n (0)
  {
    int l = ndv.length ();
    assert (odv.length () == l);
    octave_idx_type ld = 1;
    int i = 0;
    for (; i < l-1 && ndv(i) == odv(i); i++) ld *= ndv(i);
    n = l - i;
    cext = new octave_idx_type [3*n];
    // Trick to avoid three allocations
    sext = cext + n;
    dext = sext + n;

    octave_idx_type sld = ld;
    octave_idx_type dld = ld;
    for (int j = 0; j < n; j++)
      {
        cext[j] = std::min (ndv(i+j), odv(i+j));
        sext[j] = sld *= odv(i+j);
        dext[j] = dld *= ndv(i+j);
      }
    cext[0] *= ld;
  }

  ~rec_resize_helper (void) { delete [] cext; }

private:

  // recursive resizing
  template <class T>
  void do_resize_fill (const T* src, T *dest, const T& rfv, int lev) const
  {
    if (lev == 0)
      {
        std::copy (src, src+cext[0], dest);
        std::fill_n (dest + cext[0], dext[0] - cext[0], rfv);
      }
    else
      {
        octave_idx_type sd, dd, k;
        sd = sext[lev-1];
        dd = dext[lev-1];
        for (k = 0; k < cext[lev]; k++)
          do_resize_fill (src + k * sd, dest + k * dd, rfv, lev - 1);

        std::fill_n (dest + k * dd, dext[lev] - k * dd, rfv);
      }
  }

  // No copying!

  rec_resize_helper (const rec_resize_helper&);

  rec_resize_helper& operator = (const rec_resize_helper&);

public:

  template <class T>
  void resize_fill (const T* src, T *dest, const T& rfv) const
  { do_resize_fill (src, dest, rfv, n-1); }
};

template <class T>
Array<T>
Array<T>::index (const idx_vector& i) const
{
  octave_idx_type n = numel ();
  Array<T> retval;

  if (i.is_colon ())
    {
      // A(:) produces a shallow copy as a column vector.
      retval = Array<T> (*this, dim_vector (n, 1));
    }
  else
    {
      if (i.extent (n) != n)
        gripe_index_out_of_range (1, 1, i.extent (n), n); // throws

      // FIXME: this is the only place where orig_dimensions are used.
      dim_vector rd = i.orig_dimensions ();
      octave_idx_type il = i.length (n);

      // FIXME: this is for Matlab compatibility.  Matlab 2007 given
      //
      //   b = ones (3,1)
      //
      // yields the following:
      //
      //   b(zeros (0,0)) gives []
      //   b(zeros (1,0)) gives zeros (0,1)
      //   b(zeros (0,1)) gives zeros (0,1)
      //   b(zeros (0,m)) gives zeros (0,m)
      //   b(zeros (m,0)) gives zeros (m,0)
      //   b(1:2) gives ones (2,1)
      //   b(ones (2)) gives ones (2) etc.
      //
      // As you can see, the behaviour is weird, but the tests end up pretty
      // simple.  Nah, I don't want to suggest that this is ad hoc :)

      if (ndims () == 2 && n != 1 && rd.is_vector ())
        {
          if (columns () == 1)
            rd = dim_vector (il, 1);
          else if (rows () == 1)
            rd = dim_vector (1, il);
        }

      octave_idx_type l, u;
      if (il != 0 && i.is_cont_range (n, l, u))
        // If suitable, produce a shallow slice.
        retval = Array<T> (*this, rd, l, u);
      else
        {
          // Don't use resize here to avoid useless initialization for POD
          // types.
          retval = Array<T> (rd);

          if (il != 0)
            i.index (data (), n, retval.fortran_vec ());
        }
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::index (const idx_vector& i, const idx_vector& j) const
{
  // Get dimensions, allowing Fortran indexing in the 2nd dim.
  dim_vector dv = dimensions.redim (2);
  octave_idx_type r = dv(0);
  octave_idx_type c = dv(1);
  Array<T> retval;

  if (i.is_colon () && j.is_colon ())
    {
      // A(:,:) produces a shallow copy.
      retval = Array<T> (*this, dv);
    }
  else
    {
      if (i.extent (r) != r)
        gripe_index_out_of_range (2, 1, i.extent (r), r); // throws
      if (j.extent (c) != c)
        gripe_index_out_of_range (2, 2, j.extent (c), c); // throws

      octave_idx_type n = numel ();
      octave_idx_type il = i.length (r);
      octave_idx_type jl = j.length (c);

      idx_vector ii (i);

      if (ii.maybe_reduce (r, j, c))
        {
          octave_idx_type l, u;
          if (ii.length () > 0 && ii.is_cont_range (n, l, u))
            // If suitable, produce a shallow slice.
            retval = Array<T> (*this, dim_vector (il, jl), l, u);
          else
            {
              // Don't use resize to avoid useless initialization for POD types.
              retval = Array<T> (dim_vector (il, jl));

              ii.index (data (), n, retval.fortran_vec ());
            }
        }
      else
        {
          // Don't use resize to avoid useless initialization for POD types.
          retval = Array<T> (dim_vector (il, jl));

          const T* src = data ();
          T *dest = retval.fortran_vec ();

          for (octave_idx_type k = 0; k < jl; k++)
            dest += i.index (src + r * j.xelem (k), r, dest);
        }
    }

  return retval;
}

template <class T>
Array<T>
Array<T>::index (const Array<idx_vector>& ia) const
{
  int ial = ia.length ();
  Array<T> retval;

  // FIXME: is this dispatching necessary?
  if (ial == 1)
    retval = index (ia(0));
  else if (ial == 2)
    retval = index (ia(0), ia(1));
  else if (ial > 0)
    {
      // Get dimensions, allowing Fortran indexing in the last dim.
      dim_vector dv = dimensions.redim (ial);

      // Check for out of bounds conditions.
      bool all_colons = true;
      for (int i = 0; i < ial; i++)
        {
          if (ia(i).extent (dv(i)) != dv(i))
            gripe_index_out_of_range (ial, i+1, ia(i).extent (dv(i)), dv(i)); // throws

          all_colons = all_colons && ia(i).is_colon ();
        }


      if (all_colons)
        {
          // A(:,:,...,:) produces a shallow copy.
          dv.chop_trailing_singletons ();
          retval = Array<T> (*this, dv);
        }
      else
        {
          // Form result dimensions.
          dim_vector rdv = dim_vector::alloc (ial);
          for (int i = 0; i < ial; i++) rdv(i) = ia(i).length (dv(i));
          rdv.chop_trailing_singletons ();

          // Prepare for recursive indexing
          rec_index_helper rh (dv, ia);

          octave_idx_type l, u;
          if (rh.is_cont_range (l, u))
            // If suitable, produce a shallow slice.
            retval = Array<T> (*this, rdv, l, u);
          else
            {
              // Don't use resize to avoid useless initialization for POD types.
              retval = Array<T> (rdv);

              // Do it.
              rh.index (data (), retval.fortran_vec ());
            }
        }
    }

  return retval;
}

// The default fill value.  Override if you want a different one.

template <class T>
T
Array<T>::resize_fill_value (void) const
{
  static T zero = T ();
  return zero;
}

// Yes, we could do resize using index & assign.  However, that would
// possibly involve a lot more memory traffic than we actually need.

template <class T>
void
Array<T>::resize1 (octave_idx_type n, const T& rfv)
{
  if (n >= 0 && ndims () == 2)
    {
      dim_vector dv;
      // This is driven by Matlab's behaviour of giving a *row* vector
      // on some out-of-bounds assignments.  Specifically, Matlab
      // allows a(i) with out-of-bouds i when a is either of 0x0, 1x0,
      // 1x1, 0xN, and gives a row vector in all cases (yes, even the
      // last one, search me why).  Giving a column vector would make
      // much more sense (given the way trailing singleton dims are
      // treated).
      bool invalid = false;
      if (rows () == 0 || rows () == 1)
        dv = dim_vector (1, n);
      else if (columns () == 1)
        dv = dim_vector (n, 1);
      else
        invalid = true;

      if (invalid)
        gripe_invalid_resize ();
      else
        {
          octave_idx_type nx = numel ();
          if (n == nx - 1 && n > 0)
            {
              // Stack "pop" operation.
              if (rep->count == 1)
                slice_data[slice_len-1] = T ();
              slice_len--;
              dimensions = dv;
            }
          else if (n == nx + 1 && nx > 0)
            {
              // Stack "push" operation.
              if (rep->count == 1
                  && slice_data + slice_len < rep->data + rep->len)
                {
                  slice_data[slice_len++] = rfv;
                  dimensions = dv;
                }
              else
                {
                  static const octave_idx_type max_stack_chunk = 1024;
                  octave_idx_type nn = n + std::min (nx, max_stack_chunk);
                  Array<T> tmp (Array<T> (dim_vector (nn, 1)), dv, 0, n);
                  T *dest = tmp.fortran_vec ();

                  std::copy (data (), data () + nx, dest);
                  dest[nx] = rfv;

                  *this = tmp;
                }
            }
          else if (n != nx)
            {
              Array<T> tmp = Array<T> (dv);
              T *dest = tmp.fortran_vec ();

              octave_idx_type n0 = std::min (n, nx);
              octave_idx_type n1 = n - n0;
              std::copy (data (), data () + n0, dest);
              std::fill_n (dest + n0, n1, rfv);

              *this = tmp;
            }
        }
    }
  else
    gripe_invalid_resize ();
}

template <class T>
void
Array<T>::resize2 (octave_idx_type r, octave_idx_type c, const T& rfv)
{
  if (r >= 0 && c >= 0 && ndims () == 2)
    {
      octave_idx_type rx = rows ();
      octave_idx_type cx = columns ();
      if (r != rx || c != cx)
        {
          Array<T> tmp = Array<T> (dim_vector (r, c));
          T *dest = tmp.fortran_vec ();

          octave_idx_type r0 = std::min (r, rx);
          octave_idx_type r1 = r - r0;
          octave_idx_type c0 = std::min (c, cx);
          octave_idx_type c1 = c - c0;
          const T *src = data ();
          if (r == rx)
            {
              std::copy (src, src + r * c0, dest);
              dest += r * c0;
            }
          else
            {
              for (octave_idx_type k = 0; k < c0; k++)
                {
                  std::copy (src, src + r0, dest);
                  src += rx;
                  dest += r0;
                  std::fill_n (dest, r1, rfv);
                  dest += r1;
                }
            }

          std::fill_n (dest, r * c1, rfv);

          *this = tmp;
        }
    }
  else
    gripe_invalid_resize ();

}

template<class T>
void
Array<T>::resize (const dim_vector& dv, const T& rfv)
{
  int dvl = dv.length ();
  if (dvl == 2)
    resize2 (dv(0), dv(1), rfv);
  else if (dimensions != dv)
    {
      if (dimensions.length () <= dvl && ! dv.any_neg ())
        {
          Array<T> tmp (dv);
          // Prepare for recursive resizing.
          rec_resize_helper rh (dv, dimensions.redim (dvl));

          // Do it.
          rh.resize_fill (data (), tmp.fortran_vec (), rfv);
          *this = tmp;
        }
      else
        gripe_invalid_resize ();
    }
}

template <class T>
Array<T>
Array<T>::index (const idx_vector& i, bool resize_ok, const T& rfv) const
{
  Array<T> tmp = *this;
  if (resize_ok)
    {
      octave_idx_type n = numel ();
      octave_idx_type nx = i.extent (n);
      if (n != nx)
        {
          if (i.is_scalar ())
            return Array<T> (dim_vector (1, 1), rfv);
          else
            tmp.resize1 (nx, rfv);
        }

      if (tmp.numel () != nx)
        return Array<T> ();
    }

  return tmp.index (i);
}

template <class T>
Array<T>
Array<T>::index (const idx_vector& i, const idx_vector& j,
                 bool resize_ok, const T& rfv) const
{
  Array<T> tmp = *this;
  if (resize_ok)
    {
      dim_vector dv = dimensions.redim (2);
      octave_idx_type r = dv(0);
      octave_idx_type c = dv(1);
      octave_idx_type rx = i.extent (r);
      octave_idx_type cx = j.extent (c);
      if (r != rx || c != cx)
        {
          if (i.is_scalar () && j.is_scalar ())
            return Array<T> (dim_vector (1, 1), rfv);
          else
            tmp.resize2 (rx, cx, rfv);
        }

      if (tmp.rows () != rx || tmp.columns () != cx)
        return Array<T> ();
    }

  return tmp.index (i, j);
}

template <class T>
Array<T>
Array<T>::index (const Array<idx_vector>& ia,
                 bool resize_ok, const T& rfv) const
{
  Array<T> tmp = *this;
  if (resize_ok)
    {
      int ial = ia.length ();
      dim_vector dv = dimensions.redim (ial);
      dim_vector dvx = dim_vector::alloc (ial);
      for (int i = 0; i < ial; i++) dvx(i) = ia(i).extent (dv (i));
      if (! (dvx == dv))
        {
          bool all_scalars = true;
          for (int i = 0; i < ial; i++)
            all_scalars = all_scalars && ia(i).is_scalar ();
          if (all_scalars)
            return Array<T> (dim_vector (1, 1), rfv);
          else
            tmp.resize (dvx, rfv);
        }

      if (tmp.dimensions != dvx)
        return Array<T> ();
    }

  return tmp.index (ia);
}


template <class T>
void
Array<T>::assign (const idx_vector& i, const Array<T>& rhs, const T& rfv)
{
  octave_idx_type n = numel ();
  octave_idx_type rhl = rhs.numel ();

  if (rhl == 1 || i.length (n) == rhl)
    {
      octave_idx_type nx = i.extent (n);
      bool colon = i.is_colon_equiv (nx);
      // Try to resize first if necessary.
      if (nx != n)
        {
          // Optimize case A = []; A(1:n) = X with A empty.
          if (dimensions.zero_by_zero () && colon)
            {
              if (rhl == 1)
                *this = Array<T> (dim_vector (1, nx), rhs(0));
              else
                *this = Array<T> (rhs, dim_vector (1, nx));
              return;
            }

          resize1 (nx, rfv);
          n = numel ();
        }

      if (colon)
        {
          // A(:) = X makes a full fill or a shallow copy.
          if (rhl == 1)
            fill (rhs(0));
          else
            *this = rhs.reshape (dimensions);
        }
      else
        {
          if (rhl == 1)
            i.fill (rhs(0), n, fortran_vec ());
          else
            i.assign (rhs.data (), n, fortran_vec ());
        }
    }
  else
    gripe_invalid_assignment_size ();
}

template <class T>
void
Array<T>::assign (const idx_vector& i, const idx_vector& j,
                  const Array<T>& rhs, const T& rfv)
{
  bool initial_dims_all_zero = dimensions.all_zero ();

  // Get RHS extents, discarding singletons.
  dim_vector rhdv = rhs.dims ();

  // Get LHS extents, allowing Fortran indexing in the second dim.
  dim_vector dv = dimensions.redim (2);

  // Check for out-of-bounds and form resizing dimensions.
  dim_vector rdv;

  // In the special when all dimensions are zero, colons are allowed
  // to inquire the shape of RHS.  The rules are more obscure, so we
  // solve that elsewhere.
  if (initial_dims_all_zero)
    rdv = zero_dims_inquire (i, j, rhdv);
  else
    {
      rdv(0) = i.extent (dv(0));
      rdv(1) = j.extent (dv(1));
    }

  bool isfill = rhs.numel () == 1;
  octave_idx_type il = i.length (rdv(0));
  octave_idx_type jl = j.length (rdv(1));
  rhdv.chop_all_singletons ();
  bool match = (isfill
                || (rhdv.length () == 2 && il == rhdv(0) && jl == rhdv(1)));
  match = match || (il == 1 && jl == rhdv(0) && rhdv(1) == 1);

  if (match)
    {
      bool all_colons = (i.is_colon_equiv (rdv(0))
                         && j.is_colon_equiv (rdv(1)));
      // Resize if requested.
      if (rdv != dv)
        {
          // Optimize case A = []; A(1:m, 1:n) = X
          if (dv.zero_by_zero () && all_colons)
            {
              if (isfill)
                *this = Array<T> (rdv, rhs(0));
              else
                *this = Array<T> (rhs, rdv);
              return;
            }

          resize (rdv, rfv);
          dv = dimensions;
        }

      if (all_colons)
        {
          // A(:,:) = X makes a full fill or a shallow copy
          if (isfill)
            fill (rhs(0));
          else
            *this = rhs.reshape (dimensions);
        }
      else
        {
          // The actual work.
          octave_idx_type n = numel ();
          octave_idx_type r = dv(0);
          octave_idx_type c = dv(1);
          idx_vector ii (i);

          const T* src = rhs.data ();
          T *dest = fortran_vec ();

          // Try reduction first.
          if (ii.maybe_reduce (r, j, c))
            {
              if (isfill)
                ii.fill (*src, n, dest);
              else
                ii.assign (src, n, dest);
            }
          else
            {
              if (isfill)
                {
                  for (octave_idx_type k = 0; k < jl; k++)
                    i.fill (*src, r, dest + r * j.xelem (k));
                }
              else
                {
                  for (octave_idx_type k = 0; k < jl; k++)
                    src += i.assign (src, r, dest + r * j.xelem (k));
                }
            }
        }
    }
  else
    gripe_assignment_dimension_mismatch ();
}

template <class T>
void
Array<T>::assign (const Array<idx_vector>& ia,
                  const Array<T>& rhs, const T& rfv)
{
  int ial = ia.length ();

  // FIXME: is this dispatching necessary / desirable?
  if (ial == 1)
    assign (ia(0), rhs, rfv);
  else if (ial == 2)
    assign (ia(0), ia(1), rhs, rfv);
  else if (ial > 0)
    {
      bool initial_dims_all_zero = dimensions.all_zero ();

      // Get RHS extents, discarding singletons.
      dim_vector rhdv = rhs.dims ();

      // Get LHS extents, allowing Fortran indexing in the second dim.
      dim_vector dv = dimensions.redim (ial);

      // Get the extents forced by indexing.
      dim_vector rdv;

      // In the special when all dimensions are zero, colons are
      // allowed to inquire the shape of RHS.  The rules are more
      // obscure, so we solve that elsewhere.
      if (initial_dims_all_zero)
        rdv = zero_dims_inquire (ia, rhdv);
      else
        {
          rdv = dim_vector::alloc (ial);
          for (int i = 0; i < ial; i++)
            rdv(i) = ia(i).extent (dv(i));
        }

      // Check whether LHS and RHS match, up to singleton dims.
      bool match = true;
      bool all_colons = true;
      bool isfill = rhs.numel () == 1;

      rhdv.chop_all_singletons ();
      int j = 0;
      int rhdvl = rhdv.length ();
      for (int i = 0; i < ial; i++)
        {
          all_colons = all_colons && ia(i).is_colon_equiv (rdv(i));
          octave_idx_type l = ia(i).length (rdv(i));
          if (l == 1) continue;
          match = match && j < rhdvl && l == rhdv(j++);
        }

      match = match && (j == rhdvl || rhdv(j) == 1);
      match = match || isfill;

      if (match)
        {
          // Resize first if necessary.
          if (rdv != dv)
            {
              // Optimize case A = []; A(1:m, 1:n) = X
              if (dv.zero_by_zero () && all_colons)
                {
                  rdv.chop_trailing_singletons ();
                  if (isfill)
                    *this = Array<T> (rdv, rhs(0));
                  else
                    *this = Array<T> (rhs, rdv);
                  return;
                }

              resize (rdv, rfv);
              dv = rdv;
            }

          if (all_colons)
            {
              // A(:,:,...,:) = X makes a full fill or a shallow copy.
              if (isfill)
                fill (rhs(0));
              else
                *this = rhs.reshape (dimensions);
            }
          else
            {
              // Do the actual work.

              // Prepare for recursive indexing
              rec_index_helper rh (dv, ia);

              // Do it.
              if (isfill)
                rh.fill (rhs(0), fortran_vec ());
              else
                rh.assign (rhs.data (), fortran_vec ());
            }
        }
      else
        gripe_assignment_dimension_mismatch ();
    }
}

template <class T>
void
Array<T>::delete_elements (const idx_vector& i)
{
  octave_idx_type n = numel ();
  if (i.is_colon ())
    {
      *this = Array<T> ();
    }
  else if (i.length (n) != 0)
    {
      if (i.extent (n) != n)
        gripe_del_index_out_of_range (true, i.extent (n), n);

      octave_idx_type l, u;
      bool col_vec = ndims () == 2 && columns () == 1 && rows () != 1;
      if (i.is_scalar () && i(0) == n-1 && dimensions.is_vector ())
        {
          // Stack "pop" operation.
          resize1 (n-1);
        }
      else if (i.is_cont_range (n, l, u))
        {
          // Special case deleting a contiguous range.
          octave_idx_type m = n + l - u;
          Array<T> tmp (dim_vector (col_vec ? m : 1, !col_vec ? m : 1));
          const T *src = data ();
          T *dest = tmp.fortran_vec ();
          std::copy (src, src + l, dest);
          std::copy (src + u, src + n, dest + l);
          *this = tmp;
        }
      else
        {
          // Use index.
          *this = index (i.complement (n));
        }
    }
}

template <class T>
void
Array<T>::delete_elements (int dim, const idx_vector& i)
{
  if (dim < 0 || dim >= ndims ())
    {
      (*current_liboctave_error_handler)
        ("invalid dimension in delete_elements");
      return;
    }

  octave_idx_type n = dimensions (dim);
  if (i.is_colon ())
    {
      *this = Array<T> ();
    }
  else if (i.length (n) != 0)
    {
      if (i.extent (n) != n)
        gripe_del_index_out_of_range (false, i.extent (n), n);

      octave_idx_type l, u;

      if (i.is_cont_range (n, l, u))
        {
          // Special case deleting a contiguous range.
          octave_idx_type nd = n + l - u;
          octave_idx_type dl = 1;
          octave_idx_type du = 1;
          dim_vector rdv = dimensions;
          rdv(dim) = nd;
          for (int k = 0; k < dim; k++) dl *= dimensions(k);
          for (int k = dim + 1; k < ndims (); k++) du *= dimensions(k);

          // Special case deleting a contiguous range.
          Array<T> tmp = Array<T> (rdv);
          const T *src = data ();
          T *dest = tmp.fortran_vec ();
          l *= dl; u *= dl; n *= dl;
          for (octave_idx_type k = 0; k < du; k++)
            {
              std::copy (src, src + l, dest);
              dest += l;
              std::copy (src + u, src + n, dest);
              dest += n - u;
              src += n;
            }

          *this = tmp;
        }
      else
        {
          // Use index.
          Array<idx_vector> ia (dim_vector (ndims (), 1), idx_vector::colon);
          ia (dim) = i.complement (n);
          *this = index (ia);
        }
    }
}

template <class T>
void
Array<T>::delete_elements (const Array<idx_vector>& ia)
{
  int ial = ia.length ();

  if (ial == 1)
    delete_elements (ia(0));
  else
    {
      int k, dim = -1;
      for (k = 0; k < ial; k++)
        {
          if (! ia(k).is_colon ())
            {
              if (dim < 0)
                dim = k;
              else
                break;
            }
        }
      if (dim < 0)
        {
          dim_vector dv = dimensions;
          dv(0) = 0;
          *this = Array<T> (dv);
        }
      else if (k == ial)
        {
          delete_elements (dim, ia(dim));
        }
      else
        {
          // Allow the null assignment to succeed if it won't change
          // anything because the indices reference an empty slice,
          // provided that there is at most one non-colon (or
          // equivalent) index.  So, we still have the requirement of
          // deleting a slice, but it is OK if the slice is empty.

          // For compatibility with Matlab, stop checking once we see
          // more than one non-colon index or an empty index.  Matlab
          // considers "[]" to be an empty index but not "false".  We
          // accept both.

          bool empty_assignment = false;

          int num_non_colon_indices = 0;

          int nd = ndims ();

          for (int i = 0; i < ial; i++)
            {
              octave_idx_type dim_len = i >= nd ? 1 : dimensions(i);

              if (ia(i).length (dim_len) == 0)
                {
                  empty_assignment = true;
                  break;
                }

              if (! ia(i).is_colon_equiv (dim_len))
                {
                  num_non_colon_indices++;

                  if (num_non_colon_indices == 2)
                    break;
                }
            }

          if (! empty_assignment)
            (*current_liboctave_error_handler)
              ("a null assignment can only have one non-colon index");
        }
    }

}

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, octave_idx_type r, octave_idx_type c)
{
  idx_vector i (r, r + a.rows ());
  idx_vector j (c, c + a.columns ());
  if (ndims () == 2 && a.ndims () == 2)
    assign (i, j, a);
  else
    {
      Array<idx_vector> idx (dim_vector (a.ndims (), 1));
      idx(0) = i;
      idx(1) = j;
      for (int k = 2; k < a.ndims (); k++)
        idx(k) = idx_vector (0, a.dimensions(k));
      assign (idx, a);
    }

  return *this;
}

template <class T>
Array<T>&
Array<T>::insert (const Array<T>& a, const Array<octave_idx_type>& ra_idx)
{
  octave_idx_type n = ra_idx.length ();
  Array<idx_vector> idx (dim_vector (n, 1));
  const dim_vector dva = a.dims ().redim (n);
  for (octave_idx_type k = 0; k < n; k++)
    idx(k) = idx_vector (ra_idx (k), ra_idx (k) + dva(k));

  assign (idx, a);

  return *this;
}


template <class T>
Array<T>
Array<T>::transpose (void) const
{
  assert (ndims () == 2);

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr >= 8 && nc >= 8)
    {
      Array<T> result (dim_vector (nc, nr));

      // Reuse the implementation used for permuting.

      rec_permute_helper::blk_trans (data (), result.fortran_vec (), nr, nc);

      return result;
    }
  else if (nr > 1 && nc > 1)
    {
      Array<T> result (dim_vector (nc, nr));

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          result.xelem (j, i) = xelem (i, j);

      return result;
    }
  else
    {
      // Fast transpose for vectors and empty matrices.
      return Array<T> (*this, dim_vector (nc, nr));
    }
}

template <class T>
static T
no_op_fcn (const T& x)
{
  return x;
}

template <class T>
Array<T>
Array<T>::hermitian (T (*fcn) (const T&)) const
{
  assert (ndims () == 2);

  if (! fcn)
    fcn = no_op_fcn<T>;

  octave_idx_type nr = dim1 ();
  octave_idx_type nc = dim2 ();

  if (nr >= 8 && nc >= 8)
    {
      Array<T> result (dim_vector (nc, nr));

      // Blocked transpose to attempt to avoid cache misses.

      // Don't use OCTAVE_LOCAL_BUFFER here as it doesn't work with bool
      // on some compilers.
      T buf[64];

      octave_idx_type ii = 0, jj;
      for (jj = 0; jj < (nc - 8 + 1); jj += 8)
        {
          for (ii = 0; ii < (nr - 8 + 1); ii += 8)
            {
              // Copy to buffer
              for (octave_idx_type j = jj, k = 0, idxj = jj * nr;
                   j < jj + 8; j++, idxj += nr)
                for (octave_idx_type i = ii; i < ii + 8; i++)
                  buf[k++] = xelem (i + idxj);

              // Copy from buffer
              for (octave_idx_type i = ii, idxi = ii * nc; i < ii + 8;
                   i++, idxi += nc)
                for (octave_idx_type j = jj, k = i - ii; j < jj + 8;
                     j++, k+=8)
                  result.xelem (j + idxi) = fcn (buf[k]);
            }

          if (ii < nr)
            for (octave_idx_type j = jj; j < jj + 8; j++)
              for (octave_idx_type i = ii; i < nr; i++)
                result.xelem (j, i) = fcn (xelem (i, j));
        }

      for (octave_idx_type j = jj; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          result.xelem (j, i) = fcn (xelem (i, j));

      return result;
    }
  else
    {
      Array<T> result (dim_vector (nc, nr));

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          result.xelem (j, i) = fcn (xelem (i, j));

      return result;
    }
}

/*

%% Tranpose tests for matrices of the tile size and plus or minus a row
%% and with four tiles.

%!shared m7, mt7, m8, mt8, m9, mt9
%! m7 = reshape (1 : 7*8, 8, 7);
%! mt7 = [1:8; 9:16; 17:24; 25:32; 33:40; 41:48; 49:56];
%! m8 = reshape (1 : 8*8, 8, 8);
%! mt8 = mt8 = [mt7; 57:64];
%! m9 = reshape (1 : 9*8, 8, 9);
%! mt9 = [mt8; 65:72];

%!assert (m7', mt7)
%!assert ((1i*m7).', 1i * mt7)
%!assert ((1i*m7)', conj (1i * mt7))
%!assert (m8', mt8)
%!assert ((1i*m8).', 1i * mt8)
%!assert ((1i*m8)', conj (1i * mt8))
%!assert (m9', mt9)
%!assert ((1i*m9).', 1i * mt9)
%!assert ((1i*m9)', conj (1i * mt9))
%!assert ([m7, m8; m7, m8]', [mt7, mt7; mt8, mt8])
%!assert ((1i*[m7, m8; m7, m8]).', 1i * [mt7, mt7; mt8, mt8])
%!assert ((1i*[m7, m8; m7, m8])', conj (1i * [mt7, mt7; mt8, mt8]))
%!assert ([m8, m8; m8, m8]', [mt8, mt8; mt8, mt8])
%!assert ((1i*[m8, m8; m8, m8]).', 1i * [mt8, mt8; mt8, mt8])
%!assert ((1i*[m8, m8; m8, m8])', conj (1i * [mt8, mt8; mt8, mt8]))
%!assert ([m9, m8; m9, m8]', [mt9, mt9; mt8, mt8])
%!assert ((1i*[m9, m8; m9, m8]).', 1i * [mt9, mt9; mt8, mt8])
%!assert ((1i*[m9, m8; m9, m8])', conj (1i * [mt9, mt9; mt8, mt8]))

*/

template <class T>
T *
Array<T>::fortran_vec (void)
{
  make_unique ();

  return slice_data;
}

// Non-real types don't have NaNs.
template <class T>
inline bool
sort_isnan (typename ref_param<T>::type)
{
  return false;
}

template <class T>
Array<T>
Array<T>::sort (int dim, sortmode mode) const
{
  if (dim < 0)
    {
      (*current_liboctave_error_handler)
        ("sort: invalid dimension");
      return Array<T> ();
    }

  Array<T> m (dims ());

  dim_vector dv = m.dims ();

  if (m.length () < 1)
    return m;

  if (dim >= dv.length ())
    dv.resize (dim+1, 1);

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  octave_sort<T> lsort;

  if (mode != UNSORTED)
    lsort.set_compare (mode);
  else
    return m;

  if (stride == 1)
    {
      for (octave_idx_type j = 0; j < iter; j++)
        {
          // copy and partition out NaNs.
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i];
              if (sort_isnan<T> (tmp))
                v[--ku] = tmp;
              else
                v[kl++] = tmp;
            }

          // sort.
          lsort.sort (v, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (v + ku, v + ns);
              if (mode == DESCENDING)
                std::rotate (v, v + ku, v + ns);
            }

          v += ns;
          ov += ns;
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (T, buf, ns);

      for (octave_idx_type j = 0; j < iter; j++)
        {
          octave_idx_type offset = j;
          octave_idx_type offset2 = 0;

          while (offset >= stride)
            {
              offset -= stride;
              offset2++;
            }

          offset += offset2 * stride * ns;

          // gather and partition out NaNs.
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i*stride + offset];
              if (sort_isnan<T> (tmp))
                buf[--ku] = tmp;
              else
                buf[kl++] = tmp;
            }

          // sort.
          lsort.sort (buf, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (buf + ku, buf + ns);
              if (mode == DESCENDING)
                std::rotate (buf, buf + ku, buf + ns);
            }

          // scatter.
          for (octave_idx_type i = 0; i < ns; i++)
            v[i*stride + offset] = buf[i];
        }
    }

  return m;
}

template <class T>
Array<T>
Array<T>::sort (Array<octave_idx_type> &sidx, int dim,
                sortmode mode) const
{
  if (dim < 0 || dim >= ndims ())
    {
      (*current_liboctave_error_handler)
        ("sort: invalid dimension");
      return Array<T> ();
    }

  Array<T> m (dims ());

  dim_vector dv = m.dims ();

  if (m.length () < 1)
    {
      sidx = Array<octave_idx_type> (dv);
      return m;
    }

  octave_idx_type ns = dv(dim);
  octave_idx_type iter = dv.numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  octave_sort<T> lsort;

  sidx = Array<octave_idx_type> (dv);
  octave_idx_type *vi = sidx.fortran_vec ();

  if (mode != UNSORTED)
    lsort.set_compare (mode);
  else
    return m;

  if (stride == 1)
    {
      for (octave_idx_type j = 0; j < iter; j++)
        {
          // copy and partition out NaNs.
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i];
              if (sort_isnan<T> (tmp))
                {
                  --ku;
                  v[ku] = tmp;
                  vi[ku] = i;
                }
              else
                {
                  v[kl] = tmp;
                  vi[kl] = i;
                  kl++;
                }
            }

          // sort.
          lsort.sort (v, vi, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (v + ku, v + ns);
              std::reverse (vi + ku, vi + ns);
              if (mode == DESCENDING)
                {
                  std::rotate (v, v + ku, v + ns);
                  std::rotate (vi, vi + ku, vi + ns);
                }
            }

          v += ns;
          vi += ns;
          ov += ns;
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (T, buf, ns);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, bufi, ns);

      for (octave_idx_type j = 0; j < iter; j++)
        {
          octave_idx_type offset = j;
          octave_idx_type offset2 = 0;

          while (offset >= stride)
            {
              offset -= stride;
              offset2++;
            }

          offset += offset2 * stride * ns;

          // gather and partition out NaNs.
          // FIXME: impact on integer types noticeable?
          octave_idx_type kl = 0;
          octave_idx_type ku = ns;
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i*stride + offset];
              if (sort_isnan<T> (tmp))
                {
                  --ku;
                  buf[ku] = tmp;
                  bufi[ku] = i;
                }
              else
                {
                  buf[kl] = tmp;
                  bufi[kl] = i;
                  kl++;
                }
            }

          // sort.
          lsort.sort (buf, bufi, kl);

          if (ku < ns)
            {
              // NaNs are in reverse order
              std::reverse (buf + ku, buf + ns);
              std::reverse (bufi + ku, bufi + ns);
              if (mode == DESCENDING)
                {
                  std::rotate (buf, buf + ku, buf + ns);
                  std::rotate (bufi, bufi + ku, bufi + ns);
                }
            }

          // scatter.
          for (octave_idx_type i = 0; i < ns; i++)
            v[i*stride + offset] = buf[i];
          for (octave_idx_type i = 0; i < ns; i++)
            vi[i*stride + offset] = bufi[i];
        }
    }

  return m;
}

template <class T>
typename Array<T>::compare_fcn_type
safe_comparator (sortmode mode, const Array<T>& /* a */,
                 bool /* allow_chk */)
{
  if (mode == ASCENDING)
    return octave_sort<T>::ascending_compare;
  else if (mode == DESCENDING)
    return octave_sort<T>::descending_compare;
  else
    return 0;
}

template <class T>
sortmode
Array<T>::is_sorted (sortmode mode) const
{
  octave_sort<T> lsort;

  octave_idx_type n = numel ();

  if (n <= 1)
    return mode ? mode : ASCENDING;

  if (mode == UNSORTED)
    {
      // Auto-detect mode.
      compare_fcn_type compare
        = safe_comparator (ASCENDING, *this, false);

      if (compare (elem (n-1), elem (0)))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  if (mode != UNSORTED)
    {
      lsort.set_compare (safe_comparator (mode, *this, false));

      if (! lsort.is_sorted (data (), n))
        mode = UNSORTED;
    }

  return mode;

}

template <class T>
Array<octave_idx_type>
Array<T>::sort_rows_idx (sortmode mode) const
{
  Array<octave_idx_type> idx;

  octave_sort<T> lsort (safe_comparator (mode, *this, true));

  octave_idx_type r = rows ();
  octave_idx_type c = cols ();

  idx = Array<octave_idx_type> (dim_vector (r, 1));

  lsort.sort_rows (data (), idx.fortran_vec (), r, c);

  return idx;
}


template <class T>
sortmode
Array<T>::is_sorted_rows (sortmode mode) const
{
  octave_sort<T> lsort;

  octave_idx_type r = rows ();
  octave_idx_type c = cols ();

  if (r <= 1 || c == 0)
    return mode ? mode : ASCENDING;

  if (mode == UNSORTED)
    {
      // Auto-detect mode.
      compare_fcn_type compare
        = safe_comparator (ASCENDING, *this, false);

      octave_idx_type i;
      for (i = 0; i < cols (); i++)
        {
          T l = elem (0, i);
          T u = elem (rows () - 1, i);
          if (compare (l, u))
            {
              if (mode == DESCENDING)
                {
                  mode = UNSORTED;
                  break;
                }
              else
                mode = ASCENDING;
            }
          else if (compare (u, l))
            {
              if (mode == ASCENDING)
                {
                  mode = UNSORTED;
                  break;
                }
              else
                mode = DESCENDING;
            }
        }
      if (mode == UNSORTED && i == cols ())
        mode = ASCENDING;
    }

  if (mode != UNSORTED)
    {
      lsort.set_compare (safe_comparator (mode, *this, false));

      if (! lsort.is_sorted_rows (data (), r, c))
        mode = UNSORTED;
    }

  return mode;

}

// Do a binary lookup in a sorted array.
template <class T>
octave_idx_type
Array<T>::lookup (const T& value, sortmode mode) const
{
  octave_idx_type n = numel ();
  octave_sort<T> lsort;

  if (mode == UNSORTED)
    {
      // auto-detect mode
      if (n > 1 && lsort.descending_compare (elem (0), elem (n-1)))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  lsort.set_compare (mode);

  return lsort.lookup (data (), n, value);
}

template <class T>
Array<octave_idx_type>
Array<T>::lookup (const Array<T>& values, sortmode mode) const
{
  octave_idx_type n = numel ();
  octave_idx_type nval = values.numel ();
  octave_sort<T> lsort;
  Array<octave_idx_type> idx (values.dims ());

  if (mode == UNSORTED)
    {
      // auto-detect mode
      if (n > 1 && lsort.descending_compare (elem (0), elem (n-1)))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  lsort.set_compare (mode);

  // This determines the split ratio between the O(M*log2(N)) and O(M+N)
  // algorithms.
  static const double ratio = 1.0;
  sortmode vmode = UNSORTED;

  // Attempt the O(M+N) algorithm if M is large enough.
  if (nval > ratio * n / xlog2 (n + 1.0))
    {
      vmode = values.is_sorted ();
      // The table must not contain a NaN.
      if ((vmode == ASCENDING && sort_isnan<T> (values(nval-1)))
          || (vmode == DESCENDING && sort_isnan<T> (values(0))))
        vmode = UNSORTED;
    }

  if (vmode != UNSORTED)
    lsort.lookup_sorted (data (), n, values.data (), nval,
                         idx.fortran_vec (), vmode != mode);
  else
    lsort.lookup (data (), n, values.data (), nval, idx.fortran_vec ());

  return idx;
}

template <class T>
octave_idx_type
Array<T>::nnz (void) const
{
  const T *src = data ();
  octave_idx_type nel = nelem ();
  octave_idx_type retval = 0;
  const T zero = T ();
  for (octave_idx_type i = 0; i < nel; i++)
    if (src[i] != zero)
      retval++;

  return retval;
}

template <class T>
Array<octave_idx_type>
Array<T>::find (octave_idx_type n, bool backward) const
{
  Array<octave_idx_type> retval;
  const T *src = data ();
  octave_idx_type nel = nelem ();
  const T zero = T ();
  if (n < 0 || n >= nel)
    {
      // We want all elements, which means we'll almost surely need
      // to resize. So count first, then allocate array of exact size.
      octave_idx_type cnt = 0;
      for (octave_idx_type i = 0; i < nel; i++)
        cnt += src[i] != zero;

      retval.clear (cnt, 1);
      octave_idx_type *dest = retval.fortran_vec ();
      for (octave_idx_type i = 0; i < nel; i++)
        if (src[i] != zero) *dest++ = i;
    }
  else
    {
      // We want a fixed max number of elements, usually small. So be
      // optimistic, alloc the array in advance, and then resize if
      // needed.
      retval.clear (n, 1);
      if (backward)
        {
          // Do the search as a series of successive single-element searches.
          octave_idx_type k = 0;
          octave_idx_type l = nel - 1;
          for (; k < n; k++)
            {
              for (; l >= 0 && src[l] == zero; l--) ;
              if (l >= 0)
                retval(k) = l--;
              else
                break;
            }
          if (k < n)
            retval.resize2 (k, 1);
          octave_idx_type *rdata = retval.fortran_vec ();
          std::reverse (rdata, rdata + k);
        }
      else
        {
          // Do the search as a series of successive single-element searches.
          octave_idx_type k = 0;
          octave_idx_type l = 0;
          for (; k < n; k++)
            {
              for (; l != nel && src[l] == zero; l++) ;
              if (l != nel)
                retval(k) = l++;
              else
                break;
            }
          if (k < n)
            retval.resize2 (k, 1);
        }
    }

  // Fixup return dimensions, for Matlab compatibility.
  // find (zeros (0,0)) -> zeros (0,0)
  // find (zeros (1,0)) -> zeros (1,0)
  // find (zeros (0,1)) -> zeros (0,1)
  // find (zeros (0,X)) -> zeros (0,1)
  // find (zeros (1,1)) -> zeros (0,0) !!!! WHY?
  // find (zeros (0,1,0)) -> zeros (0,0)
  // find (zeros (0,1,0,1)) -> zeros (0,0) etc

  if ((numel () == 1 && retval.is_empty ())
      || (rows () == 0 && dims ().numel (1) == 0))
    retval.dimensions = dim_vector ();
  else if (rows () == 1 && ndims () == 2)
    retval.dimensions = dim_vector (1, retval.length ());

  return retval;
}

template <class T>
Array<T>
Array<T>::nth_element (const idx_vector& n, int dim) const
{
  if (dim < 0)
    {
      (*current_liboctave_error_handler)
        ("nth_element: invalid dimension");
      return Array<T> ();
    }

  dim_vector dv = dims ();
  if (dim >= dv.length ())
    dv.resize (dim+1, 1);

  octave_idx_type ns = dv(dim);

  octave_idx_type nn = n.length (ns);

  dv(dim) = std::min (nn, ns);
  dv.chop_trailing_singletons ();
  dim = std::min (dv.length (), dim);

  Array<T> m (dv);

  if (m.numel () == 0)
    return m;

  sortmode mode = UNSORTED;
  octave_idx_type lo = 0;

  switch (n.idx_class ())
    {
    case idx_vector::class_scalar:
      mode = ASCENDING;
      lo = n(0);
      break;
    case idx_vector::class_range:
      {
        octave_idx_type inc = n.increment ();
        if (inc == 1)
          {
            mode = ASCENDING;
            lo = n(0);
          }
        else if (inc == -1)
          {
            mode = DESCENDING;
            lo = ns - 1 - n(0);
          }
      }
    default:
      break;
    }

  if (mode == UNSORTED)
    {
      (*current_liboctave_error_handler)
        ("nth_element: n must be a scalar or a contiguous range");
      return Array<T> ();
    }

  octave_idx_type up = lo + nn;

  if (lo < 0 || up > ns)
    {
      (*current_liboctave_error_handler)
        ("nth_element: invalid element index");
      return Array<T> ();
    }

  octave_idx_type iter = numel () / ns;
  octave_idx_type stride = 1;

  for (int i = 0; i < dim; i++)
    stride *= dv(i);

  T *v = m.fortran_vec ();
  const T *ov = data ();

  OCTAVE_LOCAL_BUFFER (T, buf, ns);

  octave_sort<T> lsort;
  lsort.set_compare (mode);

  for (octave_idx_type j = 0; j < iter; j++)
    {
      octave_idx_type kl = 0;
      octave_idx_type ku = ns;

      if (stride == 1)
        {
          // copy without NaNs.
          // FIXME: impact on integer types noticeable?
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[i];
              if (sort_isnan<T> (tmp))
                buf[--ku] = tmp;
              else
                buf[kl++] = tmp;
            }

          ov += ns;
        }
      else
        {
          octave_idx_type offset = j % stride;
          // copy without NaNs.
          // FIXME: impact on integer types noticeable?
          for (octave_idx_type i = 0; i < ns; i++)
            {
              T tmp = ov[offset + i*stride];
              if (sort_isnan<T> (tmp))
                buf[--ku] = tmp;
              else
                buf[kl++] = tmp;
            }

          if (offset == stride-1)
            ov += ns*stride;
        }

      if (ku == ns)
        lsort.nth_element (buf, ns, lo, up);
      else if (mode == ASCENDING)
        lsort.nth_element (buf, ku, lo, std::min (ku, up));
      else
        {
          octave_idx_type nnan = ns - ku;
          octave_idx_type zero = 0;
          lsort.nth_element (buf, ku, std::max (lo - nnan, zero),
                             std::max (up - nnan, zero));
          std::rotate (buf, buf + ku, buf + ns);
        }

      if (stride == 1)
        {
          for (octave_idx_type i = 0; i < nn; i++)
            v[i] = buf[lo + i];

          v += nn;
        }
      else
        {
          octave_idx_type offset = j % stride;
          for (octave_idx_type i = 0; i < nn; i++)
            v[offset + stride * i] = buf[lo + i];
          if (offset == stride-1)
            v += nn*stride;
        }
    }

  return m;
}

#define NO_INSTANTIATE_ARRAY_SORT(T) \
 \
template <> Array<T>  \
Array<T>::sort (int, sortmode) const { return *this; } \
 \
template <> Array<T>  \
Array<T>::sort (Array<octave_idx_type> &sidx, int, sortmode) const \
{ sidx = Array<octave_idx_type> (); return *this; } \
 \
template <> sortmode  \
Array<T>::is_sorted (sortmode) const  \
{ return UNSORTED; } \
 \
Array<T>::compare_fcn_type \
safe_comparator (sortmode, const Array<T>&, bool) \
{ return 0; } \
 \
template <> Array<octave_idx_type>  \
Array<T>::sort_rows_idx (sortmode) const  \
{ return Array<octave_idx_type> (); } \
 \
template <> sortmode  \
Array<T>::is_sorted_rows (sortmode) const \
{ return UNSORTED; } \
 \
template <> octave_idx_type  \
Array<T>::lookup (T const &, sortmode) const \
{ return 0; } \
template <> Array<octave_idx_type>  \
Array<T>::lookup (const Array<T>&, sortmode) const \
{ return Array<octave_idx_type> (); } \
 \
template <> octave_idx_type \
Array<T>::nnz (void) const\
{ return 0; } \
template <> Array<octave_idx_type> \
Array<T>::find (octave_idx_type, bool) const\
{ return Array<octave_idx_type> (); } \
 \
template <> Array<T>  \
Array<T>::nth_element (const idx_vector&, int) const { return Array<T> (); }


template <class T>
Array<T>
Array<T>::diag (octave_idx_type k) const
{
  dim_vector dv = dims ();
  octave_idx_type nd = dv.length ();
  Array<T> d;

  if (nd > 2)
    (*current_liboctave_error_handler) ("Matrix must be 2-dimensional");
  else
    {
      octave_idx_type nnr = dv (0);
      octave_idx_type nnc = dv (1);

      if (nnr == 0 && nnc == 0)
        ; // do nothing for empty matrix
      else if (nnr != 1 && nnc != 1)
        {
          // Extract diag from matrix
          if (k > 0)
            nnc -= k;
          else if (k < 0)
            nnr += k;

          if (nnr > 0 && nnc > 0)
            {
              octave_idx_type ndiag = (nnr < nnc) ? nnr : nnc;

              d.resize (dim_vector (ndiag, 1));

              if (k > 0)
                {
                  for (octave_idx_type i = 0; i < ndiag; i++)
                    d.xelem (i) = elem (i, i+k);
                }
              else if (k < 0)
                {
                  for (octave_idx_type i = 0; i < ndiag; i++)
                    d.xelem (i) = elem (i-k, i);
                }
              else
                {
                  for (octave_idx_type i = 0; i < ndiag; i++)
                    d.xelem (i) = elem (i, i);
                }
            }
          else  // Matlab returns [] 0x1 for out-of-range diagonal
            d.resize (dim_vector (0, 1));
        }
      else
        {
          // Create diag matrix from vector
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
              d = Array<T> (dim_vector (n, n), resize_fill_value ());

              for (octave_idx_type i = 0; i < nnc; i++)
                d.xelem (i+roff, i+coff) = elem (0, i);
            }
          else
            {
              octave_idx_type n = nnr + std::abs (k);
              d = Array<T> (dim_vector (n, n), resize_fill_value ());

              for (octave_idx_type i = 0; i < nnr; i++)
                d.xelem (i+roff, i+coff) = elem (i, 0);
            }
        }
    }

  return d;
}

template <class T>
Array<T>
Array<T>::diag (octave_idx_type m, octave_idx_type n) const
{
  Array<T> retval;

  if (ndims () == 2 && (rows () == 1 || cols () == 1))
    {
      retval.resize (dim_vector (m, n), resize_fill_value ());

      for (octave_idx_type i = 0; i < numel (); i++)
        retval.xelem (i, i) = xelem (i);
    }
  else
    (*current_liboctave_error_handler)
      ("cat: invalid dimension");

  return retval;
}

template <class T>
Array<T>
Array<T>::cat (int dim, octave_idx_type n, const Array<T> *array_list)
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

  if (n == 1)
    return array_list[0];
  else if (n == 0)
    return Array<T> ();

  // Special case:
  //
  //   cat (dim, [], ..., [], A, ...)
  //
  // with dim > 2, A not 0x0, and at least three arguments to
  // concatenate is equivalent to
  //
  //   cat (dim, A, ...)
  //
  // Note that this check must be performed here because for full-on
  // braindead Matlab compatibility, we need to have things like
  //
  //   cat (3, [], [], A)
  //
  // succeed, but to have things like
  //
  //   cat (3, cat (3, [], []), A)
  //   cat (3, zeros (0, 0, 2), A)
  //
  // fail.  See also bug report #31615.

  octave_idx_type istart = 0;

  if (n > 2 && dim > 1)
    {
      for (octave_idx_type i = 0; i < n; i++)
        {
          dim_vector dv = array_list[i].dims ();

          if (dv.zero_by_zero ())
            istart++;
          else
            break;
        }

      // Don't skip any initial aguments if they are all empty.
      if (istart >= n)
        istart = 0;
    }

  dim_vector dv = array_list[istart++].dims ();

  for (octave_idx_type i = istart; i < n; i++)
    if (! (dv.*concat_rule) (array_list[i].dims (), dim))
      (*current_liboctave_error_handler)
        ("cat: dimension mismatch");

  Array<T> retval (dv);

  if (retval.is_empty ())
    return retval;

  int nidx = std::max (dv.length (), dim + 1);
  Array<idx_vector> idxa (dim_vector (nidx, 1), idx_vector::colon);
  octave_idx_type l = 0;

  for (octave_idx_type i = 0; i < n; i++)
    {
      // NOTE: This takes some thinking, but no matter what the above rules
      // are, an empty array can always be skipped at this point, because
      // the result dimensions are already determined, and there is no way
      // an empty array may contribute a nonzero piece along the dimension
      // at this point, unless an empty array can be promoted to a non-empty
      // one (which makes no sense). I repeat, *no way*, think about it.
      if (array_list[i].is_empty ())
        continue;

      octave_quit ();

      octave_idx_type u;
      if (dim < array_list[i].ndims ())
        u = l + array_list[i].dims ()(dim);
      else
        u = l + 1;

      idxa(dim) = idx_vector (l, u);

      retval.assign (idxa, array_list[i]);

      l = u;
    }

  return retval;
}

template <class T>
void
Array<T>::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "rep address: " << rep << '\n'
     << prefix << "rep->len:    " << rep->len << '\n'
     << prefix << "rep->data:   " << static_cast<void *> (rep->data) << '\n'
     << prefix << "rep->count:  " << rep->count << '\n'
     << prefix << "slice_data:  " << static_cast<void *> (slice_data) << '\n'
     << prefix << "slice_len:   " << slice_len << '\n';

  // 2D info:
  //
  //     << pefix << "rows: " << rows () << "\n"
  //     << prefix << "cols: " << cols () << "\n";
}

template <class T>
bool Array<T>::optimize_dimensions (const dim_vector& dv)
{
  bool retval = dimensions == dv;
  if (retval)
    dimensions = dv;

  return retval;
}

template <class T>
void Array<T>::instantiation_guard ()
{
  // This guards against accidental implicit instantiations.
  // Array<T> instances should always be explicit and use INSTANTIATE_ARRAY.
  T::__xXxXx__ ();
}

#define INSTANTIATE_ARRAY(T, API) \
  template <> void Array<T>::instantiation_guard () { } \
  template class API Array<T>

// FIXME: is this used?

template <class T>
std::ostream&
operator << (std::ostream& os, const Array<T>& a)
{
  dim_vector a_dims = a.dims ();

  int n_dims = a_dims.length ();

  os << n_dims << "-dimensional array";

  if (n_dims)
    os << " (" << a_dims.str () << ")";

  os <<"\n\n";

  if (n_dims)
    {
      os << "data:";

      Array<octave_idx_type> ra_idx (dim_vector (n_dims, 1), 0);

      // Number of times the first 2d-array is to be displayed.

      octave_idx_type m = 1;
      for (int i = 2; i < n_dims; i++)
        m *= a_dims(i);

      if (m == 1)
        {
          octave_idx_type rows = 0;
          octave_idx_type cols = 0;

          switch (n_dims)
            {
            case 2:
              rows = a_dims(0);
              cols = a_dims(1);

              for (octave_idx_type j = 0; j < rows; j++)
                {
                  ra_idx(0) = j;
                  for (octave_idx_type k = 0; k < cols; k++)
                    {
                      ra_idx(1) = k;
                      os << " " << a.elem (ra_idx);
                    }
                  os << "\n";
                }
              break;

            default:
              rows = a_dims(0);

              for (octave_idx_type k = 0; k < rows; k++)
                {
                  ra_idx(0) = k;
                  os << " " << a.elem (ra_idx);
                }
              break;
            }

          os << "\n";
        }
      else
        {
          octave_idx_type rows = a_dims(0);
          octave_idx_type cols = a_dims(1);

          for (int i = 0; i < m; i++)
            {
              os << "\n(:,:,";

              for (int j = 2; j < n_dims - 1; j++)
                os << ra_idx(j) + 1 << ",";

              os << ra_idx(n_dims - 1) + 1 << ") = \n";

              for (octave_idx_type j = 0; j < rows; j++)
                {
                  ra_idx(0) = j;

                  for (octave_idx_type k = 0; k < cols; k++)
                    {
                      ra_idx(1) = k;
                      os << " " << a.elem (ra_idx);
                    }

                  os << "\n";
                }

              os << "\n";

              if (i != m - 1)
                increment_index (ra_idx, a_dims, 2);
            }
        }
    }

  return os;
}
