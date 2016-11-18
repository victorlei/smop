/*

Copyright (C) 2003-2015 John W. Eaton
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

#include "Array-util.h"
#include "dim-vector.h"
#include "lo-error.h"
#include "oct-locbuf.h"

bool
index_in_bounds (const Array<octave_idx_type>& ra_idx,
                 const dim_vector& dimensions)
{
  bool retval = true;

  int n = ra_idx.length ();

  if (n == dimensions.length ())
    {
      for (int i = 0; i < n; i++)
        {
          if (ra_idx(i) < 0 || ra_idx(i) >= dimensions(i))
            {
              retval = false;
              break;
            }
        }
    }
  else
    retval = false;

  return retval;
}

void
increment_index (Array<octave_idx_type>& ra_idx, const dim_vector& dimensions,
                 int start_dimension)
{
  ra_idx(start_dimension)++;

  int n = ra_idx.length () - 1;
  int nda = dimensions.length ();

  for (int i = start_dimension; i < n; i++)
    {
      if (ra_idx(i) < (i < nda ? dimensions(i) : 1))
        break;
      else
        {
          ra_idx(i) = 0;
          ra_idx(i+1)++;
        }
    }
}

octave_idx_type
get_scalar_idx (Array<octave_idx_type>& idx, dim_vector& dims)
{
  octave_idx_type retval (-1);

  int n = idx.length ();

  if (n > 0)
    {
      retval = idx(--n);

      while (--n >= 0)
        {
          retval *= dims (n);

          retval += idx(n);
        }
    }
  return retval;
}

octave_idx_type
num_ones (const Array<octave_idx_type>& ra_idx)
{
  octave_idx_type retval = 0;

  for (octave_idx_type i = 0; i < ra_idx.length (); i++)
    {
      if (ra_idx (i) == 1)
        retval++;
    }

  return retval;
}

bool
is_scalar (const dim_vector& dim)
{
  bool retval = true;

  int n = dim.length ();

  if (n == 0)
    {
      retval = false;
    }
  else
    {
      for (int i = 0; i < n; i ++)
        {
          if (dim (i) != 1)
            {
              retval = false;

              break;
            }
        }
    }
  return retval;
}

bool
is_vector (const dim_vector& dim)
{
  int m = 0;
  int n = dim.length ();

  if (n == 0)
    m = 2;
  else
    {
      for (int i = 0; i < n; i ++)
        if (dim (i) > 1)
          m++;
        else if (dim(i) < 1)
          m += 2;
    }

  return (m < 2);
}

bool
any_ones (const Array<octave_idx_type>& arr)
{
  bool retval = false;

  for (octave_idx_type i = 0; i < arr.length (); i++)
    {
      if (arr (i) == 1)
        {
          retval = true;

          break;
        }
    }
  return retval;
}

octave_idx_type
compute_index (octave_idx_type n, const dim_vector& dims)
{
  if (n < 0)
    gripe_invalid_index ();
  if (n >= dims.numel ())
    gripe_index_out_of_range (1, 1, n+1, dims.numel ());

  return n;
}

octave_idx_type
compute_index (octave_idx_type i, octave_idx_type j, const dim_vector& dims)
{
  if (i < 0 || j < 0)
    gripe_invalid_index ();
  if (i >= dims(0))
    gripe_index_out_of_range (2, 1, i+1, dims(0));
  if (j >= dims.numel (1))
    gripe_index_out_of_range (2, 2, j+1, dims.numel (1));

  return j*dims(0) + i;
}

octave_idx_type
compute_index (octave_idx_type i, octave_idx_type j, octave_idx_type k,
               const dim_vector& dims)
{
  if (i < 0 || j < 0 || k < 0)
    gripe_invalid_index ();
  if (i >= dims(0))
    gripe_index_out_of_range (3, 1, i+1, dims(0));
  if (j >= dims(1))
    gripe_index_out_of_range (3, 2, j+1, dims(1));
  if (k >= dims.numel (2))
    gripe_index_out_of_range (3, 3, k+1, dims.numel (2));

  return (k*dims(1) + j)*dims(0) + i;
}

octave_idx_type
compute_index (const Array<octave_idx_type>& ra_idx, const dim_vector& dims)
{
  int nd = ra_idx.length ();
  const dim_vector dv = dims.redim (nd);
  for (int d = 0; d < nd; d++)
    {
      if (ra_idx(d) < 0)
        gripe_invalid_index ();
      if (ra_idx(d) >= dv(d))
        gripe_index_out_of_range (nd, d+1, ra_idx(d)+1, dv(d));
    }

  return dv.compute_index (ra_idx.data ());
}

Array<octave_idx_type>
conv_to_int_array (const Array<idx_vector>& a)
{
  Array<octave_idx_type> retval (a.dims ());

  for (octave_idx_type i = 0; i < a.length (); i++)
    retval(i) = a(i).elem (0);

  return retval;
}

Array<idx_vector>
conv_to_array (const idx_vector *tmp, const octave_idx_type len)
{
  Array<idx_vector> retval (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    retval(i) = tmp[i];

  return retval;
}

dim_vector
freeze (Array<idx_vector>& ra_idx, const dim_vector& dimensions, int resize_ok)
{
  dim_vector retval;

  int n = ra_idx.length ();

  assert (n == dimensions.length ());

  retval.resize (n);

  static const char *tag[3] = { "row", "column", 0 };

  for (int i = 0; i < n; i++)
    retval(i) = ra_idx(i).freeze (dimensions(i), tag[i < 2 ? i : 3],
                                  resize_ok);

  return retval;
}

bool
vector_equivalent (const dim_vector& dv)
{
  int n = dv.length ();

  bool found_first = false;

  for (int i = 0; i < n; i++)
    {
      if (dv(i) != 1)
        {
          if (! found_first)
            found_first = true;
          else
            return false;
        }
    }

  return true;
}

bool
all_ok (const Array<idx_vector>& ra_idx)
{
  bool retval = true;

  octave_idx_type n = ra_idx.length ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (! ra_idx(i))
        {
          retval = false;
          break;
        }
    }

  return retval;
}

bool
any_orig_empty (const Array<idx_vector>& ra_idx)
{
  bool retval = false;

  octave_idx_type n = ra_idx.length ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (ra_idx(i).orig_empty ())
        {
          retval = true;
          break;
        }
    }

  return retval;
}

bool
all_colon_equiv (const Array<idx_vector>& ra_idx,
                 const dim_vector& frozen_lengths)
{
  bool retval = true;

  octave_idx_type idx_n = ra_idx.length ();

  int n = frozen_lengths.length ();

  assert (idx_n == n);

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (! ra_idx(i).is_colon_equiv (frozen_lengths(i)))
        {
          retval = false;
          break;
        }
    }

  return retval;
}

bool
all_ones (const Array<octave_idx_type>& arr)
{
  bool retval = true;

  for (octave_idx_type i = 0; i < arr.length (); i++)
    {
      if (arr(i) != 1)
        {
          retval = false;
          break;
        }
    }

  return retval;
}

Array<octave_idx_type>
get_elt_idx (const Array<idx_vector>& ra_idx,
             const Array<octave_idx_type>& result_idx)
{
  octave_idx_type n = ra_idx.length ();

  Array<octave_idx_type> retval (dim_vector (n, 1));

  for (octave_idx_type i = 0; i < n; i++)
    retval(i) = ra_idx(i).elem (result_idx(i));

  return retval;
}

Array<octave_idx_type>
get_ra_idx (octave_idx_type idx, const dim_vector& dims)
{
  Array<octave_idx_type> retval;

  int n_dims = dims.length ();

  retval.resize (dim_vector (n_dims, 1));

  for (int i = 0; i < n_dims; i++)
    retval(i) = 0;

  assert (idx > 0 || idx < dims.numel ());

  for (octave_idx_type i = 0; i < idx; i++)
    increment_index (retval, dims);

  // FIXME: the solution using increment_index is not efficient.

#if 0
  octave_idx_type var = 1;
  for (int i = 0; i < n_dims; i++)
    {
      std::cout << "idx: " << idx << ", var: " << var
                << ", dims(" << i << "): " << dims(i) <<"\n";
      retval(i) = ((int)floor(((idx) / (double)var))) % dims(i);
      idx -= var * retval(i);
      var = dims(i);
    }
#endif

  return retval;
}

dim_vector
zero_dims_inquire (const Array<idx_vector>& ia, const dim_vector& rhdv)
{
  int ial = ia.length ();
  int rhdvl = rhdv.length ();
  dim_vector rdv = dim_vector::alloc (ial);
  bool *scalar = new bool [ial];
  bool *colon = new bool [ial];
  // Mark scalars and colons, count non-scalar indices.
  int nonsc = 0;
  bool all_colons = true;
  for (int i = 0; i < ial; i++)
    {
      // FIXME: should we check for length() instead?
      scalar[i] = ia(i).is_scalar ();
      colon[i] = ia(i).is_colon ();
      if (! scalar[i]) nonsc++;
      if (! colon[i]) rdv(i) = ia(i).extent (0);
      all_colons = all_colons && colon[i];
    }

  // If the number of nonscalar indices matches the dimensionality of
  // RHS, we try an exact match, inquiring even singleton dimensions.
  if (all_colons)
    {
      rdv = rhdv;
      rdv.resize (ial, 1);
    }
  else if (nonsc == rhdvl)
    {
      for (int i = 0, j = 0; i < ial; i++)
        {
          if (scalar[i]) continue;
          if (colon[i])
            rdv(i) = rhdv(j);
          j++;
        }
    }
  else
    {
      dim_vector rhdv0 = rhdv;
      rhdv0.chop_all_singletons ();
      int rhdv0l = rhdv0.length ();
      for (int i = 0, j = 0; i < ial; i++)
        {
          if (scalar[i]) continue;
          if (colon[i])
            rdv(i) = (j < rhdv0l) ? rhdv0(j++) : 1;
        }
    }

  delete [] scalar;
  delete [] colon;

  return rdv;
}

dim_vector
zero_dims_inquire (const idx_vector& i, const idx_vector& j,
                   const dim_vector& rhdv)
{
  bool icol = i.is_colon ();
  bool jcol = j.is_colon ();
  dim_vector rdv;
  if (icol && jcol && rhdv.length () == 2)
    {
      rdv(0) = rhdv(0);
      rdv(1) = rhdv(1);
    }
  else if (rhdv.length () == 2
           && ! i.is_scalar () && ! j.is_scalar ())
    {
      rdv(0) = icol ? rhdv(0) : i.extent (0);
      rdv(1) = jcol ? rhdv(1) : j.extent (0);
    }
  else
    {
      dim_vector rhdv0 = rhdv;
      rhdv0.chop_all_singletons ();
      int k = 0;
      rdv(0) = i.extent (0);
      if (icol)
        rdv(0) = rhdv0(k++);
      else if (! i.is_scalar ())
        k++;
      rdv(1) = j.extent (0);
      if (jcol)
        rdv(1) = rhdv0(k++);
      else if (! j.is_scalar ())
        k++;
    }

  return rdv;
}

// A helper class.
struct sub2ind_helper
{
  octave_idx_type *ind, n;

  sub2ind_helper (octave_idx_type *_ind, octave_idx_type _n)
    : ind(_ind), n(_n) { }

  void operator ()(octave_idx_type k) { (*ind++ *= n) += k; }
};

idx_vector
sub2ind (const dim_vector& dv, const Array<idx_vector>& idxa)
{
  idx_vector retval;
  octave_idx_type len = idxa.length ();

  if (len >= 1)
    {
      const dim_vector dvx = dv.redim (len);
      bool all_ranges = true;
      octave_idx_type clen = -1;

      for (octave_idx_type i = 0; i < len; i++)
        {
          idx_vector idx = idxa(i);
          octave_idx_type n = dvx(i);

          all_ranges = all_ranges && idx.is_range ();
          if (clen < 0)
            clen = idx.length (n);
          else if (clen != idx.length (n))
            current_liboctave_error_handler ("sub2ind: lengths of indices must match");

          if (idx.extent (n) > n)
            current_liboctave_error_handler ("sub2ind: index out of range");
        }

      if (len == 1)
        retval = idxa(0);
      else if (clen == 1)
        {
          // All scalars case - the result is a scalar.
          octave_idx_type idx = idxa(len-1)(0);
          for (octave_idx_type i = len - 2; i >= 0; i--)
            idx = idx * dvx(i) + idxa(i)(0);
          retval = idx_vector (idx);
        }
      else if (all_ranges && clen != 0)
        {
          // All ranges case - the result is a range.
          octave_idx_type start = 0;
          octave_idx_type step = 0;
          for (octave_idx_type i = len - 1; i >= 0; i--)
            {
              octave_idx_type xstart = idxa(i)(0);
              octave_idx_type xstep = idxa(i)(1) - xstart;
              start = start * dvx(i) + xstart;
              step = step * dvx(i) + xstep;
            }
          retval = idx_vector::make_range (start, step, clen);
        }
      else
        {
          Array<octave_idx_type> idx (idxa(0).orig_dimensions ());
          octave_idx_type *idx_vec = idx.fortran_vec ();

          for (octave_idx_type i = len - 1; i >= 0; i--)
            {
              if (i < len - 1)
                idxa(i).loop (clen, sub2ind_helper (idx_vec, dvx(i)));
              else
                idxa(i).copy_data (idx_vec);
            }

          retval = idx_vector (idx);
        }
    }
  else
    current_liboctave_error_handler ("sub2ind: needs at least 2 indices");

  return retval;
}

Array<idx_vector>
ind2sub (const dim_vector& dv, const idx_vector& idx)
{
  octave_idx_type len = idx.length (0);
  octave_idx_type n = dv.length ();
  Array<idx_vector> retval (dim_vector (n, 1));
  octave_idx_type numel = dv.numel ();

  if (idx.extent (numel) > numel)
    current_liboctave_error_handler ("ind2sub: index out of range");
  else
    {
      if (idx.is_scalar ())
        {
          octave_idx_type k = idx(0);
          for (octave_idx_type j = 0; j < n; j++)
            {
              retval(j) = k % dv(j);
              k /= dv(j);
            }
        }
      else
        {
          OCTAVE_LOCAL_BUFFER (Array<octave_idx_type>, rdata, n);

          dim_vector odv = idx.orig_dimensions ();
          for (octave_idx_type j = 0; j < n; j++)
            rdata[j] = Array<octave_idx_type> (odv);

          for (octave_idx_type i = 0; i < len; i++)
            {
              octave_idx_type k = idx(i);
              for (octave_idx_type j = 0; j < n; j++)
                {
                  rdata[j](i) = k % dv(j);
                  k /= dv(j);
                }
            }

          for (octave_idx_type j = 0; j < n; j++)
            retval(j) = rdata[j];
        }


    }

  return retval;
}

int
permute_vector_compare (const void *a, const void *b)
{
  const permute_vector *pva = static_cast<const permute_vector *> (a);
  const permute_vector *pvb = static_cast<const permute_vector *> (b);

  return pva->pidx > pvb->pidx;
}
