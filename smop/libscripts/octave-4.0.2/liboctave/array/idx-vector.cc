/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009-2010 VZLU Prague

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

#include <cstdlib>

#include <iostream>

#include "idx-vector.h"
#include "Array.h"
#include "Array-util.h"
#include "Sparse.h"
#include "Range.h"

#include "oct-locbuf.h"
#include "lo-error.h"
#include "lo-mappers.h"

static void
gripe_invalid_range (void)
{
  (*current_liboctave_error_handler)
    ("invalid range used as index");
}

static void
gripe_index_out_of_range (void)
{
  (*current_liboctave_error_handler)
    ("internal error: idx_vector index out of range");
}

idx_vector::idx_vector_rep *
idx_vector::nil_rep (void)
{
  static idx_vector_rep ivr;
  return &ivr;
}

idx_vector::idx_vector_rep *
idx_vector::err_rep (void)
{
  static idx_vector_rep ivr;
  ivr.err = true;
  return &ivr;
}

Array<octave_idx_type>
idx_vector::idx_base_rep::as_array (void)
{
  (*current_liboctave_error_handler)
    ("internal error: as_array not allowed for this index class");

  return Array<octave_idx_type> ();
}


idx_vector::idx_colon_rep::idx_colon_rep (char c)
{
  if (c != ':')
    {
      (*current_liboctave_error_handler)
        ("internal error: invalid character converted to idx_vector; must be ':'");
      err = true;
    }
}

octave_idx_type
idx_vector::idx_colon_rep::checkelem (octave_idx_type i) const
{
  if (i < 0)
    {
      gripe_index_out_of_range ();
      return 0;
    }
  else
    return i;
}

idx_vector::idx_base_rep *
idx_vector::idx_colon_rep::sort_idx (Array<octave_idx_type>&)
{
  (*current_liboctave_error_handler)
    ("internal error: idx_colon_rep::sort_idx");

  count++;
  return this;
}

std::ostream&
idx_vector::idx_colon_rep::print (std::ostream& os) const
{
  return os << ":";
}


idx_vector::idx_range_rep::idx_range_rep (octave_idx_type _start,
                                          octave_idx_type _limit,
                                          octave_idx_type _step)
  : start(_start), len (_step ? std::max ((_limit - _start) / _step, static_cast<octave_idx_type> (0)) : -1), step (_step)
{
  if (len < 0)
    {
      gripe_invalid_range ();
      err = true;
    }
  else if (start < 0 || (step < 0 && start + (len-1)*step < 0))
    {
      gripe_invalid_index ();
      err = true;
    }
}

idx_vector::idx_range_rep::idx_range_rep (const Range& r)
  : start (0), len (r.nelem ()), step (1)
{
  if (len < 0)
    {
      gripe_invalid_range ();
      err = true;
    }
  else if (len > 0)
    {
      if (r.all_elements_are_ints ())
        {
          start = static_cast<octave_idx_type> (r.base ()) - 1;
          step = static_cast<octave_idx_type> (r.inc ());
          if (start < 0 || (step < 0 && start + (len-1)*step < 0))
            {
              gripe_invalid_index ();
              err = true;
            }
        }
      else
        {
          gripe_invalid_index ();
          err = true;
        }
    }
}

octave_idx_type
idx_vector::idx_range_rep::checkelem (octave_idx_type i) const
{
  if (i < 0 || i >= len)
    {
      gripe_index_out_of_range ();
      return 0;
    }
  else
    return start + i*step;
}

idx_vector::idx_base_rep *
idx_vector::idx_range_rep::sort_uniq_clone (bool)
{
  if (step < 0)
    return new idx_range_rep (start + (len - 1)*step, len, -step, DIRECT);
  else
    {
      count++;
      return this;
    }
}

idx_vector::idx_base_rep *
idx_vector::idx_range_rep::sort_idx (Array<octave_idx_type>& idx)
{
  if (step < 0 && len > 0)
    {
      idx.clear (1, len);
      for (octave_idx_type i = 0; i < len; i++)
        idx.xelem (i) = len - 1 - i;
      return new idx_range_rep (start + (len - 1)*step, len, -step, DIRECT);
    }
  else
    {
      idx.clear (1, len);
      for (octave_idx_type i = 0; i < len; i++)
        idx.xelem (i) = i;
      count++;
      return this;
    }
}

std::ostream&
idx_vector::idx_range_rep::print (std::ostream& os) const
{
  os << start << ':' << step << ':' << start + len*step;
  return os;
}

Range
idx_vector::idx_range_rep::unconvert (void) const
{
  return Range (static_cast<double> (start+1),
                static_cast<double> (step), len);
}

Array<octave_idx_type>
idx_vector::idx_range_rep::as_array (void)
{
  Array<octave_idx_type> retval (dim_vector (1, len));
  for (octave_idx_type i = 0; i < len; i++)
    retval.xelem (i) = start + i*step;

  return retval;
}

inline octave_idx_type
convert_index (octave_idx_type i, bool& conv_error,
               octave_idx_type& ext)
{
  if (i <= 0)
    conv_error = true;

  if (ext < i)
    ext = i;

  return i - 1;
}

inline octave_idx_type
convert_index (double x, bool& conv_error, octave_idx_type& ext)
{
  octave_idx_type i = static_cast<octave_idx_type> (x);

  if (static_cast<double> (i) != x)
    conv_error = true;

  return convert_index (i, conv_error, ext);
}

inline octave_idx_type
convert_index (float x, bool& conv_error, octave_idx_type& ext)
{
  return convert_index (static_cast<double> (x), conv_error, ext);
}

template <class T>
inline octave_idx_type
convert_index (octave_int<T> x, bool& conv_error,
               octave_idx_type& ext)
{
  octave_idx_type i = octave_int<octave_idx_type> (x).value ();

  return convert_index (i, conv_error, ext);
}


template <class T>
idx_vector::idx_scalar_rep::idx_scalar_rep (T x)
  : data (0)
{
  octave_idx_type dummy = 0;

  data = convert_index (x, err, dummy);

  if (err)
    gripe_invalid_index ();
}

idx_vector::idx_scalar_rep::idx_scalar_rep (octave_idx_type i)
  : data (i)
{
  if (data < 0)
    {
      gripe_invalid_index ();
      err = true;
    }
}

octave_idx_type
idx_vector::idx_scalar_rep::checkelem (octave_idx_type i) const
{
  if (i != 0)
    gripe_index_out_of_range ();

  return data;
}

idx_vector::idx_base_rep *
idx_vector::idx_scalar_rep::sort_idx (Array<octave_idx_type>& idx)
{
  idx.clear (1, 1);
  idx.fill (0);
  count++;
  return this;
}

std::ostream& idx_vector::idx_scalar_rep::print (std::ostream& os) const
{
  return os << data;
}

double
idx_vector::idx_scalar_rep::unconvert (void) const
{
  return data + 1;
}

Array<octave_idx_type>
idx_vector::idx_scalar_rep::as_array (void)
{
  return Array<octave_idx_type> (dim_vector (1, 1), data);
}


template <class T>
idx_vector::idx_vector_rep::idx_vector_rep (const Array<T>& nda)
  : data (0), len (nda.numel ()), ext (0), aowner (0), orig_dims (nda.dims ())
{
  if (len != 0)
    {
      octave_idx_type *d = new octave_idx_type [len];
      for (octave_idx_type i = 0; i < len; i++)
        d[i] = convert_index (nda.xelem (i), err, ext);
      data = d;

      if (err)
        {
          delete [] data;
          gripe_invalid_index ();
        }
    }
}

// Note that this makes a shallow copy of the index array.

idx_vector::idx_vector_rep::idx_vector_rep (const Array<octave_idx_type>& inda)
  : data (inda.data ()), len (inda.numel ()), ext (0),
    aowner (new Array<octave_idx_type> (inda)), orig_dims (inda.dims ())
{
  if (len != 0)
    {
      octave_idx_type max = -1;
      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_idx_type k = inda.xelem (i);
          if (k < 0)
            err = true;
          else if (k > max)
            max = k;
        }

      ext = max + 1;

      if (err)
        gripe_invalid_index ();
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (const Array<octave_idx_type>& inda,
                                            octave_idx_type _ext, direct)
  : data (inda.data ()), len (inda.numel ()), ext (_ext),
    aowner (new Array<octave_idx_type> (inda)), orig_dims (inda.dims ())
{
  // No checking.
  if (ext < 0)
    {
      octave_idx_type max = -1;
      for (octave_idx_type i = 0; i < len; i++)
        if (data[i] > max)
          max = data[i];

      ext = max + 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (bool b)
  : data (0), len (b ? 1 : 0), ext (0), aowner (0), orig_dims (len, len)
{
  if (len != 0)
    {
      octave_idx_type *d = new octave_idx_type [1];
      d[0] = 0;
      data = d;
      ext = 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (const Array<bool>& bnda,
                                            octave_idx_type nnz)
  : data (0), len (nnz), ext (0), aowner (0), orig_dims ()
{
  if (nnz < 0)
    len = bnda.nnz ();

  const dim_vector dv = bnda.dims ();

  if (! dv.all_zero ())
    orig_dims = ((dv.length () == 2 && dv(0) == 1)
                 ? dim_vector (1, len) : dim_vector (len, 1));

  if (len != 0)
    {
      octave_idx_type *d = new octave_idx_type [len];

      octave_idx_type ntot = bnda.length ();

      octave_idx_type k = 0;
      for (octave_idx_type i = 0; i < ntot; i++)
        if (bnda.xelem (i))
          d[k++] = i;

      data = d;

      ext = d[k-1] + 1;
    }
}

idx_vector::idx_vector_rep::idx_vector_rep (const Sparse<bool>& bnda)
  : data (0), len (bnda.nnz ()), ext (0), aowner (0), orig_dims ()
{
  const dim_vector dv = bnda.dims ();

  if (! dv.all_zero ())
    orig_dims = ((dv.length () == 2 && dv(0) == 1)
                 ? dim_vector (1, len) : dim_vector (len, 1));

  if (len != 0)
    {
      octave_idx_type *d = new octave_idx_type [len];

      octave_idx_type k = 0;
      octave_idx_type nc = bnda.cols ();
      octave_idx_type nr = bnda.rows ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = bnda.cidx (j); i < bnda.cidx (j+1); i++)
          if (bnda.data (i))
            d[k++] = j * nr + bnda.ridx (i);

      data = d;

      ext = d[k-1] + 1;
    }
}

idx_vector::idx_vector_rep::~idx_vector_rep (void)
{
  if (aowner)
    delete aowner;
  else
    delete [] data;
}

octave_idx_type
idx_vector::idx_vector_rep::checkelem (octave_idx_type n) const
{
  if (n < 0 || n >= len)
    {
      gripe_invalid_index ();
      return 0;
    }

  return xelem (n);
}

idx_vector::idx_base_rep *
idx_vector::idx_vector_rep::sort_uniq_clone (bool uniq)
{
  if (len == 0)
    {
      count++;
      return this;
    }

  // This is wrapped in auto_ptr so that we don't leak on out-of-memory.
  std::auto_ptr<idx_vector_rep> new_rep (
    new idx_vector_rep (0, len, ext, orig_dims, DIRECT));

  if (ext > len*xlog2 (1.0 + len))
    {
      // Use standard sort via octave_sort.
      octave_idx_type *new_data = new octave_idx_type [len];
      new_rep->data = new_data;

      std::copy (data, data + len, new_data);
      octave_sort<octave_idx_type> lsort;
      lsort.set_compare (ASCENDING);
      lsort.sort (new_data, len);

      if (uniq)
        {
          octave_idx_type new_len = std::unique (new_data, new_data + len)
                                    - new_data;
          new_rep->len = new_len;
          if (new_rep->orig_dims.length () == 2 && new_rep->orig_dims(0) == 1)
            new_rep->orig_dims = dim_vector (1, new_len);
          else
            new_rep->orig_dims = dim_vector (new_len, 1);
        }
    }
  else if (uniq)
    {
      // Use two-pass bucket sort (only a mask array needed).
      OCTAVE_LOCAL_BUFFER_INIT (bool, has, ext, false);
      for (octave_idx_type i = 0; i < len; i++)
        has[data[i]] = true;

      octave_idx_type new_len = 0;
      for (octave_idx_type i = 0; i < ext; i++)
        new_len += has[i];

      new_rep->len = new_len;
      if (new_rep->orig_dims.length () == 2 && new_rep->orig_dims(0) == 1)
        new_rep->orig_dims = dim_vector (1, new_len);
      else
        new_rep->orig_dims = dim_vector (new_len, 1);

      octave_idx_type *new_data = new octave_idx_type [new_len];
      new_rep->data = new_data;

      for (octave_idx_type i = 0, j = 0; i < ext; i++)
        if (has[i])
          new_data[j++] = i;
    }
  else
    {
      // Use two-pass bucket sort.
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, cnt, ext, 0);
      for (octave_idx_type i = 0; i < len; i++)
        cnt[data[i]]++;

      octave_idx_type *new_data = new octave_idx_type [len];
      new_rep->data = new_data;

      for (octave_idx_type i = 0, j = 0; i < ext; i++)
        {
          for (octave_idx_type k = 0; k < cnt[i]; k++)
            new_data[j++] = i;
        }
    }

  return new_rep.release ();
}

idx_vector::idx_base_rep *
idx_vector::idx_vector_rep::sort_idx (Array<octave_idx_type>& idx)
{
  // This is wrapped in auto_ptr so that we don't leak on out-of-memory.
  std::auto_ptr<idx_vector_rep> new_rep (
    new idx_vector_rep (0, len, ext, orig_dims, DIRECT));

  if (ext > len*xlog2 (1.0 + len))
    {
      // Use standard sort via octave_sort.
      idx.clear (orig_dims);
      octave_idx_type *idx_data = idx.fortran_vec ();
      for (octave_idx_type i = 0; i < len; i++)
        idx_data[i] = i;

      octave_idx_type *new_data = new octave_idx_type [len];
      new_rep->data = new_data;
      std::copy (data, data + len, new_data);

      octave_sort<octave_idx_type> lsort;
      lsort.set_compare (ASCENDING);
      lsort.sort (new_data, idx_data, len);
    }
  else
    {
      // Use two-pass bucket sort.
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, cnt, ext, 0);

      for (octave_idx_type i = 0; i < len; i++)
        cnt[data[i]]++;

      idx.clear (orig_dims);
      octave_idx_type *idx_data = idx.fortran_vec ();

      octave_idx_type *new_data = new octave_idx_type [len];
      new_rep->data = new_data;

      for (octave_idx_type i = 0, k = 0; i < ext; i++)
        {
          octave_idx_type j = cnt[i];
          cnt[i] = k;
          k += j;
        }

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_idx_type j = data[i];
          octave_idx_type k = cnt[j]++;
          new_data[k] = j;
          idx_data[k] = i;
        }
    }

  return new_rep.release ();
}

std::ostream&
idx_vector::idx_vector_rep::print (std::ostream& os) const
{
  os << '[';

  for (octave_idx_type ii = 0; ii < len - 1; ii++)
    os << data[ii] << ',' << ' ';

  if (len > 0)
    os << data[len-1]; os << ']';

  return os;
}

Array<double>
idx_vector::idx_vector_rep::unconvert (void) const
{
  Array<double> retval (orig_dims);
  for (octave_idx_type i = 0; i < len; i++)
    retval.xelem (i) = data[i] + 1;
  return retval;
}

Array<octave_idx_type>
idx_vector::idx_vector_rep::as_array (void)
{
  if (aowner)
    return *aowner;
  else
    {
      Array<octave_idx_type> retval (orig_dims);
      std::memcpy (retval.fortran_vec (), data, len*sizeof (octave_idx_type));
      // Delete the old copy and share the data instead to save memory.
      delete [] data;
      data = retval.fortran_vec ();
      aowner = new Array<octave_idx_type> (retval);
      return retval;
    }
}


idx_vector::idx_mask_rep::idx_mask_rep (bool b)
  : data (0), len (b ? 1 : 0), ext (0), lsti (-1), lste (-1),
    aowner (0), orig_dims (len, len)
{
  if (len != 0)
    {
      bool *d = new bool [1];
      d[0] = true;
      data = d;
      ext = 1;
    }
}

idx_vector::idx_mask_rep::idx_mask_rep (const Array<bool>& bnda,
                                        octave_idx_type nnz)
  : data (0), len (nnz), ext (bnda.numel ()), lsti (-1), lste (-1),
    aowner (0), orig_dims ()
{
  if (nnz < 0)
    len = bnda.nnz ();

  // We truncate the extent as much as possible. For Matlab
  // compatibility, but maybe it's not a bad idea anyway.
  while (ext > 0 && ! bnda(ext-1))
    ext--;

  const dim_vector dv = bnda.dims ();

  if (! dv.all_zero ())
    orig_dims = ((dv.length () == 2 && dv(0) == 1)
                 ? dim_vector (1, len) : dim_vector (len, 1));

  aowner = new Array<bool> (bnda);
  data = bnda.data ();
}

idx_vector::idx_mask_rep::~idx_mask_rep (void)
{
  if (aowner)
    delete aowner;
  else
    delete [] data;
}

octave_idx_type
idx_vector::idx_mask_rep::xelem (octave_idx_type n) const
{
  if (n == lsti + 1)
    {
      lsti = n;
      while (! data[++lste]) ;
    }
  else
    {
      lsti = n++;
      lste = -1;
      while (n > 0)
        if (data[++lste]) --n;
    }
  return lste;
}

octave_idx_type
idx_vector::idx_mask_rep::checkelem (octave_idx_type n) const
{
  if (n < 0 || n >= len)
    {
      gripe_invalid_index ();
      return 0;
    }

  return xelem (n);
}

std::ostream&
idx_vector::idx_mask_rep::print (std::ostream& os) const
{
  os << '[';

  for (octave_idx_type ii = 0; ii < ext - 1; ii++)
    os << data[ii] << ',' << ' ';

  if (ext > 0)
    os << data[ext-1]; os << ']';

  return os;
}

Array<bool>
idx_vector::idx_mask_rep::unconvert (void) const
{
  if (aowner)
    return *aowner;
  else
    {
      Array<bool> retval (dim_vector (ext, 1));
      for (octave_idx_type i = 0; i < ext; i++)
        retval.xelem (i) = data[i];
      return retval;
    }
}

Array<octave_idx_type>
idx_vector::idx_mask_rep::as_array (void)
{
  if (aowner)
    return aowner->find ().reshape (orig_dims);
  else
    {
      Array<bool> retval (orig_dims);
      for (octave_idx_type i = 0, j = 0; i < ext; i++)
        if (data[i])
          retval.xelem (j++) = i;

      return retval;
    }
}

idx_vector::idx_base_rep *
idx_vector::idx_mask_rep::sort_idx (Array<octave_idx_type>& idx)
{
  idx.clear (len, 1);
  for (octave_idx_type i = 0; i < len; i++)
    idx.xelem (i) = i;

  count++;
  return this;
}

const idx_vector idx_vector::colon (new idx_vector::idx_colon_rep ());

idx_vector::idx_vector (const Array<bool>& bnda)
  : rep (0)
{
  // Convert only if it means saving at least half the memory.
  static const int factor = (2 * sizeof (octave_idx_type));
  octave_idx_type nnz = bnda.nnz ();
  if (nnz <= bnda.numel () / factor)
    rep = new idx_vector_rep (bnda, nnz);
  else
    rep = new idx_mask_rep (bnda, nnz);
}

bool
idx_vector::maybe_reduce (octave_idx_type n, const idx_vector& j,
                          octave_idx_type nj)
{
  bool reduced = false;

  // Empty index always reduces.
  if (rep->length (n) == 0)
    {
      *this = idx_vector ();
      return true;
    }

  // Possibly skip singleton dims.
  if (n == 1 && rep->is_colon_equiv (n))
    {
      *this = j;
      return true;
    }

  if (nj == 1 && j.is_colon_equiv (nj))
    return true;

  switch (j.idx_class ())
    {
    case class_colon:
      switch (rep->idx_class ())
        {
        case class_colon:
          // (:,:) reduces to (:)
          reduced = true;
          break;

        case class_scalar:
          {
            // (i,:) reduces to a range.
            idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
            octave_idx_type k = r->get_data ();
            *this = new idx_range_rep (k, nj, n, DIRECT);
            reduced = true;
          }
          break;

        case class_range:
          {
            // (i:k:end,:) reduces to a range if i <= k and k divides n.
            idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
            octave_idx_type s = r->get_start ();
            octave_idx_type l = r->length (n);
            octave_idx_type t = r->get_step ();
            if (l*t == n)
              {
                *this = new idx_range_rep (s, l * nj, t, DIRECT);
                reduced = true;
              }
          }
          break;

        default:
          break;
        }
      break;

    case class_range:
      switch (rep->idx_class ())
        {
        case class_colon:
          {
            // (:,i:j) reduces to a range (the step must be 1)
            idx_range_rep * rj = dynamic_cast<idx_range_rep *> (j.rep);
            if (rj->get_step () == 1)
              {
                octave_idx_type sj = rj->get_start ();
                octave_idx_type lj = rj->length (nj);
                *this = new idx_range_rep (sj * n, lj * n, 1, DIRECT);
                reduced = true;
              }
          }
          break;

        case class_scalar:
          {
            // (k,i:d:j) reduces to a range.
            idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
            idx_range_rep * rj = dynamic_cast<idx_range_rep *> (j.rep);
            octave_idx_type k = r->get_data ();
            octave_idx_type sj = rj->get_start ();
            octave_idx_type lj = rj->length (nj);
            octave_idx_type tj = rj->get_step ();
            *this = new idx_range_rep (n * sj + k, lj, n * tj, DIRECT);
            reduced = true;
          }
          break;

        case class_range:
          {
            // (i:k:end,p:q) reduces to a range if i <= k and k divides n.
            // (ones (1, m), ones (1, n)) reduces to (ones (1, m*n))
            idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
            octave_idx_type s = r->get_start ();
            octave_idx_type l = r->length (n);
            octave_idx_type t = r->get_step ();
            idx_range_rep * rj = dynamic_cast<idx_range_rep *> (j.rep);
            octave_idx_type sj = rj->get_start ();
            octave_idx_type lj = rj->length (nj);
            octave_idx_type tj = rj->get_step ();
            if ((l*t == n && tj == 1) || (t == 0 && tj == 0))
              {
                *this = new idx_range_rep (s + n * sj, l * lj, t, DIRECT);
                reduced = true;
              }
          }
          break;

        default:
          break;
        }
      break;

    case class_scalar:
      switch (rep->idx_class ())
        {
        case class_scalar:
          {
            // (i,j) reduces to a single index.
            idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
            idx_scalar_rep * rj = dynamic_cast<idx_scalar_rep *> (j.rep);
            octave_idx_type k = r->get_data () + n * rj->get_data ();
            *this = new idx_scalar_rep (k, DIRECT);
            reduced = true;
          }
          break;

        case class_range:
          {
            // (i:d:j,k) reduces to a range.
            idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
            idx_scalar_rep * rj = dynamic_cast<idx_scalar_rep *> (j.rep);
            octave_idx_type s = r->get_start ();
            octave_idx_type l = r->length (nj);
            octave_idx_type t = r->get_step ();
            octave_idx_type k = rj->get_data ();
            *this = new idx_range_rep (n * k + s, l, t, DIRECT);
            reduced = true;
          }
          break;

        case class_colon:
          {
            // (:,k) reduces to a range.
            idx_scalar_rep * rj = dynamic_cast<idx_scalar_rep *> (j.rep);
            octave_idx_type k = rj->get_data ();
            *this = new idx_range_rep (n * k, n, 1, DIRECT);
            reduced = true;
          }
          break;

        default:
          break;
        }
      break;

    default:
      break;
    }

  return reduced;
}

bool
idx_vector::is_cont_range (octave_idx_type n,
                           octave_idx_type& l, octave_idx_type& u) const
{
  bool res = false;

  switch (rep->idx_class ())
    {
    case class_colon:
      l = 0; u = n;
      res = true;
      break;

    case class_range:
      {
        idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
        if (r->get_step () == 1)
          {
            l = r->get_start ();
            u = l + r->length (n);
            res = true;
          }
      }
      break;

    case class_scalar:
      {
        idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
        l = r->get_data ();
        u = l + 1;
        res = true;
      }
      break;

    case class_mask:
      {
        idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
        octave_idx_type ext = r->extent (0);
        octave_idx_type len = r->length (0);
        if (ext == len)
          {
            l = 0;
            u = len;
            res = true;
          }
      }

    default:
      break;
    }

  return res;
}

octave_idx_type
idx_vector::increment (void) const
{
  octave_idx_type retval = 0;

  switch (rep->idx_class ())
    {
    case class_colon:
      retval = 1;
      break;

    case class_range:
      retval = dynamic_cast<idx_range_rep *> (rep) -> get_step ();
      break;

    case class_vector:
    case class_mask:
      {
        if (length (0) > 1)
          retval = elem (1) - elem (0);
      }
      break;

    default:
      break;
    }

  return retval;
}

const octave_idx_type *
idx_vector::raw (void)
{
  if (rep->idx_class () != class_vector)
    *this = idx_vector (as_array (), extent (0));

  idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);

  assert (r != 0);

  return r->get_data ();
}

void
idx_vector::copy_data (octave_idx_type *data) const
{
  octave_idx_type len = rep->length (0);

  switch (rep->idx_class ())
    {
    case class_colon:
      current_liboctave_error_handler ("colon not allowed");
      break;

    case class_range:
      {
        idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
        octave_idx_type start = r->get_start ();
        octave_idx_type step = r->get_step ();
        octave_idx_type i, j;
        if (step == 1)
          for (i = start, j = start + len; i < j; i++) *data++ = i;
        else if (step == -1)
          for (i = start, j = start - len; i > j; i--) *data++ = i;
        else
          for (i = 0, j = start; i < len; i++, j += step) *data++ = j;
      }
      break;

    case class_scalar:
      {
        idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
        *data = r->get_data ();
      }
      break;

    case class_vector:
      {
        idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);
        const octave_idx_type *rdata = r->get_data ();
        std::copy (rdata, rdata + len, data);
      }
      break;

    case class_mask:
      {
        idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
        const bool *mask = r->get_data ();
        octave_idx_type ext = r->extent (0);
        for (octave_idx_type i = 0, j = 0; i < ext; i++)
          if (mask[i])
            data[j++] = i;
      }
      break;

    default:
      assert (false);
      break;
    }
}

idx_vector
idx_vector::complement (octave_idx_type n) const
{
  idx_vector retval;
  if (extent (n) > n)
    (*current_liboctave_error_handler)
      ("internal error: out of range complement index requested");

  if (idx_class () == class_mask)
    {
      idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
      octave_idx_type nz = r->length (0);
      octave_idx_type ext = r->extent (0);
      Array<bool> mask (dim_vector (n, 1));
      const bool *data = r->get_data ();
      bool *ndata = mask.fortran_vec ();
      for (octave_idx_type i = 0; i < ext; i++)
        ndata[i] = ! data[i];
      for (octave_idx_type i = ext; i < n; i++)
        ndata[i] = true;
      retval = new idx_mask_rep (mask, n - nz);
    }
  else
    {
      Array<bool> mask (dim_vector (n, 1), true);
      fill (false, length (n), mask.fortran_vec ());
      retval = idx_vector (mask);
    }

  return retval;
}

bool
idx_vector::is_permutation (octave_idx_type n) const
{
  bool retval = false;

  if (is_colon_equiv (n))
    retval = true;
  else if (length(n) == n && extent(n) == n)
    {
      OCTAVE_LOCAL_BUFFER_INIT (bool, left, n, true);

      retval = true;

      for (octave_idx_type i = 0, len = length (); i < len; i++)
        {
          octave_idx_type k = xelem (i);
          if (left[k])
            left[k] = false;
          else
            {
              retval = false;
              break;
            }
        }
    }

  return retval;
}

idx_vector
idx_vector::inverse_permutation (octave_idx_type n) const
{
  assert (n == length (n));

  idx_vector retval;

  switch (idx_class ())
    {
    case class_range:
      {
        if (increment () == -1)
          retval = sorted ();
        else
          retval = *this;
        break;
      }
    case class_vector:
      {
        idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (rep);
        const octave_idx_type *ri = r->get_data ();
        Array<octave_idx_type> idx (orig_dimensions ());
        for (octave_idx_type i = 0; i < n; i++)
          idx.xelem (ri[i]) = i;
        retval = new idx_vector_rep (idx, r->extent (0), DIRECT);
        break;
      }
    default:
      retval = *this;
      break;
    }

  return retval;
}

idx_vector
idx_vector::unmask (void) const
{
  if (idx_class () == class_mask)
    {
      idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
      const bool *data = r->get_data ();
      octave_idx_type ext = r->extent (0);
      octave_idx_type len = r->length (0);
      octave_idx_type *idata = new octave_idx_type [len];

      for (octave_idx_type i = 0, j = 0; i < ext; i++)
        if (data[i])
          idata[j++] = i;

      ext = len > 0 ? idata[len - 1] + 1 : 0;

      return new idx_vector_rep (idata, len, ext, r->orig_dimensions (),
                                 DIRECT);
    }
  else
    return *this;
}

void idx_vector::unconvert (idx_class_type& iclass,
                            double& scalar, Range& range,
                            Array<double>& array, Array<bool>& mask) const
{
  iclass = idx_class ();
  switch (iclass)
    {
    case class_colon:
      break;

    case class_range:
      {
        idx_range_rep *r = dynamic_cast<idx_range_rep *> (rep);
        range = r->unconvert ();
      }
      break;

    case class_scalar:
      {
        idx_scalar_rep *r = dynamic_cast<idx_scalar_rep *> (rep);
        scalar = r->unconvert ();
      }
      break;

    case class_vector:
      {
        idx_vector_rep *r = dynamic_cast<idx_vector_rep *> (rep);
        array = r->unconvert ();
      }
      break;

    case class_mask:
      {
        idx_mask_rep *r = dynamic_cast<idx_mask_rep *> (rep);
        mask = r->unconvert ();
      }
      break;

    default:
      assert (false);
      break;
    }
}

Array<octave_idx_type>
idx_vector::as_array (void) const
{
  return rep->as_array ();
}

bool
idx_vector::is_vector (void) const
{
  return idx_class () != class_vector || orig_dimensions ().is_vector ();
}

octave_idx_type
idx_vector::freeze (octave_idx_type z_len, const char *, bool resize_ok)
{
  if (! resize_ok && extent (z_len) > z_len)
    {
      (*current_liboctave_error_handler)
        ("invalid matrix index = %d", extent (z_len));
      rep->err = true;
      chkerr ();
    }

  return length (z_len);
}

octave_idx_type
idx_vector::ones_count () const
{
  octave_idx_type n = 0;

  if (is_colon ())
    n = 1;
  else
    {
      for (octave_idx_type i = 0; i < length (1); i++)
        if (xelem (i) == 0)
          n++;
    }

  return n;
}

// Instantiate the octave_int constructors we want.
#define INSTANTIATE_SCALAR_VECTOR_REP_CONST(T) \
  template OCTAVE_API idx_vector::idx_scalar_rep::idx_scalar_rep (T); \
  template OCTAVE_API idx_vector::idx_vector_rep::idx_vector_rep (const Array<T>&);

INSTANTIATE_SCALAR_VECTOR_REP_CONST (float)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (double)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int8)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int16)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int32)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_int64)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint8)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint16)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint32)
INSTANTIATE_SCALAR_VECTOR_REP_CONST (octave_uint64)

/*

%!error id=Octave:index-out-of-bounds 1(find ([1,1] != 0))
%!assert ((1:3)(find ([1,0,1] != 0)), [1,3])

*/
