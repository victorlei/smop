/*

Copyright (C) 2003-2015 John W. Eaton
Copyirght (C) 2009, 2010 VZLU Prague

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

#if !defined (octave_dim_vector_h)
#define octave_dim_vector_h 1

#include <cassert>
#include <limits>

#include <sstream>
#include <string>

#include "lo-error.h"
#include "lo-macros.h"
#include "oct-refcount.h"

// Rationale: This implementation is more tricky than Array, but the
// big plus is that dim_vector requires only one allocation instead of
// two.  It is (slightly) patterned after GCC's basic_string
// implementation.  rep is a pointer to an array of memory, comprising
// count, length, and the data:
//
//          <count>
//          <ndims>
//  rep --> <dims[0]>
//          <dims[1]>
//          ...
//
// The inlines count(), ndims() recover this data from the rep.  Note
// that rep points to the beginning of dims to grant faster access
// (reinterpret_cast is assumed to be an inexpensive operation).

class
OCTAVE_API
dim_vector
{
private:

  octave_idx_type *rep;

  octave_idx_type& ndims (void) const { return rep[-1]; }

  octave_idx_type& count (void) const { return rep[-2]; }

  //! Construct a new rep with count = 1 and ndims given.

  static octave_idx_type *newrep (int ndims)
  {
    octave_idx_type *r = new octave_idx_type [ndims + 2];

    *r++ = 1;
    *r++ = ndims;

    return r;
  }

  //! Clone this->rep.

  octave_idx_type *clonerep (void)
  {
    int l = ndims ();

    octave_idx_type *r = new octave_idx_type [l + 2];

    *r++ = 1;
    *r++ = l;

    for (int i = 0; i < l; i++)
      r[i] = rep[i];

    return r;
  }

  //! Clone and resize this->rep to length n, filling by given value.

  octave_idx_type *resizerep (int n, octave_idx_type fill_value)
  {
    int l = ndims ();

    if (n < 2)
      n = 2;

    octave_idx_type *r = new octave_idx_type [n + 2];

    *r++ = 1;
    *r++ = n;

    if (l > n)
      l = n;

    int j;
    for (j = 0; j < l; j++)
      r[j] = rep[j];
    for (; j < n; j++)
      r[j] = fill_value;

    return r;
  }

  //! Free the rep.

  void freerep (void)
  {
    assert (count () == 0);
    delete [] (rep - 2);
  }

  void make_unique (void)
  {
    if (count () > 1)
      {
        octave_idx_type *new_rep = clonerep ();

        if (OCTREFCOUNT_ATOMIC_DECREMENT(&(count())) == 0)
          freerep ();

        rep = new_rep;
      }
  }

public:

// There are constructors for up to 7 dimensions initialized this way.
// More can be added if necessary.
#define ASSIGN_REP(i) rep[i] = d ## i;
#define DIM_VECTOR_CTOR(N) \
  dim_vector (OCT_MAKE_DECL_LIST (octave_idx_type, d, N)) \
    : rep (newrep (N)) \
  { \
    OCT_ITERATE_MACRO (ASSIGN_REP, N) \
  }

  //! Construct dim_vector for 2 dimensional array.
  /*!
    It can be used to construct a 2D array.  Example:

    @code{.cc}
    dim_vector dv (7, 5);
    Matrix mat (dv);
    @endcode

    The constructed dim_vector @c dv will have two elements, @f$[7, 5]@f$,
    one for each dimension.  It can then be used to construct a Matrix
    with such dimensions, i.e., 7 rows and 5 columns.

    There are constructors available for up to 7 dimensions.  For a higher
    number of dimensions, use redim() or resize().

    Note that that there is no constructor for a 1 element dim_vector.
    This is because there are no 1 dimensional Array in liboctave.  Such
    constructor did exist in liboctave but was removed in version 4.0.0
    due to its potential for confusion.
  */
  DIM_VECTOR_CTOR (2)

  //! Construct dim_vector for 3 dimensional array.
  /*!
    It can be used to construct a 3D array.  Example:

    @code{.cc}
    NDArray A (dim_vector (7, 5, 4));
    @endcode

    This will construct a 3 dimensional NDArray of lengths 7, 5, and 4,
    on the first, second, and third dimension (rows, columns, and pages)
    respectively.
  */
  DIM_VECTOR_CTOR (3)

  //! Construct dim_vector for 4 dimensional array.
  //! @see dim_vector(octave_idx_type d0, octave_idx_type d1)
  DIM_VECTOR_CTOR (4)
  //! Construct dim_vector for 5 dimensional array.
  //! @see dim_vector(octave_idx_type d0, octave_idx_type d1)
  DIM_VECTOR_CTOR (5)
  //! Construct dim_vector for 6 dimensional array.
  //! @see dim_vector(octave_idx_type d0, octave_idx_type d1)
  DIM_VECTOR_CTOR (6)
  //! Construct dim_vector for 7 dimensional array.
  //! @see dim_vector(octave_idx_type d0, octave_idx_type d1)
  DIM_VECTOR_CTOR (7)

#undef ASSIGN_REP
#undef DIM_VECTOR_CTOR

  octave_idx_type& elem (int i)
  {
#ifdef BOUNDS_CHECKING
    assert (i >= 0 && i < ndims ());
#endif
    make_unique ();
    return rep[i];
  }

  octave_idx_type elem (int i) const
  {
#ifdef BOUNDS_CHECKING
    assert (i >= 0 && i < ndims ());
#endif
    return rep[i];
  }

  void chop_trailing_singletons (void)
  {
    int l = ndims ();
    if (l > 2 && rep[l-1] == 1)
      {
        make_unique ();
        do
          l--;
        while (l > 2 && rep[l-1] == 1);
        ndims () = l;
      }
  }

  void chop_all_singletons (void);

  // WARNING: Only call by jit
  octave_idx_type *to_jit (void) const
  {
    return rep;
  }

private:

  static octave_idx_type *nil_rep (void);

public:

  static octave_idx_type dim_max (void);

  explicit dim_vector (void) : rep (nil_rep ())
  { OCTREFCOUNT_ATOMIC_INCREMENT (&(count())); }

  dim_vector (const dim_vector& dv) : rep (dv.rep)
  { OCTREFCOUNT_ATOMIC_INCREMENT (&(count())); }

  // FIXME: Should be private, but required by array constructor for jit
  explicit dim_vector (octave_idx_type *r) : rep (r) { }

  static dim_vector alloc (int n)
  {
    return dim_vector (newrep (n < 2 ? 2 : n));
  }

  dim_vector& operator = (const dim_vector& dv)
  {
    if (&dv != this)
      {
        if (OCTREFCOUNT_ATOMIC_DECREMENT (&(count())) == 0)
          freerep ();

        rep = dv.rep;
        OCTREFCOUNT_ATOMIC_INCREMENT (&(count()));
      }

    return *this;
  }

  ~dim_vector (void)
  {
    if (OCTREFCOUNT_ATOMIC_DECREMENT (&(count())) == 0)
      freerep ();
  }

  int length (void) const { return ndims (); }

  octave_idx_type& operator () (int i) { return elem (i); }

  octave_idx_type operator () (int i) const { return elem (i); }

  void resize (int n, int fill_value = 0)
  {
    int len = length ();

    if (n != len)
      {
        octave_idx_type *r = resizerep (n, fill_value);

        if (OCTREFCOUNT_ATOMIC_DECREMENT (&(count())) == 0)
          freerep ();

        rep = r;
      }
  }

  std::string str (char sep = 'x') const;

  bool all_zero (void) const
  {
    bool retval = true;

    for (int i = 0; i < length (); i++)
      {
        if (elem (i) != 0)
          {
            retval = false;
            break;
          }
      }

    return retval;
  }

  bool empty_2d (void) const
  {
    return length () == 2 && (elem (0) == 0 || elem (1) == 0);
  }


  bool zero_by_zero (void) const
  {
    return length () == 2 && elem (0) == 0 && elem (1) == 0;
  }

  bool any_zero (void) const
  {
    bool retval = false;

    for (int i = 0; i < length (); i++)
      {
        if (elem (i) == 0)
          {
            retval = true;
            break;
          }
      }

    return retval;
  }

  int num_ones (void) const;

  bool all_ones (void) const
  {
    return (num_ones () == length ());
  }

  //! Number of elements that a matrix with this dimensions would have.
  /*!
     Return the number of elements that a matrix with this dimension
     vector would have, NOT the number of dimensions (elements in the
     dimension vector).
  */

  octave_idx_type numel (int n = 0) const
  {
    int n_dims = length ();

    octave_idx_type retval = 1;

    for (int i = n; i < n_dims; i++)
      retval *= elem (i);

    return retval;
  }

  /*!
     The following function will throw a std::bad_alloc ()
     exception if the requested size is larger than can be indexed by
     octave_idx_type. This may be smaller than the actual amount of
     memory that can be safely allocated on a system.  However, if we
     don't fail here, we can end up with a mysterious crash inside a
     function that is iterating over an array using octave_idx_type
     indices.
  */

  octave_idx_type safe_numel (void) const;

  bool any_neg (void) const
  {
    int n_dims = length ();
    int i;

    for (i = 0; i < n_dims; i++)
      if (elem (i) < 0)
        break;

    return i < n_dims;
  }

  dim_vector squeeze (void) const;

  //! This corresponds to cat().
  bool concat (const dim_vector& dvb, int dim);

  //! This corresponds to [,] (horzcat, dim = 0) and [;] (vertcat, dim = 1).
  // The rules are more relaxed here.
  bool hvcat (const dim_vector& dvb, int dim);

  /*!
      Force certain dimensionality, preserving numel ().  Missing
      dimensions are set to 1, redundant are folded into the trailing
      one.  If n = 1, the result is 2d and the second dim is 1
      (dim_vectors are always at least 2D).
  */
  dim_vector redim (int n) const;

  dim_vector as_column (void) const
  {
    if (length () == 2 && elem (1) == 1)
      return *this;
    else
      return dim_vector (numel (), 1);
  }

  dim_vector as_row (void) const
  {
    if (length () == 2 && elem (0) == 1)
      return *this;
    else
      return dim_vector (1, numel ());
  }

  bool is_vector (void) const
  {
    return (length () == 2 && (elem (0) == 1 || elem (1) == 1));
  }

  int first_non_singleton (int def = 0) const
  {
    for (int i = 0; i < length (); i++)
      {
        if (elem (i) != 1)
          return i;
      }

    return def;
  }

  //! Compute a linear index from an index tuple.

  octave_idx_type compute_index (const octave_idx_type *idx) const
  {
    octave_idx_type k = 0;
    for (int i = length () - 1; i >= 0; i--)
      k = k * rep[i] + idx[i];

    return k;
  }

  //! Ditto, but the tuple may be incomplete (nidx < length ()).

  octave_idx_type compute_index (const octave_idx_type *idx, int nidx) const
  {
    octave_idx_type k = 0;
    for (int i = nidx - 1; i >= 0; i--)
      k = k * rep[i] + idx[i];

    return k;
  }

  /*/!
      Increment a multi-dimensional index tuple, optionally starting
      from an offset position and return the index of the last index
      position that was changed, or length () if just cycled over.
  */

  int increment_index (octave_idx_type *idx, int start = 0) const
  {
    int i;
    for (i = start; i < length (); i++)
      {
        if (++(*idx) == rep[i])
          *idx++ = 0;
        else
          break;
      }
    return i;
  }

  //! Return cumulative dimensions.

  dim_vector cumulative (void) const
  {
    int nd = length ();
    dim_vector retval = alloc (nd);

    octave_idx_type k = 1;
    for (int i = 0; i < nd; i++)
      retval.rep[i] = k *= rep[i];

    return retval;
  }

  //! Compute a linear index from an index tuple.  Dimensions are
  //! required to be cumulative.

  octave_idx_type cum_compute_index (const octave_idx_type *idx) const
  {
    octave_idx_type k = idx[0];

    for (int i = 1; i < length (); i++)
      k += rep[i-1] * idx[i];

    return k;
  }


  friend bool operator == (const dim_vector& a, const dim_vector& b);
};

inline bool
operator == (const dim_vector& a, const dim_vector& b)
{
  // Fast case.
  if (a.rep == b.rep)
    return true;

  bool retval = true;

  int a_len = a.length ();
  int b_len = b.length ();

  if (a_len != b_len)
    retval = false;
  else
    {
      for (int i = 0; i < a_len; i++)
        {
          if (a(i) != b(i))
            {
              retval = false;
              break;
            }
        }
    }

  return retval;
}

inline bool
operator != (const dim_vector& a, const dim_vector& b)
{
  return ! operator == (a, b);
}

#endif
