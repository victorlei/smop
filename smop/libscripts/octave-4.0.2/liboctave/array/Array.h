// Template array classes
/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
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

#if !defined (octave_Array_h)
#define octave_Array_h 1

#include <cassert>
#include <cstddef>

#include <algorithm>
#include <iosfwd>

#include "dim-vector.h"
#include "idx-vector.h"
#include "lo-traits.h"
#include "lo-utils.h"
#include "oct-sort.h"
#include "quit.h"
#include "oct-refcount.h"

//!Handles the reference counting for all the derived classes.
template <class T>
class
Array
{
protected:

  //! The real representation of all arrays.
  class ArrayRep
  {
  public:

    T *data;
    octave_idx_type len;
    octave_refcount<int> count;

    ArrayRep (T *d, octave_idx_type l)
      : data (new T [l]), len (l), count (1)
    {
      std::copy (d, d+l, data);
    }

    template <class U>
    ArrayRep (U *d, octave_idx_type l)
      : data (new T [l]), len (l), count (1)
    {
      std::copy (d, d+l, data);
    }

    ArrayRep (void) : data (0), len (0), count (1) { }

    explicit ArrayRep (octave_idx_type n)
      : data (new T [n]), len (n), count (1) { }

    explicit ArrayRep (octave_idx_type n, const T& val)
      : data (new T [n]), len (n), count (1)
    {
      std::fill_n (data, n, val);
    }

    ArrayRep (const ArrayRep& a)
      : data (new T [a.len]), len (a.len), count (1)
    {
      std::copy (a.data, a.data + a.len, data);
    }

    ~ArrayRep (void) { delete [] data; }

    octave_idx_type length (void) const { return len; }

  private:

    // No assignment!

    ArrayRep& operator = (const ArrayRep& a);
  };

  //--------------------------------------------------------------------

public:

  void make_unique (void)
  {
    if (rep->count > 1)
      {
        ArrayRep *r = new ArrayRep (slice_data, slice_len);

        if (--rep->count == 0)
          delete rep;

        rep = r;
        slice_data = rep->data;
      }
  }

  typedef T element_type;

  typedef typename ref_param<T>::type crefT;

  typedef bool (*compare_fcn_type) (typename ref_param<T>::type,
                                    typename ref_param<T>::type);

protected:

  dim_vector dimensions;

  typename Array<T>::ArrayRep *rep;

  // Rationale:
  // slice_data is a pointer to rep->data, denoting together with slice_len the
  // actual portion of the data referenced by this Array<T> object. This allows
  // to make shallow copies not only of a whole array, but also of contiguous
  // subranges. Every time rep is directly manipulated, slice_data and slice_len
  // need to be properly updated.

  T* slice_data;
  octave_idx_type slice_len;

  //! slice constructor
  Array (const Array<T>& a, const dim_vector& dv,
         octave_idx_type l, octave_idx_type u)
    : dimensions (dv), rep(a.rep), slice_data (a.slice_data+l), slice_len (u-l)
  {
    rep->count++;
    dimensions.chop_trailing_singletons ();
  }

private:

  static typename Array<T>::ArrayRep *nil_rep (void);

protected:

  //! For jit support
  Array (T *sdata, octave_idx_type slen, octave_idx_type *adims, void *arep)
    : dimensions (adims),
      rep (reinterpret_cast<typename Array<T>::ArrayRep *> (arep)),
      slice_data (sdata), slice_len (slen) { }

public:

  //! Empty ctor (0 by 0).
  Array (void)
    : dimensions (), rep (nil_rep ()), slice_data (rep->data),
      slice_len (rep->len)
  {
    rep->count++;
  }

  //! nD uninitialized ctor.
  explicit Array (const dim_vector& dv)
    : dimensions (dv),
      rep (new typename Array<T>::ArrayRep (dv.safe_numel ())),
      slice_data (rep->data), slice_len (rep->len)
  {
    dimensions.chop_trailing_singletons ();
  }

  //! nD initialized ctor.
  explicit Array (const dim_vector& dv, const T& val)
    : dimensions (dv),
      rep (new typename Array<T>::ArrayRep (dv.safe_numel ())),
      slice_data (rep->data), slice_len (rep->len)
  {
    fill (val);
    dimensions.chop_trailing_singletons ();
  }

  //! Reshape constructor.
  Array (const Array<T>& a, const dim_vector& dv);

  //! Type conversion case.
  template <class U>
  Array (const Array<U>& a)
    : dimensions (a.dims ()),
      rep (new typename Array<T>::ArrayRep (a.data (), a.length ())),
      slice_data (rep->data), slice_len (rep->len)
  { }

  //! No type conversion case.
  Array (const Array<T>& a)
    : dimensions (a.dimensions), rep (a.rep), slice_data (a.slice_data),
      slice_len (a.slice_len)
  {
    rep->count++;
  }

public:

  virtual ~Array (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  Array<T>& operator = (const Array<T>& a)
  {
    if (this != &a)
      {
        if (--rep->count == 0)
          delete rep;

        rep = a.rep;
        rep->count++;

        dimensions = a.dimensions;
        slice_data = a.slice_data;
        slice_len = a.slice_len;
      }

    return *this;
  }

  void fill (const T& val);

  void clear (void);
  void clear (const dim_vector& dv);

  void clear (octave_idx_type r, octave_idx_type c)
  { clear (dim_vector (r, c)); }

  // Number of elements in the array. These are all synonyms.
  //@{
  //! Number of elements in the array.
  //! Synonymous with length(), nelem(), and numel().
  octave_idx_type capacity (void) const { return slice_len; }

  //! Number of elements in the array.
  /*! Synonymous with capacity(), nelem(), and numel().

      @note
      This is @em not the same as @c %length() at the Octave interpreter.
      At the Octave interpreter, the function @c %length() returns the
      length of the greatest dimension.  This method returns the total
      number of elements.
   */
  octave_idx_type length (void) const { return capacity (); }

  //! Number of elements in the array.
  //! Synonymous with capacity(), length(), and numel().
  octave_idx_type nelem (void) const { return capacity (); }

  //! Number of elements in the array.
  //! Synonymous with capacity(), length(), and nelem().
  octave_idx_type numel (void) const { return nelem (); }
  //@}

  //! Return the array as a column vector.
  Array<T> as_column (void) const
  {
    Array<T> retval (*this);
    if (dimensions.length () != 2 || dimensions(1) != 1)
      retval.dimensions = dim_vector (numel (), 1);

    return retval;
  }

  //! Return the array as a row vector.
  Array<T> as_row (void) const
  {
    Array<T> retval (*this);
    if (dimensions.length () != 2 || dimensions(0) != 1)
      retval.dimensions = dim_vector (1, numel ());

    return retval;
  }

  //! Return the array as a matrix.
  Array<T> as_matrix (void) const
  {
    Array<T> retval (*this);
    if (dimensions.length () != 2)
      retval.dimensions = dimensions.redim (2);

    return retval;
  }

  //! @name First dimension
  //!
  //! Get the first dimension of the array (number of rows)
  //@{
  octave_idx_type dim1 (void) const { return dimensions(0); }
  octave_idx_type rows (void) const { return dim1 (); }
  //@}

  //! @name Second dimension
  //!
  //! Get the second dimension of the array (number of columns)
  //@{
  octave_idx_type dim2 (void) const { return dimensions(1); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }
  //@}

  //! @name Third dimension
  //!
  //! Get the third dimension of the array (number of pages)
  //@{
  octave_idx_type dim3 (void) const { return dimensions(2); }
  octave_idx_type pages (void) const { return dim3 (); }
  //@}

  size_t byte_size (void) const
  { return static_cast<size_t> (numel ()) * sizeof (T); }

  //! Return a const-reference so that dims ()(i) works efficiently.
  const dim_vector& dims (void) const { return dimensions; }

  //! Chop off leading singleton dimensions
  Array<T> squeeze (void) const;

  octave_idx_type compute_index (octave_idx_type i, octave_idx_type j) const;
  octave_idx_type compute_index (octave_idx_type i, octave_idx_type j,
                                 octave_idx_type k) const;
  octave_idx_type compute_index (const Array<octave_idx_type>& ra_idx) const;

  octave_idx_type compute_index_unchecked (const Array<octave_idx_type>& ra_idx)
  const
  { return dimensions.compute_index (ra_idx.data (), ra_idx.length ()); }

  // No checking, even for multiple references, ever.

  T& xelem (octave_idx_type n) { return slice_data[n]; }
  crefT xelem (octave_idx_type n) const { return slice_data[n]; }

  T& xelem (octave_idx_type i, octave_idx_type j)
  { return xelem (dim1 ()*j+i); }
  crefT xelem (octave_idx_type i, octave_idx_type j) const
  { return xelem (dim1 ()*j+i); }

  T& xelem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return xelem (i, dim2 ()*k+j); }
  crefT xelem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const
  { return xelem (i, dim2 ()*k+j); }

  T& xelem (const Array<octave_idx_type>& ra_idx)
  { return xelem (compute_index_unchecked (ra_idx)); }

  crefT xelem (const Array<octave_idx_type>& ra_idx) const
  { return xelem (compute_index_unchecked (ra_idx)); }

  // FIXME: would be nice to fix this so that we don't unnecessarily force
  //        a copy, but that is not so easy, and I see no clean way to do it.

  T& checkelem (octave_idx_type n);
  T& checkelem (octave_idx_type i, octave_idx_type j);
  T& checkelem (octave_idx_type i, octave_idx_type j, octave_idx_type k);
  T& checkelem (const Array<octave_idx_type>& ra_idx);

  T& elem (octave_idx_type n)
  {
    make_unique ();
    return xelem (n);
  }

  T& elem (octave_idx_type i, octave_idx_type j) { return elem (dim1 ()*j+i); }

  T& elem (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return elem (i, dim2 ()*k+j); }

  T& elem (const Array<octave_idx_type>& ra_idx)
  { return Array<T>::elem (compute_index_unchecked (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T& operator () (octave_idx_type n) { return checkelem (n); }
  T& operator () (octave_idx_type i, octave_idx_type j)
  { return checkelem (i, j); }
  T& operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return checkelem (i, j, k); }
  T& operator () (const Array<octave_idx_type>& ra_idx)
  { return checkelem (ra_idx); }
#else
  T& operator () (octave_idx_type n) { return elem (n); }
  T& operator () (octave_idx_type i, octave_idx_type j) { return elem (i, j); }
  T& operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return elem (i, j, k); }
  T& operator () (const Array<octave_idx_type>& ra_idx)
  { return elem (ra_idx); }
#endif

  crefT checkelem (octave_idx_type n) const;
  crefT checkelem (octave_idx_type i, octave_idx_type j) const;
  crefT checkelem (octave_idx_type i, octave_idx_type j,
                   octave_idx_type k) const;
  crefT checkelem (const Array<octave_idx_type>& ra_idx) const;

  crefT elem (octave_idx_type n) const { return xelem (n); }

  crefT elem (octave_idx_type i, octave_idx_type j) const
  { return xelem (i, j); }

  crefT elem (octave_idx_type i, octave_idx_type j, octave_idx_type k) const
  { return xelem (i, j, k); }

  crefT elem (const Array<octave_idx_type>& ra_idx) const
  { return Array<T>::xelem (compute_index_unchecked (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  crefT operator () (octave_idx_type n) const { return checkelem (n); }
  crefT operator () (octave_idx_type i, octave_idx_type j) const
  { return checkelem (i, j); }
  crefT operator () (octave_idx_type i, octave_idx_type j,
                     octave_idx_type k) const
  { return checkelem (i, j, k); }
  crefT operator () (const Array<octave_idx_type>& ra_idx) const
  { return checkelem (ra_idx); }
#else
  crefT operator () (octave_idx_type n) const { return elem (n); }
  crefT operator () (octave_idx_type i, octave_idx_type j) const
  { return elem (i, j); }
  crefT operator () (octave_idx_type i, octave_idx_type j,
                     octave_idx_type k) const
  { return elem (i, j, k); }
  crefT operator () (const Array<octave_idx_type>& ra_idx) const
  { return elem (ra_idx); }
#endif

  // Fast extractors. All of these produce shallow copies.
  // Warning: none of these do check bounds, unless BOUNDS_CHECKING is on!

  //! Extract column: A(:,k+1).
  Array<T> column (octave_idx_type k) const;
  //! Extract page: A(:,:,k+1).
  Array<T> page (octave_idx_type k) const;

  //! Extract a slice from this array as a column vector: A(:)(lo+1:up).
  //! Must be 0 <= lo && up <= numel. May be up < lo.
  Array<T> linear_slice (octave_idx_type lo, octave_idx_type up) const;

  Array<T> reshape (octave_idx_type nr, octave_idx_type nc) const
  { return Array<T> (*this, dim_vector (nr, nc)); }

  Array<T> reshape (const dim_vector& new_dims) const
  { return Array<T> (*this, new_dims); }

  Array<T> permute (const Array<octave_idx_type>& vec, bool inv = false) const;
  Array<T> ipermute (const Array<octave_idx_type>& vec) const
  { return permute (vec, true); }

  bool is_square (void) const { return (dim1 () == dim2 ()); }

  bool is_empty (void) const { return numel () == 0; }

  bool is_vector (void) const { return dimensions.is_vector (); }

  Array<T> transpose (void) const;
  Array<T> hermitian (T (*fcn) (const T&) = 0) const;

  const T *data (void) const { return slice_data; }

  const T *fortran_vec (void) const { return data (); }

  T *fortran_vec (void);

  bool is_shared (void) { return rep->count > 1; }

  int ndims (void) const { return dimensions.length (); }

  //@{
  //! Indexing without resizing.
  Array<T> index (const idx_vector& i) const;

  Array<T> index (const idx_vector& i, const idx_vector& j) const;

  Array<T> index (const Array<idx_vector>& ia) const;
  //@}

  virtual T resize_fill_value (void) const;

  //@{
  //! Resizing (with fill).
  void resize2 (octave_idx_type nr, octave_idx_type nc, const T& rfv);
  void resize2 (octave_idx_type nr, octave_idx_type nc)
  {
    resize2 (nr, nc, resize_fill_value ());
  }

  void resize1 (octave_idx_type n, const T& rfv);
  void resize1 (octave_idx_type n) { resize1 (n, resize_fill_value ()); }

  void resize (const dim_vector& dv, const T& rfv);
  void resize (const dim_vector& dv) { resize (dv, resize_fill_value ()); }
  //@}

  //@{
  //! Indexing with possible resizing and fill

  // FIXME: this is really a corner case, that should better be
  // handled directly in liboctinterp.


  Array<T> index (const idx_vector& i, bool resize_ok, const T& rfv) const;
  Array<T> index (const idx_vector& i, bool resize_ok) const
  {
    return index (i, resize_ok, resize_fill_value ());
  }

  Array<T> index (const idx_vector& i, const idx_vector& j, bool resize_ok,
                  const T& rfv) const;
  Array<T> index (const idx_vector& i, const idx_vector& j,
                  bool resize_ok) const
  {
    return index (i, j, resize_ok, resize_fill_value ());
  }

  Array<T> index (const Array<idx_vector>& ia, bool resize_ok,
                  const T& rfv) const;
  Array<T> index (const Array<idx_vector>& ia, bool resize_ok) const
  {
    return index (ia, resize_ok, resize_fill_value ());
  }
  //@}


  //@{
  //! Indexed assignment (always with resize & fill).
  void assign (const idx_vector& i, const Array<T>& rhs, const T& rfv);
  void assign (const idx_vector& i, const Array<T>& rhs)
  {
    assign (i, rhs, resize_fill_value ());
  }

  void assign (const idx_vector& i, const idx_vector& j, const Array<T>& rhs,
               const T& rfv);
  void assign (const idx_vector& i, const idx_vector& j, const Array<T>& rhs)
  {
    assign (i, j, rhs, resize_fill_value ());
  }

  void assign (const Array<idx_vector>& ia, const Array<T>& rhs, const T& rfv);
  void assign (const Array<idx_vector>& ia, const Array<T>& rhs)
  {
    assign (ia, rhs, resize_fill_value ());
  }
  //@}

  //@{
  //! Deleting elements.

  //! A(I) = [] (with a single subscript)
  void delete_elements (const idx_vector& i);

  //! A(:,...,I,...,:) = [] (>= 2 subscripts, one of them is non-colon)
  void delete_elements (int dim, const idx_vector& i);

  //! Dispatcher to the above two.
  void delete_elements (const Array<idx_vector>& ia);
  //@}

  //! Insert an array into another at a specified position. If
  //! size (a) is [d1 d2 ... dN] and idx is [i1 i2 ... iN], this
  //! method is equivalent to x(i1:i1+d1-1, i2:i2+d2-1, ... ,
  //! iN:iN+dN-1) = a.
  Array<T>& insert (const Array<T>& a, const Array<octave_idx_type>& idx);

  //! This is just a special case for idx = [r c 0 ...]
  Array<T>& insert (const Array<T>& a, octave_idx_type r, octave_idx_type c);

  void maybe_economize (void)
  {
    if (rep->count == 1 && slice_len != rep->len)
      {
        ArrayRep *new_rep = new ArrayRep (slice_data, slice_len);
        delete rep;
        rep = new_rep;
        slice_data = rep->data;
      }
  }

  void print_info (std::ostream& os, const std::string& prefix) const;

  //! Give a pointer to the data in mex format. Unsafe. This function
  //! exists to support the MEX interface. You should not use it
  //! anywhere else.
  void *mex_get_data (void) const { return const_cast<T *> (data ()); }

  Array<T> sort (int dim = 0, sortmode mode = ASCENDING) const;
  Array<T> sort (Array<octave_idx_type> &sidx, int dim = 0,
                 sortmode mode = ASCENDING) const;

  //! Ordering is auto-detected or can be specified.
  sortmode is_sorted (sortmode mode = UNSORTED) const;

  //! Sort by rows returns only indices.
  Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const;

  //! Ordering is auto-detected or can be specified.
  sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  //! @brief Do a binary lookup in a sorted array. Must not contain NaNs.
  //! Mode can be specified or is auto-detected by comparing 1st and last element.
  octave_idx_type lookup (const T& value, sortmode mode = UNSORTED) const;

  //! Ditto, but for an array of values, specializing on the case when values
  //! are sorted. NaNs get the value N.
  Array<octave_idx_type> lookup (const Array<T>& values,
                                 sortmode mode = UNSORTED) const;

  //! Count nonzero elements.
  octave_idx_type nnz (void) const;

  //! Find indices of (at most n) nonzero elements. If n is specified,
  //! backward specifies search from backward.
  Array<octave_idx_type> find (octave_idx_type n = -1,
                               bool backward = false) const;

  //! Returns the n-th element in increasing order, using the same
  //! ordering as used for sort. n can either be a scalar index or a
  //! contiguous range.
  Array<T> nth_element (const idx_vector& n, int dim = 0) const;

  //! Get the kth super or subdiagonal. The zeroth diagonal is the
  //! ordinary diagonal.
  Array<T> diag (octave_idx_type k = 0) const;

  Array<T> diag (octave_idx_type m, octave_idx_type n) const;

  //! Concatenation along a specified (0-based) dimension, equivalent
  //! to cat(). dim = -1 corresponds to dim = 0 and dim = -2
  //! corresponds to dim = 1, but apply the looser matching rules of
  //! vertcat/horzcat.
  static Array<T>
  cat (int dim, octave_idx_type n, const Array<T> *array_list);

  //! Apply function fcn to each element of the Array<T>. This function
  //! is optimised with a manually unrolled loop.
  template <class U, class F>
  Array<U>
  map (F fcn) const
  {
    octave_idx_type len = length ();

    const T *m = data ();

    Array<U> result (dims ());
    U *p = result.fortran_vec ();

    octave_idx_type i;
    for (i = 0; i < len - 3; i += 4)
      {
        octave_quit ();

        p[i] = fcn (m[i]);
        p[i+1] = fcn (m[i+1]);
        p[i+2] = fcn (m[i+2]);
        p[i+3] = fcn (m[i+3]);
      }

    octave_quit ();

    for (; i < len; i++)
      p[i] = fcn (m[i]);

    return result;
  }

  //@{
  //! Overloads for function references.
  template <class U>
  Array<U>
  map (U (&fcn) (T)) const
  { return map<U, U (&) (T)> (fcn); }

  template <class U>
  Array<U>
  map (U (&fcn) (const T&)) const
  { return map<U, U (&) (const T&)> (fcn); }
  //@}

  //! Generic any/all test functionality with arbitrary predicate.
  template <class F, bool zero>
  bool test (F fcn) const
  {
    return any_all_test<F, T, zero> (fcn, data (), length ());
  }

  //@{
  //! Simpler calls.
  template <class F>
  bool test_any (F fcn) const
  { return test<F, false> (fcn); }

  template <class F>
  bool test_all (F fcn) const
  { return test<F, true> (fcn); }
  //@}

  //@{
  //! Overloads for function references.
  bool test_any (bool (&fcn) (T)) const
  { return test<bool (&) (T), false> (fcn); }

  bool test_any (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), false> (fcn); }

  bool test_all (bool (&fcn) (T)) const
  { return test<bool (&) (T), true> (fcn); }

  bool test_all (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), true> (fcn); }
  //@}

  template <class U> friend class Array;

  //! Returns true if this->dims () == dv, and if so, replaces this->dimensions
  //! by a shallow copy of dv. This is useful for maintaining several arrays with
  //! supposedly equal dimensions (e.g. structs in the interpreter).
  bool optimize_dimensions (const dim_vector& dv);

  //@{
  //! WARNING: Only call these functions from jit

  int *jit_ref_count (void) { return rep->count.get (); }

  T *jit_slice_data (void) const { return slice_data; }

  octave_idx_type *jit_dimensions (void) const { return dimensions.to_jit (); }

  void *jit_array_rep (void) const { return rep; }
  //@}

private:
  static void instantiation_guard ();
};

//! This is a simple wrapper template that will subclass an Array<T>
//! type or any later type derived from it and override the default
//! non-const operator() to not check for the array's uniqueness. It
//! is, however, the user's responsibility to ensure the array is
//! actually unaliased whenever elements are accessed.
template<class ArrayClass>
class NoAlias : public ArrayClass
{
  typedef typename ArrayClass::element_type T;
public:
  NoAlias () : ArrayClass () { }

  // FIXME: this would be simpler once C++0x is available
  template <class X>
    explicit NoAlias (X x) : ArrayClass (x) { }

  template <class X, class Y>
    explicit NoAlias (X x, Y y) : ArrayClass (x, y) { }

  template <class X, class Y, class Z>
    explicit NoAlias (X x, Y y, Z z) : ArrayClass (x, y, z) { }

  T& operator () (octave_idx_type n)
  { return ArrayClass::xelem (n); }
  T& operator () (octave_idx_type i, octave_idx_type j)
  { return ArrayClass::xelem (i, j); }
  T& operator () (octave_idx_type i, octave_idx_type j, octave_idx_type k)
  { return ArrayClass::xelem (i, j, k); }
  T& operator () (const Array<octave_idx_type>& ra_idx)
  { return ArrayClass::xelem (ra_idx); }
};

template <class T>
std::ostream&
operator << (std::ostream& os, const Array<T>& a);

#endif
