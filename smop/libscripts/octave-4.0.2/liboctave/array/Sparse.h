// Template sparse classes
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

#if !defined (octave_Sparse_h)
#define octave_Sparse_h 1

#include <cassert>
#include <cstddef>

#include <iosfwd>
#include <algorithm>

#include "Array.h"
#include "dim-vector.h"
#include "lo-error.h"
#include "lo-utils.h"

#include "oct-sort.h"

class idx_vector;
class PermMatrix;

// Two dimensional sparse class.  Handles the reference counting for
// all the derived classes.

template <class T>
class
Sparse
{
public:

  typedef T element_type;

protected:
  //--------------------------------------------------------------------
  // The real representation of all Sparse arrays.
  //--------------------------------------------------------------------

  class OCTAVE_API SparseRep
  {
  public:

    T *d;
    octave_idx_type *r;
    octave_idx_type *c;
    octave_idx_type nzmx;
    octave_idx_type nrows;
    octave_idx_type ncols;
    octave_refcount<int> count;

    SparseRep (void)
      : d (0), r (0), c (new octave_idx_type [1]), nzmx (0), nrows (0),
        ncols (0), count (1)
    {
      c[0] = 0;
    }

    SparseRep (octave_idx_type n)
      : d (0), r (0), c (new octave_idx_type [n+1]), nzmx (0), nrows (n),
        ncols (n), count (1)
    {
      for (octave_idx_type i = 0; i < n + 1; i++)
        c[i] = 0;
    }

    SparseRep (octave_idx_type nr, octave_idx_type nc, octave_idx_type nz = 0)
      : d (nz > 0 ? new T [nz] : 0),
        r (nz > 0 ? new octave_idx_type [nz] : 0),
        c (new octave_idx_type [nc+1]), nzmx (nz), nrows (nr),
        ncols (nc), count (1)
    {
      for (octave_idx_type i = 0; i < nc + 1; i++)
        c[i] = 0;
    }

    SparseRep (const SparseRep& a)
      : d (new T [a.nzmx]), r (new octave_idx_type [a.nzmx]),
        c (new octave_idx_type [a.ncols + 1]),
        nzmx (a.nzmx), nrows (a.nrows), ncols (a.ncols), count (1)
    {
      octave_idx_type nz = a.nnz ();
      std::copy (a.d, a.d + nz, d);
      std::copy (a.r, a.r + nz, r);
      std::copy (a.c, a.c + ncols + 1, c);
    }

    ~SparseRep (void) { delete [] d; delete [] r; delete [] c; }

    octave_idx_type length (void) const { return nzmx; }

    octave_idx_type nnz (void) const { return c[ncols]; }

    T& elem (octave_idx_type _r, octave_idx_type _c);

    T celem (octave_idx_type _r, octave_idx_type _c) const;

    T& data (octave_idx_type i) { return d[i]; }

    T cdata (octave_idx_type i) const { return d[i]; }

    octave_idx_type& ridx (octave_idx_type i) { return r[i]; }

    octave_idx_type cridx (octave_idx_type i) const { return r[i]; }

    octave_idx_type& cidx (octave_idx_type i) { return c[i]; }

    octave_idx_type ccidx (octave_idx_type i) const { return c[i]; }

    void maybe_compress (bool remove_zeros);

    void change_length (octave_idx_type nz);

    bool indices_ok (void) const;

  private:

    // No assignment!

    SparseRep& operator = (const SparseRep& a);
  };

  //--------------------------------------------------------------------

  void make_unique (void)
  {
    if (rep->count > 1)
      {
        SparseRep *r = new SparseRep (*rep);

        if (--rep->count == 0)
          delete rep;

        rep = r;
      }
  }

public:

  // !!! WARNING !!! -- these should be protected, not public.  You
  // should not access these data members directly!

  typename Sparse<T>::SparseRep *rep;

  dim_vector dimensions;

private:

  static typename Sparse<T>::SparseRep *nil_rep (void);

public:

  Sparse (void)
    : rep (nil_rep ()), dimensions (dim_vector(0,0))
  {
    rep->count++;
  }

  explicit Sparse (octave_idx_type n)
    : rep (new typename Sparse<T>::SparseRep (n)),
      dimensions (dim_vector (n, n)) { }

  explicit Sparse (octave_idx_type nr, octave_idx_type nc)
    : rep (new typename Sparse<T>::SparseRep (nr, nc)),
      dimensions (dim_vector (nr, nc)) { }

  explicit Sparse (octave_idx_type nr, octave_idx_type nc, T val);

  Sparse (const dim_vector& dv, octave_idx_type nz)
    : rep (new typename Sparse<T>::SparseRep (dv(0), dv(1), nz)),
      dimensions (dv) { }

  Sparse (octave_idx_type nr, octave_idx_type nc, octave_idx_type nz)
    : rep (new typename Sparse<T>::SparseRep (nr, nc, nz)),
      dimensions (dim_vector (nr, nc)) { }

  // Both SparseMatrix and SparseBoolMatrix need this ctor, and this
  // is their only common ancestor.
  explicit Sparse (const PermMatrix& a);

  // Type conversion case. Preserves capacity ().
  template <class U>
  Sparse (const Sparse<U>& a)
    : rep (new typename Sparse<T>::SparseRep (a.rep->nrows, a.rep->ncols,
           a.rep->nzmx)),
    dimensions (a.dimensions)
  {
    octave_idx_type nz = a.nnz ();
    std::copy (a.rep->d, a.rep->d + nz, rep->d);
    std::copy (a.rep->r, a.rep->r + nz, rep->r);
    std::copy (a.rep->c, a.rep->c + rep->ncols + 1, rep->c);
  }

  // No type conversion case.
  Sparse (const Sparse<T>& a)
    : rep (a.rep), dimensions (a.dimensions)
  {
    rep->count++;
  }

public:

  Sparse (const dim_vector& dv);

  Sparse (const Sparse<T>& a, const dim_vector& dv);

  Sparse (const Array<T>& a, const idx_vector& r, const idx_vector& c,
          octave_idx_type nr = -1, octave_idx_type nc = -1,
          bool sum_terms = true, octave_idx_type nzm = -1);

  // Sparsify a normal matrix
  Sparse (const Array<T>& a);

  virtual ~Sparse (void);

  Sparse<T>& operator = (const Sparse<T>& a);

  // Note that nzmax and capacity are the amount of storage for
  // nonzero elements, while nnz is the actual number of nonzero
  // terms.
  octave_idx_type nzmax (void) const { return rep->length (); }
  octave_idx_type capacity (void) const { return nzmax (); }
  octave_idx_type nnz (void) const { return rep->nnz (); }

  // Querying the number of elements (incl. zeros) may overflow the index type,
  // so don't do it unless you really need it.
  octave_idx_type numel (void) const
  {
    return dimensions.safe_numel ();
  }

  octave_idx_type nelem (void) const { return capacity (); }
  octave_idx_type length (void) const { return numel (); }

  octave_idx_type dim1 (void) const { return dimensions(0); }
  octave_idx_type dim2 (void) const { return dimensions(1); }

  octave_idx_type rows (void) const { return dim1 (); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }

  octave_idx_type get_row_index (octave_idx_type k) { return ridx (k); }
  octave_idx_type get_col_index (octave_idx_type k)
  {
    octave_idx_type ret = 0;
    while (cidx (ret+1) < k)
      ret++;
    return ret;
  }

  size_t byte_size (void) const
  {
    return (static_cast<size_t>(cols () + 1) * sizeof (octave_idx_type)
            + static_cast<size_t> (capacity ())
            * (sizeof (T) + sizeof (octave_idx_type)));
  }

  dim_vector dims (void) const { return dimensions; }

  Sparse<T> squeeze (void) const { return *this; }

  octave_idx_type compute_index (const Array<octave_idx_type>& ra_idx) const;

  T range_error (const char *fcn, octave_idx_type n) const;
  T& range_error (const char *fcn, octave_idx_type n);

  T range_error (const char *fcn, octave_idx_type i, octave_idx_type j) const;
  T& range_error (const char *fcn, octave_idx_type i, octave_idx_type j);

  T range_error (const char *fcn, const Array<octave_idx_type>& ra_idx) const;
  T& range_error (const char *fcn, const Array<octave_idx_type>& ra_idx);

  // No checking, even for multiple references, ever.

  T& xelem (octave_idx_type n)
  {
    octave_idx_type i = n % rows ();
    octave_idx_type j = n / rows ();
    return xelem (i, j);
  }

  T xelem (octave_idx_type n) const
  {
    octave_idx_type i = n % rows ();
    octave_idx_type j = n / rows ();
    return xelem (i, j);
  }

  T& xelem (octave_idx_type i, octave_idx_type j) { return rep->elem (i, j); }
  T xelem (octave_idx_type i, octave_idx_type j) const
  {
    return rep->celem (i, j);
  }

  T& xelem (const Array<octave_idx_type>& ra_idx)
  { return xelem (compute_index (ra_idx)); }

  T xelem (const Array<octave_idx_type>& ra_idx) const
  { return xelem (compute_index (ra_idx)); }

  // FIXME: would be nice to fix this so that we don't
  // unnecessarily force a copy, but that is not so easy, and I see no
  // clean way to do it.

  T& checkelem (octave_idx_type n)
  {
    if (n < 0 || n >= numel ())
      return range_error ("T& Sparse<T>::checkelem", n);
    else
      {
        make_unique ();
        return xelem (n);
      }
  }

  T& checkelem (octave_idx_type i, octave_idx_type j)
  {
    if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
      return range_error ("T& Sparse<T>::checkelem", i, j);
    else
      {
        make_unique ();
        return xelem (i, j);
      }
  }

  T& checkelem (const Array<octave_idx_type>& ra_idx)
  {
    octave_idx_type i = compute_index (ra_idx);

    if (i < 0)
      return range_error ("T& Sparse<T>::checkelem", ra_idx);
    else
      return elem (i);
  }

  T& elem (octave_idx_type n)
  {
    make_unique ();
    return xelem (n);
  }

  T& elem (octave_idx_type i, octave_idx_type j)
  {
    make_unique ();
    return xelem (i, j);
  }

  T& elem (const Array<octave_idx_type>& ra_idx)
  { return Sparse<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T& operator () (octave_idx_type n)
  {
    return checkelem (n);
  }

  T& operator () (octave_idx_type i, octave_idx_type j)
  {
    return checkelem (i, j);
  }

  T& operator () (const Array<octave_idx_type>& ra_idx)
  {
    return checkelem (ra_idx);
  }

#else
  T& operator () (octave_idx_type n)
  {
    return elem (n);
  }

  T& operator () (octave_idx_type i, octave_idx_type j)
  {
    return elem (i, j);
  }

  T& operator () (const Array<octave_idx_type>& ra_idx)
  {
    return elem (ra_idx);
  }

#endif

  T checkelem (octave_idx_type n) const
  {
    if (n < 0 || n >= numel ())
      return range_error ("T Sparse<T>::checkelem", n);
    else
      return xelem (n);
  }

  T checkelem (octave_idx_type i, octave_idx_type j) const
  {
    if (i < 0 || j < 0 || i >= dim1 () || j >= dim2 ())
      return range_error ("T Sparse<T>::checkelem", i, j);
    else
      return xelem (i, j);
  }

  T checkelem (const Array<octave_idx_type>& ra_idx) const
  {
    octave_idx_type i = compute_index (ra_idx);

    if (i < 0)
      return range_error ("T Sparse<T>::checkelem", ra_idx);
    else
      return Sparse<T>::elem (i);
  }

  T elem (octave_idx_type n) const { return xelem (n); }

  T elem (octave_idx_type i, octave_idx_type j) const { return xelem (i, j); }

  T elem (const Array<octave_idx_type>& ra_idx) const
  { return Sparse<T>::elem (compute_index (ra_idx)); }

#if defined (BOUNDS_CHECKING)
  T operator () (octave_idx_type n) const { return checkelem (n); }
  T operator () (octave_idx_type i, octave_idx_type j) const
  {
    return checkelem (i, j);
  }

  T operator () (const Array<octave_idx_type>& ra_idx) const
  {
    return checkelem (ra_idx);
  }

#else
  T operator () (octave_idx_type n) const { return elem (n); }
  T operator () (octave_idx_type i, octave_idx_type j) const
  {
    return elem (i, j);
  }

  T operator () (const Array<octave_idx_type>& ra_idx) const
  {
    return elem (ra_idx);
  }
#endif

  Sparse<T> maybe_compress (bool remove_zeros = false)
  {
    if (remove_zeros)
      make_unique (); // Needs to unshare because elements are removed.

    rep->maybe_compress (remove_zeros);
    return (*this);
  }

  Sparse<T> reshape (const dim_vector& new_dims) const;

  Sparse<T> permute (const Array<octave_idx_type>& vec, bool inv = false) const;

  Sparse<T> ipermute (const Array<octave_idx_type>& vec) const
  {
    return permute (vec, true);
  }

  void resize1 (octave_idx_type n);

  void resize (octave_idx_type r, octave_idx_type c);

  void resize (const dim_vector& dv);

  void change_capacity (octave_idx_type nz)
  {
    if (nz < nnz ())
      make_unique (); // Unshare now because elements will be truncated.
    rep->change_length (nz);
  }

  Sparse<T>& insert (const Sparse<T>& a, octave_idx_type r, octave_idx_type c);
  Sparse<T>& insert (const Sparse<T>& a, const Array<octave_idx_type>& idx);

  bool is_square (void) const { return (dim1 () == dim2 ()); }

  bool is_empty (void) const { return (rows () < 1 && cols () < 1); }

  Sparse<T> transpose (void) const;

  T* data (void) { make_unique (); return rep->d; }
  T& data (octave_idx_type i) { make_unique (); return rep->data (i); }
  T* xdata (void) { return rep->d; }
  T& xdata (octave_idx_type i) { return rep->data (i); }

  T data (octave_idx_type i) const { return rep->data (i); }
  // FIXME: shouldn't this be returning const T*?
  T* data (void) const { return rep->d; }

  octave_idx_type* ridx (void) { make_unique (); return rep->r; }
  octave_idx_type& ridx (octave_idx_type i)
  {
    make_unique (); return rep->ridx (i);
  }

  octave_idx_type* xridx (void) { return rep->r; }
  octave_idx_type& xridx (octave_idx_type i) { return rep->ridx (i); }

  octave_idx_type ridx (octave_idx_type i) const { return rep->cridx (i); }
  // FIXME: shouldn't this be returning const octave_idx_type*?
  octave_idx_type* ridx (void) const { return rep->r; }

  octave_idx_type* cidx (void) { make_unique (); return rep->c; }
  octave_idx_type& cidx (octave_idx_type i)
  {
    make_unique (); return rep->cidx (i);
  }

  octave_idx_type* xcidx (void) { return rep->c; }
  octave_idx_type& xcidx (octave_idx_type i) { return rep->cidx (i); }

  octave_idx_type cidx (octave_idx_type i) const { return rep->ccidx (i); }
  // FIXME: shouldn't this be returning const octave_idx_type*?
  octave_idx_type* cidx (void) const { return rep->c; }

  octave_idx_type ndims (void) const { return dimensions.length (); }

  void delete_elements (const idx_vector& i);

  void delete_elements (int dim, const idx_vector& i);

  void delete_elements (const idx_vector& i, const idx_vector& j);

  Sparse<T> index (const idx_vector& i, bool resize_ok = false) const;

  Sparse<T> index (const idx_vector& i, const idx_vector& j,
                   bool resize_ok = false) const;

  void assign (const idx_vector& i, const Sparse<T>& rhs);

  void assign (const idx_vector& i, const idx_vector& j, const Sparse<T>& rhs);

  void print_info (std::ostream& os, const std::string& prefix) const;

  // Unsafe.  These functions exist to support the MEX interface.
  // You should not use them anywhere else.
  void *mex_get_data (void) const { return const_cast<T *> (data ()); }

  octave_idx_type *mex_get_ir (void) const
  {
    return const_cast<octave_idx_type *> (ridx ());
  }

  octave_idx_type *mex_get_jc (void) const
  {
    return const_cast<octave_idx_type *> (cidx ());
  }

  Sparse<T> sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;
  Sparse<T> sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
                  sortmode mode = ASCENDING) const;

  Sparse<T> diag (octave_idx_type k = 0) const;

  // dim = -1 and dim = -2 are special; see Array<T>::cat description.
  static Sparse<T>
  cat (int dim, octave_idx_type n, const Sparse<T> *sparse_list);

  Array<T> array_value (void) const;

  // Generic any/all test functionality with arbitrary predicate.
  template <class F, bool zero>
  bool test (F fcn) const
  {
    return any_all_test<F, T, zero> (fcn, data (), nnz ());
  }

  // Simpler calls.
  template <class F>
  bool test_any (F fcn) const
  { return test<F, false> (fcn); }

  template <class F>
  bool test_all (F fcn) const
  { return test<F, true> (fcn); }

  // Overloads for function references.
  bool test_any (bool (&fcn) (T)) const
  { return test<bool (&) (T), false> (fcn); }

  bool test_any (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), false> (fcn); }

  bool test_all (bool (&fcn) (T)) const
  { return test<bool (&) (T), true> (fcn); }

  bool test_all (bool (&fcn) (const T&)) const
  { return test<bool (&) (const T&), true> (fcn); }

  template <class U, class F>
  Sparse<U>
  map (F fcn) const
  {
    Sparse<U> result;
    U f_zero = fcn (0.);

    if (f_zero != 0.)
      {
        octave_idx_type nr = rows ();
        octave_idx_type nc = cols ();

        result = Sparse<U> (nr, nc, f_zero);

        for (octave_idx_type j = 0; j < nc; j++)
          for (octave_idx_type i = cidx (j); i < cidx (j+1); i++)
            {
              octave_quit ();
              /* Use data instead of elem for better performance. */
              result.data (ridx (i) + j * nr) = fcn (data (i));
            }

        result.maybe_compress (true);
      }
    else
      {
        octave_idx_type nz = nnz ();
        octave_idx_type nr = rows ();
        octave_idx_type nc = cols ();

        result = Sparse<U> (nr, nc, nz);
        octave_idx_type ii = 0;
        result.cidx (ii) = 0;

        for (octave_idx_type j = 0; j < nc; j++)
          {
            for (octave_idx_type i = cidx (j); i < cidx (j+1); i++)
              {
                U val = fcn (data (i));
                if (val != 0.0)
                  {
                    result.data (ii) = val;
                    result.ridx (ii++) = ridx (i);
                  }
                octave_quit ();
              }
            result.cidx (j+1) = ii;
          }

        result.maybe_compress (false);
      }

    return result;
  }

  // Overloads for function references.
  template <class U>
  Sparse<U>
  map (U (&fcn) (T)) const
  { return map<U, U (&) (T)> (fcn); }

  template <class U>
  Sparse<U>
  map (U (&fcn) (const T&)) const
  { return map<U, U (&) (const T&)> (fcn); }

  bool indices_ok (void) const { return rep->indices_ok (); }
};

template<typename T>
std::istream&
read_sparse_matrix (std::istream& is, Sparse<T>& a,
                    T (*read_fcn) (std::istream&))
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nzmax ();

  if (nr > 0 && nc > 0)
    {
      octave_idx_type itmp;
      octave_idx_type jtmp;
      octave_idx_type iold = 0;
      octave_idx_type jold = 0;
      octave_idx_type ii = 0;
      T tmp;

      a.cidx (0) = 0;
      for (octave_idx_type i = 0; i < nz; i++)
        {
          itmp = 0; jtmp = 0;
          is >> itmp;
          itmp--;

          is >> jtmp;
          jtmp--;

          if (itmp < 0 || itmp >= nr)
            {
              (*current_liboctave_error_handler)
                ("invalid sparse matrix: row index = %d out of range",
                 itmp + 1);
              is.setstate (std::ios::failbit);
              goto done;
            }

          if (jtmp < 0 || jtmp >= nc)
            {
              (*current_liboctave_error_handler)
                ("invalid sparse matrix: column index = %d out of range",
                 jtmp + 1);
              is.setstate (std::ios::failbit);
              goto done;
            }

          if (jtmp < jold)
            {
              (*current_liboctave_error_handler)
                ("invalid sparse matrix: column indices must appear in ascending order");
              is.setstate (std::ios::failbit);
              goto done;
            }
          else if (jtmp > jold)
            {
              for (octave_idx_type j = jold; j < jtmp; j++)
                a.cidx (j+1) = ii;
            }
          else if (itmp < iold)
            {
              (*current_liboctave_error_handler)
                ("invalid sparse matrix: row indices must appear in ascending order in each column");
              is.setstate (std::ios::failbit);
              goto done;
            }

          iold = itmp;
          jold = jtmp;

          tmp = read_fcn (is);

          if (is)
            {
              a.data (ii) = tmp;
              a.ridx (ii++) = itmp;
            }
          else
            goto done;
        }

      for (octave_idx_type j = jold; j < nc; j++)
        a.cidx (j+1) = ii;
    }

done:

  return is;
}

#endif
