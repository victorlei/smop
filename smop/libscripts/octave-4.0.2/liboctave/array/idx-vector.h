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

#if !defined (octave_idx_vector_h)
#define octave_idx_vector_h 1

#include <cassert>
#include <cstring>

#include <algorithm>
#include <iosfwd>
#include <memory>

#include "dim-vector.h"
#include "oct-inttypes.h"
#include "oct-refcount.h"

template<class T> class Array;
template<class T> class Sparse;
class Range;

// Design rationale:
// idx_vector is a reference-counting, polymorphic pointer, that can contain
// 4 types of index objects: a magic colon, a range, a scalar, or an index vector.
// Polymorphic methods for single element access are provided, as well as
// templates implementing "early dispatch", i.e. hoisting the checks for index
// type out of loops.

class
OCTAVE_API
idx_vector
{
public:

  enum idx_class_type
  {
    class_invalid = -1,
    class_colon = 0,
    class_range,
    class_scalar,
    class_vector,
    class_mask
  };

  template<class T> friend class std::auto_ptr;

private:

  class OCTAVE_API idx_base_rep
  {
  public:
    idx_base_rep (void) : count (1), err (false) { }

    virtual ~idx_base_rep (void) { }

    // Non-range-checking element query.
    virtual octave_idx_type xelem (octave_idx_type i) const = 0;

    // Range-checking element query.
    virtual octave_idx_type checkelem (octave_idx_type i) const = 0;

    // Length of the index vector.
    virtual octave_idx_type length (octave_idx_type n) const = 0;

    // The maximum index + 1. The actual dimension is passed in.
    virtual octave_idx_type extent (octave_idx_type n) const = 0;

    // Index class.
    virtual idx_class_type idx_class (void) const { return class_invalid; }

    // Sorts, maybe uniqifies, and returns a clone object pointer.
    virtual idx_base_rep *sort_uniq_clone (bool uniq = false) = 0;
    // Sorts, and returns a sorting permutation (aka Array::sort).
    virtual idx_base_rep *sort_idx (Array<octave_idx_type>&) = 0;

    // Checks whether the index is colon or a range equivalent to colon.
    virtual bool is_colon_equiv (octave_idx_type) const { return false; }

    // The original dimensions of object (used when subscribing by matrices).
    virtual dim_vector orig_dimensions (void) const { return dim_vector (); }

    // i/o
    virtual std::ostream& print (std::ostream& os) const = 0;

    virtual Array<octave_idx_type> as_array (void);

    octave_refcount<int> count;

    bool err;

  private:

    // No copying!
    idx_base_rep (const idx_base_rep&);
    idx_base_rep& operator = (const idx_base_rep&);
  };

  // The magic colon index.
  class OCTAVE_API idx_colon_rep : public idx_base_rep
  {
  public:
    idx_colon_rep (void) { }

    idx_colon_rep (char c);

    octave_idx_type xelem (octave_idx_type i) const { return i; }

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type n) const { return n; }

    octave_idx_type extent (octave_idx_type n) const { return n; }

    idx_class_type idx_class (void) const { return class_colon; }

    idx_base_rep *sort_uniq_clone (bool = false)
    { count++; return this; }

    idx_base_rep *sort_idx (Array<octave_idx_type>&);

    bool is_colon_equiv (octave_idx_type) const { return true; }

    std::ostream& print (std::ostream& os) const;

  private:


    // No copying!
    idx_colon_rep (const idx_colon_rep& idx);
    idx_colon_rep& operator = (const idx_colon_rep& idx);
  };

  // To distinguish the "direct" constructors that blindly trust the data.
  enum direct { DIRECT };

  // The integer range index.
  class OCTAVE_API idx_range_rep : public idx_base_rep
  {
  public:
    idx_range_rep (octave_idx_type _start, octave_idx_type _len,
                   octave_idx_type _step, direct)
      : idx_base_rep (), start(_start), len(_len), step(_step) { }

    idx_range_rep (void)
      : start(0), len(0), step(1) { }

    // Zero-based constructor.
    idx_range_rep (octave_idx_type _start, octave_idx_type _limit,
                   octave_idx_type _step);

    idx_range_rep (const Range&);

    octave_idx_type xelem (octave_idx_type i) const
    { return start + i * step; }

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return len; }

    octave_idx_type extent (octave_idx_type n) const
    { return len ? std::max (n, (start + 1 + (step < 0 ? 0 : step * (len - 1))))
                 : n; }

    idx_class_type idx_class (void) const { return class_range; }

    idx_base_rep *sort_uniq_clone (bool uniq = false);

    idx_base_rep *sort_idx (Array<octave_idx_type>&);

    bool is_colon_equiv (octave_idx_type n) const
    { return start == 0 && step == 1 && len == n; }

    dim_vector orig_dimensions (void) const
    { return dim_vector (1, len); }

    octave_idx_type get_start (void) const { return start; }

    octave_idx_type get_step (void) const { return step; }

    std::ostream& print (std::ostream& os) const;

    Range unconvert (void) const;

    Array<octave_idx_type> as_array (void);

  private:


    // No copying!
    idx_range_rep (const idx_range_rep& idx);
    idx_range_rep& operator = (const idx_range_rep& idx);

    octave_idx_type start, len, step;

  };

  // The integer scalar index.
  class OCTAVE_API idx_scalar_rep : public idx_base_rep
  {
  public:
    idx_scalar_rep (octave_idx_type i, direct)
      : data (i) { }

    idx_scalar_rep (void)
      : data (0) { }

    // Zero-based constructor.
    idx_scalar_rep (octave_idx_type i);

    template <class T>
    idx_scalar_rep (T x);

    octave_idx_type xelem (octave_idx_type) const { return data; }

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return 1; }

    octave_idx_type extent (octave_idx_type n) const
    { return std::max (n, data + 1); }

    idx_class_type idx_class (void) const { return class_scalar; }

    idx_base_rep *sort_uniq_clone (bool = false)
    { count++; return this; }

    idx_base_rep *sort_idx (Array<octave_idx_type>&);

    bool is_colon_equiv (octave_idx_type n) const
    { return n == 1 && data == 0; }

    dim_vector orig_dimensions (void) const { return dim_vector (1, 1); }

    octave_idx_type get_data (void) const { return data; }

    std::ostream& print (std::ostream& os) const;

    double unconvert (void) const;

    Array<octave_idx_type> as_array (void);

  private:


    // No copying!
    idx_scalar_rep (const idx_scalar_rep& idx);
    idx_scalar_rep& operator = (const idx_scalar_rep& idx);

    octave_idx_type data;

  };

  // The integer vector index.
  class OCTAVE_API idx_vector_rep : public idx_base_rep
  {
  public:
    // Direct constructor.
    idx_vector_rep (octave_idx_type *_data, octave_idx_type _len,
                    octave_idx_type _ext, const dim_vector& od, direct)
      : data (_data), len (_len), ext (_ext), aowner (0), orig_dims (od) { }

    idx_vector_rep (void)
      : data (0), len (0), ext (0), aowner (0), orig_dims ()
    { }

    // Zero-based constructor.
    idx_vector_rep (const Array<octave_idx_type>& inda);

    idx_vector_rep (const Array<octave_idx_type>& inda,
                    octave_idx_type _ext, direct);

    template <class T>
    idx_vector_rep (const Array<T>&);

    idx_vector_rep (bool);

    idx_vector_rep (const Array<bool>&, octave_idx_type = -1);

    idx_vector_rep (const Sparse<bool>&);

    ~idx_vector_rep (void);

    octave_idx_type xelem (octave_idx_type i) const { return data[i]; }

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return len; }

    octave_idx_type extent (octave_idx_type n) const
    { return std::max (n, ext); }

    idx_class_type idx_class (void) const { return class_vector; }

    idx_base_rep *sort_uniq_clone (bool uniq = false);

    idx_base_rep *sort_idx (Array<octave_idx_type>&);

    dim_vector orig_dimensions (void) const { return orig_dims; }

    const octave_idx_type *get_data (void) const { return data; }

    std::ostream& print (std::ostream& os) const;

    Array<double> unconvert (void) const;

    Array<octave_idx_type> as_array (void);

  private:


    // No copying!
    idx_vector_rep (const idx_vector_rep& idx);
    idx_vector_rep& operator = (const idx_vector_rep& idx);

    const octave_idx_type *data;
    octave_idx_type len;
    octave_idx_type ext;

    // This is a trick to allow user-given zero-based arrays to be used
    // as indices without copying.  If the following pointer is nonzero,
    // we do not own the data, but rather have an Array<octave_idx_type>
    // object that provides us the data.  Note that we need a pointer
    // because we deferred the Array<T> declaration and we do not want
    // it yet to be defined.

    Array<octave_idx_type> *aowner;

    dim_vector orig_dims;
  };

  // The logical mask index.
  class OCTAVE_API idx_mask_rep : public idx_base_rep
  {
  public:
    // Direct constructor.
    idx_mask_rep (bool *_data, octave_idx_type _len,
                  octave_idx_type _ext, const dim_vector& od, direct)
      : data (_data), len (_len), ext (_ext), lsti (-1), lste (-1),
        aowner (0), orig_dims (od) { }

    idx_mask_rep (void)
      : data (0), len (0), ext (0), lsti (-1), lste (-1), aowner (0),
        orig_dims ()
    { }

    idx_mask_rep (bool);

    idx_mask_rep (const Array<bool>&, octave_idx_type = -1);

    ~idx_mask_rep (void);

    octave_idx_type xelem (octave_idx_type i) const;

    octave_idx_type checkelem (octave_idx_type i) const;

    octave_idx_type length (octave_idx_type) const { return len; }

    octave_idx_type extent (octave_idx_type n) const
    { return std::max (n, ext); }

    idx_class_type idx_class (void) const { return class_mask; }

    idx_base_rep *sort_uniq_clone (bool = false)
    { count++; return this; }

    idx_base_rep *sort_idx (Array<octave_idx_type>&);

    dim_vector orig_dimensions (void) const { return orig_dims; }

    bool is_colon_equiv (octave_idx_type n) const
    { return len == n && ext == n; }

    const bool *get_data (void) const { return data; }

    std::ostream& print (std::ostream& os) const;

    Array<bool> unconvert (void) const;

    Array<octave_idx_type> as_array (void);

  private:


    // No copying!
    idx_mask_rep (const idx_mask_rep& idx);
    idx_mask_rep& operator = (const idx_mask_rep& idx);

    const bool *data;
    octave_idx_type len;
    octave_idx_type ext;

    // FIXME: I'm not sure if this is a good design. Maybe it would be
    // better to employ some sort of generalized iteration scheme.
    mutable octave_idx_type lsti;
    mutable octave_idx_type lste;

    // This is a trick to allow user-given mask arrays to be used as
    // indices without copying.  If the following pointer is nonzero, we
    // do not own the data, but rather have an Array<bool> object that
    // provides us the data.  Note that we need a pointer because we
    // deferred the Array<T> declaration and we do not want it yet to be
    // defined.

    Array<bool> *aowner;

    dim_vector orig_dims;
  };

  idx_vector (idx_base_rep *r) : rep (r) { }

  // The shared empty vector representation (for fast default
  // constructor).
  static idx_vector_rep *nil_rep (void);

  // The shared empty vector representation with the error flag set.
  static idx_vector_rep *err_rep (void);

  // If there was an error in constructing the rep, replace it with
  // empty vector for safety.
  void chkerr (void)
  {
    if (rep->err)
      {
        if (--rep->count == 0)
          delete rep;
        rep = err_rep ();
        rep->count++;
      }
  }

public:

  // Fast empty constructor.
  idx_vector (void) : rep (nil_rep ()) { rep->count++; }

  // Zero-based constructors (for use from C++).
  idx_vector (octave_idx_type i) : rep (new idx_scalar_rep (i))
  { chkerr (); }

  idx_vector (octave_idx_type start, octave_idx_type limit,
              octave_idx_type step = 1)
    : rep (new idx_range_rep (start, limit, step))
  { chkerr (); }

  static idx_vector
  make_range (octave_idx_type start, octave_idx_type step,
              octave_idx_type len)
  {
    return idx_vector (new idx_range_rep (start, len, step, DIRECT));
  }

  idx_vector (const Array<octave_idx_type>& inda)
    : rep (new idx_vector_rep (inda))
  { chkerr (); }

  // Directly pass extent, no checking.
  idx_vector (const Array<octave_idx_type>& inda, octave_idx_type ext)
    : rep (new idx_vector_rep (inda, ext, DIRECT))
  { }

  // Colon is best constructed by simply copying (or referencing) this member.
  static const idx_vector colon;

  // or passing ':' here
  idx_vector (char c) : rep (new idx_colon_rep (c)) { chkerr (); }

  // Conversion constructors (used by interpreter).

  template <class T>
  idx_vector (octave_int<T> x) : rep (new idx_scalar_rep (x)) { chkerr (); }

  idx_vector (double x) : rep (new idx_scalar_rep (x)) { chkerr (); }

  idx_vector (float x) : rep (new idx_scalar_rep (x)) { chkerr (); }

  // A scalar bool does not necessarily map to scalar index.
  idx_vector (bool x) : rep (new idx_mask_rep (x)) { chkerr (); }

  template <class T>
  idx_vector (const Array<octave_int<T> >& nda) : rep (new idx_vector_rep (nda))
  { chkerr (); }

  idx_vector (const Array<double>& nda) : rep (new idx_vector_rep (nda))
  { chkerr (); }

  idx_vector (const Array<float>& nda) : rep (new idx_vector_rep (nda))
  { chkerr (); }

  idx_vector (const Array<bool>& nda);

  idx_vector (const Range& r)
    : rep (new idx_range_rep (r))
  { chkerr (); }

  idx_vector (const Sparse<bool>& nda) : rep (new idx_vector_rep (nda))
  { chkerr (); }

  idx_vector (const idx_vector& a) : rep (a.rep) { rep->count++; }

  ~idx_vector (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  idx_vector& operator = (const idx_vector& a)
  {
    if (this != &a)
      {
        if (--rep->count == 0)
          delete rep;

        rep = a.rep;
        rep->count++;
      }
    return *this;
  }

  idx_class_type idx_class (void) const { return rep->idx_class (); }

  octave_idx_type length (octave_idx_type n = 0) const
  { return rep->length (n); }

  octave_idx_type extent (octave_idx_type n) const
  { return rep->extent (n); }

  octave_idx_type xelem (octave_idx_type n) const
  { return rep->xelem (n); }

  octave_idx_type checkelem (octave_idx_type n) const
  { return rep->checkelem (n); }

  octave_idx_type operator () (octave_idx_type n) const
  {
#if defined (BOUNDS_CHECKING)
    return rep->checkelem (n);
#else
    return rep->xelem (n);
#endif
  }

  operator bool (void) const
  { return ! rep->err; }

  bool is_colon (void) const
  { return rep->idx_class () == class_colon; }

  bool is_scalar (void) const
  { return rep->idx_class () == class_scalar; }

  bool is_range (void) const
  { return rep->idx_class () == class_range; }

  bool is_colon_equiv (octave_idx_type n) const
  { return rep->is_colon_equiv (n); }

  idx_vector sorted (bool uniq = false) const
  { return idx_vector (rep->sort_uniq_clone (uniq)); }

  idx_vector sorted (Array<octave_idx_type>& sidx) const
  { return idx_vector (rep->sort_idx (sidx)); }

  dim_vector orig_dimensions (void) const { return rep->orig_dimensions (); }

  octave_idx_type orig_rows (void) const
  { return orig_dimensions () (0); }

  octave_idx_type orig_columns (void) const
  { return orig_dimensions () (1); }

  int orig_empty (void) const
  { return (! is_colon () && orig_dimensions ().any_zero ()); }

  // i/o

  std::ostream& print (std::ostream& os) const { return rep->print (os); }

  friend std::ostream& operator << (std::ostream& os, const idx_vector& a)
  { return a.print (os); }

  // Slice with specializations. No checking of bounds!
  //
  // This is equivalent to the following loop (but much faster):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   dest[i] = src[idx(i)];
  // return i;
  //
  template <class T>
  octave_idx_type
  index (const T *src, octave_idx_type n, T *dest) const
  {
    octave_idx_type len = rep->length (n);

    switch (rep->idx_class ())
      {
      case class_colon:
        std::copy (src, src + len, dest);
        break;

      case class_range:
        {
          idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          const T *ssrc = src + start;
          if (step == 1)
            std::copy (ssrc, ssrc + len, dest);
          else if (step == -1)
            std::reverse_copy (ssrc - len + 1, ssrc + 1, dest);
          else if (step == 0)
            std::fill_n (dest, len, *ssrc);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                dest[i] = ssrc[j];
            }
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
          dest[0] = src[r->get_data ()];
        }
        break;

      case class_vector:
        {
          idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++)
            dest[i] = src[data[i]];
        }
        break;

      case class_mask:
        {
          idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) *dest++ = src[i];
        }
        break;

      default:
        assert (false);
        break;
      }

    return len;
  }

  // Slice assignment with specializations. No checking of bounds!
  //
  // This is equivalent to the following loop (but much faster):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   dest[idx(i)] = src[i];
  // return i;
  //
  template <class T>
  octave_idx_type
  assign (const T *src, octave_idx_type n, T *dest) const
  {
    octave_idx_type len = rep->length (n);

    switch (rep->idx_class ())
      {
      case class_colon:
        std::copy (src, src + len, dest);
        break;

      case class_range:
        {
          idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          T *sdest = dest + start;
          if (step == 1)
            std::copy (src, src + len, sdest);
          else if (step == -1)
            std::reverse_copy (src, src + len, sdest - len + 1);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                sdest[j] = src[i];
            }
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
          dest[r->get_data ()] = src[0];
        }
        break;

      case class_vector:
        {
          idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++)
            dest[data[i]] = src[i];
        }
        break;

      case class_mask:
        {
          idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) dest[i] = *src++;
        }
        break;

      default:
        assert (false);
        break;
      }

    return len;
  }

  // Slice fill with specializations. No checking of bounds!
  //
  // This is equivalent to the following loop (but much faster):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   dest[idx(i)] = val;
  // return i;
  //
  template <class T>
  octave_idx_type
  fill (const T& val, octave_idx_type n, T *dest) const
  {
    octave_idx_type len = rep->length (n);

    switch (rep->idx_class ())
      {
      case class_colon:
        std::fill (dest, dest + len, val);
        break;

      case class_range:
        {
          idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          T *sdest = dest + start;
          if (step == 1)
            std::fill (sdest, sdest + len, val);
          else if (step == -1)
            std::fill (sdest - len + 1, sdest + 1, val);
          else
            {
              for (octave_idx_type i = 0, j = 0; i < len; i++, j += step)
                sdest[j] = val;
            }
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
          dest[r->get_data ()] = val;
        }
        break;

      case class_vector:
        {
          idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++)
            dest[data[i]] = val;
        }
        break;

      case class_mask:
        {
          idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) dest[i] = val;
        }
        break;

      default:
        assert (false);
        break;
      }

    return len;
  }

  // Generic non-breakable indexed loop. The loop body should be
  // encapsulated in a single functor body.  This is equivalent to the
  // following loop (but faster, at least for simple inlined bodies):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++) body (idx(i));

  template <class Functor>
  void
  loop (octave_idx_type n, Functor body) const
  {
    octave_idx_type len = rep->length (n);

    switch (rep->idx_class ())
      {
      case class_colon:
        for (octave_idx_type i = 0; i < len; i++) body (i);
        break;

      case class_range:
        {
          idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          octave_idx_type i, j;
          if (step == 1)
            for (i = start, j = start + len; i < j; i++) body (i);
          else if (step == -1)
            for (i = start, j = start - len; i > j; i--) body (i);
          else
            for (i = 0, j = start; i < len; i++, j += step) body (j);
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
          body (r->get_data ());
        }
        break;

      case class_vector:
        {
          idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);
          const octave_idx_type *data = r->get_data ();
          for (octave_idx_type i = 0; i < len; i++) body (data[i]);
        }
        break;

      case class_mask:
        {
          idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          for (octave_idx_type i = 0; i < ext; i++)
            if (data[i]) body (i);
        }
        break;

      default:
        assert (false);
        break;
      }

  }

  // Generic breakable indexed loop. The loop body should be
  // encapsulated in a single functor body.  This is equivalent to the
  // following loop (but faster, at least for simple inlined bodies):
  //
  // for (octave_idx_type i = 0; i < idx->length (n); i++)
  //   if (body (idx(i))) break;
  // return i;
  //

  template <class Functor>
  octave_idx_type
  bloop (octave_idx_type n, Functor body) const
  {
    octave_idx_type len = rep->length (n), ret;

    switch (rep->idx_class ())
      {
      case class_colon:
        {
          octave_idx_type i;
          for (i = 0; i < len && body (i); i++) ;
          ret = i;
        }
        break;

      case class_range:
        {
          idx_range_rep * r = dynamic_cast<idx_range_rep *> (rep);
          octave_idx_type start = r->get_start ();
          octave_idx_type step = r->get_step ();
          octave_idx_type i, j;
          if (step == 1)
            for (i = start, j = start + len; i < j && body (i); i++) ;
          else if (step == -1)
            for (i = start, j = start - len; i > j && body (i); i--) ;
          else
            for (i = 0, j = start; i < len && body (j); i++, j += step) ;
          ret = i;
        }
        break;

      case class_scalar:
        {
          idx_scalar_rep * r = dynamic_cast<idx_scalar_rep *> (rep);
          ret = body (r->get_data ()) ? 1 : 0;
        }
        break;

      case class_vector:
        {
          idx_vector_rep * r = dynamic_cast<idx_vector_rep *> (rep);
          const octave_idx_type *data = r->get_data ();
          octave_idx_type i;
          for (i = 0; i < len && body (data[i]); i++) ;
          ret = i;
        }
        break;

      case class_mask:
        {
          idx_mask_rep * r = dynamic_cast<idx_mask_rep *> (rep);
          const bool *data = r->get_data ();
          octave_idx_type ext = r->extent (0);
          octave_idx_type j = 0;
          for (octave_idx_type i = 0; i < ext; i++)
            {
              if (data[i])
                {
                  if (body (i))
                    break;
                  else
                    j++;
                }
            }

          ret = j;
        }
        break;

      default:
        assert (false);
        break;
      }

    return ret;
  }

  // Rationale:
  // This method is the key to "smart indexing". When indexing cartesian
  // arrays, sometimes consecutive index vectors can be reduced into a
  // single index.  If rows (A) = k and i.maybe_reduce (j) gives k, then
  // A(i,j)(:) is equal to A(k)(:).

  // If the next index can be reduced, returns true and updates this.
  bool maybe_reduce (octave_idx_type n, const idx_vector& j,
                     octave_idx_type nj);

  bool is_cont_range (octave_idx_type n,
                      octave_idx_type& l, octave_idx_type& u) const;

  // Returns the increment for ranges and colon, 0 for scalars and empty
  // vectors, 1st difference otherwise.
  octave_idx_type increment (void) const;

  idx_vector
  complement (octave_idx_type n) const;

  bool is_permutation (octave_idx_type n) const;

  // Returns the inverse permutation. If this is not a permutation on 1:n, the
  // result is undefined (but no error unless extent () != n).
  idx_vector inverse_permutation (octave_idx_type n) const;

  // Copies all the indices to a given array. Not allowed for colons.
  void copy_data (octave_idx_type *data) const;

  // If the index is a mask, convert it to index vector.
  idx_vector unmask (void) const;

  // Unconverts the index to a scalar, Range, double array or a mask.
  void unconvert (idx_class_type& iclass,
                  double& scalar, Range& range,
                  Array<double>& array, Array<bool>& mask) const;

  Array<octave_idx_type> as_array (void) const;

  // Raw pointer to index array.  This is non-const because it may be
  // necessary to mutate the index.
  const octave_idx_type *raw (void);

  bool is_vector (void) const;

  // FIXME: these are here for compatibility.  They should be removed
  // when no longer in use.

  octave_idx_type elem (octave_idx_type n) const
  { return (*this) (n); }

  bool is_colon_equiv (octave_idx_type n, int) const
  { return is_colon_equiv (n); }

  octave_idx_type
  freeze (octave_idx_type z_len, const char *tag, bool resize_ok = false);

  void sort (bool uniq = false)
  { *this = sorted (uniq); }

  octave_idx_type ones_count (void) const;

  octave_idx_type max (void) const { return extent (1) - 1; }

private:

  idx_base_rep *rep;

};

#endif
