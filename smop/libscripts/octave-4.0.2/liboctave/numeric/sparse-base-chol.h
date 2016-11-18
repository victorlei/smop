/*

Copyright (C) 2005-2015 David Bateman
Copyright (C) 1998-2005 Andy Adler

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

#if !defined (octave_sparse_base_chol_h)
#define octave_sparse_base_chol_h 1

#include "oct-sparse.h"
#include "dColVector.h"

template <class chol_type, class chol_elt, class p_type>
class
sparse_base_chol
{
protected:
#ifdef HAVE_CHOLMOD
  class sparse_base_chol_rep
  {
  public:
    sparse_base_chol_rep (void)
      : count (1), Lsparse (0), Common (), is_pd (false), minor_p (0),
        perms (), cond (0)
    { }

    sparse_base_chol_rep (const chol_type& a, bool natural, bool force)
      : count (1), Lsparse (0), Common (), is_pd (false), minor_p (0),
        perms (), cond (0)
    {
      init (a, natural, force);
    }

    sparse_base_chol_rep (const chol_type& a, octave_idx_type& info,
                          bool natural, bool force)
      : count (1), Lsparse (0), Common (), is_pd (false), minor_p (0),
        perms (), cond (0)
    {
      info = init (a, natural, force);
    }

    ~sparse_base_chol_rep (void)
    {
      if (is_pd)
        CHOLMOD_NAME (free_sparse) (&Lsparse, &Common);
    }

    cholmod_sparse * L (void) const { return Lsparse; }

    octave_idx_type P (void) const
    {
      return (minor_p == static_cast<octave_idx_type>(Lsparse->ncol) ?
              0 : minor_p + 1);
    }

    ColumnVector perm (void) const { return perms + 1; }

    p_type Q (void) const;

    bool is_positive_definite (void) const { return is_pd; }

    double rcond (void) const { return cond; }

    octave_refcount<int> count;

  private:
    cholmod_sparse *Lsparse;

    cholmod_common Common;

    bool is_pd;

    octave_idx_type minor_p;

    ColumnVector perms;

    double cond;

    octave_idx_type init (const chol_type& a, bool natural, bool force);

    void drop_zeros (const cholmod_sparse* S);

    // No copying!

    sparse_base_chol_rep (const sparse_base_chol_rep&);

    sparse_base_chol_rep& operator = (const sparse_base_chol_rep&);
  };
#else
  class sparse_base_chol_rep
  {
  public:
    sparse_base_chol_rep (void)
      : count (1), is_pd (false), minor_p (0), perms (), cond (0) { }

    sparse_base_chol_rep (const chol_type& a, bool natural, bool force)
      : count (1), is_pd (false), minor_p (0), perms (), cond (0)
    {
      init (a, natural, force);
    }

    sparse_base_chol_rep (const chol_type& a, octave_idx_type& info,
                          bool natural, bool force)
      : count (1), is_pd (false), minor_p (0), perms (), cond (0)
    {
      info = init (a, natural, force);
    }

    ~sparse_base_chol_rep (void) { }

    octave_idx_type P (void) const { return 0; }

    ColumnVector perm (void) const { return perms + 1; }

    p_type Q (void) const;

    bool is_positive_definite (void) const { return is_pd; }

    double rcond (void) const { return cond; }

    octave_refcount<int> count;

  private:
    bool is_pd;

    octave_idx_type minor_p;

    ColumnVector perms;

    double cond;

    octave_idx_type init (const chol_type& a, bool natural, bool force);

    // No copying!

    sparse_base_chol_rep (const sparse_base_chol_rep&);

    sparse_base_chol_rep& operator = (const sparse_base_chol_rep&);
  };
#endif

private:
  sparse_base_chol_rep *rep;

public:

  sparse_base_chol (void)
    : rep (new typename
           sparse_base_chol<chol_type, chol_elt, p_type>
           ::sparse_base_chol_rep ())
  { }

  sparse_base_chol (const chol_type& a, bool natural, bool force)
    : rep (new typename
           sparse_base_chol<chol_type, chol_elt, p_type>
           ::sparse_base_chol_rep (a, natural, force))
  { }

  sparse_base_chol (const chol_type& a, octave_idx_type& info,
                    bool natural, bool force)
    : rep (new typename
           sparse_base_chol<chol_type, chol_elt, p_type>
           ::sparse_base_chol_rep (a, info, natural, force))
  { }

  sparse_base_chol (const sparse_base_chol<chol_type, chol_elt, p_type>& a)
    : rep (a.rep)
  { rep->count++; }

  virtual ~sparse_base_chol (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  sparse_base_chol& operator = (const sparse_base_chol& a)
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

  chol_type L (void) const;

  chol_type R (void) const { return L ().hermitian (); }

  octave_idx_type P (void) const { return rep->P (); }

  ColumnVector perm (void) const { return rep->perm (); }

  p_type Q (void) const { return rep->Q (); }

  bool is_positive_definite (void) const
  { return rep->is_positive_definite (); }

  double rcond (void) const { return rep->rcond (); }

  chol_type inverse (void) const;
};

#endif
