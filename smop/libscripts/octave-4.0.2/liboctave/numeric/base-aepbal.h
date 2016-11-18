/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#if !defined (octave_base_aepbal_h)
#define octave_base_aepbal_h 1

template <class MatrixT, class VectorT>
class base_aepbal
{
protected:
  MatrixT balanced_mat;
  VectorT scale;
  octave_idx_type ilo, ihi;
  char job;

  base_aepbal (void) : balanced_mat (), scale (), ilo (), ihi (), job () { }

public:

  base_aepbal (const base_aepbal& a)
    : balanced_mat (a.balanced_mat), scale (a.scale),
      ilo(a.ilo), ihi(a.ihi), job(a.job)
  {
  }

  base_aepbal& operator = (const base_aepbal& a)
  {
    balanced_mat = a.balanced_mat;
    scale = a.scale;
    ilo = a.ilo;
    ihi = a.ihi;
    job = a.job;
    return *this;
  }

  virtual ~base_aepbal (void) { }

  MatrixT balanced_matrix (void) const { return balanced_mat; }

  VectorT permuting_vector (void) const
  {
    octave_idx_type n = balanced_mat.rows ();
    VectorT pv (n);
    for (octave_idx_type i = 0; i < n; i++)
      pv(i) = i+1;
    for (octave_idx_type i = n-1; i >= ihi; i--)
      {
        octave_idx_type j = scale(i) - 1;
        std::swap (pv(i), pv(j));
      }
    for (octave_idx_type i = 0; i < ilo-1; i++)
      {
        octave_idx_type j = scale(i) - 1;
        std::swap (pv(i), pv(j));
      }

    return pv;
  }

  VectorT scaling_vector (void) const
  {
    octave_idx_type n = balanced_mat.rows ();
    VectorT scv (n);
    for (octave_idx_type i = 0; i < ilo-1; i++)
      scv(i) = 1;
    for (octave_idx_type i = ilo-1; i < ihi; i++)
      scv(i) = scale(i);
    for (octave_idx_type i = ihi; i < n; i++)
      scv(i) = 1;

    return scv;
  }
};

#endif
