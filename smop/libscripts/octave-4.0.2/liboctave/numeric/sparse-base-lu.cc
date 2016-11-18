/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#include "sparse-base-lu.h"

#include "PermMatrix.h"

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
lu_type
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Y (void) const
{
  octave_idx_type nr = Lfact.rows ();
  octave_idx_type nz = Lfact.cols ();
  octave_idx_type nc = Ufact.cols ();

  lu_type Yout (nr, nc, Lfact.nnz () + Ufact.nnz () - (nr<nz?nr:nz));
  octave_idx_type ii = 0;
  Yout.xcidx (0) = 0;

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = Ufact.cidx (j); i < Ufact.cidx (j + 1); i++)
        {
          Yout.xridx (ii) = Ufact.ridx (i);
          Yout.xdata (ii++) = Ufact.data (i);
        }
      if (j < nz)
        {
          // Note the +1 skips the 1.0 on the diagonal
          for (octave_idx_type i = Lfact.cidx (j) + 1;
               i < Lfact.cidx (j +1); i++)
            {
              Yout.xridx (ii) = Lfact.ridx (i);
              Yout.xdata (ii++) = Lfact.data (i);
            }
        }
      Yout.xcidx (j + 1) = ii;
    }

  return Yout;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
p_type
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pr (void) const
{

  octave_idx_type nr = Lfact.rows ();

  p_type Pout (nr, nr, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    {
      Pout.cidx (i) = i;
      Pout.ridx (P (i)) = i;
      Pout.data (i) = 1;
    }
  Pout.cidx (nr) = nr;

  return Pout;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
ColumnVector
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pr_vec (void) const
{

  octave_idx_type nr = Lfact.rows ();

  ColumnVector Pout (nr);

  for (octave_idx_type i = 0; i < nr; i++)
    Pout.xelem (i) = static_cast<double> (P(i) + 1);

  return Pout;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
PermMatrix
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pr_mat (void) const
{
  return PermMatrix (P, false);
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
p_type
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pc (void) const
{
  octave_idx_type nc = Ufact.cols ();

  p_type Pout (nc, nc, nc);

  for (octave_idx_type i = 0; i < nc; i++)
    {
      Pout.cidx (i) = i;
      Pout.ridx (i) = Q (i);
      Pout.data (i) = 1;
    }
  Pout.cidx (nc) = nc;

  return Pout;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
ColumnVector
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pc_vec (void) const
{

  octave_idx_type nc = Ufact.cols ();

  ColumnVector Pout (nc);

  for (octave_idx_type i = 0; i < nc; i++)
    Pout.xelem (i) = static_cast<double> (Q(i) + 1);

  return Pout;
}

template <class lu_type, class lu_elt_type, class p_type, class p_elt_type>
PermMatrix
sparse_base_lu <lu_type, lu_elt_type, p_type, p_elt_type> :: Pc_mat (void) const
{
  return PermMatrix (Q, true);
}
