/*

Copyright (C) 2002-2015 Kai Habel
Copyright (C) 2008-2009 Jaroslav Hajek

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

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "f77-fcn.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dpchim, DPCHIM) (const octave_idx_type& n, const double *x,
                             const double *f, double *d,
                             const octave_idx_type &incfd,
                             octave_idx_type *ierr);

  F77_RET_T
  F77_FUNC (pchim, PCHIM) (const octave_idx_type& n, const float *x,
                           const float *f, float *d,
                           const octave_idx_type& incfd,
                           octave_idx_type *ierr);
}

// Wrapper for SLATEC/PCHIP function DPCHIM to calculate the derivates
// for piecewise polynomials.

DEFUN (__pchip_deriv__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __pchip_deriv__ (@var{x}, @var{y}, @var{dim})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
  const int nargin = args.length ();

  bool rows = (nargin == 3 && args(2).uint_value () == 2);

  if (nargin >= 2)
    {
      if (args(0).is_single_type () || args(1).is_single_type ())
        {
          FloatColumnVector xvec (args(0).float_vector_value ());
          FloatMatrix ymat (args(1).float_matrix_value ());

          octave_idx_type nx = xvec.length ();

          if (nx < 2)
            {
              error ("__pchip_deriv__: X must be at least of length 2");
              return retval;
            }

          octave_idx_type nyr = ymat.rows ();
          octave_idx_type nyc = ymat.columns ();

          if (nx != (rows ? nyc : nyr))
            {
              error ("__pchip_deriv__: X and Y dimension mismatch");
              return retval;
            }

          FloatMatrix dmat (nyr, nyc);

          octave_idx_type ierr;
          const octave_idx_type incfd = rows ? nyr : 1;
          volatile const octave_idx_type inc = rows ? 1 : nyr;
          volatile octave_idx_type k = 0;

          for (volatile octave_idx_type i = (rows ? nyr : nyc); i > 0; i--)
            {
              F77_XFCN (pchim, PCHIM, (nx, xvec.data (),
                                       ymat.data () + k * inc,
                                       dmat.fortran_vec () + k * inc,
                                       incfd, &ierr));

              k++;

              if (ierr < 0)
                {
                  error ("__pchip_deriv__: PCHIM failed with ierr = %i", ierr);
                  return retval;
                }
            }

          retval = dmat;
        }
      else
        {
          ColumnVector xvec (args(0).vector_value ());
          Matrix ymat (args(1).matrix_value ());

          octave_idx_type nx = xvec.length ();

          if (nx < 2)
            {
              error ("__pchip_deriv__: X must be at least of length 2");
              return retval;
            }

          octave_idx_type nyr = ymat.rows ();
          octave_idx_type nyc = ymat.columns ();

          if (nx != (rows ? nyc : nyr))
            {
              error ("__pchip_deriv__: X and Y dimension mismatch");
              return retval;
            }

          Matrix dmat (nyr, nyc);

          octave_idx_type ierr;
          const octave_idx_type incfd = rows ? nyr : 1;
          volatile const octave_idx_type inc = rows ? 1 : nyr;
          volatile octave_idx_type k = 0;

          for (volatile octave_idx_type i = (rows ? nyr : nyc); i > 0; i--)
            {
              F77_XFCN (dpchim, DPCHIM, (nx, xvec.data (),
                                         ymat.data () + k * inc,
                                         dmat.fortran_vec () + k * inc,
                                         incfd, &ierr));
              k++;

              if (ierr < 0)
                {
                  error ("__pchip_deriv__: DPCHIM failed with ierr = %i", ierr);
                  return retval;
                }
            }

          retval = dmat;
        }
    }

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
