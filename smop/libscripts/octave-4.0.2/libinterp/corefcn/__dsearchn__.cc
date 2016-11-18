/*

Copyright (C) 2007-2015 David Bateman

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

#include <iostream>
#include <fstream>
#include <string>

#include "lo-math.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

DEFUN (__dsearchn__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{idx}, @var{d}] =} dsearch (@var{x}, @var{xi})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin != 2)
    {
      print_usage ();
      return retval;
    }

  Matrix x = args(0).matrix_value ().transpose ();
  Matrix xi = args(1).matrix_value ().transpose ();

  if (! error_state)
    {
      if (x.rows () != xi.rows () || x.columns () < 1)
        error ("__dsearch__: number of rows of X and XI must match");
      else
        {
          octave_idx_type n = x.rows ();
          octave_idx_type nx = x.columns ();
          octave_idx_type nxi = xi.columns ();

          ColumnVector idx (nxi);
          double *pidx = idx.fortran_vec ();
          ColumnVector dist (nxi);
          double *pdist = dist.fortran_vec ();

#define DIST(dd, y, yi, m) \
  dd = 0.; \
  for (octave_idx_type k = 0; k < m; k++) \
   { \
     double yd = y[k] - yi[k]; \
     dd += yd * yd; \
   } \
  dd = sqrt (dd);

          const double *pxi = xi.fortran_vec ();
          for (octave_idx_type i = 0; i < nxi; i++)
            {
              double d0;
              const double *px = x.fortran_vec ();
              DIST(d0, px, pxi, n);
              *pidx = 1.;
              for (octave_idx_type j = 1; j < nx; j++)
                {
                  px += n;
                  double d;
                  DIST (d, px, pxi, n);
                  if (d < d0)
                    {
                      d0 = d;
                      *pidx = static_cast<double>(j + 1);
                    }
                  OCTAVE_QUIT;
                }

              *pdist++ = d0;
              pidx++;
              pxi += n;
            }

          retval(1) = dist;
          retval(0) = idx;
        }
    }

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
