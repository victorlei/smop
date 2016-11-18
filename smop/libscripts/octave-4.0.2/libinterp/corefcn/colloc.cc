/*

Copyright (C) 1996-2015 John W. Eaton

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

#include <string>

#include "CollocWt.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN (colloc, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{r}, @var{amat}, @var{bmat}, @var{q}] =} colloc (@var{n}, \"left\", \"right\")\n\
Compute derivative and integral weight matrices for orthogonal collocation.\n\
\n\
Reference: @nospell{J. Villadsen}, @nospell{M. L. Michelsen},\n\
@cite{Solution of Differential Equation Models by Polynomial Approximation}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  if (! args(0).is_scalar_type ())
    {
      error ("colloc: N must be a scalar");
      return retval;
    }

  double tmp = args(0).double_value ();

  if (error_state)
    return retval;

  if (xisnan (tmp))
    {
      error ("colloc: N cannot be NaN");
      return retval;
    }

  octave_idx_type ncol = NINTbig (tmp);
  if (ncol < 0)
    {
      error ("colloc: N must be positive");
      return retval;
    }

  octave_idx_type ntot = ncol;
  octave_idx_type left = 0;
  octave_idx_type right = 0;

  for (int i = 1; i < nargin; i++)
    {
      if (args(i).is_defined ())
        {
          if (! args(i).is_string ())
            {
              error ("colloc: expecting string argument \"left\" or \"right\"");
              return retval;
            }

          std::string s = args(i).string_value ();

          if ((s.length () == 1 && (s[0] == 'R' || s[0] == 'r'))
              || s == "right")
            {
              right = 1;
            }
          else if ((s.length () == 1 && (s[0] == 'L' || s[0] == 'l'))
                   || s == "left")
            {
              left = 1;
            }
          else
            {
              error ("colloc: unrecognized argument");
              return retval;
            }
        }
      else
        {
          error ("colloc: unexpected empty argument");
          return retval;
        }
    }

  ntot += left + right;
  if (ntot < 1)
    {
      error ("colloc: the total number of roots must be positive");
      return retval;
    }

  CollocWt wts (ncol, left, right);

  ColumnVector r = wts.roots ();
  Matrix A = wts.first ();
  Matrix B = wts.second ();
  ColumnVector q = wts.quad_weights ();

  retval(3) = q;
  retval(2) = B;
  retval(1) = A;
  retval(0) = r;

  return retval;
}
