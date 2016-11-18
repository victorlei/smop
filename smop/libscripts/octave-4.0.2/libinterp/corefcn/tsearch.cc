/*

Copyright (C) 2002-2015 Andreas Stahel

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

// Author: Andreas Stahel <Andreas.Stahel@hta-bi.bfh.ch>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "lo-ieee.h"
#include "lo-math.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

inline double max (double a, double b, double c)
{
  if (a < b)
    return (b < c ? c : b);
  else
    return (a < c ? c : a);
}

inline double min (double a, double b, double c)
{
  if (a > b)
    return (b > c ? c : b);
  else
    return (a > c ? c : a);
}

#define REF(x,k,i) x(static_cast<octave_idx_type>(elem((k), (i))) - 1)

// for large data set the algorithm is very slow
// one should presort (how?) either the elements of the points of evaluation
// to cut down the time needed to decide which triangle contains the
// given point

// e.g., build up a neighbouring triangle structure and use a simplex-like
// method to traverse it

DEFUN (tsearch, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{idx} =} tsearch (@var{x}, @var{y}, @var{t}, @var{xi}, @var{yi})\n\
Search for the enclosing Delaunay convex hull.\n\
\n\
For @code{@var{t} = delaunay (@var{x}, @var{y})}, finds the index in @var{t}\n\
containing the points @code{(@var{xi}, @var{yi})}.  For points outside the\n\
convex hull, @var{idx} is NaN.\n\
@seealso{delaunay, delaunayn}\n\
@end deftypefn")
{
  const double eps=1.0e-12;

  octave_value_list retval;
  const int nargin = args.length ();
  if (nargin != 5)
    {
      print_usage ();
      return retval;
    }

  const ColumnVector x (args(0).vector_value ());
  const ColumnVector y (args(1).vector_value ());
  const Matrix elem (args(2).matrix_value ());
  const ColumnVector xi (args(3).vector_value ());
  const ColumnVector yi (args(4).vector_value ());

  if (error_state)
    return retval;

  const octave_idx_type nelem = elem.rows ();

  ColumnVector minx (nelem);
  ColumnVector maxx (nelem);
  ColumnVector miny (nelem);
  ColumnVector maxy (nelem);
  for (octave_idx_type k = 0; k < nelem; k++)
    {
      minx(k) = min (REF (x, k, 0), REF (x, k, 1), REF (x, k, 2)) - eps;
      maxx(k) = max (REF (x, k, 0), REF (x, k, 1), REF (x, k, 2)) + eps;
      miny(k) = min (REF (y, k, 0), REF (y, k, 1), REF (y, k, 2)) - eps;
      maxy(k) = max (REF (y, k, 0), REF (y, k, 1), REF (y, k, 2)) + eps;
    }

  const octave_idx_type np = xi.length ();
  ColumnVector values (np);

  double x0, y0, a11, a12, a21, a22, det;
  x0 = y0 = 0.0;
  a11 = a12 = a21 = a22 = 0.0;
  det = 0.0;

  octave_idx_type k = nelem; // k is a counter of elements
  for (octave_idx_type kp = 0; kp < np; kp++)
    {
      const double xt = xi(kp);
      const double yt = yi(kp);

      // check if last triangle contains the next point
      if (k < nelem)
        {
          const double dx1 = xt - x0;
          const double dx2 = yt - y0;
          const double c1 = (a22 * dx1 - a21 * dx2) / det;
          const double c2 = (-a12 * dx1 + a11 * dx2) / det;
          if (c1 >= -eps && c2 >= -eps && (c1 + c2) <= (1 + eps))
            {
              values(kp) = double(k+1);
              continue;
            }
        }

      // it doesn't, so go through all elements
      for (k = 0; k < nelem; k++)
        {
          OCTAVE_QUIT;
          if (xt >= minx(k) && xt <= maxx(k) && yt >= miny(k) && yt <= maxy(k))
            {
              // element inside the minimum rectangle: examine it closely
              x0  = REF (x, k, 0);
              y0  = REF (y, k, 0);
              a11 = REF (x, k, 1) - x0;
              a12 = REF (y, k, 1) - y0;
              a21 = REF (x, k, 2) - x0;
              a22 = REF (y, k, 2) - y0;
              det = a11 * a22 - a21 * a12;

              // solve the system
              const double dx1 = xt - x0;
              const double dx2 = yt - y0;
              const double c1 = (a22 * dx1 - a21 * dx2) / det;
              const double c2 = (-a12 * dx1 + a11 * dx2) / det;
              if ((c1 >= -eps) && (c2 >= -eps) && ((c1 + c2) <= (1 + eps)))
                {
                  values(kp) = double(k+1);
                  break;
                }
            } //endif # examine this element closely
        } //endfor # each element

      if (k == nelem)
        values(kp) = lo_ieee_nan_value ();

    } //endfor # kp

  retval(0) = values;

  return retval;
}

/*
%!shared x, y, tri
%! x = [-1;-1;1];
%! y = [-1;1;-1];
%! tri = [1, 2, 3];
%!assert (tsearch (x,y,tri,-1,-1), 1)
%!assert (tsearch (x,y,tri, 1,-1), 1)
%!assert (tsearch (x,y,tri,-1, 1), 1)
%!assert (tsearch (x,y,tri,-1/3, -1/3), 1)
%!assert (tsearch (x,y,tri, 1, 1), NaN)

%!error tsearch ()
*/
