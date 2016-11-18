/*

Copyright (C) 2009-2015 VZLU Prague

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

#include "Array-util.h"
#include "oct-locbuf.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"


static dim_vector
get_dim_vector (const octave_value& val, const char *name)
{
  RowVector dimsv = val.row_vector_value (false, true);
  dim_vector dv;
  octave_idx_type n = dimsv.length ();

  if (n < 1)
    error ("%s: dimension vector DIMS must not be empty", name);
  else
    {
      dv.resize (std::max (n, static_cast<octave_idx_type> (2)));
      dv(1) = 1;
      for (octave_idx_type i = 0; i < n; i++)
        {
          octave_idx_type ii = dimsv(i);
          if (ii == dimsv(i) && ii >= 0)
            dv(i) = ii;
          else
            {
              error ("%s: dimension vector DIMS must contain integers", name);
              break;
            }
        }
    }

  return dv;
}

DEFUN (sub2ind, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Function File} {@var{ind} =} sub2ind (@var{dims}, @var{i}, @var{j})\n\
@deftypefnx {Function File} {@var{ind} =} sub2ind (@var{dims}, @var{s1}, @var{s2}, @dots{}, @var{sN})\n\
Convert subscripts to a linear index.\n\
\n\
The following example shows how to convert the two-dimensional index\n\
@code{(2,3)} of a 3-by-3 matrix to a linear index.  The matrix is linearly\n\
indexed moving from one column to next, filling up all rows in each column.\n\
\n\
@example\n\
@group\n\
linear_index = sub2ind ([3, 3], 2, 3)\n\
@result{} 8\n\
@end group\n\
@end example\n\
@seealso{ind2sub}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin < 2)
    print_usage ();
  else
    {
      dim_vector dv = get_dim_vector (args(0), "sub2ind");
      Array<idx_vector> idxa (dim_vector (nargin-1, 1));

      if (! error_state)
        {
          dv = dv.redim (nargin - 1);
          for (int j = 0; j < nargin - 1; j++)
            {
              if (args(j+1).is_numeric_type ())
                {
                  idxa(j) = args(j+1).index_vector ();
                  if (error_state)
                    break;
                  else if (j > 0 && args(j+1).dims () != args(1).dims ())
                    error ("sub2ind: all subscripts must be of the same size");
                }
              else
                error ("sub2ind: subscripts must be numeric");

              if (error_state)
                break;
            }
        }

      if (! error_state)
        {
          idx_vector idx = sub2ind (dv, idxa);
          retval = idx;
        }
    }

  return retval;
}

/*
## Test evaluation
%!test
%! s1 = [ 1   1   1   1 ; 2   2   2   2 ];
%! s2 = [ 1   1   2   2 ; 1   1   2   2 ];
%! s3 = [ 1   2   1   2 ; 1   2   1   2 ];
%! in = [ 1 101  11 111 ; 2 102  12 112 ];
%! assert (sub2ind ([10 10 10], s1, s2, s3), in);

# Test low index
%!assert (sub2ind ([10 10 10], 1, 1, 1), 1)
%!error <subscript indices> sub2ind ([10 10 10], 0, 1, 1)
%!error <subscript indices> sub2ind ([10 10 10], 1, 0, 1)
%!error <subscript indices> sub2ind ([10 10 10], 1, 1, 0)

# Test high index
%!assert (sub2ind ([10 10 10], 10, 10, 10), 1000)
%!error <index out of range> sub2ind ([10 10 10], 11, 10, 10)
%!error <index out of range> sub2ind ([10 10 10], 10, 11, 10)
%!error <index out of range> sub2ind ([10 10 10], 10, 10, 11)

# Test high index in the trailing dimensions
%!assert (sub2ind ([10, 1], 2, 1, 1), 2)
%!error <index out of range> sub2ind ([10, 1], 1, 2, 1)
%!error <index out of range> sub2ind ([10, 1], 1, 1, 2)
%!assert (sub2ind ([10 10], 2, 2, 1), 12)
%!error <index out of range> sub2ind ([10 10], 2, 1, 2)
%!error <index out of range> sub2ind ([10 10], 1, 2, 2)

# Test handling of empty arguments
%!assert (sub2ind ([10 10], zeros (0,0), zeros (0,0)), zeros (0,0))
%!assert (sub2ind ([10 10], zeros (2,0), zeros (2,0)), zeros (2,0))
%!assert (sub2ind ([10 10], zeros (0,2), zeros (0,2)), zeros (0,2))
%!error <all subscripts .* same size> sub2ind ([10 10 10], zeros (0,2), zeros (2,0))

# Test handling of arguments of different size
%!error <all subscripts .* same size> sub2ind ([10 10], ones (1,2), ones (1,3))
%!error <all subscripts .* same size> sub2ind ([10 10], ones (1,2), ones (2,1))

## Test input validation
%!error <dimension vector> sub2ind ([10 10.5], 1, 1)
%!error <subscript indices> sub2ind ([10 10], 1.5, 1)
%!error <subscript indices> sub2ind ([10 10], 1, 1.5)
*/

DEFUN (ind2sub, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {[@var{s1}, @var{s2}, @dots{}, @var{sN}] =} ind2sub (@var{dims}, @var{ind})\n\
Convert a linear index to subscripts.\n\
\n\
The following example shows how to convert the linear index @code{8}\n\
in a 3-by-3 matrix into a subscript.  The matrix is linearly indexed\n\
moving from one column to next, filling up all rows in each column.\n\
\n\
@example\n\
@group\n\
[r, c] = ind2sub ([3, 3], 8)\n\
    @result{} r =  2\n\
    @result{} c =  3\n\
@end group\n\
@end example\n\
@seealso{sub2ind}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin != 2)
    print_usage ();
  else
    {
      dim_vector dv = get_dim_vector (args(0), "ind2sub");
      idx_vector idx = args(1).index_vector ();
      if (! error_state)
        {
          if (nargout > dv.length ())
            dv = dv.redim (nargout);

          Array<idx_vector> idxa = ind2sub (dv, idx);
          retval = Array<octave_value> (idxa);
        }
    }

  return retval;
}
