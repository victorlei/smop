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

#include "f77-fcn.h"
#include "mx-base.h"
#include "error.h"
#include "defun.h"
#include "parse.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (ddot3, DDOT3) (const octave_idx_type&, const octave_idx_type&,
                           const octave_idx_type&, const double*,
                           const double*, double*);

  F77_RET_T
  F77_FUNC (sdot3, SDOT3) (const octave_idx_type&, const octave_idx_type&,
                           const octave_idx_type&, const float*,
                           const float*, float*);

  F77_RET_T
  F77_FUNC (zdotc3, ZDOTC3) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const Complex*,
                             const Complex*, Complex*);

  F77_RET_T
  F77_FUNC (cdotc3, CDOTC3) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const FloatComplex*,
                             const FloatComplex*, FloatComplex*);

  F77_RET_T
  F77_FUNC (dmatm3, DMATM3) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&,
                             const double*, const double*, double*);

  F77_RET_T
  F77_FUNC (smatm3, SMATM3) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&,
                             const float*, const float*, float*);

  F77_RET_T
  F77_FUNC (zmatm3, ZMATM3) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&,
                             const Complex*, const Complex*, Complex*);

  F77_RET_T
  F77_FUNC (cmatm3, CMATM3) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&,
                             const FloatComplex*, const FloatComplex*,
                             FloatComplex*);
}

static void
get_red_dims (const dim_vector& x, const dim_vector& y, int dim,
              dim_vector& z, octave_idx_type& m, octave_idx_type& n,
              octave_idx_type& k)
{
  int nd = x.length ();
  assert (nd == y.length ());
  z = dim_vector::alloc (nd);
  m = 1, n = 1, k = 1;
  for (int i = 0; i < nd; i++)
    {
      if (i < dim)
        {
          z(i) = x(i);
          m *= x(i);
        }
      else if (i > dim)
        {
          z(i) = x(i);
          n *= x(i);
        }
      else
        {
          k = x(i);
          z(i) = 1;
        }
    }
}

DEFUN (dot, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} dot (@var{x}, @var{y}, @var{dim})\n\
Compute the dot product of two vectors.\n\
\n\
If @var{x} and @var{y} are matrices, calculate the dot products along the\n\
first non-singleton dimension.\n\
\n\
If the optional argument @var{dim} is given, calculate the dot products\n\
along this dimension.\n\
\n\
This is equivalent to\n\
@code{sum (conj (@var{X}) .* @var{Y}, @var{dim})},\n\
but avoids forming a temporary array and is faster.  When @var{X} and\n\
@var{Y} are column vectors, the result is equivalent to\n\
@code{@var{X}' * @var{Y}}.\n\
@seealso{cross, divergence}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value argx = args(0);
  octave_value argy = args(1);

  if (argx.is_numeric_type () && argy.is_numeric_type ())
    {
      dim_vector dimx = argx.dims ();
      dim_vector dimy = argy.dims ();
      bool match = dimx == dimy;
      if (! match && nargin == 2
          && dimx.is_vector () && dimy.is_vector ())
        {
          // Change to column vectors.
          dimx = dimx.redim (1);
          argx = argx.reshape (dimx);
          dimy = dimy.redim (1);
          argy = argy.reshape (dimy);
          match = ! error_state && (dimx == dimy);
        }

      if (match)
        {
          int dim;
          if (nargin == 2)
            dim = dimx.first_non_singleton ();
          else
            dim = args(2).int_value (true) - 1;

          if (error_state)
            ;
          else if (dim < 0)
            error ("dot: DIM must be a valid dimension");
          else
            {
              octave_idx_type m, n, k;
              dim_vector dimz;
              if (argx.is_complex_type () || argy.is_complex_type ())
                {
                  if (argx.is_single_type () || argy.is_single_type ())
                    {
                      FloatComplexNDArray x = argx.float_complex_array_value ();
                      FloatComplexNDArray y = argy.float_complex_array_value ();
                      get_red_dims (dimx, dimy, dim, dimz, m, n, k);
                      FloatComplexNDArray z(dimz);
                      if (! error_state)
                        F77_XFCN (cdotc3, CDOTC3, (m, n, k,
                                                   x.data (), y.data (),
                                                   z.fortran_vec ()));
                      retval = z;
                    }
                  else
                    {
                      ComplexNDArray x = argx.complex_array_value ();
                      ComplexNDArray y = argy.complex_array_value ();
                      get_red_dims (dimx, dimy, dim, dimz, m, n, k);
                      ComplexNDArray z(dimz);
                      if (! error_state)
                        F77_XFCN (zdotc3, ZDOTC3, (m, n, k,
                                                   x.data (), y.data (),
                                                   z.fortran_vec ()));
                      retval = z;
                    }
                }
              else if (argx.is_float_type () && argy.is_float_type ())
                {
                  if (argx.is_single_type () || argy.is_single_type ())
                    {
                      FloatNDArray x = argx.float_array_value ();
                      FloatNDArray y = argy.float_array_value ();
                      get_red_dims (dimx, dimy, dim, dimz, m, n, k);
                      FloatNDArray z(dimz);
                      if (! error_state)
                        F77_XFCN (sdot3, SDOT3, (m, n, k, x.data (), y.data (),
                                                 z.fortran_vec ()));
                      retval = z;
                    }
                  else
                    {
                      NDArray x = argx.array_value ();
                      NDArray y = argy.array_value ();
                      get_red_dims (dimx, dimy, dim, dimz, m, n, k);
                      NDArray z(dimz);
                      if (! error_state)
                        F77_XFCN (ddot3, DDOT3, (m, n, k, x.data (), y.data (),
                                                 z.fortran_vec ()));
                      retval = z;
                    }
                }
              else
                {
                  // Non-optimized evaluation.
                  octave_value_list tmp;
                  tmp(1) = dim + 1;
                  tmp(0) = do_binary_op (octave_value::op_el_mul, argx, argy);
                  if (! error_state)
                    {
                      tmp = feval ("sum", tmp, 1);
                      if (! tmp.empty ())
                        retval = tmp(0);
                    }
                }
            }
        }
      else
        error ("dot: sizes of X and Y must match");

    }
  else
    error ("dot: X and Y must be numeric");

  return retval;
}

/*
%!assert (dot ([1, 2], [2, 3]), 8)

%!test
%! x = [2, 1; 2, 1];
%! y = [-0.5, 2; 0.5, -2];
%! assert (dot (x, y), [0 0]);
%! assert (dot (single (x), single (y)), single ([0 0]));

%!test
%! x = [1+i, 3-i; 1-i, 3-i];
%! assert (dot (x, x), [4, 20]);
%! assert (dot (single (x), single (x)), single ([4, 20]));

%!test
%! x = int8 ([1 2]);
%! y = int8 ([2 3]);
%! assert (dot (x, y), 8);

%!test
%! x = int8 ([1 2; 3 4]);
%! y = int8 ([5 6; 7 8]);
%! assert (dot (x, y), [26 44]);
%! assert (dot (x, y, 2), [17; 53]);
%! assert (dot (x, y, 3), [5 12; 21 32]);

%% Test input validation
%!error dot ()
%!error dot (1)
%!error dot (1,2,3,4)
%!error <X and Y must be numeric> dot ({1,2}, [3,4])
%!error <X and Y must be numeric> dot ([1,2], {3,4})
%!error <sizes of X and Y must match> dot ([1 2], [1 2 3])
%!error <sizes of X and Y must match> dot ([1 2]', [1 2 3]')
%!error <sizes of X and Y must match> dot (ones (2,2), ones (2,3))
%!error <DIM must be a valid dimension> dot ([1 2], [1 2], 0)
*/

DEFUN (blkmm, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} blkmm (@var{A}, @var{B})\n\
Compute products of matrix blocks.\n\
\n\
The blocks are given as 2-dimensional subarrays of the arrays @var{A},\n\
@var{B}.  The size of @var{A} must have the form @code{[m,k,@dots{}]} and\n\
size of @var{B} must be @code{[k,n,@dots{}]}.  The result is then of size\n\
@code{[m,n,@dots{}]} and is computed as follows:\n\
\n\
@example\n\
@group\n\
for i = 1:prod (size (@var{A})(3:end))\n\
  @var{C}(:,:,i) = @var{A}(:,:,i) * @var{B}(:,:,i)\n\
endfor\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 2)
    {
      print_usage ();
      return retval;
    }

  octave_value argx = args(0);
  octave_value argy = args(1);

  if (argx.is_numeric_type () && argy.is_numeric_type ())
    {
      const dim_vector dimx = argx.dims ();
      const dim_vector dimy = argy.dims ();
      int nd = dimx.length ();
      octave_idx_type m = dimx(0);
      octave_idx_type k = dimx(1);
      octave_idx_type n = dimy(1);
      octave_idx_type np = 1;
      bool match = dimy(0) == k && nd == dimy.length ();
      dim_vector dimz = dim_vector::alloc (nd);
      dimz(0) = m;
      dimz(1) = n;
      for (int i = 2; match && i < nd; i++)
        {
          match = match && dimx(i) == dimy(i);
          dimz(i) = dimx(i);
          np *= dimz(i);
        }

      if (match)
        {
          if (argx.is_complex_type () || argy.is_complex_type ())
            {
              if (argx.is_single_type () || argy.is_single_type ())
                {
                  FloatComplexNDArray x = argx.float_complex_array_value ();
                  FloatComplexNDArray y = argy.float_complex_array_value ();
                  FloatComplexNDArray z(dimz);
                  if (! error_state)
                    F77_XFCN (cmatm3, CMATM3, (m, n, k, np,
                                               x.data (), y.data (),
                                               z.fortran_vec ()));
                  retval = z;
                }
              else
                {
                  ComplexNDArray x = argx.complex_array_value ();
                  ComplexNDArray y = argy.complex_array_value ();
                  ComplexNDArray z(dimz);
                  if (! error_state)
                    F77_XFCN (zmatm3, ZMATM3, (m, n, k, np,
                                               x.data (), y.data (),
                                               z.fortran_vec ()));
                  retval = z;
                }
            }
          else
            {
              if (argx.is_single_type () || argy.is_single_type ())
                {
                  FloatNDArray x = argx.float_array_value ();
                  FloatNDArray y = argy.float_array_value ();
                  FloatNDArray z(dimz);
                  if (! error_state)
                    F77_XFCN (smatm3, SMATM3, (m, n, k, np,
                                               x.data (), y.data (),
                                               z.fortran_vec ()));
                  retval = z;
                }
              else
                {
                  NDArray x = argx.array_value ();
                  NDArray y = argy.array_value ();
                  NDArray z(dimz);
                  if (! error_state)
                    F77_XFCN (dmatm3, DMATM3, (m, n, k, np,
                                               x.data (), y.data (),
                                               z.fortran_vec ()));
                  retval = z;
                }
            }
        }
      else
        error ("blkmm: A and B dimensions don't match: (%s) and (%s)",
               dimx.str ().c_str (), dimy.str ().c_str ());

    }
  else
    error ("blkmm: A and B must be numeric");

  return retval;
}

/*
%!test
%! x(:,:,1) = [1 2; 3 4];
%! x(:,:,2) = [1 1; 1 1];
%! z(:,:,1) = [7 10; 15 22];
%! z(:,:,2) = [2 2; 2 2];
%! assert (blkmm (x,x), z);
%! assert (blkmm (single (x), single (x)), single (z));
%! assert (blkmm (x, single (x)), single (z));

%!test
%! x(:,:,1) = [1 2; 3 4];
%! x(:,:,2) = [1i 1i; 1i 1i];
%! z(:,:,1) = [7 10; 15 22];
%! z(:,:,2) = [-2 -2; -2 -2];
%! assert (blkmm (x,x), z);
%! assert (blkmm (single (x), single (x)), single (z));
%! assert (blkmm (x, single (x)), single (z));

%% Test input validation
%!error blkmm ()
%!error blkmm (1)
%!error blkmm (1,2,3)
%!error <A and B dimensions don't match> blkmm (ones (2,2), ones (3,3))
%!error <A and B must be numeric> blkmm ({1,2}, [3,4])
%!error <A and B must be numeric> blkmm ([3,4], {1,2})
*/

