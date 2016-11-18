/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2009 Jaroslav Hajek
Copyright (C) 2009-2010 VZLU Prague
Copyright (C) 2012 Carlo de Falco

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

#include <sys/types.h>
#include <sys/times.h>

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include <cfloat>
#include <ctime>

#include <string>

#include "lo-ieee.h"
#include "lo-math.h"
#include "oct-base64.h"
#include "oct-time.h"
#include "str-vec.h"
#include "quit.h"
#include "mx-base.h"
#include "oct-binmap.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-class.h"
#include "ov-float.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-cx-sparse.h"
#include "parse.h"
#include "pt-mat.h"
#include "utils.h"
#include "variables.h"
#include "pager.h"
#include "xnorm.h"

#if ! defined (CLOCKS_PER_SEC)
#if defined (CLK_TCK)
#define CLOCKS_PER_SEC CLK_TCK
#else
#error "no definition for CLOCKS_PER_SEC!"
#endif
#endif

#if ! defined (HAVE_HYPOTF) && defined (HAVE__HYPOTF)
#define hypotf _hypotf
#define HAVE_HYPOTF 1
#endif

#define ANY_ALL(FCN) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  if (nargin == 1 || nargin == 2) \
    { \
      int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1); \
 \
      if (! error_state) \
        { \
          if (dim >= -1) \
            retval = args(0).FCN (dim); \
          else \
            error (#FCN ": invalid dimension argument = %d", dim + 1); \
        } \
      else \
        error (#FCN ": expecting dimension argument to be an integer"); \
    } \
  else \
    print_usage (); \
 \
  return retval

DEFUN (all, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} all (@var{x})\n\
@deftypefnx {Built-in Function} {} all (@var{x}, @var{dim})\n\
For a vector argument, return true (logical 1) if all elements of the vector\n\
are nonzero.\n\
\n\
For a matrix argument, return a row vector of logical ones and\n\
zeros with each element indicating whether all of the elements of the\n\
corresponding column of the matrix are nonzero.  For example:\n\
\n\
@example\n\
@group\n\
all ([2, 3; 1, 0])\n\
    @result{} [ 1, 0 ]\n\
@end group\n\
@end example\n\
\n\
If the optional argument @var{dim} is supplied, work along dimension\n\
@var{dim}.\n\
@seealso{any}\n\
@end deftypefn")
{
  ANY_ALL (all);
}

/*
%!test
%! x = ones (3);
%! x(1,1) = 0;
%! assert (all (all (rand (3) + 1) == [1, 1, 1]) == 1);
%! assert (all (all (x) == [0, 1, 1]) == 1);
%! assert (all (x, 1) == [0, 1, 1]);
%! assert (all (x, 2) == [0; 1; 1]);

%!test
%! x = ones (3, "single");
%! x(1,1) = 0;
%! assert (all (all (single (rand (3) + 1)) == [1, 1, 1]) == 1);
%! assert (all (all (x) == [0, 1, 1]) == 1);
%! assert (all (x, 1) == [0, 1, 1]);
%! assert (all (x, 2) == [0; 1; 1]);

%!error all ()
%!error all (1, 2, 3)
*/

DEFUN (any, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} any (@var{x})\n\
@deftypefnx {Built-in Function} {} any (@var{x}, @var{dim})\n\
For a vector argument, return true (logical 1) if any element of the vector\n\
is nonzero.\n\
\n\
For a matrix argument, return a row vector of logical ones and\n\
zeros with each element indicating whether any of the elements of the\n\
corresponding column of the matrix are nonzero.  For example:\n\
\n\
@example\n\
@group\n\
any (eye (2, 4))\n\
 @result{} [ 1, 1, 0, 0 ]\n\
@end group\n\
@end example\n\
\n\
If the optional argument @var{dim} is supplied, work along dimension\n\
@var{dim}.  For example:\n\
\n\
@example\n\
@group\n\
any (eye (2, 4), 2)\n\
 @result{} [ 1; 1 ]\n\
@end group\n\
@end example\n\
@seealso{all}\n\
@end deftypefn")
{
  ANY_ALL (any);
}

/*
%!test
%! x = zeros (3);
%! x(3,3) = 1;
%! assert (all (any (x) == [0, 0, 1]) == 1);
%! assert (all (any (ones (3)) == [1, 1, 1]) == 1);
%! assert (any (x, 1) == [0, 0, 1]);
%! assert (any (x, 2) == [0; 0; 1]);

%!test
%! x = zeros (3, "single");
%! x(3,3) = 1;
%! assert (all (any (x) == [0, 0, 1]) == 1);
%! assert (all (any (ones (3, "single")) == [1, 1, 1]) == 1);
%! assert (any (x, 1) == [0, 0, 1]);
%! assert (any (x, 2) == [0; 0; 1]);

%!error any ()
%!error any (1, 2, 3)
*/

// These mapping functions may also be useful in other places, eh?

DEFUN (atan2, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atan2 (@var{y}, @var{x})\n\
Compute atan (@var{y} / @var{x}) for corresponding elements of @var{y}\n\
and @var{x}.\n\
\n\
@var{y} and @var{x} must match in size and orientation.\n\
@seealso{tan, tand, tanh, atanh}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (! args(0).is_numeric_type ())
        gripe_wrong_type_arg ("atan2", args(0));
      else if (! args(1).is_numeric_type ())
        gripe_wrong_type_arg ("atan2", args(1));
      else if (args(0).is_complex_type () || args(1).is_complex_type ())
        error ("atan2: not defined for complex numbers");
      else if (args(0).is_single_type () || args(1).is_single_type ())
        {
          if (args(0).is_scalar_type () && args(1).is_scalar_type ())
            retval = atan2f (args(0).float_value (), args(1).float_value ());
          else
            {
              FloatNDArray a0 = args(0).float_array_value ();
              FloatNDArray a1 = args(1).float_array_value ();
              retval = binmap<float> (a0, a1, ::atan2f, "atan2");
            }
        }
      else
        {
          if (args(0).is_scalar_type () && args(1).is_scalar_type ())
            retval = atan2 (args(0).scalar_value (), args(1).scalar_value ());
          else if (args(0).is_sparse_type ())
            {
              SparseMatrix m0 = args(0).sparse_matrix_value ();
              SparseMatrix m1 = args(1).sparse_matrix_value ();
              retval = binmap<double> (m0, m1, ::atan2, "atan2");
            }
          else
            {
              NDArray a0 = args(0).array_value ();
              NDArray a1 = args(1).array_value ();
              retval = binmap<double> (a0, a1, ::atan2, "atan2");
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (size (atan2 (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (atan2 (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (atan2 (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (atan2 (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (atan2 (1, 2)), [1, 1])

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! y = [0, rt3, 1, rt3, -rt3, -1, -rt3, 0];
%! x = [1, 3, 1, 1, 1, 1, 3, 1];
%! assert (atan2 (y, x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = single ([0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0]);
%! y = single ([0, rt3, 1, rt3, -rt3, -1, -rt3, 0]);
%! x = single ([1, 3, 1, 1, 1, 1, 3, 1]);
%! assert (atan2 (y, x), v, sqrt (eps ("single")));

## Test sparse implementations
%!shared xs
%! xs = sparse (0:3);
%!test
%! y = atan2 (1, xs);
%! assert (issparse (y), false);
%! assert (nnz (y), 4);
%! assert (y, atan2 (1, 0:3));
%!test
%! y = atan2 (0, xs);
%! assert (issparse (y), false);
%! assert (nnz (y), 0);
%! assert (y, zeros (1,4));
%!test
%! y = atan2 (xs, 1);
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, 1)));
%!test
%! y = atan2 (xs, 0);
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, 0)));
%!test
%! y = atan2 (xs, sparse (ones (1, 4)));
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, ones (1,4))));
%!test
%! y = atan2 (xs, sparse (zeros (1,4)));
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, zeros (1,4))));

%!error atan2 ()
%!error atan2 (1, 2, 3)
*/


static octave_value
do_hypot (const octave_value& x, const octave_value& y)
{
  octave_value retval;

  octave_value arg0 = x;
  octave_value arg1 = y;
  if (! arg0.is_numeric_type ())
    gripe_wrong_type_arg ("hypot", arg0);
  else if (! arg1.is_numeric_type ())
    gripe_wrong_type_arg ("hypot", arg1);
  else
    {
      if (arg0.is_complex_type ())
        arg0 = arg0.abs ();
      if (arg1.is_complex_type ())
        arg1 = arg1.abs ();

      if (arg0.is_single_type () || arg1.is_single_type ())
        {
          if (arg0.is_scalar_type () && arg1.is_scalar_type ())
            retval = hypotf (arg0.float_value (), arg1.float_value ());
          else
            {
              FloatNDArray a0 = arg0.float_array_value ();
              FloatNDArray a1 = arg1.float_array_value ();
              retval = binmap<float> (a0, a1, ::hypotf, "hypot");
            }
        }
      else
        {
          if (arg0.is_scalar_type () && arg1.is_scalar_type ())
            retval = hypot (arg0.scalar_value (), arg1.scalar_value ());
          else if (arg0.is_sparse_type () || arg1.is_sparse_type ())
            {
              SparseMatrix m0 = arg0.sparse_matrix_value ();
              SparseMatrix m1 = arg1.sparse_matrix_value ();
              retval = binmap<double> (m0, m1, ::hypot, "hypot");
            }
          else
            {
              NDArray a0 = arg0.array_value ();
              NDArray a1 = arg1.array_value ();
              retval = binmap<double> (a0, a1, ::hypot, "hypot");
            }
        }
    }

  return retval;
}

DEFUN (hypot, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} hypot (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {} hypot (@var{x}, @var{y}, @var{z}, @dots{})\n\
Compute the element-by-element square root of the sum of the squares of\n\
@var{x} and @var{y}.\n\
\n\
This is equivalent to\n\
@code{sqrt (@var{x}.^2 + @var{y}.^2)}, but is calculated in a manner that\n\
avoids overflows for large values of @var{x} or @var{y}.\n\
\n\
@code{hypot} can also be called with more than 2 arguments; in this case,\n\
the arguments are accumulated from left to right:\n\
\n\
@example\n\
@group\n\
hypot (hypot (@var{x}, @var{y}), @var{z})\n\
hypot (hypot (hypot (@var{x}, @var{y}), @var{z}), @var{w}), etc.\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      retval = do_hypot (args(0), args(1));
    }
  else if (nargin >= 3)
    {
      retval = args(0);
      for (int i = 1; i < nargin && ! error_state; i++)
        retval = do_hypot (retval, args(i));
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (size (hypot (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (hypot (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (hypot (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (hypot (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (hypot (1, 2)), [1, 1])
%!assert (hypot (1:10, 1:10), sqrt (2) * [1:10], 16*eps)
%!assert (hypot (single (1:10), single (1:10)), single (sqrt (2) * [1:10]))

## Test sparse implementations
%!shared xs
%! xs = sparse (0:3);
%!test
%! y = hypot (1, xs);
%! assert (nnz (y), 4);
%! assert (y, sparse (hypot (1, 0:3)));
%!test
%! y = hypot (0, xs);
%! assert (nnz (y), 3);
%! assert (y, xs);
%!test
%! y = hypot (xs, 1);
%! assert (nnz (y), 4);
%! assert (y, sparse (hypot (0:3, 1)));
%!test
%! y = hypot (xs, 0);
%! assert (nnz (y), 3);
%! assert (y, xs);
%!test
%! y = hypot (sparse ([0 0]), sparse ([0 1]));
%! assert (nnz (y), 1);
%! assert (y, sparse ([0 1]));
%!test
%! y = hypot (sparse ([0 1]), sparse ([0 0]));
%! assert (nnz (y), 1);
%! assert (y, sparse ([0 1]));

*/

template<typename T, typename ET>
void
map_2_xlog2 (const Array<T>& x, Array<T>& f, Array<ET>& e)
{
  f = Array<T>(x.dims ());
  e = Array<ET>(x.dims ());
  for (octave_idx_type i = 0; i < x.numel (); i++)
    {
      int exp;
      f.xelem (i) = xlog2 (x(i), exp);
      e.xelem (i) = exp;
    }
}

DEFUN (log2, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} log2 (@var{x})\n\
@deftypefnx {Mapping Function} {[@var{f}, @var{e}] =} log2 (@var{x})\n\
Compute the base-2 logarithm of each element of @var{x}.\n\
\n\
If called with two output arguments, split @var{x} into\n\
binary mantissa and exponent so that\n\
@tex\n\
${1 \\over 2} \\le \\left| f \\right| < 1$\n\
@end tex\n\
@ifnottex\n\
@w{@code{1/2 <= abs(f) < 1}}\n\
@end ifnottex\n\
and @var{e} is an integer.  If\n\
@tex\n\
$x = 0$, $f = e = 0$.\n\
@end tex\n\
@ifnottex\n\
@w{@code{x = 0}}, @w{@code{f = e = 0}}.\n\
@end ifnottex\n\
@seealso{pow2, log, log10, exp}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      if (nargout < 2)
        retval(0) = args(0).log2 ();
      else if (args(0).is_single_type ())
        {
          if (args(0).is_real_type ())
            {
              FloatNDArray f;
              FloatNDArray x = args(0).float_array_value ();
              // FIXME: should E be an int value?
              FloatMatrix e;
              map_2_xlog2 (x, f, e);
              retval(1) = e;
              retval(0) = f;
            }
          else if (args(0).is_complex_type ())
            {
              FloatComplexNDArray f;
              FloatComplexNDArray x = args(0).float_complex_array_value ();
              // FIXME: should E be an int value?
              FloatNDArray e;
              map_2_xlog2 (x, f, e);
              retval(1) = e;
              retval(0) = f;
            }
        }
      else if (args(0).is_real_type ())
        {
          NDArray f;
          NDArray x = args(0).array_value ();
          // FIXME: should E be an int value?
          Matrix e;
          map_2_xlog2 (x, f, e);
          retval(1) = e;
          retval(0) = f;
        }
      else if (args(0).is_complex_type ())
        {
          ComplexNDArray f;
          ComplexNDArray x = args(0).complex_array_value ();
          // FIXME: should E be an int value?
          NDArray e;
          map_2_xlog2 (x, f, e);
          retval(1) = e;
          retval(0) = f;
        }
      else
        gripe_wrong_type_arg ("log2", args(0));
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (log2 ([1/4, 1/2, 1, 2, 4]), [-2, -1, 0, 1, 2])
%!assert (log2 (Inf), Inf)
%!assert (isnan (log2 (NaN)))
%!assert (log2 (4*i), 2 + log2 (1*i))
%!assert (log2 (complex (0,Inf)), Inf + log2 (i))

%!test
%! [f, e] = log2 ([0,-1; 2,-4; Inf,-Inf]);
%! assert (f, [0,-0.5; 0.5,-0.5; Inf,-Inf]);
%! assert (e(1:2,:), [0,1;2,3]);

%!test
%! [f, e] = log2 (complex (zeros (3, 2), [0,-1; 2,-4; Inf,-Inf]));
%! assert (f, complex (zeros (3, 2), [0,-0.5; 0.5,-0.5; Inf,-Inf]));
%! assert (e(1:2,:), [0,1; 2,3]);

# bug #42583
%!assert (all (log2 (pow2 (-1074:1023)) == -1074:1023))
*/

DEFUN (rem, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} rem (@var{x}, @var{y})\n\
Return the remainder of the division @code{@var{x} / @var{y}}.\n\
\n\
The remainder is computed using the expression\n\
\n\
@example\n\
x - y .* fix (x ./ y)\n\
@end example\n\
\n\
An error message is printed if the dimensions of the arguments do not agree,\n\
or if either of the arguments is complex.\n\
@seealso{mod}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (! args(0).is_numeric_type ())
        gripe_wrong_type_arg ("rem", args(0));
      else if (! args(1).is_numeric_type ())
        gripe_wrong_type_arg ("rem", args(1));
      else if (args(0).is_complex_type () || args(1).is_complex_type ())
        error ("rem: not defined for complex numbers");
      else if (args(0).is_integer_type () || args(1).is_integer_type ())
        {
          builtin_type_t btyp0 = args(0).builtin_type ();
          builtin_type_t btyp1 = args(1).builtin_type ();
          if (btyp0 == btyp_double || btyp0 == btyp_float)
            btyp0 = btyp1;
          if (btyp1 == btyp_double || btyp1 == btyp_float)
            btyp1 = btyp0;

          if (btyp0 == btyp1)
            {
              switch (btyp0)
                {
#define MAKE_INT_BRANCH(X) \
                case btyp_ ## X: \
                    { \
                    X##NDArray a0 = args(0).X##_array_value (); \
                    X##NDArray a1 = args(1).X##_array_value (); \
                    retval = binmap<octave_##X,octave_##X,octave_##X> (a0, a1, rem, "rem"); \
                    } \
                  break;
                MAKE_INT_BRANCH (int8);
                MAKE_INT_BRANCH (int16);
                MAKE_INT_BRANCH (int32);
                MAKE_INT_BRANCH (int64);
                MAKE_INT_BRANCH (uint8);
                MAKE_INT_BRANCH (uint16);
                MAKE_INT_BRANCH (uint32);
                MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH
                default:
                  panic_impossible ();
                }
            }
          else
            error ("rem: cannot combine %s and %d",
                   args(0).class_name ().c_str (),
                   args(1).class_name ().c_str ());
        }
      else if (args(0).is_single_type () || args(1).is_single_type ())
        {
          if (args(0).is_scalar_type () && args(1).is_scalar_type ())
            retval = xrem (args(0).float_value (), args(1).float_value ());
          else
            {
              FloatNDArray a0 = args(0).float_array_value ();
              FloatNDArray a1 = args(1).float_array_value ();
              retval = binmap<float> (a0, a1, xrem<float>, "rem");
            }
        }
      else
        {
          if (args(0).is_scalar_type () && args(1).is_scalar_type ())
            retval = xrem (args(0).scalar_value (), args(1).scalar_value ());
          else if (args(0).is_sparse_type () || args(1).is_sparse_type ())
            {
              SparseMatrix m0 = args(0).sparse_matrix_value ();
              SparseMatrix m1 = args(1).sparse_matrix_value ();
              retval = binmap<double> (m0, m1, xrem<double>, "rem");
            }
          else
            {
              NDArray a0 = args(0).array_value ();
              NDArray a1 = args(1).array_value ();
              retval = binmap<double> (a0, a1, xrem<double>, "rem");
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (size (rem (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (rem (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (rem (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (rem (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (rem (1, 2)), [1, 1])

%!assert (rem ([1, 2, 3; -1, -2, -3], 2), [1, 0, 1; -1, 0, -1])
%!assert (rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3)),[1, 0, 1; -1, 0, -1])
%!assert (rem (uint8 ([1, 2, 3; -1, -2, -3]), uint8 (2)), uint8 ([1, 0, 1; -1, 0, -1]))
%!assert (uint8 (rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3))),uint8 ([1, 0, 1; -1, 0, -1]))

## Test sparse implementations
%!shared xs
%! xs = sparse (0:3);
%!test
%! y = rem (11, xs);
%! assert (nnz (y), 3);
%! assert (y, sparse (rem (11, 0:3)));
%!test
%! y = rem (0, xs);
%! assert (nnz (y), 0);
%! assert (y, sparse (zeros (1,4)));
%!test
%! y = rem (xs, 2);
%! assert (nnz (y), 2);
%! assert (y, sparse (rem (0:3, 2)));
%!test
%! y = rem (xs, 1);
%! assert (nnz (y), 0);
%! assert (y, sparse (rem (0:3, 1)));
%!test
%! y = rem (sparse ([11 11 11 11]), xs);
%! assert (nnz (y), 3);
%! assert (y, sparse (rem (11, 0:3)));
%!test
%! y = rem (sparse ([0 0 0 0]), xs);
%! assert (nnz (y), 0);
%! assert (y, sparse (zeros (1,4)));

%!error rem (uint (8), int8 (5))
%!error rem (uint8 ([1, 2]), uint8 ([3, 4, 5]))
%!error rem ()
%!error rem (1, 2, 3)
%!error rem ([1, 2], [3, 4, 5])
%!error rem (i, 1)

# bug 42627
%!assert (rem (0.94, 0.01), 0.0);
*/

DEFUN (mod, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} mod (@var{x}, @var{y})\n\
Compute the modulo of @var{x} and @var{y}.\n\
\n\
Conceptually this is given by\n\
\n\
@example\n\
x - y .* floor (x ./ y)\n\
@end example\n\
\n\
@noindent\n\
and is written such that the correct modulus is returned for integer types.\n\
This function handles negative values correctly.  That is,\n\
@code{mod (-1, 3)} is 2, not -1, as @code{rem (-1, 3)} returns.\n\
@code{mod (@var{x}, 0)} returns @var{x}.\n\
\n\
An error results if the dimensions of the arguments do not agree, or if\n\
either of the arguments is complex.\n\
@seealso{rem}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (! args(0).is_numeric_type ())
        gripe_wrong_type_arg ("mod", args(0));
      else if (! args(1).is_numeric_type ())
        gripe_wrong_type_arg ("mod", args(1));
      else if (args(0).is_complex_type () || args(1).is_complex_type ())
        error ("mod: not defined for complex numbers");
      else if (args(0).is_integer_type () || args(1).is_integer_type ())
        {
          builtin_type_t btyp0 = args(0).builtin_type ();
          builtin_type_t btyp1 = args(1).builtin_type ();
          if (btyp0 == btyp_double || btyp0 == btyp_float)
            btyp0 = btyp1;
          if (btyp1 == btyp_double || btyp1 == btyp_float)
            btyp1 = btyp0;

          if (btyp0 == btyp1)
            {
              switch (btyp0)
                {
#define MAKE_INT_BRANCH(X) \
                case btyp_ ## X: \
                    { \
                    X##NDArray a0 = args(0).X##_array_value (); \
                    X##NDArray a1 = args(1).X##_array_value (); \
                    retval = binmap<octave_##X,octave_##X,octave_##X> (a0, a1, mod, "mod"); \
                    } \
                  break;
                MAKE_INT_BRANCH (int8);
                MAKE_INT_BRANCH (int16);
                MAKE_INT_BRANCH (int32);
                MAKE_INT_BRANCH (int64);
                MAKE_INT_BRANCH (uint8);
                MAKE_INT_BRANCH (uint16);
                MAKE_INT_BRANCH (uint32);
                MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH
                default:
                  panic_impossible ();
                }
            }
          else
            error ("mod: cannot combine %s and %d",
                   args(0).class_name ().c_str (),
                   args(1).class_name ().c_str ());
        }
      else if (args(0).is_single_type () || args(1).is_single_type ())
        {
          if (args(0).is_scalar_type () && args(1).is_scalar_type ())
            retval = xmod (args(0).float_value (), args(1).float_value ());
          else
            {
              FloatNDArray a0 = args(0).float_array_value ();
              FloatNDArray a1 = args(1).float_array_value ();
              retval = binmap<float> (a0, a1, xmod<float>, "mod");
            }
        }
      else
        {
          if (args(0).is_scalar_type () && args(1).is_scalar_type ())
            retval = xmod (args(0).scalar_value (), args(1).scalar_value ());
          else if (args(0).is_sparse_type () || args(1).is_sparse_type ())
            {
              SparseMatrix m0 = args(0).sparse_matrix_value ();
              SparseMatrix m1 = args(1).sparse_matrix_value ();
              retval = binmap<double> (m0, m1, xmod<double>, "mod");
            }
          else
            {
              NDArray a0 = args(0).array_value ();
              NDArray a1 = args(1).array_value ();
              retval = binmap<double> (a0, a1, xmod<double>, "mod");
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
## empty input test
%!assert (isempty (mod ([], [])))

## x mod y, y != 0 tests
%!assert (mod (5, 3), 2)
%!assert (mod (-5, 3), 1)
%!assert (mod (0, 3), 0)
%!assert (mod ([-5, 5, 0], [3, 3, 3]), [1, 2, 0])
%!assert (mod ([-5; 5; 0], [3; 3; 3]), [1; 2; 0])
%!assert (mod ([-5, 5; 0, 3], [3, 3 ; 3, 1]), [1, 2 ; 0, 0])

## x mod 0 tests
%!assert (mod (5, 0), 5)
%!assert (mod (-5, 0), -5)
%!assert (mod ([-5, 5, 0], [3, 0, 3]), [1, 5, 0])
%!assert (mod ([-5; 5; 0], [3; 0; 3]), [1; 5; 0])
%!assert (mod ([-5, 5; 0, 3], [3, 0 ; 3, 1]), [1, 5 ; 0, 0])
%!assert (mod ([-5, 5; 0, 3], [0, 0 ; 0, 0]), [-5, 5; 0, 3])

## mixed scalar/matrix tests
%!assert (mod ([-5, 5; 0, 3], 0), [-5, 5; 0, 3])
%!assert (mod ([-5, 5; 0, 3], 3), [1, 2; 0, 0])
%!assert (mod (-5, [0,0; 0,0]), [-5, -5; -5, -5])
%!assert (mod (-5, [3,0; 3,1]), [1, -5; 1, 0])
%!assert (mod (-5, [3,2; 3,1]), [1, 1; 1, 0])

## integer types
%!assert (mod (uint8 (5), uint8 (4)), uint8 (1))
%!assert (mod (uint8 ([1:5]), uint8 (4)), uint8 ([1,2,3,0,1]))
%!assert (mod (uint8 ([1:5]), uint8 (0)), uint8 ([1:5]))
%!error (mod (uint8 (5), int8 (4)))

## mixed integer/real types
%!assert (mod (uint8 (5), 4), uint8 (1))
%!assert (mod (5, uint8 (4)), uint8 (1))
%!assert (mod (uint8 ([1:5]), 4), uint8 ([1,2,3,0,1]))

## non-integer real numbers
%!assert (mod (2.1, 0.1), 0)
%!assert (mod (2.1, 0.2), 0.1, eps)

# bug 42627
%!assert (mod (0.94, 0.01), 0.0);
*/

// FIXME: Need to convert reduction functions of this file for single precision

#define NATIVE_REDUCTION_1(FCN, TYPE, DIM) \
  (arg.is_ ## TYPE ## _type ()) \
    { \
      TYPE ## NDArray tmp = arg. TYPE ##_array_value (); \
      \
      if (! error_state) \
        { \
          retval = tmp.FCN (DIM); \
        } \
    }

#define NATIVE_REDUCTION(FCN, BOOL_FCN) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  bool isnative = false; \
  bool isdouble = false; \
  \
  if (nargin > 1 && args(nargin - 1).is_string ()) \
    { \
      std::string str = args(nargin - 1).string_value (); \
      \
      if (str == "native") \
        isnative = true; \
      else if (str == "double") \
        isdouble = true; \
      else \
        error ("sum: unrecognized string argument"); \
      nargin --; \
    } \
  \
  if (nargin == 1 || nargin == 2) \
    { \
      octave_value arg = args(0); \
 \
      int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1); \
 \
      if (! error_state) \
        { \
          if (dim >= -1) \
            { \
              if (arg.is_sparse_type ()) \
                { \
                  if (arg.is_real_type ()) \
                    { \
                      SparseMatrix tmp = arg.sparse_matrix_value (); \
                      \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else \
                    { \
                      SparseComplexMatrix tmp \
                        = arg.sparse_complex_matrix_value (); \
                      \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                } \
              else \
                { \
                  if (isnative) \
                    { \
                      if NATIVE_REDUCTION_1 (FCN, uint8, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, uint16, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, uint32, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, uint64, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int8, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int16, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int32, dim) \
                      else if NATIVE_REDUCTION_1 (FCN, int64, dim) \
                      else if (arg.is_bool_type ()) \
                        { \
                          boolNDArray tmp = arg.bool_array_value (); \
                          if (! error_state) \
                            retval = boolNDArray (tmp.BOOL_FCN (dim)); \
                        } \
                      else if (arg.is_char_matrix ()) \
                        { \
                          error (#FCN, ": invalid char type"); \
                        } \
                      else if (!isdouble && arg.is_single_type ()) \
                        { \
                          if (arg.is_complex_type ()) \
                            { \
                              FloatComplexNDArray tmp = \
                                arg.float_complex_array_value (); \
                              \
                              if (! error_state) \
                                retval = tmp.FCN (dim); \
                            } \
                          else if (arg.is_real_type ()) \
                            { \
                              FloatNDArray tmp = arg.float_array_value (); \
                              \
                              if (! error_state) \
                                retval = tmp.FCN (dim); \
                            } \
                        } \
                      else if (arg.is_complex_type ()) \
                        { \
                          ComplexNDArray tmp = arg.complex_array_value (); \
                          \
                          if (! error_state) \
                            retval = tmp.FCN (dim); \
                        } \
                      else if (arg.is_real_type ()) \
                        { \
                          NDArray tmp = arg.array_value (); \
                          \
                          if (! error_state) \
                            retval = tmp.FCN (dim); \
                        } \
                      else \
                        { \
                          gripe_wrong_type_arg (#FCN, arg); \
                          return retval; \
                        } \
                    } \
                  else if (arg.is_bool_type ()) \
                    { \
                      boolNDArray tmp = arg.bool_array_value (); \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else if (!isdouble && arg.is_single_type ()) \
                    { \
                      if (arg.is_real_type ()) \
                        { \
                          FloatNDArray tmp = arg.float_array_value (); \
                          \
                          if (! error_state) \
                            retval = tmp.FCN (dim); \
                        } \
                      else if (arg.is_complex_type ()) \
                        { \
                          FloatComplexNDArray tmp = \
                            arg.float_complex_array_value (); \
                          \
                          if (! error_state) \
                            retval = tmp.FCN (dim); \
                        } \
                    } \
                  else if (arg.is_real_type ()) \
                    { \
                      NDArray tmp = arg.array_value (); \
                      \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else if (arg.is_complex_type ()) \
                    { \
                      ComplexNDArray tmp = arg.complex_array_value (); \
                      \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else \
                    { \
                      gripe_wrong_type_arg (#FCN, arg); \
                      return retval; \
                    } \
                } \
            } \
          else \
            error (#FCN ": invalid dimension argument = %d", dim + 1); \
        } \
      \
    } \
  else \
    print_usage (); \
 \
  return retval

#define DATA_REDUCTION(FCN) \
 \
  octave_value retval; \
 \
  int nargin = args.length (); \
 \
  if (nargin == 1 || nargin == 2) \
    { \
      octave_value arg = args(0); \
 \
      int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1); \
 \
      if (! error_state) \
        { \
          if (dim >= -1) \
            { \
              if (arg.is_real_type ()) \
                { \
                  if (arg.is_sparse_type ()) \
                    { \
                      SparseMatrix tmp = arg.sparse_matrix_value (); \
 \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else if (arg.is_single_type ()) \
                    { \
                      FloatNDArray tmp = arg.float_array_value (); \
 \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else \
                    { \
                      NDArray tmp = arg.array_value (); \
 \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                } \
              else if (arg.is_complex_type ()) \
                { \
                  if (arg.is_sparse_type ()) \
                    { \
                      SparseComplexMatrix tmp = arg.sparse_complex_matrix_value (); \
 \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else if (arg.is_single_type ()) \
                    { \
                      FloatComplexNDArray tmp \
                        = arg.float_complex_array_value (); \
 \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                  else \
                    { \
                      ComplexNDArray tmp = arg.complex_array_value (); \
 \
                      if (! error_state) \
                        retval = tmp.FCN (dim); \
                    } \
                } \
              else \
                { \
                  gripe_wrong_type_arg (#FCN, arg); \
                  return retval; \
                } \
            } \
          else \
            error (#FCN ": invalid dimension argument = %d", dim + 1); \
        } \
    } \
  else \
    print_usage (); \
 \
  return retval

DEFUN (cumprod, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} cumprod (@var{x})\n\
@deftypefnx {Built-in Function} {} cumprod (@var{x}, @var{dim})\n\
Cumulative product of elements along dimension @var{dim}.\n\
\n\
If @var{dim} is omitted, it defaults to the first non-singleton dimension.\n\
@seealso{prod, cumsum}\n\
@end deftypefn")
{
  DATA_REDUCTION (cumprod);
}

/*
%!assert (cumprod ([1, 2, 3]), [1, 2, 6])
%!assert (cumprod ([-1; -2; -3]), [-1; 2; -6])
%!assert (cumprod ([i, 2+i, -3+2i, 4]), [i, -1+2i, -1-8i, -4-32i])
%!assert (cumprod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i])

%!assert (cumprod (single ([1, 2, 3])), single ([1, 2, 6]))
%!assert (cumprod (single ([-1; -2; -3])), single ([-1; 2; -6]))
%!assert (cumprod (single ([i, 2+i, -3+2i, 4])), single ([i, -1+2i, -1-8i, -4-32i]))
%!assert (cumprod (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single ([1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i]))

%!assert (cumprod ([2, 3; 4, 5], 1), [2, 3; 8, 15])
%!assert (cumprod ([2, 3; 4, 5], 2), [2, 6; 4, 20])

%!assert (cumprod (single ([2, 3; 4, 5]), 1), single ([2, 3; 8, 15]))
%!assert (cumprod (single ([2, 3; 4, 5]), 2), single ([2, 6; 4, 20]))

%!error cumprod ()
*/

DEFUN (cumsum, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} cumsum (@var{x})\n\
@deftypefnx {Built-in Function} {} cumsum (@var{x}, @var{dim})\n\
@deftypefnx {Built-in Function} {} cumsum (@dots{}, \"native\")\n\
@deftypefnx {Built-in Function} {} cumsum (@dots{}, \"double\")\n\
@deftypefnx {Built-in Function} {} cumsum (@dots{}, \"extra\")\n\
Cumulative sum of elements along dimension @var{dim}.\n\
\n\
If @var{dim} is omitted, it defaults to the first non-singleton dimension.\n\
\n\
See @code{sum} for an explanation of the optional parameters\n\
@qcode{\"native\"}, @qcode{\"double\"}, and @qcode{\"extra\"}.\n\
@seealso{sum, cumprod}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  bool isnative = false;
  bool isdouble = false;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "native")
        isnative = true;
      else if (str == "double")
        isdouble = true;
      else
        error ("cumsum: unrecognized string argument");
      nargin --;
    }

  if (error_state)
    return retval;

  if (nargin == 1 || nargin == 2)
    {
      octave_value arg = args(0);

      int dim = -1;
      if (nargin == 2)
        {
          dim = args(1).int_value () - 1;
          if (dim < 0)
            error ("cumsum: invalid dimension argument = %d", dim + 1);
        }

      if (! error_state)
        {
          switch (arg.builtin_type ())
            {
            case btyp_double:
              if (arg.is_sparse_type ())
                retval = arg.sparse_matrix_value ().cumsum (dim);
              else
                retval = arg.array_value ().cumsum (dim);
              break;
            case btyp_complex:
              if (arg.is_sparse_type ())
                retval = arg.sparse_complex_matrix_value ().cumsum (dim);
              else
                retval = arg.complex_array_value ().cumsum (dim);
              break;
            case btyp_float:
              if (isdouble)
                retval = arg.array_value ().cumsum (dim);
              else
                retval = arg.float_array_value ().cumsum (dim);
              break;
            case btyp_float_complex:
              if (isdouble)
                retval = arg.complex_array_value ().cumsum (dim);
              else
                retval = arg.float_complex_array_value ().cumsum (dim);
              break;

#define MAKE_INT_BRANCH(X) \
            case btyp_ ## X: \
              if (isnative) \
                retval = arg.X ## _array_value ().cumsum (dim); \
              else \
                retval = arg.array_value ().cumsum (dim); \
              break;
            MAKE_INT_BRANCH (int8);
            MAKE_INT_BRANCH (int16);
            MAKE_INT_BRANCH (int32);
            MAKE_INT_BRANCH (int64);
            MAKE_INT_BRANCH (uint8);
            MAKE_INT_BRANCH (uint16);
            MAKE_INT_BRANCH (uint32);
            MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH

            case btyp_bool:
              if (arg.is_sparse_type ())
                {
                  SparseMatrix cs = arg.sparse_matrix_value ().cumsum (dim);
                  if (isnative)
                    retval = cs != 0.0;
                  else
                    retval = cs;
                }
              else
                {
                  NDArray cs = arg.bool_array_value ().cumsum (dim);
                  if (isnative)
                    retval = cs != 0.0;
                  else
                    retval = cs;
                }
              break;

            default:
              gripe_wrong_type_arg ("cumsum", arg);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (cumsum ([1, 2, 3]), [1, 3, 6])
%!assert (cumsum ([-1; -2; -3]), [-1; -3; -6])
%!assert (cumsum ([i, 2+i, -3+2i, 4]), [i, 2+2i, -1+4i, 3+4i])
%!assert (cumsum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i])

%!assert (cumsum (single ([1, 2, 3])), single ([1, 3, 6]))
%!assert (cumsum (single ([-1; -2; -3])), single ([-1; -3; -6]))
%!assert (cumsum (single ([i, 2+i, -3+2i, 4])), single ([i, 2+2i, -1+4i, 3+4i]))
%!assert (cumsum (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single ([1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i]))

%!assert (cumsum ([1, 2; 3, 4], 1), [1, 2; 4, 6])
%!assert (cumsum ([1, 2; 3, 4], 2), [1, 3; 3, 7])

%!assert (cumsum (single ([1, 2; 3, 4]), 1), single ([1, 2; 4, 6]))
%!assert (cumsum (single ([1, 2; 3, 4]), 2), single ([1, 3; 3, 7]))

%!error cumsum ()
*/

DEFUN (diag, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{M} =} diag (@var{v})\n\
@deftypefnx {Built-in Function} {@var{M} =} diag (@var{v}, @var{k})\n\
@deftypefnx {Built-in Function} {@var{M} =} diag (@var{v}, @var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {@var{v} =} diag (@var{M})\n\
@deftypefnx {Built-in Function} {@var{v} =} diag (@var{M}, @var{k})\n\
Return a diagonal matrix with vector @var{v} on diagonal @var{k}.\n\
\n\
The second argument is optional.  If it is positive, the vector is placed on\n\
the @var{k}-th superdiagonal.  If it is negative, it is placed on the\n\
@var{-k}-th subdiagonal.  The default value of @var{k} is 0, and the vector\n\
is placed on the main diagonal.  For example:\n\
\n\
@example\n\
@group\n\
diag ([1, 2, 3], 1)\n\
   @result{}  0  1  0  0\n\
       0  0  2  0\n\
       0  0  0  3\n\
       0  0  0  0\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
The 3-input form returns a diagonal matrix with vector @var{v} on the main\n\
diagonal and the resulting matrix being of size @var{m} rows x @var{n}\n\
columns.\n\
\n\
Given a matrix argument, instead of a vector, @code{diag} extracts the\n\
@var{k}-th diagonal of the matrix.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).diag ();
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    {
      octave_idx_type k = args(1).int_value ();

      if (error_state)
        error ("diag: invalid argument K");
      else
        retval = args(0).diag (k);
    }
  else if (nargin == 3)
    {
      octave_value arg0 = args(0);

      if (arg0.ndims () == 2 && (arg0.rows () == 1 || arg0.columns () == 1))
        {
          octave_idx_type m = args(1).int_value ();
          octave_idx_type n = args(2).int_value ();

          if (! error_state)
            retval = arg0.diag (m, n);
          else
            error ("diag: invalid dimensions");
        }
      else
        error ("diag: V must be a vector");
    }
  else
    print_usage ();

  return retval;
}

/*

%!assert (full (diag ([1; 2; 3])), [1, 0, 0; 0, 2, 0; 0, 0, 3])
%!assert (diag ([1; 2; 3], 1), [0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0])
%!assert (diag ([1; 2; 3], 2), [0, 0, 1, 0, 0; 0, 0, 0, 2, 0; 0, 0, 0, 0, 3; 0, 0, 0, 0, 0; 0, 0, 0, 0, 0])
%!assert (diag ([1; 2; 3],-1), [0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0])
%!assert (diag ([1; 2; 3],-2), [0, 0, 0, 0, 0; 0, 0, 0, 0, 0; 1, 0, 0, 0, 0; 0, 2, 0, 0, 0; 0, 0, 3, 0, 0])

%!assert (diag ([1, 0, 0; 0, 2, 0; 0, 0, 3]), [1; 2; 3])
%!assert (diag ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0], 1), [1; 2; 3])
%!assert (diag ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0], -1), [1; 2; 3])
%!assert (diag (ones (1, 0), 2), zeros (2))
%!assert (diag (1:3, 4, 2), [1, 0; 0, 2; 0, 0; 0, 0])

%!assert (full (diag (single ([1; 2; 3]))), single ([1, 0, 0; 0, 2, 0; 0, 0, 3]))
%!assert (diag (single ([1; 2; 3]), 1), single ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]))
%!assert (diag (single ([1; 2; 3]), 2), single ([0, 0, 1, 0, 0; 0, 0, 0, 2, 0; 0, 0, 0, 0, 3; 0, 0, 0, 0, 0; 0, 0, 0, 0, 0]))
%!assert (diag (single ([1; 2; 3]),-1), single ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]))
%!assert (diag (single ([1; 2; 3]),-2), single ([0, 0, 0, 0, 0; 0, 0, 0, 0, 0; 1, 0, 0, 0, 0; 0, 2, 0, 0, 0; 0, 0, 3, 0, 0]))

%!assert (diag (single ([1, 0, 0; 0, 2, 0; 0, 0, 3])), single ([1; 2; 3]))
%!assert (diag (single ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]), 1), single ([1; 2; 3]))
%!assert (diag (single ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]), -1), single ([1; 2; 3]))

%!assert (diag (int8 ([1; 2; 3])), int8 ([1, 0, 0; 0, 2, 0; 0, 0, 3]))
%!assert (diag (int8 ([1; 2; 3]), 1), int8 ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]))
%!assert (diag (int8 ([1; 2; 3]), 2), int8 ([0, 0, 1, 0, 0; 0, 0, 0, 2, 0; 0, 0, 0, 0, 3; 0, 0, 0, 0, 0; 0, 0, 0, 0, 0]))
%!assert (diag (int8 ([1; 2; 3]),-1), int8 ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]))
%!assert (diag (int8 ([1; 2; 3]),-2), int8 ([0, 0, 0, 0, 0; 0, 0, 0, 0, 0; 1, 0, 0, 0, 0; 0, 2, 0, 0, 0; 0, 0, 3, 0, 0]))

%!assert (diag (int8 ([1, 0, 0; 0, 2, 0; 0, 0, 3])), int8 ([1; 2; 3]))
%!assert (diag (int8 ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]), 1), int8 ([1; 2; 3]))
%!assert (diag (int8 ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]), -1), int8 ([1; 2; 3]))

## bug #37411
%!assert (diag (diag ([5, 2, 3])(:,1)), diag([5 0 0 ]))
%!assert (diag (diag ([5, 2, 3])(:,1), 2),  [0 0 5 0 0; zeros(4, 5)])
%!assert (diag (diag ([5, 2, 3])(:,1), -2), [[0 0 5 0 0]', zeros(5, 4)])

## Test non-square size
%!assert (diag ([1,2,3], 6, 3), [1 0 0; 0 2 0; 0 0 3; 0 0 0; 0 0 0; 0 0 0])
%!assert (diag (1, 2, 3), [1,0,0; 0,0,0]);
%!assert (diag ({1}, 2, 3), {1,[],[]; [],[],[]});
%!assert (diag ({1,2}, 3, 4), {1,[],[],[]; [],2,[],[]; [],[],[],[]});

## Test out-of-range diagonals
%!assert (diag (ones (3,3), 4), zeros (0, 1))
%!assert (diag (cell (3,3), 4), cell (0, 1))
%!assert (diag (sparse (ones (3,3)), 4), sparse (zeros (0, 1)))

%% Test input validation
%!error <Invalid call to diag> diag ()
%!error <Invalid call to diag> diag (1,2,3,4)
%!error diag (ones (2), 3, 3)
%!error diag (1:3, -4, 3)

%!assert (diag (1, 3, 3), diag ([1, 0, 0]))
%!assert (diag (i, 3, 3), diag ([i, 0, 0]))
%!assert (diag (single (1), 3, 3), diag ([single(1), 0, 0]))
%!assert (diag (single (i), 3, 3), diag ([single(i), 0, 0]))
%!assert (diag ([1, 2], 3, 3), diag ([1, 2, 0]))
%!assert (diag ([1, 2]*i, 3, 3), diag ([1, 2, 0]*i))
%!assert (diag (single ([1, 2]), 3, 3), diag (single ([1, 2, 0])))
%!assert (diag (single ([1, 2]*i), 3, 3), diag (single ([1, 2, 0]*i)))
*/

DEFUN (prod, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} prod (@var{x})\n\
@deftypefnx {Built-in Function} {} prod (@var{x}, @var{dim})\n\
@deftypefnx {Built-in Function} {} prod (@dots{}, \"native\")\n\
@deftypefnx {Built-in Function} {} prod (@dots{}, \"double\")\n\
Product of elements along dimension @var{dim}.\n\
\n\
If @var{dim} is omitted, it defaults to the first non-singleton dimension.\n\
\n\
The optional @qcode{\"type\"} input determines the class of the variable\n\
used for calculations.  If the argument @qcode{\"native\"} is given, then\n\
the operation is performed in the same type as the original argument, rather\n\
than the default double type.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
prod ([true, true])\n\
   @result{} 1\n\
prod ([true, true], \"native\")\n\
   @result{} true\n\
@end group\n\
@end example\n\
\n\
On the contrary, if @qcode{\"double\"} is given, the operation is performed\n\
in double precision even for single precision inputs.\n\
@seealso{cumprod, sum}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  bool isnative = false;
  bool isdouble = false;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "native")
        isnative = true;
      else if (str == "double")
        isdouble = true;
      else
        error ("prod: unrecognized type argument '%s'", str.c_str ());
      nargin --;
    }

  if (error_state)
    return retval;

  if (nargin == 1 || nargin == 2)
    {
      octave_value arg = args(0);

      int dim = -1;
      if (nargin == 2)
        {
          dim = args(1).int_value () - 1;
          if (dim < 0)
            error ("prod: invalid dimension DIM = %d", dim + 1);
        }

      if (! error_state)
        {
          switch (arg.builtin_type ())
            {
            case btyp_double:
              if (arg.is_sparse_type ())
                retval = arg.sparse_matrix_value ().prod (dim);
              else
                retval = arg.array_value ().prod (dim);
              break;
            case btyp_complex:
              if (arg.is_sparse_type ())
                retval = arg.sparse_complex_matrix_value ().prod (dim);
              else
                retval = arg.complex_array_value ().prod (dim);
              break;
            case btyp_float:
              if (isdouble)
                retval = arg.float_array_value ().dprod (dim);
              else
                retval = arg.float_array_value ().prod (dim);
              break;
            case btyp_float_complex:
              if (isdouble)
                retval = arg.float_complex_array_value ().dprod (dim);
              else
                retval = arg.float_complex_array_value ().prod (dim);
              break;

#define MAKE_INT_BRANCH(X) \
            case btyp_ ## X: \
              if (isnative) \
                retval = arg.X ## _array_value ().prod (dim); \
              else \
                retval = arg.array_value ().prod (dim); \
              break;
            MAKE_INT_BRANCH (int8);
            MAKE_INT_BRANCH (int16);
            MAKE_INT_BRANCH (int32);
            MAKE_INT_BRANCH (int64);
            MAKE_INT_BRANCH (uint8);
            MAKE_INT_BRANCH (uint16);
            MAKE_INT_BRANCH (uint32);
            MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH

            // GAGME: Accursed Matlab compatibility...
            case btyp_char:
              retval = arg.array_value (true).prod (dim);
              break;
            case btyp_bool:
              if (arg.is_sparse_type ())
                {
                  if (isnative)
                    retval = arg.sparse_bool_matrix_value ().all (dim);
                  else
                    retval = arg.sparse_matrix_value ().prod (dim);
                }
              else if (isnative)
                retval = arg.bool_array_value ().all (dim);
              else
                retval = NDArray (arg.bool_array_value ().all (dim));
              break;

            default:
              gripe_wrong_type_arg ("prod", arg);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (prod ([1, 2, 3]), 6)
%!assert (prod ([-1; -2; -3]), -6)
%!assert (prod ([i, 2+i, -3+2i, 4]), -4 - 32i)
%!assert (prod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [-1+i, -8+8i, -27+27i])

%!assert (prod (single ([1, 2, 3])), single (6))
%!assert (prod (single ([-1; -2; -3])), single (-6))
%!assert (prod (single ([i, 2+i, -3+2i, 4])), single (-4 - 32i))
%!assert (prod (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single ([-1+i, -8+8i, -27+27i]))

%% Test sparse
%!assert (prod (sparse ([1, 2, 3])), sparse (6))
%!assert (prod (sparse ([-1; -2; -3])), sparse (-6))
## Commented out until bug #42290 is fixed
#%!assert (prod (sparse ([i, 2+i, -3+2i, 4])), sparse (-4 - 32i))
#%!assert (prod (sparse ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), sparse ([-1+i, -8+8i, -27+27i]))

%!assert (prod ([1, 2; 3, 4], 1), [3, 8])
%!assert (prod ([1, 2; 3, 4], 2), [2; 12])
%!assert (prod (zeros (1, 0)), 1)
%!assert (prod (zeros (1, 0), 1), zeros (1, 0))
%!assert (prod (zeros (1, 0), 2), 1)
%!assert (prod (zeros (0, 1)), 1)
%!assert (prod (zeros (0, 1), 1), 1)
%!assert (prod (zeros (0, 1), 2), zeros (0, 1))
%!assert (prod (zeros (2, 0)), zeros (1, 0))
%!assert (prod (zeros (2, 0), 1), zeros (1, 0))
%!assert (prod (zeros (2, 0), 2), [1; 1])
%!assert (prod (zeros (0, 2)), [1, 1])
%!assert (prod (zeros (0, 2), 1), [1, 1])
%!assert (prod (zeros (0, 2), 2), zeros (0, 1))

%!assert (prod (single ([1, 2; 3, 4]), 1), single ([3, 8]))
%!assert (prod (single ([1, 2; 3, 4]), 2), single ([2; 12]))
%!assert (prod (zeros (1, 0, "single")), single (1))
%!assert (prod (zeros (1, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (prod (zeros (1, 0, "single"), 2), single (1))
%!assert (prod (zeros (0, 1, "single")), single (1))
%!assert (prod (zeros (0, 1, "single"), 1), single (1))
%!assert (prod (zeros (0, 1, "single"), 2), zeros (0, 1, "single"))
%!assert (prod (zeros (2, 0, "single")), zeros (1, 0, "single"))
%!assert (prod (zeros (2, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (prod (zeros (2, 0, "single"), 2), single ([1; 1]))
%!assert (prod (zeros (0, 2, "single")), single ([1, 1]))
%!assert (prod (zeros (0, 2, "single"), 1), single ([1, 1]))
%!assert (prod (zeros (0, 2, "single"), 2), zeros (0, 1, "single"))

%% Test "double" type argument
%!assert (prod (single ([1, 2, 3]), "double"), 6)
%!assert (prod (single ([-1; -2; -3]), "double"), -6)
%!assert (prod (single ([i, 2+i, -3+2i, 4]), "double"), -4 - 32i)
%!assert (prod (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), "double"), [-1+i, -8+8i, -27+27i])

%% Test "native" type argument
%!assert (prod (uint8 ([1, 2, 3]), "native"), uint8 (6))
%!assert (prod (uint8 ([-1; -2; -3]), "native"), uint8 (0))
%!assert (prod (int8 ([1, 2, 3]), "native"), int8 (6))
%!assert (prod (int8 ([-1; -2; -3]), "native"), int8 (-6))
%!assert (prod ([true false; true true], "native"), [true false])
%!assert (prod ([true false; true true], 2, "native"), [false; true])

%% Test input validation
%!error prod ()
%!error prod (1,2,3)
%!error <unrecognized type argument 'foobar'> prod (1, "foobar")
*/

static bool
all_scalar_1x1 (const octave_value_list& args)
{
  int n_args = args.length ();
  for (int i = 0; i < n_args; i++)
    if (args(i).numel () != 1)
      return false;

  return true;
}

template <class TYPE, class T>
static void
single_type_concat (Array<T>& result,
                    const octave_value_list& args,
                    int dim)
{
  int n_args = args.length ();
  if (! (equal_types<T, char>::value
         || equal_types<T, octave_value>::value)
      && all_scalar_1x1 (args))
    {
      // Optimize all scalars case.
      dim_vector dv (1, 1);
      if (dim == -1 || dim == -2)
        dim = -dim - 1;
      else if (dim >= 2)
        dv.resize (dim+1, 1);
      dv(dim) = n_args;

      result.clear (dv);

      for (int j = 0; j < n_args && ! error_state; j++)
        {
          octave_quit ();

          result(j) = octave_value_extract<T> (args(j));
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (Array<T>, array_list, n_args);

      for (int j = 0; j < n_args && ! error_state; j++)
        {
          octave_quit ();

          array_list[j] = octave_value_extract<TYPE> (args(j));
        }

      if (! error_state)
        result = Array<T>::cat (dim, n_args, array_list);
    }
}

template <class TYPE, class T>
static void
single_type_concat (Sparse<T>& result,
                    const octave_value_list& args,
                    int dim)
{
  int n_args = args.length ();
  OCTAVE_LOCAL_BUFFER (Sparse<T>, sparse_list, n_args);

  for (int j = 0; j < n_args && ! error_state; j++)
    {
      octave_quit ();

      sparse_list[j] = octave_value_extract<TYPE> (args(j));
    }

  if (! error_state)
    result = Sparse<T>::cat (dim, n_args, sparse_list);
}

// Dispatcher.
template<class TYPE>
static TYPE
do_single_type_concat (const octave_value_list& args, int dim)
{
  TYPE result;

  single_type_concat<TYPE, typename TYPE::element_type> (result, args, dim);

  return result;
}

template<class MAP>
static void
single_type_concat_map (octave_map& result,
                        const octave_value_list& args,
                        int dim)
{
  int n_args = args.length ();
  OCTAVE_LOCAL_BUFFER (MAP, map_list, n_args);

  for (int j = 0; j < n_args && ! error_state; j++)
    {
      octave_quit ();

      map_list[j] = octave_value_extract<MAP> (args(j));
    }

  if (! error_state)
    result = octave_map::cat (dim, n_args, map_list);
}

static octave_map
do_single_type_concat_map (const octave_value_list& args,
                           int dim)
{
  octave_map result;
  if (all_scalar_1x1 (args)) // optimize all scalars case.
    single_type_concat_map<octave_scalar_map> (result, args, dim);
  else
    single_type_concat_map<octave_map> (result, args, dim);

  return result;
}

static octave_value
attempt_type_conversion (const octave_value& ov, std::string dtype)
{
  octave_value retval;

  // First try to find function in the class of OV that can convert to
  // the dispatch type dtype.  It will have the name of the dispatch
  // type.

  std::string cname = ov.class_name ();

  octave_value fcn = symbol_table::find_method (dtype, cname);

  if (fcn.is_defined ())
    {
      octave_value_list result
        = fcn.do_multi_index_op (1, octave_value_list (1, ov));

      if (! error_state && result.length () > 0)
        retval = result(0);
      else
        error ("conversion from %s to %s failed", dtype.c_str (),
               cname.c_str ());
    }
  else
    {
      // No conversion function available.  Try the constructor for the
      // dispatch type.

      fcn = symbol_table::find_method (dtype, dtype);

      if (fcn.is_defined ())
        {
          octave_value_list result
            = fcn.do_multi_index_op (1, octave_value_list (1, ov));

          if (! error_state && result.length () > 0)
            retval = result(0);
          else
            error ("%s constructor failed for %s argument", dtype.c_str (),
                   cname.c_str ());
        }
      else
        error ("no constructor for %s!", dtype.c_str ());
    }

  return retval;
}

octave_value
do_class_concat (const octave_value_list& ovl, std::string cattype, int dim)
{
  octave_value retval;

  // Get dominant type for list

  std::string dtype = get_dispatch_type (ovl);

  octave_value fcn = symbol_table::find_method (cattype, dtype);

  if (fcn.is_defined ())
    {
      // Have method for dominant type, so call it and let it handle
      // conversions.

      octave_value_list tmp2 = fcn.do_multi_index_op (1, ovl);

      if (! error_state)
        {
          if (tmp2.length () > 0)
            retval = tmp2(0);
          else
            {
              error ("%s/%s method did not return a value",
                     dtype.c_str (), cattype.c_str ());
              goto done;
            }
        }
      else
        goto done;
    }
  else
    {
      // No method for dominant type, so attempt type conversions for
      // all elements that are not of the dominant type, then do the
      // default operation for octave_class values.

      octave_idx_type j = 0;
      octave_idx_type len = ovl.length ();
      octave_value_list tmp (len, octave_value ());
      for (octave_idx_type k = 0; k < len; k++)
        {
          octave_value elt = ovl(k);

          std::string t1_type = elt.class_name ();

          if (t1_type == dtype)
            tmp(j++) = elt;
          else if (elt.is_object () || ! elt.is_empty ())
            {
              tmp(j++) = attempt_type_conversion (elt, dtype);

              if (error_state)
                goto done;
            }
        }

      tmp.resize (j);

      octave_map m = do_single_type_concat_map (tmp, dim);

      std::string cname = tmp(0).class_name ();
      std::list<std::string> parents = tmp(0).parent_class_name_list ();

      retval = octave_value (new octave_class (m, cname, parents));
    }

done:
  return retval;
}

static octave_value
do_cat (const octave_value_list& xargs, int dim, std::string fname)
{
  octave_value retval;

  // We may need to convert elements of the list to cells, so make a
  // copy.  This should be efficient, it is done mostly by incrementing
  // reference counts.
  octave_value_list args = xargs;

  int n_args = args.length ();

  if (n_args == 0)
    retval = Matrix ();
  else if (n_args == 1)
    retval = args(0);
  else if (n_args > 1)
    {
      std::string result_type;

      bool all_strings_p = true;
      bool all_sq_strings_p = true;
      bool all_dq_strings_p = true;
      bool all_real_p = true;
      bool all_cmplx_p = true;
      bool any_sparse_p = false;
      bool any_cell_p = false;
      bool any_class_p = false;

      bool first_elem_is_struct = false;

      for (int i = 0; i < n_args; i++)
        {
          if (i == 0)
            {
              result_type = args(i).class_name ();

              first_elem_is_struct = args(i).is_map ();
            }
          else
            result_type = get_concat_class (result_type, args(i).class_name ());

          if (all_strings_p && ! args(i).is_string ())
            all_strings_p = false;
          if (all_sq_strings_p && ! args(i).is_sq_string ())
            all_sq_strings_p = false;
          if (all_dq_strings_p && ! args(i).is_dq_string ())
            all_dq_strings_p = false;
          if (all_real_p && ! args(i).is_real_type ())
            all_real_p = false;
          if (all_cmplx_p && ! (args(i).is_complex_type ()
              || args(i).is_real_type ()))
            all_cmplx_p = false;
          if (!any_sparse_p && args(i).is_sparse_type ())
            any_sparse_p = true;
          if (!any_cell_p && args(i).is_cell ())
            any_cell_p = true;
          if (!any_class_p && args(i).is_object ())
            any_class_p = true;
        }

      if (any_cell_p && ! any_class_p && ! first_elem_is_struct)
        {
          for (int i = 0; i < n_args; i++)
            {
              if (! args(i).is_cell ())
                args(i) = Cell (args(i));
            }
        }

      if (any_class_p)
        {
          retval = do_class_concat (args, fname, dim);
        }
      else if (result_type == "double")
        {
          if (any_sparse_p)
            {
              if (all_real_p)
                retval = do_single_type_concat<SparseMatrix> (args, dim);
              else
                retval = do_single_type_concat<SparseComplexMatrix> (args, dim);
            }
          else
            {
              if (all_real_p)
                retval = do_single_type_concat<NDArray> (args, dim);
              else
                retval = do_single_type_concat<ComplexNDArray> (args, dim);
            }
        }
      else if (result_type == "single")
        {
          if (all_real_p)
            retval = do_single_type_concat<FloatNDArray> (args, dim);
          else
            retval = do_single_type_concat<FloatComplexNDArray> (args, dim);
        }
      else if (result_type == "char")
        {
          char type = all_dq_strings_p ? '"' : '\'';

          if (! all_strings_p)
            gripe_implicit_conversion ("Octave:num-to-str",
                                       "numeric", result_type);
          else
            maybe_warn_string_concat (all_dq_strings_p, all_sq_strings_p);

          charNDArray result = do_single_type_concat<charNDArray> (args, dim);

          retval = octave_value (result, type);
        }
      else if (result_type == "logical")
        {
          if (any_sparse_p)
            retval = do_single_type_concat<SparseBoolMatrix> (args, dim);
          else
            retval = do_single_type_concat<boolNDArray> (args, dim);
        }
      else if (result_type == "int8")
        retval = do_single_type_concat<int8NDArray> (args, dim);
      else if (result_type == "int16")
        retval = do_single_type_concat<int16NDArray> (args, dim);
      else if (result_type == "int32")
        retval = do_single_type_concat<int32NDArray> (args, dim);
      else if (result_type == "int64")
        retval = do_single_type_concat<int64NDArray> (args, dim);
      else if (result_type == "uint8")
        retval = do_single_type_concat<uint8NDArray> (args, dim);
      else if (result_type == "uint16")
        retval = do_single_type_concat<uint16NDArray> (args, dim);
      else if (result_type == "uint32")
        retval = do_single_type_concat<uint32NDArray> (args, dim);
      else if (result_type == "uint64")
        retval = do_single_type_concat<uint64NDArray> (args, dim);
      else if (result_type == "cell")
        retval = do_single_type_concat<Cell> (args, dim);
      else if (result_type == "struct")
        retval = do_single_type_concat_map (args, dim);
      else
        {
          dim_vector  dv = args(0).dims ();

          // Default concatenation.
          bool (dim_vector::*concat_rule) (const dim_vector&, int)
            = &dim_vector::concat;

          if (dim == -1 || dim == -2)
            {
              concat_rule = &dim_vector::hvcat;
              dim = -dim - 1;
            }

          for (int i = 1; i < args.length (); i++)
            {
              if (! (dv.*concat_rule) (args(i).dims (), dim))
                {
                  // Dimensions do not match.
                  error ("cat: dimension mismatch");
                  return retval;
                }
            }

          // The lines below might seem crazy, since we take a copy
          // of the first argument, resize it to be empty and then resize
          // it to be full. This is done since it means that there is no
          // recopying of data, as would happen if we used a single resize.
          // It should be noted that resize operation is also significantly
          // slower than the do_cat_op function, so it makes sense to have
          // an empty matrix and copy all data.
          //
          // We might also start with a empty octave_value using
          //   tmp = octave_value_typeinfo::lookup_type
          //                                (args(1).type_name());
          // and then directly resize. However, for some types there might
          // be some additional setup needed, and so this should be avoided.

          octave_value tmp = args(0);
          tmp = tmp.resize (dim_vector (0,0)).resize (dv);

          if (error_state)
            return retval;

          int dv_len = dv.length ();
          Array<octave_idx_type> ra_idx (dim_vector (dv_len, 1), 0);

          for (int j = 0; j < n_args; j++)
            {
              // Can't fast return here to skip empty matrices as something
              // like cat (1,[],single ([])) must return an empty matrix of
              // the right type.
              tmp = do_cat_op (tmp, args(j), ra_idx);

              if (error_state)
                return retval;

              dim_vector dv_tmp = args(j).dims ();

              if (dim >= dv_len)
                {
                  if (j > 1)
                    error ("%s: indexing error", fname.c_str ());
                  break;
                }
              else
                ra_idx (dim) += (dim < dv_tmp.length () ?
                                 dv_tmp (dim) : 1);
            }
          retval = tmp;
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (horzcat, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} horzcat (@var{array1}, @var{array2}, @dots{}, @var{arrayN})\n\
Return the horizontal concatenation of N-D array objects, @var{array1},\n\
@var{array2}, @dots{}, @var{arrayN} along dimension 2.\n\
\n\
Arrays may also be concatenated horizontally using the syntax for creating\n\
new matrices.  For example:\n\
\n\
@example\n\
@var{hcat} = [ @var{array1}, @var{array2}, @dots{} ]\n\
@end example\n\
@seealso{cat, vertcat}\n\
@end deftypefn")
{
  return do_cat (args, -2, "horzcat");
}

/*
## Test concatenation with all zero matrices
%!assert (horzcat ("", 65*ones (1,10)), "AAAAAAAAAA");
%!assert (horzcat (65*ones (1,10), ""), "AAAAAAAAAA");

%!assert (class (horzcat (int64 (1), int64 (1))), "int64")
%!assert (class (horzcat (int64 (1), int32 (1))), "int64")
%!assert (class (horzcat (int64 (1), int16 (1))), "int64")
%!assert (class (horzcat (int64 (1), int8 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint64 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint32 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint16 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint8 (1))), "int64")
%!assert (class (horzcat (int64 (1), single (1))), "int64")
%!assert (class (horzcat (int64 (1), double (1))), "int64")
%!assert (class (horzcat (int64 (1), cell (1))), "cell")
%!assert (class (horzcat (int64 (1), true)), "int64")
%!assert (class (horzcat (int64 (1), "a")), "char")

%!assert (class (horzcat (int32 (1), int64 (1))), "int32")
%!assert (class (horzcat (int32 (1), int32 (1))), "int32")
%!assert (class (horzcat (int32 (1), int16 (1))), "int32")
%!assert (class (horzcat (int32 (1), int8 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint64 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint32 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint16 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint8 (1))), "int32")
%!assert (class (horzcat (int32 (1), single (1))), "int32")
%!assert (class (horzcat (int32 (1), double (1))), "int32")
%!assert (class (horzcat (int32 (1), cell (1))), "cell")
%!assert (class (horzcat (int32 (1), true)), "int32")
%!assert (class (horzcat (int32 (1), "a")), "char")

%!assert (class (horzcat (int16 (1), int64 (1))), "int16")
%!assert (class (horzcat (int16 (1), int32 (1))), "int16")
%!assert (class (horzcat (int16 (1), int16 (1))), "int16")
%!assert (class (horzcat (int16 (1), int8 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint64 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint32 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint16 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint8 (1))), "int16")
%!assert (class (horzcat (int16 (1), single (1))), "int16")
%!assert (class (horzcat (int16 (1), double (1))), "int16")
%!assert (class (horzcat (int16 (1), cell (1))), "cell")
%!assert (class (horzcat (int16 (1), true)), "int16")
%!assert (class (horzcat (int16 (1), "a")), "char")

%!assert (class (horzcat (int8 (1), int64 (1))), "int8")
%!assert (class (horzcat (int8 (1), int32 (1))), "int8")
%!assert (class (horzcat (int8 (1), int16 (1))), "int8")
%!assert (class (horzcat (int8 (1), int8 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint64 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint32 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint16 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint8 (1))), "int8")
%!assert (class (horzcat (int8 (1), single (1))), "int8")
%!assert (class (horzcat (int8 (1), double (1))), "int8")
%!assert (class (horzcat (int8 (1), cell (1))), "cell")
%!assert (class (horzcat (int8 (1), true)), "int8")
%!assert (class (horzcat (int8 (1), "a")), "char")

%!assert (class (horzcat (uint64 (1), int64 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), int32 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), int16 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), int8 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint64 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint32 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint16 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint8 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), single (1))), "uint64")
%!assert (class (horzcat (uint64 (1), double (1))), "uint64")
%!assert (class (horzcat (uint64 (1), cell (1))), "cell")
%!assert (class (horzcat (uint64 (1), true)), "uint64")
%!assert (class (horzcat (uint64 (1), "a")), "char")

%!assert (class (horzcat (uint32 (1), int64 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), int32 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), int16 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), int8 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint64 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint32 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint16 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint8 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), single (1))), "uint32")
%!assert (class (horzcat (uint32 (1), double (1))), "uint32")
%!assert (class (horzcat (uint32 (1), cell (1))), "cell")
%!assert (class (horzcat (uint32 (1), true)), "uint32")
%!assert (class (horzcat (uint32 (1), "a")), "char")

%!assert (class (horzcat (uint16 (1), int64 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), int32 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), int16 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), int8 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint64 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint32 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint16 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint8 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), single (1))), "uint16")
%!assert (class (horzcat (uint16 (1), double (1))), "uint16")
%!assert (class (horzcat (uint16 (1), cell (1))), "cell")
%!assert (class (horzcat (uint16 (1), true)), "uint16")
%!assert (class (horzcat (uint16 (1), "a")), "char")

%!assert (class (horzcat (uint8 (1), int64 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), int32 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), int16 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), int8 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint64 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint32 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint16 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint8 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), single (1))), "uint8")
%!assert (class (horzcat (uint8 (1), double (1))), "uint8")
%!assert (class (horzcat (uint8 (1), cell (1))), "cell")
%!assert (class (horzcat (uint8 (1), true)), "uint8")
%!assert (class (horzcat (uint8 (1), "a")), "char")

%!assert (class (horzcat (single (1), int64 (1))), "int64")
%!assert (class (horzcat (single (1), int32 (1))), "int32")
%!assert (class (horzcat (single (1), int16 (1))), "int16")
%!assert (class (horzcat (single (1), int8 (1))), "int8")
%!assert (class (horzcat (single (1), uint64 (1))), "uint64")
%!assert (class (horzcat (single (1), uint32 (1))), "uint32")
%!assert (class (horzcat (single (1), uint16 (1))), "uint16")
%!assert (class (horzcat (single (1), uint8 (1))), "uint8")
%!assert (class (horzcat (single (1), single (1))), "single")
%!assert (class (horzcat (single (1), double (1))), "single")
%!assert (class (horzcat (single (1), cell (1))), "cell")
%!assert (class (horzcat (single (1), true)), "single")
%!assert (class (horzcat (single (1), "a")), "char")

%!assert (class (horzcat (double (1), int64 (1))), "int64")
%!assert (class (horzcat (double (1), int32 (1))), "int32")
%!assert (class (horzcat (double (1), int16 (1))), "int16")
%!assert (class (horzcat (double (1), int8 (1))), "int8")
%!assert (class (horzcat (double (1), uint64 (1))), "uint64")
%!assert (class (horzcat (double (1), uint32 (1))), "uint32")
%!assert (class (horzcat (double (1), uint16 (1))), "uint16")
%!assert (class (horzcat (double (1), uint8 (1))), "uint8")
%!assert (class (horzcat (double (1), single (1))), "single")
%!assert (class (horzcat (double (1), double (1))), "double")
%!assert (class (horzcat (double (1), cell (1))), "cell")
%!assert (class (horzcat (double (1), true)), "double")
%!assert (class (horzcat (double (1), "a")), "char")

%!assert (class (horzcat (cell (1), int64 (1))), "cell")
%!assert (class (horzcat (cell (1), int32 (1))), "cell")
%!assert (class (horzcat (cell (1), int16 (1))), "cell")
%!assert (class (horzcat (cell (1), int8 (1))), "cell")
%!assert (class (horzcat (cell (1), uint64 (1))), "cell")
%!assert (class (horzcat (cell (1), uint32 (1))), "cell")
%!assert (class (horzcat (cell (1), uint16 (1))), "cell")
%!assert (class (horzcat (cell (1), uint8 (1))), "cell")
%!assert (class (horzcat (cell (1), single (1))), "cell")
%!assert (class (horzcat (cell (1), double (1))), "cell")
%!assert (class (horzcat (cell (1), cell (1))), "cell")
%!assert (class (horzcat (cell (1), true)), "cell")
%!assert (class (horzcat (cell (1), "a")), "cell")

%!assert (class (horzcat (true, int64 (1))), "int64")
%!assert (class (horzcat (true, int32 (1))), "int32")
%!assert (class (horzcat (true, int16 (1))), "int16")
%!assert (class (horzcat (true, int8 (1))), "int8")
%!assert (class (horzcat (true, uint64 (1))), "uint64")
%!assert (class (horzcat (true, uint32 (1))), "uint32")
%!assert (class (horzcat (true, uint16 (1))), "uint16")
%!assert (class (horzcat (true, uint8 (1))), "uint8")
%!assert (class (horzcat (true, single (1))), "single")
%!assert (class (horzcat (true, double (1))), "double")
%!assert (class (horzcat (true, cell (1))), "cell")
%!assert (class (horzcat (true, true)), "logical")
%!assert (class (horzcat (true, "a")), "char")

%!assert (class (horzcat ("a", int64 (1))), "char")
%!assert (class (horzcat ("a", int32 (1))), "char")
%!assert (class (horzcat ("a", int16 (1))), "char")
%!assert (class (horzcat ("a", int8 (1))), "char")
%!assert (class (horzcat ("a", int64 (1))), "char")
%!assert (class (horzcat ("a", int32 (1))), "char")
%!assert (class (horzcat ("a", int16 (1))), "char")
%!assert (class (horzcat ("a", int8 (1))), "char")
%!assert (class (horzcat ("a", single (1))), "char")
%!assert (class (horzcat ("a", double (1))), "char")
%!assert (class (horzcat ("a", cell (1))), "cell")
%!assert (class (horzcat ("a", true)), "char")
%!assert (class (horzcat ("a", "a")), "char")

%!assert (class (horzcat (cell (1), struct ("foo", "bar"))), "cell")

%!error horzcat (struct ("foo", "bar"), cell (1))
*/

DEFUN (vertcat, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} vertcat (@var{array1}, @var{array2}, @dots{}, @var{arrayN})\n\
Return the vertical concatenation of N-D array objects, @var{array1},\n\
@var{array2}, @dots{}, @var{arrayN} along dimension 1.\n\
\n\
Arrays may also be concatenated vertically using the syntax for creating\n\
new matrices.  For example:\n\
\n\
@example\n\
@var{vcat} = [ @var{array1}; @var{array2}; @dots{} ]\n\
@end example\n\
@seealso{cat, horzcat}\n\
@end deftypefn")
{
  return do_cat (args, -1, "vertcat");
}

/*
%!test
%! c = {"foo"; "bar"; "bazoloa"};
%! assert (vertcat (c, "a", "bc", "def"), {"foo"; "bar"; "bazoloa"; "a"; "bc"; "def"});
*/

DEFUN (cat, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cat (@var{dim}, @var{array1}, @var{array2}, @dots{}, @var{arrayN})\n\
Return the concatenation of N-D array objects, @var{array1},\n\
@var{array2}, @dots{}, @var{arrayN} along dimension @var{dim}.\n\
\n\
@example\n\
@group\n\
A = ones (2, 2);\n\
B = zeros (2, 2);\n\
cat (2, A, B)\n\
  @result{} 1 1 0 0\n\
     1 1 0 0\n\
@end group\n\
@end example\n\
\n\
Alternatively, we can concatenate @var{A} and @var{B} along the\n\
second dimension in the following way:\n\
\n\
@example\n\
@group\n\
[A, B]\n\
@end group\n\
@end example\n\
\n\
@var{dim} can be larger than the dimensions of the N-D array objects\n\
and the result will thus have @var{dim} dimensions as the\n\
following example shows:\n\
\n\
@example\n\
@group\n\
cat (4, ones (2, 2), zeros (2, 2))\n\
  @result{} ans(:,:,1,1) =\n\
\n\
       1 1\n\
       1 1\n\
\n\
     ans(:,:,1,2) =\n\
\n\
       0 0\n\
       0 0\n\
@end group\n\
@end example\n\
@seealso{horzcat, vertcat}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () > 0)
    {
      int dim = args(0).int_value () - 1;

      if (! error_state)
        {
          if (dim >= 0)
            retval = do_cat (args.slice (1, args.length () - 1), dim, "cat");
          else
            error ("cat: DIM must be a valid dimension");
        }
      else
        error ("cat: DIM must be an integer");
    }
  else
    print_usage ();

  return retval;
}

/*
%!function ret = __testcat (t1, t2, tr, cmplx)
%!  assert (cat (1, cast ([], t1), cast ([], t2)), cast ([], tr));
%!
%!  assert (cat (1, cast (1, t1), cast (2, t2)), cast ([1; 2], tr));
%!  assert (cat (1, cast (1, t1), cast ([2; 3], t2)), cast ([1; 2; 3], tr));
%!  assert (cat (1, cast ([1; 2], t1), cast (3, t2)), cast ([1; 2; 3], tr));
%!  assert (cat (1, cast ([1; 2], t1), cast ([3; 4], t2)), cast ([1; 2; 3; 4], tr));
%!  assert (cat (2, cast (1, t1), cast (2, t2)), cast ([1, 2], tr));
%!  assert (cat (2, cast (1, t1), cast ([2, 3], t2)), cast ([1, 2, 3], tr));
%!  assert (cat (2, cast ([1, 2], t1), cast (3, t2)), cast ([1, 2, 3], tr));
%!  assert (cat (2, cast ([1, 2], t1), cast ([3, 4], t2)), cast ([1, 2, 3, 4], tr));
%!
%!  assert ([cast(1, t1); cast(2, t2)], cast ([1; 2], tr));
%!  assert ([cast(1, t1); cast([2; 3], t2)], cast ([1; 2; 3], tr));
%!  assert ([cast([1; 2], t1); cast(3, t2)], cast ([1; 2; 3], tr));
%!  assert ([cast([1; 2], t1); cast([3; 4], t2)], cast ([1; 2; 3; 4], tr));
%!  assert ([cast(1, t1), cast(2, t2)], cast ([1, 2], tr));
%!  assert ([cast(1, t1), cast([2, 3], t2)], cast ([1, 2, 3], tr));
%!  assert ([cast([1, 2], t1), cast(3, t2)], cast ([1, 2, 3], tr));
%!  assert ([cast([1, 2], t1), cast([3, 4], t2)], cast ([1, 2, 3, 4], tr));
%!
%!  if (nargin == 3 || cmplx)
%!    assert (cat (1, cast (1i, t1), cast (2, t2)), cast ([1i; 2], tr));
%!    assert (cat (1, cast (1i, t1), cast ([2; 3], t2)), cast ([1i; 2; 3], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast (3, t2)), cast ([1i; 2; 3], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast ([3; 4], t2)), cast ([1i; 2; 3; 4], tr));
%!    assert (cat (2, cast (1i, t1), cast (2, t2)), cast ([1i, 2], tr));
%!    assert (cat (2, cast (1i, t1), cast ([2, 3], t2)), cast ([1i, 2, 3], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast (3, t2)), cast ([1i, 2, 3], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast ([3, 4], t2)), cast ([1i, 2, 3, 4], tr));
%!
%!    assert ([cast(1i, t1); cast(2, t2)], cast ([1i; 2], tr));
%!    assert ([cast(1i, t1); cast([2; 3], t2)], cast ([1i; 2; 3], tr));
%!    assert ([cast([1i; 2], t1); cast(3, t2)], cast ([1i; 2; 3], tr));
%!    assert ([cast([1i; 2], t1); cast([3; 4], t2)], cast ([1i; 2; 3; 4], tr));
%!    assert ([cast(1i, t1), cast(2, t2)], cast ([1i, 2], tr));
%!    assert ([cast(1i, t1), cast([2, 3], t2)], cast ([1i, 2, 3], tr));
%!    assert ([cast([1i, 2], t1), cast(3, t2)], cast ([1i, 2, 3], tr));
%!    assert ([cast([1i, 2], t1), cast([3, 4], t2)], cast ([1i, 2, 3, 4], tr));
%!
%!    assert (cat (1, cast (1, t1), cast (2i, t2)), cast ([1; 2i], tr));
%!    assert (cat (1, cast (1, t1), cast ([2i; 3], t2)), cast ([1; 2i; 3], tr));
%!    assert (cat (1, cast ([1; 2], t1), cast (3i, t2)), cast ([1; 2; 3i], tr));
%!    assert (cat (1, cast ([1; 2], t1), cast ([3i; 4], t2)), cast ([1; 2; 3i; 4], tr));
%!    assert (cat (2, cast (1, t1), cast (2i, t2)), cast ([1, 2i], tr));
%!    assert (cat (2, cast (1, t1), cast ([2i, 3], t2)), cast ([1, 2i, 3], tr));
%!    assert (cat (2, cast ([1, 2], t1), cast (3i, t2)), cast ([1, 2, 3i], tr));
%!    assert (cat (2, cast ([1, 2], t1), cast ([3i, 4], t2)), cast ([1, 2, 3i, 4], tr));
%!
%!    assert ([cast(1, t1); cast(2i, t2)], cast ([1; 2i], tr));
%!    assert ([cast(1, t1); cast([2i; 3], t2)], cast ([1; 2i; 3], tr));
%!    assert ([cast([1; 2], t1); cast(3i, t2)], cast ([1; 2; 3i], tr));
%!    assert ([cast([1; 2], t1); cast([3i; 4], t2)], cast ([1; 2; 3i; 4], tr));
%!    assert ([cast(1, t1), cast(2i, t2)], cast ([1, 2i], tr));
%!    assert ([cast(1, t1), cast([2i, 3], t2)], cast ([1, 2i, 3], tr));
%!    assert ([cast([1, 2], t1), cast(3i, t2)], cast ([1, 2, 3i], tr));
%!    assert ([cast([1, 2], t1), cast([3i, 4], t2)], cast ([1, 2, 3i, 4], tr));
%!
%!    assert (cat (1, cast (1i, t1), cast (2i, t2)), cast ([1i; 2i], tr));
%!    assert (cat (1, cast (1i, t1), cast ([2i; 3], t2)), cast ([1i; 2i; 3], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast (3i, t2)), cast ([1i; 2; 3i], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast ([3i; 4], t2)), cast ([1i; 2; 3i; 4], tr));
%!    assert (cat (2, cast (1i, t1), cast (2i, t2)), cast ([1i, 2i], tr));
%!    assert (cat (2, cast (1i, t1), cast ([2i, 3], t2)), cast ([1i, 2i, 3], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast (3i, t2)), cast ([1i, 2, 3i], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast ([3i, 4], t2)), cast ([1i, 2, 3i, 4], tr));
%!
%!    assert ([cast(1i, t1); cast(2i, t2)], cast ([1i; 2i], tr));
%!    assert ([cast(1i, t1); cast([2i; 3], t2)], cast ([1i; 2i; 3], tr));
%!    assert ([cast([1i; 2], t1); cast(3i, t2)], cast ([1i; 2; 3i], tr));
%!    assert ([cast([1i; 2], t1); cast([3i; 4], t2)], cast ([1i; 2; 3i; 4], tr));
%!    assert ([cast(1i, t1), cast(2i, t2)], cast ([1i, 2i], tr));
%!    assert ([cast(1i, t1), cast([2i, 3], t2)], cast ([1i, 2i, 3], tr));
%!    assert ([cast([1i, 2], t1), cast(3i, t2)], cast ([1i, 2, 3i], tr));
%!    assert ([cast([1i, 2], t1), cast([3i, 4], t2)], cast ([1i, 2, 3i, 4], tr));
%!  endif
%!  ret = true;
%!endfunction

%!assert (__testcat ("double", "double", "double"))
%!assert (__testcat ("single", "double", "single"))
%!assert (__testcat ("double", "single", "single"))
%!assert (__testcat ("single", "single", "single"))

%!assert (__testcat ("double", "int8", "int8", false))
%!assert (__testcat ("int8", "double", "int8", false))
%!assert (__testcat ("single", "int8", "int8", false))
%!assert (__testcat ("int8", "single", "int8", false))
%!assert (__testcat ("int8", "int8", "int8", false))
%!assert (__testcat ("double", "int16", "int16", false))
%!assert (__testcat ("int16", "double", "int16", false))
%!assert (__testcat ("single", "int16", "int16", false))
%!assert (__testcat ("int16", "single", "int16", false))
%!assert (__testcat ("int16", "int16", "int16", false))
%!assert (__testcat ("double", "int32", "int32", false))
%!assert (__testcat ("int32", "double", "int32", false))
%!assert (__testcat ("single", "int32", "int32", false))
%!assert (__testcat ("int32", "single", "int32", false))
%!assert (__testcat ("int32", "int32", "int32", false))
%!assert (__testcat ("double", "int64", "int64", false))
%!assert (__testcat ("int64", "double", "int64", false))
%!assert (__testcat ("single", "int64", "int64", false))
%!assert (__testcat ("int64", "single", "int64", false))
%!assert (__testcat ("int64", "int64", "int64", false))

%!assert (__testcat ("double", "uint8", "uint8", false))
%!assert (__testcat ("uint8", "double", "uint8", false))
%!assert (__testcat ("single", "uint8", "uint8", false))
%!assert (__testcat ("uint8", "single", "uint8", false))
%!assert (__testcat ("uint8", "uint8", "uint8", false))
%!assert (__testcat ("double", "uint16", "uint16", false))
%!assert (__testcat ("uint16", "double", "uint16", false))
%!assert (__testcat ("single", "uint16", "uint16", false))
%!assert (__testcat ("uint16", "single", "uint16", false))
%!assert (__testcat ("uint16", "uint16", "uint16", false))
%!assert (__testcat ("double", "uint32", "uint32", false))
%!assert (__testcat ("uint32", "double", "uint32", false))
%!assert (__testcat ("single", "uint32", "uint32", false))
%!assert (__testcat ("uint32", "single", "uint32", false))
%!assert (__testcat ("uint32", "uint32", "uint32", false))
%!assert (__testcat ("double", "uint64", "uint64", false))
%!assert (__testcat ("uint64", "double", "uint64", false))
%!assert (__testcat ("single", "uint64", "uint64", false))
%!assert (__testcat ("uint64", "single", "uint64", false))
%!assert (__testcat ("uint64", "uint64", "uint64", false))

%!assert (cat (3, [], [1,2;3,4]), [1,2;3,4])
%!assert (cat (3, [1,2;3,4], []), [1,2;3,4])
%!assert (cat (3, [], [1,2;3,4], []), [1,2;3,4])
%!assert (cat (3, [], [], []), zeros (0, 0, 3))

%!assert (cat (3, [], [], 1, 2), cat (3, 1, 2))
%!assert (cat (3, [], [], [1,2;3,4]), [1,2;3,4])
%!assert (cat (4, [], [], [1,2;3,4]), [1,2;3,4])

%!assert ([zeros(3,2,2); ones(1,2,2)], repmat ([0;0;0;1],[1,2,2]))
%!assert ([zeros(3,2,2); ones(1,2,2)], vertcat (zeros (3,2,2), ones (1,2,2)))

%!error <dimension mismatch> cat (3, cat (3, [], []), [1,2;3,4])
%!error <dimension mismatch> cat (3, zeros (0, 0, 2), [1,2;3,4])
*/

static octave_value
do_permute (const octave_value_list& args, bool inv)
{
  octave_value retval;

  if (args.length () == 2 && args(1).length () >= args(1).ndims ())
    {
      Array<int> vec = args(1).int_vector_value ();

      // FIXME: maybe we should create an idx_vector object
      // here and pass that to permute?

      int n = vec.length ();

      for (int i = 0; i < n; i++)
        vec(i)--;

      octave_value ret = args(0).permute (vec, inv);

      if (! error_state)
        retval = ret;
    }
  else
    print_usage ();

  return retval;
}

DEFUN (permute, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} permute (@var{A}, @var{perm})\n\
Return the generalized transpose for an N-D array object @var{A}.\n\
\n\
The permutation vector @var{perm} must contain the elements\n\
@w{@code{1:ndims (A)}} (in any order, but each element must appear only\n\
once).  The @var{N}th dimension of @var{A} gets remapped to dimension\n\
@code{@var{PERM}(@var{N})}.  For example:\n\
\n\
@example\n\
@group\n\
@var{x} = zeros ([2, 3, 5, 7]);\n\
size (@var{x})\n\
   @result{}  2   3   5   7\n\
\n\
size (permute (@var{x}, [2, 1, 3, 4]))\n\
   @result{}  3   2   5   7\n\
\n\
size (permute (@var{x}, [1, 3, 4, 2]))\n\
   @result{}  2   5   7   3\n\
\n\
## The identity permutation\n\
size (permute (@var{x}, [1, 2, 3, 4]))\n\
   @result{}  2   3   5   7\n\
@end group\n\
@end example\n\
@seealso{ipermute}\n\
@end deftypefn")
{
  return do_permute (args, false);
}

DEFUN (ipermute, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ipermute (@var{A}, @var{iperm})\n\
The inverse of the @code{permute} function.\n\
\n\
The expression\n\
\n\
@example\n\
ipermute (permute (A, perm), perm)\n\
@end example\n\
\n\
@noindent\n\
returns the original array @var{A}.\n\
@seealso{permute}\n\
@end deftypefn")
{
  return do_permute (args, true);
}

DEFUN (length, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} length (@var{a})\n\
Return the length of the object @var{a}.\n\
\n\
The length is 0 for empty objects, 1 for scalars, and the number of elements\n\
for vectors.  For matrix or N-dimensional objects, the length is the number\n\
of elements along the largest dimension\n\
(equivalent to @w{@code{max (size (@var{a}))}}).\n\
@seealso{numel, size}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).length ();
  else
    print_usage ();

  return retval;
}

DEFUN (ndims, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ndims (@var{a})\n\
Return the number of dimensions of @var{a}.\n\
\n\
For any array, the result will always be greater than or equal to 2.\n\
Trailing singleton dimensions are not counted.\n\
\n\
@example\n\
@group\n\
ndims (ones (4, 1, 2, 1))\n\
    @result{} 3\n\
@end group\n\
@end example\n\
@seealso{size}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).ndims ();
  else
    print_usage ();

  return retval;
}

DEFUN (numel, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} numel (@var{a})\n\
@deftypefnx {Built-in Function} {} numel (@var{a}, @var{idx1}, @var{idx2}, @dots{})\n\
Return the number of elements in the object @var{a}.\n\
\n\
Optionally, if indices @var{idx1}, @var{idx2}, @dots{} are supplied,\n\
return the number of elements that would result from the indexing\n\
\n\
@example\n\
@var{a}(@var{idx1}, @var{idx2}, @dots{})\n\
@end example\n\
\n\
Note that the indices do not have to be scalar numbers.  For example,\n\
\n\
@example\n\
@group\n\
@var{a} = 1;\n\
@var{b} = ones (2, 3);\n\
numel (@var{a}, @var{b})\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will return 6, as this is the number of ways to index with @var{b}.\n\
Or the index could be the string @qcode{\":\"} which represents the colon\n\
operator.  For example,\n\
\n\
@example\n\
@group\n\
@var{a} = ones (5, 3);\n\
numel (@var{a}, 2, \":\")\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will return 3 as the second row has three column entries.\n\
\n\
This method is also called when an object appears as lvalue with cs-list\n\
indexing, i.e., @code{object@{@dots{}@}} or @code{object(@dots{}).field}.\n\
@seealso{size}\n\
@end deftypefn")
{
  octave_value retval;
  octave_idx_type nargin = args.length ();

  if (nargin == 1)
    retval = args(0).numel ();
  else if (nargin > 1)
    {
      // Don't use numel (const octave_value_list&) here as that corresponds to
      // an overloaded call, not to builtin!
      retval = dims_to_numel (args(0).dims (), args.slice (1, nargin-1));
    }
  else
    print_usage ();

  return retval;
}

DEFUN (size, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} size (@var{a})\n\
@deftypefnx {Built-in Function} {} size (@var{a}, @var{dim})\n\
Return the number of rows and columns of @var{a}.\n\
\n\
With one input argument and one output argument, the result is returned\n\
in a row vector.  If there are multiple output arguments, the number of\n\
rows is assigned to the first, and the number of columns to the second,\n\
etc.  For example:\n\
\n\
@example\n\
@group\n\
size ([1, 2; 3, 4; 5, 6])\n\
   @result{} [ 3, 2 ]\n\
\n\
[nr, nc] = size ([1, 2; 3, 4; 5, 6])\n\
    @result{} nr = 3\n\
    @result{} nc = 2\n\
@end group\n\
@end example\n\
\n\
If given a second argument, @code{size} will return the size of the\n\
corresponding dimension.  For example,\n\
\n\
@example\n\
@group\n\
size ([1, 2; 3, 4; 5, 6], 2)\n\
    @result{} 2\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
returns the number of columns in the given matrix.\n\
@seealso{numel, ndims, length, rows, columns}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      const dim_vector dimensions = args(0).dims ();

      if (nargout > 1)
        {
          const dim_vector rdims = dimensions.redim (nargout);
          retval.resize (nargout);
          for (int i = 0; i < nargout; i++)
            retval(i) = rdims(i);
        }
      else
        {
          int ndims = dimensions.length ();

          NoAlias<Matrix> m (1, ndims);

          for (int i = 0; i < ndims; i++)
            m(i) = dimensions(i);

          retval(0) = m;
        }
    }
  else if (nargin == 2 && nargout < 2)
    {
      octave_idx_type nd = args(1).int_value (true);

      if (error_state)
        error ("size: DIM must be a scalar");
      else
        {
          const dim_vector dv = args(0).dims ();

          if (nd > 0)
            {
              if (nd <= dv.length ())
                retval(0) = dv(nd-1);
              else
                retval(0) = 1;
            }
          else
            error ("size: requested dimension DIM (= %d) out of range", nd);
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (size_equal, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} size_equal (@var{a}, @var{b}, @dots{})\n\
Return true if the dimensions of all arguments agree.\n\
\n\
Trailing singleton dimensions are ignored.\n\
When called with a single or no argument @code{size_equal} returns true.\n\
@seealso{size, numel, ndims}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = true;

  if (nargin >= 1)
    {
      dim_vector a_dims = args(0).dims ();

      for (int i = 1; i < nargin; ++i)
        {
          dim_vector b_dims = args(i).dims ();

          if (a_dims != b_dims)
            {
              retval = false;
              break;
            }
        }
    }

  return retval;
}

DEFUN (nnz, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{n} =} nnz (@var{a})\n\
Return the number of nonzero elements in @var{a}.\n\
@seealso{nzmax, nonzeros, find}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).nnz ();
  else
    print_usage ();

  return retval;
}

DEFUN (nzmax, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{n} =} nzmax (@var{SM})\n\
Return the amount of storage allocated to the sparse matrix @var{SM}.\n\
\n\
Note that Octave tends to crop unused memory at the first opportunity\n\
for sparse objects.  Thus, in general the value of @code{nzmax} will be the\n\
same as @code{nnz} except for some cases of user-created sparse objects.\n\
@seealso{nnz, spalloc, sparse}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).nzmax ();
  else
    print_usage ();

  return retval;
}

DEFUN (rows, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rows (@var{a})\n\
Return the number of rows of @var{a}.\n\
@seealso{columns, size, length, numel, isscalar, isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).rows ();
  else
    print_usage ();

  return retval;
}

DEFUN (columns, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} columns (@var{a})\n\
Return the number of columns of @var{a}.\n\
@seealso{rows, size, length, numel, isscalar, isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).columns ();
  else
    print_usage ();

  return retval;
}

DEFUN (sum, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} sum (@var{x})\n\
@deftypefnx {Built-in Function} {} sum (@var{x}, @var{dim})\n\
@deftypefnx {Built-in Function} {} sum (@dots{}, \"native\")\n\
@deftypefnx {Built-in Function} {} sum (@dots{}, \"double\")\n\
@deftypefnx {Built-in Function} {} sum (@dots{}, \"extra\")\n\
Sum of elements along dimension @var{dim}.\n\
\n\
If @var{dim} is omitted, it defaults to the first non-singleton dimension.\n\
\n\
The optional @qcode{\"type\"} input determines the class of the variable\n\
used for calculations.  If the argument @qcode{\"native\"} is given, then\n\
the operation is performed in the same type as the original argument, rather\n\
than the default double type.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
sum ([true, true])\n\
   @result{} 2\n\
sum ([true, true], \"native\")\n\
   @result{} true\n\
@end group\n\
@end example\n\
\n\
On the contrary, if @qcode{\"double\"} is given, the sum is performed in\n\
double precision even for single precision inputs.\n\
\n\
For double precision inputs, the @qcode{\"extra\"} option will use a more\n\
accurate algorithm than straightforward summation.  For single precision\n\
inputs, @qcode{\"extra\"} is the same as @qcode{\"double\"}.  Otherwise,\n\
@qcode{\"extra\"} has no effect.\n\
@seealso{cumsum, sumsq, prod}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  bool isnative = false;
  bool isdouble = false;
  bool isextra = false;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "native")
        isnative = true;
      else if (str == "double")
        isdouble = true;
      else if (str == "extra")
        isextra = true;
      else
        error ("sum: unrecognized type argument '%s'", str.c_str ());
      nargin --;
    }

  if (error_state)
    return retval;

  if (nargin == 1 || nargin == 2)
    {
      octave_value arg = args(0);

      int dim = -1;
      if (nargin == 2)
        {
          dim = args(1).int_value () - 1;
          if (dim < 0)
            error ("sum: invalid dimension DIM = %d", dim + 1);
        }

      if (! error_state)
        {
          switch (arg.builtin_type ())
            {
            case btyp_double:
              if (arg.is_sparse_type ())
                {
                  if (isextra)
                    warning ("sum: 'extra' not yet implemented for sparse matrices");
                  retval = arg.sparse_matrix_value ().sum (dim);
                }
              else if (isextra)
                retval = arg.array_value ().xsum (dim);
              else
                retval = arg.array_value ().sum (dim);
              break;
            case btyp_complex:
              if (arg.is_sparse_type ())
                {
                  if (isextra)
                    warning ("sum: 'extra' not yet implemented for sparse matrices");
                  retval = arg.sparse_complex_matrix_value ().sum (dim);
                }
              else if (isextra)
                retval = arg.complex_array_value ().xsum (dim);
              else
                retval = arg.complex_array_value ().sum (dim);
              break;
            case btyp_float:
              if (isdouble || isextra)
                retval = arg.float_array_value ().dsum (dim);
              else
                retval = arg.float_array_value ().sum (dim);
              break;
            case btyp_float_complex:
              if (isdouble || isextra)
                retval = arg.float_complex_array_value ().dsum (dim);
              else
                retval = arg.float_complex_array_value ().sum (dim);
              break;

#define MAKE_INT_BRANCH(X) \
            case btyp_ ## X: \
              if (isnative) \
                retval = arg.X ## _array_value ().sum (dim); \
              else \
                retval = arg.X ## _array_value ().dsum (dim); \
              break;
            MAKE_INT_BRANCH (int8);
            MAKE_INT_BRANCH (int16);
            MAKE_INT_BRANCH (int32);
            MAKE_INT_BRANCH (int64);
            MAKE_INT_BRANCH (uint8);
            MAKE_INT_BRANCH (uint16);
            MAKE_INT_BRANCH (uint32);
            MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH

            // GAGME: Accursed Matlab compatibility...
            case btyp_char:
              if (isextra)
                retval = arg.array_value (true).xsum (dim);
              else
                retval = arg.array_value (true).sum (dim);
              break;
            case btyp_bool:
              if (arg.is_sparse_type ())
                {
                  if (isnative)
                    retval = arg.sparse_bool_matrix_value ().any (dim);
                  else
                    retval = arg.sparse_bool_matrix_value ().sum (dim);
                }
              else if (isnative)
                retval = arg.bool_array_value ().any (dim);
              else
                retval = arg.bool_array_value ().sum (dim);
              break;

            default:
              gripe_wrong_type_arg ("sum", arg);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (sum ([1, 2, 3]), 6)
%!assert (sum ([-1; -2; -3]), -6)
%!assert (sum ([i, 2+i, -3+2i, 4]), 3+4i)
%!assert (sum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [2+2i, 4+4i, 6+6i])

%!assert (sum (single ([1, 2, 3])), single (6))
%!assert (sum (single ([-1; -2; -3])), single (-6))
%!assert (sum (single ([i, 2+i, -3+2i, 4])), single (3+4i))
%!assert (sum (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])), single ([2+2i, 4+4i, 6+6i]))

%!assert (sum ([1, 2; 3, 4], 1), [4, 6])
%!assert (sum ([1, 2; 3, 4], 2), [3; 7])
%!assert (sum (zeros (1, 0)), 0)
%!assert (sum (zeros (1, 0), 1), zeros (1, 0))
%!assert (sum (zeros (1, 0), 2), 0)
%!assert (sum (zeros (0, 1)), 0)
%!assert (sum (zeros (0, 1), 1), 0)
%!assert (sum (zeros (0, 1), 2), zeros (0, 1))
%!assert (sum (zeros (2, 0)),  zeros (1, 0))
%!assert (sum (zeros (2, 0), 1), zeros (1, 0))
%!assert (sum (zeros (2, 0), 2),  [0; 0])
%!assert (sum (zeros (0, 2)), [0, 0])
%!assert (sum (zeros (0, 2), 1), [0, 0])
%!assert (sum (zeros (0, 2), 2), zeros (0, 1))
%!assert (sum (zeros (2, 2, 0, 3)), zeros (1, 2, 0, 3))
%!assert (sum (zeros (2, 2, 0, 3), 2), zeros (2, 1, 0, 3))
%!assert (sum (zeros (2, 2, 0, 3), 3), zeros (2, 2, 1, 3))
%!assert (sum (zeros (2, 2, 0, 3), 4), zeros (2, 2, 0))
%!assert (sum (zeros (2, 2, 0, 3), 7), zeros (2, 2, 0, 3))

%!assert (sum (single ([1, 2; 3, 4]), 1), single ([4, 6]))
%!assert (sum (single ([1, 2; 3, 4]), 2), single ([3; 7]))
%!assert (sum (zeros (1, 0, "single")), single (0))
%!assert (sum (zeros (1, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (sum (zeros (1, 0, "single"), 2), single (0))
%!assert (sum (zeros (0, 1, "single")), single (0))
%!assert (sum (zeros (0, 1, "single"), 1), single (0))
%!assert (sum (zeros (0, 1, "single"), 2), zeros (0, 1, "single"))
%!assert (sum (zeros (2, 0, "single")),  zeros (1, 0, "single"))
%!assert (sum (zeros (2, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (sum (zeros (2, 0, "single"), 2),  single ([0; 0]))
%!assert (sum (zeros (0, 2, "single")), single ([0, 0]))
%!assert (sum (zeros (0, 2, "single"), 1), single ([0, 0]))
%!assert (sum (zeros (0, 2, "single"), 2), zeros (0, 1, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single")), zeros (1, 2, 0, 3, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 2), zeros (2, 1, 0, 3, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 3), zeros (2, 2, 1, 3, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 4), zeros (2, 2, 0, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 7), zeros (2, 2, 0, 3, "single"))

## Test "native"
%!assert (sum ([true,true]), 2)
%!assert (sum ([true,true], "native"), true)
%!assert (sum (int8 ([127,10,-20])), 117)
%!assert (sum (int8 ([127,10,-20]), "native"), int8 (107))

;-)
%!assert (sum ("Octave") + "8", sumsq (primes (17)))

%!error sum ()
%!error sum (1,2,3)
%!error <unrecognized type argument 'foobar'> sum (1, "foobar")
*/

DEFUN (sumsq, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} sumsq (@var{x})\n\
@deftypefnx {Built-in Function} {} sumsq (@var{x}, @var{dim})\n\
Sum of squares of elements along dimension @var{dim}.\n\
\n\
If @var{dim} is omitted, it defaults to the first non-singleton dimension.\n\
\n\
This function is conceptually equivalent to computing\n\
\n\
@example\n\
sum (x .* conj (x), dim)\n\
@end example\n\
\n\
@noindent\n\
but it uses less memory and avoids calling @code{conj} if @var{x} is real.\n\
@seealso{sum, prod}\n\
@end deftypefn")
{
  DATA_REDUCTION (sumsq);
}

/*
%!assert (sumsq ([1, 2, 3]), 14)
%!assert (sumsq ([-1; -2; 4i]), 21)
%!assert (sumsq ([1, 2, 3; 2, 3, 4; 4i, 6i, 2]), [21, 49, 29])

%!assert (sumsq (single ([1, 2, 3])), single (14))
%!assert (sumsq (single ([-1; -2; 4i])), single (21))
%!assert (sumsq (single ([1, 2, 3; 2, 3, 4; 4i, 6i, 2])), single ([21, 49, 29]))

%!assert (sumsq ([1, 2; 3, 4], 1), [10, 20])
%!assert (sumsq ([1, 2; 3, 4], 2), [5; 25])

%!assert (sumsq (single ([1, 2; 3, 4]), 1), single ([10, 20]))
%!assert (sumsq (single ([1, 2; 3, 4]), 2), single ([5; 25]))

%!error sumsq ()
*/

DEFUN (islogical, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} islogical (@var{x})\n\
@deftypefnx {Built-in Function} {} isbool (@var{x})\n\
Return true if @var{x} is a logical object.\n\
@seealso{isfloat, isinteger, ischar, isnumeric, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_bool_type ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (isbool, islogical);

/*
%!assert (islogical (true), true)
%!assert (islogical (false), true)
%!assert (islogical ([true, false]), true)
%!assert (islogical (1), false)
%!assert (islogical (1i), false)
%!assert (islogical ([1,1]), false)
%!assert (islogical (single (1)), false)
%!assert (islogical (single (1i)), false)
%!assert (islogical (single ([1,1])), false)
%!assert (islogical (sparse ([true, false])), true)
%!assert (islogical (sparse ([1, 0])), false)
*/

DEFUN (isinteger, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isinteger (@var{x})\n\
Return true if @var{x} is an integer object (int8, uint8, int16, etc.).\n\
\n\
Note that @w{@code{isinteger (14)}} is false because numeric constants in\n\
Octave are double precision floating point values.\n\
@seealso{isfloat, ischar, islogical, isnumeric, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_integer_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (iscomplex, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscomplex (@var{x})\n\
Return true if @var{x} is a complex-valued numeric object.\n\
@seealso{isreal, isnumeric, islogical, ischar, isfloat, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_complex_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (isfloat, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isfloat (@var{x})\n\
Return true if @var{x} is a floating-point numeric object.\n\
\n\
Objects of class double or single are floating-point objects.\n\
@seealso{isinteger, ischar, islogical, isnumeric, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_float_type ();
  else
    print_usage ();

  return retval;
}

// FIXME: perhaps this should be implemented with an
// octave_value member function?

DEFUN (complex, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} complex (@var{x})\n\
@deftypefnx {Built-in Function} {} complex (@var{re}, @var{im})\n\
Return a complex value from real arguments.\n\
\n\
With 1 real argument @var{x}, return the complex result\n\
@w{@code{@var{x} + 0i}}.\n\
\n\
With 2 real arguments, return the complex result\n\
@w{@code{@var{re} + @var{im}}}.\n\
@code{complex} can often be more convenient than expressions such as\n\
@w{@code{a + i*b}}.\n\
For example:\n\
\n\
@example\n\
@group\n\
complex ([1, 2], [3, 4])\n\
  @result{} [ 1 + 3i   2 + 4i ]\n\
@end group\n\
@end example\n\
@seealso{real, imag, iscomplex, abs, arg}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

      if (arg.is_complex_type ())
        retval = arg;
      else
        {
          if (arg.is_sparse_type ())
            {
              SparseComplexMatrix val = arg.sparse_complex_matrix_value ();

              if (! error_state)
                retval = octave_value (new octave_sparse_complex_matrix (val));
            }
          else if (arg.is_single_type ())
            {
              if (arg.numel () == 1)
                {
                  FloatComplex val = arg.float_complex_value ();

                  if (! error_state)
                    retval = octave_value (new octave_float_complex (val));
                }
              else
                {
                  FloatComplexNDArray val = arg.float_complex_array_value ();

                  if (! error_state)
                    retval = octave_value (new octave_float_complex_matrix (val));
                }
            }
          else
            {
              if (arg.numel () == 1)
                {
                  Complex val = arg.complex_value ();

                  if (! error_state)
                    retval = octave_value (new octave_complex (val));
                }
              else
                {
                  ComplexNDArray val = arg.complex_array_value ();

                  if (! error_state)
                    retval = octave_value (new octave_complex_matrix (val));
                }
            }

          if (error_state)
            error ("complex: invalid conversion");
        }
    }
  else if (nargin == 2)
    {
      octave_value re = args(0);
      octave_value im = args(1);

      if (re.is_sparse_type () && im.is_sparse_type ())
        {
          const SparseMatrix re_val = re.sparse_matrix_value ();
          const SparseMatrix im_val = im.sparse_matrix_value ();

          if (!error_state)
            {
              if (re.numel () == 1)
                {
                  SparseComplexMatrix result;
                  if (re_val.nnz () == 0)
                    result = Complex (0, 1) * SparseComplexMatrix (im_val);
                  else
                    {
                      octave_idx_type nr = im_val.rows ();
                      octave_idx_type nc = im_val.cols ();
                      result = SparseComplexMatrix (nr, nc, re_val(0));

                      for (octave_idx_type j = 0; j < nc; j++)
                        {
                          octave_idx_type off = j * nr;
                          for (octave_idx_type i = im_val.cidx (j);
                               i < im_val.cidx (j + 1); i++)
                            result.data (im_val.ridx (i) + off) +=
                              Complex (0, im_val.data (i));
                        }
                    }
                  retval = octave_value (new octave_sparse_complex_matrix (result));
                }
              else if (im.numel () == 1)
                {
                  SparseComplexMatrix result;
                  if (im_val.nnz () == 0)
                    result = SparseComplexMatrix (re_val);
                  else
                    {
                      octave_idx_type nr = re_val.rows ();
                      octave_idx_type nc = re_val.cols ();
                      result = SparseComplexMatrix (nr, nc,
                                                    Complex (0, im_val(0)));

                      for (octave_idx_type j = 0; j < nc; j++)
                        {
                          octave_idx_type off = j * nr;
                          for (octave_idx_type i = re_val.cidx (j);
                               i < re_val.cidx (j + 1); i++)
                            result.data (re_val.ridx (i) + off) +=
                              re_val.data (i);
                        }
                    }
                  retval = octave_value (new octave_sparse_complex_matrix (result));
                }
              else
                {
                  if (re_val.dims () == im_val.dims ())
                    {
                      SparseComplexMatrix result;
                      result = SparseComplexMatrix (re_val)
                               + Complex (0, 1) * SparseComplexMatrix (im_val);
                      retval = octave_value (
                                 new octave_sparse_complex_matrix (result));
                    }
                  else
                    error ("complex: dimension mismatch");
                }
            }
        }
      else if (re.is_single_type () || im.is_single_type ())
        {
          if (re.numel () == 1)
            {
              float re_val = re.float_value ();

              if (im.numel () == 1)
                {
                  float im_val = im.double_value ();

                  if (! error_state)
                    retval = octave_value (
                               new octave_float_complex (FloatComplex (re_val,
                                                                       im_val)));
                }
              else
                {
                  const FloatNDArray im_val = im.float_array_value ();

                  if (! error_state)
                    {
                      FloatComplexNDArray result (im_val.dims (),
                                                  FloatComplex ());

                      for (octave_idx_type i = 0; i < im_val.numel (); i++)
                        result.xelem (i) = FloatComplex (re_val, im_val(i));

                      retval = octave_value (
                                 new octave_float_complex_matrix (result));
                    }
                }
            }
          else
            {
              const FloatNDArray re_val = re.float_array_value ();

              if (im.numel () == 1)
                {
                  float im_val = im.float_value ();

                  if (! error_state)
                    {
                      FloatComplexNDArray result (re_val.dims (),
                                                  FloatComplex ());

                      for (octave_idx_type i = 0; i < re_val.numel (); i++)
                        result.xelem (i) = FloatComplex (re_val(i), im_val);

                      retval = octave_value (
                                 new octave_float_complex_matrix (result));
                    }
                }
              else
                {
                  const FloatNDArray im_val = im.float_array_value ();

                  if (! error_state)
                    {
                      if (re_val.dims () == im_val.dims ())
                        {
                          FloatComplexNDArray result (re_val.dims (),
                                                      FloatComplex ());

                          for (octave_idx_type i = 0; i < re_val.numel (); i++)
                            result.xelem (i) = FloatComplex (re_val(i),
                                                             im_val(i));

                          retval = octave_value (
                                     new octave_float_complex_matrix (result));
                        }
                      else
                        error ("complex: dimension mismatch");
                    }
                }
            }
        }
      else if (re.numel () == 1)
        {
          double re_val = re.double_value ();

          if (im.numel () == 1)
            {
              double im_val = im.double_value ();

              if (! error_state)
                retval = octave_value (new octave_complex (Complex (re_val,
                                                                    im_val)));
            }
          else
            {
              const NDArray im_val = im.array_value ();

              if (! error_state)
                {
                  ComplexNDArray result (im_val.dims (), Complex ());

                  for (octave_idx_type i = 0; i < im_val.numel (); i++)
                    result.xelem (i) = Complex (re_val, im_val(i));

                  retval = octave_value (new octave_complex_matrix (result));
                }
            }
        }
      else
        {
          const NDArray re_val = re.array_value ();

          if (im.numel () == 1)
            {
              double im_val = im.double_value ();

              if (! error_state)
                {
                  ComplexNDArray result (re_val.dims (), Complex ());

                  for (octave_idx_type i = 0; i < re_val.numel (); i++)
                    result.xelem (i) = Complex (re_val(i), im_val);

                  retval = octave_value (new octave_complex_matrix (result));
                }
            }
          else
            {
              const NDArray im_val = im.array_value ();

              if (! error_state)
                {
                  if (re_val.dims () == im_val.dims ())
                    {
                      ComplexNDArray result (re_val.dims (), Complex ());

                      for (octave_idx_type i = 0; i < re_val.numel (); i++)
                        result.xelem (i) = Complex (re_val(i), im_val(i));

                      retval = octave_value (
                                 new octave_complex_matrix (result));
                    }
                  else
                    error ("complex: dimension mismatch");
                }
            }
        }

      if (error_state)
        error ("complex: invalid conversion");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (isreal, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isreal (@var{x})\n\
Return true if @var{x} is a non-complex matrix or scalar.\n\
\n\
For compatibility with @sc{matlab}, this includes logical and character\n\
matrices.\n\
@seealso{iscomplex, isnumeric, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_real_type ();
  else
    print_usage ();

  return retval;
}

DEFUN (isempty, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isempty (@var{a})\n\
Return true if @var{a} is an empty matrix (any one of its dimensions is\n\
zero).\n\
@seealso{isnull, isa}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    retval = args(0).is_empty ();
  else
    print_usage ();

  return retval;
}

/*
%% Debian bug #706376
%!assert (isempty (speye(2^16)), false)
*/

DEFUN (isnumeric, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isnumeric (@var{x})\n\
Return true if @var{x} is a numeric object, i.e., an integer, real, or\n\
complex array.\n\
\n\
Logical and character arrays are not considered to be numeric.\n\
@seealso{isinteger, isfloat, isreal, iscomplex, islogical, ischar, iscell, isstruct, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_numeric_type ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (isnumeric (1), true)
%!assert (isnumeric (1i), true)
%!assert (isnumeric ([1,1]), true)
%!assert (isnumeric (single (1)), true)
%!assert (isnumeric (single (1i)), true)
%!assert (isnumeric (single ([1,1])), true)
%!assert (isnumeric (int8 (1)), true)
%!assert (isnumeric (uint8 ([1,1])), true)
%!assert (isnumeric ("Hello World"), false)
%!assert (isnumeric (true), false)
%!assert (isnumeric (false), false)
%!assert (isnumeric ([true, false]), false)
%!assert (isnumeric (sparse ([true, false])), false)
*/

DEFUN (isscalar, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isscalar (@var{x})\n\
Return true if @var{x} is a scalar.\n\
@seealso{isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).numel () == 1;
  else
    print_usage ();

  return retval;
}

/*
%!assert (isscalar (1))
%!assert (isscalar ([1, 2]), false)
%!assert (isscalar ([]), false)
%!assert (isscalar ([1, 2; 3, 4]), false)

%!assert (isscalar ("t"))
%!assert (isscalar ("test"), false)
%!assert (isscalar (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isscalar (s));

%% Test input validation
%!error isscalar ()
%!error isscalar (1, 2)
*/

DEFUN (isvector, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} isvector (@var{x})\n\
Return true if @var{x} is a vector.\n\
\n\
A vector is a 2-D array where one of the dimensions is equal to 1.  As a\n\
consequence a 1x1 array, or scalar, is also a vector.\n\
@seealso{isscalar, ismatrix, size, rows, columns, length}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      dim_vector sz = args(0).dims ();
      retval = sz.length () == 2 && (sz(0) == 1 || sz(1) == 1);
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (isvector (1), true)
%!assert (isvector ([1; 2; 3]), true)
%!assert (isvector ([1, 2, 3]), true)
%!assert (isvector ([]), false)
%!assert (isvector ([1, 2; 3, 4]), false)

%!assert (isvector ("t"), true)
%!assert (isvector ("test"), true)
%!assert (isvector (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isvector (s), true);

%% Test input validation
%!error isvector ()
%!error isvector ([1, 2], 2)
*/

DEFUN (isrow, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} isrow (@var{x})\n\
Return true if @var{x} is a row vector 1xN with non-negative N.\n\
@seealso{iscolumn, isscalar, isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      dim_vector sz = args(0).dims ();
      retval = sz.length () == 2 && sz(0) == 1;
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (isrow ([1, 2, 3]))
%!assert (isrow ([1; 2; 3]), false)
%!assert (isrow (1))
%!assert (isrow ([]), false)
%!assert (isrow ([1, 2; 3, 4]), false)

%!assert (isrow (ones (1, 0)), true)
%!assert (isrow (ones (1, 1)), true)
%!assert (isrow (ones (1, 2)), true)
%!assert (isrow (ones (1, 1, 1)), true)
%!assert (isrow (ones (1, 1, 1, 1)), true)

%!assert (isrow (ones (0, 0)), false)
%!assert (isrow (ones (1, 1, 0)), false)


%!assert (isrow ("t"), true)
%!assert (isrow ("test"), true)
%!assert (isrow (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isrow (s), true);

%% Test input validation
%!error isrow ()
%!error isrow ([1, 2], 2)
*/

DEFUN (iscolumn, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} iscolumn (@var{x})\n\
Return true if @var{x} is a column vector Nx1 with non-negative N.\n\
@seealso{isrow, isscalar, isvector, ismatrix}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      dim_vector sz = args(0).dims ();
      retval = sz.length () == 2 && sz(1) == 1;
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (iscolumn ([1, 2, 3]), false)
%!assert (iscolumn ([1; 2; 3]), true)
%!assert (iscolumn (1), true)
%!assert (iscolumn ([]), false)
%!assert (iscolumn ([1, 2; 3, 4]), false)

%!assert (iscolumn ("t"), true)
%!assert (iscolumn ("test"), false)
%!assert (iscolumn (["test"; "ing"]), false)

%!assert (iscolumn (ones (0, 1)), true)
%!assert (iscolumn (ones (1, 1)), true)
%!assert (iscolumn (ones (2, 1)), true)
%!assert (iscolumn (ones (1, 1, 1)), true)
%!assert (iscolumn (ones (1, 1, 1, 1)), true)

%!assert (iscolumn (ones (0, 0)), false)
%!assert (iscolumn (ones (0, 1, 0)), false)

%!test
%! s.a = 1;
%! assert (iscolumn (s));

%% Test input validation
%!error iscolumn ()
%!error iscolumn ([1, 2], 2)
*/

DEFUN (ismatrix, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ismatrix (@var{a})\n\
Return true if @var{a} is a 2-D array.\n\
@seealso{isscalar, isvector, iscell, isstruct, issparse, isa}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      dim_vector sz = args(0).dims ();
      retval = (sz.length () == 2) && (sz(0) >= 0) && (sz(1) >= 0);
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (ismatrix ([]), true)
%!assert (ismatrix (1), true)
%!assert (ismatrix ([1, 2, 3]), true)
%!assert (ismatrix ([1, 2; 3, 4]), true)

%!assert (ismatrix (zeros (0)), true)
%!assert (ismatrix (zeros (0, 0)), true)
%!assert (ismatrix (zeros (0, 0, 0)), false)
%!assert (ismatrix (zeros (3, 2, 4)), false)

%!assert (ismatrix (single ([])), true)
%!assert (ismatrix (single (1)), true)
%!assert (ismatrix (single ([1, 2, 3])), true)
%!assert (ismatrix (single ([1, 2; 3, 4])), true)

%!assert (ismatrix ("t"), true)
%!assert (ismatrix ("test"), true)
%!assert (ismatrix (["test"; "ing"]), true)

%!test
%! s.a = 1;
%! assert (ismatrix (s), true);

%!error ismatrix ()
%!error ismatrix ([1, 2; 3, 4], 2)
*/

DEFUN (issquare, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} issquare (@var{x})\n\
Return true if @var{x} is a square matrix.\n\
@seealso{isscalar, isvector, ismatrix, size}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      dim_vector sz = args(0).dims ();
      retval = sz.length () == 2 && sz(0) == sz(1);
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (issquare ([]))
%!assert (issquare (1))
%!assert (! issquare ([1, 2]))
%!assert (issquare ([1, 2; 3, 4]))
%!assert (! issquare ([1, 2; 3, 4; 5, 6]))
%!assert (! issquare (ones (3,3,3)))
%!assert (issquare ("t"))
%!assert (! issquare ("test"))
%!assert (issquare (["test"; "ing"; "1"; "2"]))
%!test
%! s.a = 1;
%! assert (issquare (s));
%!assert (issquare ({1, 2; 3, 4}))
%!assert (sparse (([1, 2; 3, 4])))

%% Test input validation
%!error issquare ()
%!error issquare ([1, 2; 3, 4], 2)
*/

static octave_value
fill_matrix (const octave_value_list& args, int val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
        return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

            if (error_state)
              {
                error ("%s: expecting scalar integer arguments", fcn);
                break;
              }
          }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // FIXME: perhaps this should be made extensible by
      // using the class name to lookup a function to call to create
      // the new value.

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
        {
          switch (dt)
            {
            case oct_data_conv::dt_int8:
              retval = int8NDArray (dims, val);
              break;

            case oct_data_conv::dt_uint8:
              retval = uint8NDArray (dims, val);
              break;

            case oct_data_conv::dt_int16:
              retval = int16NDArray (dims, val);
              break;

            case oct_data_conv::dt_uint16:
              retval = uint16NDArray (dims, val);
              break;

            case oct_data_conv::dt_int32:
              retval = int32NDArray (dims, val);
              break;

            case oct_data_conv::dt_uint32:
              retval = uint32NDArray (dims, val);
              break;

            case oct_data_conv::dt_int64:
              retval = int64NDArray (dims, val);
              break;

            case oct_data_conv::dt_uint64:
              retval = uint64NDArray (dims, val);
              break;

            case oct_data_conv::dt_single:
              retval = FloatNDArray (dims, val);
              break;

            case oct_data_conv::dt_double:
              {
                if (val == 1 && dims.length () == 2 && dims (0) == 1)
                  retval = Range (1.0, 0.0, dims (1)); // packed form
                else
                  retval = NDArray (dims, val);
              }
              break;

            case oct_data_conv::dt_logical:
              retval = boolNDArray (dims, val);
              break;

            default:
              error ("%s: invalid class name", fcn);
              break;
            }
        }
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, double val, float fval,
             const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
        return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

            if (error_state)
              {
                error ("%s: expecting scalar integer arguments", fcn);
                break;
              }
          }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
        {
          switch (dt)
            {
            case oct_data_conv::dt_single:
              retval = FloatNDArray (dims, fval);
              break;

            case oct_data_conv::dt_double:
              retval = NDArray (dims, val);
              break;

            default:
              error ("%s: invalid class name", fcn);
              break;
            }
        }
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, double val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
        return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

            if (error_state)
              {
                error ("%s: expecting scalar integer arguments", fcn);
                break;
              }
          }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
        {
          switch (dt)
            {
            case oct_data_conv::dt_single:
              retval = FloatNDArray (dims, static_cast<float> (val));
              break;

            case oct_data_conv::dt_double:
              retval = NDArray (dims, val);
              break;

            default:
              error ("%s: invalid class name", fcn);
              break;
            }
        }
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, const Complex& val,
             const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
        return retval;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

            if (error_state)
              {
                error ("%s: expecting scalar integer arguments", fcn);
                break;
              }
          }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
        {
          switch (dt)
            {
            case oct_data_conv::dt_single:
              retval = FloatComplexNDArray (dims,
                                            static_cast<FloatComplex> (val));
              break;

            case oct_data_conv::dt_double:
              retval = ComplexNDArray (dims, val);
              break;

            default:
              error ("%s: invalid class name", fcn);
              break;
            }
        }
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, bool val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector dims (1, 1);

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            dims(i) = args(i).is_empty () ? 0 : args(i).idx_type_value ();

            if (error_state)
              {
                error ("%s: expecting scalar integer arguments", fcn);
                break;
              }
          }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, fcn);

      // Note that automatic narrowing will handle conversion from
      // NDArray to scalar.

      if (! error_state)
        retval = boolNDArray (dims, val);
    }

  return retval;
}

DEFUN (ones, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} ones (@var{n})\n\
@deftypefnx {Built-in Function} {} ones (@var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {} ones (@var{m}, @var{n}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} ones ([@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {} ones (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all 1.\n\
\n\
If invoked with a single scalar integer argument @var{n}, return a square\n\
@nospell{NxN} matrix.\n\
\n\
If invoked with two or more scalar integer arguments, or a vector of integer\n\
values, return an array with the given dimensions.\n\
\n\
To create a constant matrix whose values are all the same use an expression\n\
such as\n\
\n\
@example\n\
val_matrix = val * ones (m, n)\n\
@end example\n\
\n\
The optional argument @var{class} specifies the class of the return array\n\
and defaults to double.  For example:\n\
\n\
@example\n\
val = ones (m,n, \"uint8\")\n\
@end example\n\
@seealso{zeros}\n\
@end deftypefn")
{
  return fill_matrix (args, 1, "ones");
}

/*
%!assert (ones (3), [1, 1, 1; 1, 1, 1; 1, 1, 1])
%!assert (ones (2, 3), [1, 1, 1; 1, 1, 1])
%!assert (ones (3, 2), [1, 1; 1, 1; 1, 1])
%!assert (size (ones (3, 4, 5)), [3, 4, 5])

%!assert (ones (3, "single"), single ([1, 1, 1; 1, 1, 1; 1, 1, 1]))
%!assert (ones (2, 3, "single"), single ([1, 1, 1; 1, 1, 1]))
%!assert (ones (3, 2, "single"), single ([1, 1; 1, 1; 1, 1]))
%!assert (size (ones (3, 4, 5, "single")), [3, 4, 5])

%!assert (ones (3, "int8"), int8 ([1, 1, 1; 1, 1, 1; 1, 1, 1]))
%!assert (ones (2, 3, "int8"), int8 ([1, 1, 1; 1, 1, 1]))
%!assert (ones (3, 2, "int8"), int8 ([1, 1; 1, 1; 1, 1]))
%!assert (size (ones (3, 4, 5, "int8")), [3, 4, 5])
*/

DEFUN (zeros, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} zeros (@var{n})\n\
@deftypefnx {Built-in Function} {} zeros (@var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {} zeros (@var{m}, @var{n}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} zeros ([@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {} zeros (@dots{}, @var{class})\n\
Return a matrix or N-dimensional array whose elements are all 0.\n\
\n\
If invoked with a single scalar integer argument, return a square\n\
@nospell{NxN} matrix.\n\
\n\
If invoked with two or more scalar integer arguments, or a vector of integer\n\
values, return an array with the given dimensions.\n\
\n\
The optional argument @var{class} specifies the class of the return array\n\
and defaults to double.  For example:\n\
\n\
@example\n\
val = zeros (m,n, \"uint8\")\n\
@end example\n\
@seealso{ones}\n\
@end deftypefn")
{
  return fill_matrix (args, 0, "zeros");
}

/*
%!assert (zeros (3), [0, 0, 0; 0, 0, 0; 0, 0, 0])
%!assert (zeros (2, 3), [0, 0, 0; 0, 0, 0])
%!assert (zeros (3, 2), [0, 0; 0, 0; 0, 0])
%!assert (size (zeros (3, 4, 5)), [3, 4, 5])

%!assert (zeros (3, "single"), single ([0, 0, 0; 0, 0, 0; 0, 0, 0]))
%!assert (zeros (2, 3, "single"), single ([0, 0, 0; 0, 0, 0]))
%!assert (zeros (3, 2, "single"), single ([0, 0; 0, 0; 0, 0]))
%!assert (size (zeros (3, 4, 5, "single")), [3, 4, 5])

%!assert (zeros (3, "int8"), int8 ([0, 0, 0; 0, 0, 0; 0, 0, 0]))
%!assert (zeros (2, 3, "int8"), int8 ([0, 0, 0; 0, 0, 0]))
%!assert (zeros (3, 2, "int8"), int8 ([0, 0; 0, 0; 0, 0]))
%!assert (size (zeros (3, 4, 5, "int8")), [3, 4, 5])
*/

DEFUN (Inf, args, ,
       "-*- texinfo -*-\n\
@c List other form of function in documentation index\n\
@findex inf\n\
\n\
@deftypefn  {Built-in Function} {} Inf\n\
@deftypefnx {Built-in Function} {} Inf (@var{n})\n\
@deftypefnx {Built-in Function} {} Inf (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} Inf (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} Inf (@dots{}, @var{class})\n\
Return a scalar, matrix or N-dimensional array whose elements are all equal\n\
to the IEEE representation for positive infinity.\n\
\n\
Infinity is produced when results are too large to be represented using the\n\
IEEE floating point format for numbers.  Two common examples which produce\n\
infinity are division by zero and overflow.\n\
\n\
@example\n\
@group\n\
[ 1/0 e^800 ]\n\
@result{} Inf   Inf\n\
@end group\n\
@end example\n\
\n\
When called with no arguments, return a scalar with the value @samp{Inf}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{isinf, NaN}\n\
@end deftypefn")
{
  return fill_matrix (args, lo_ieee_inf_value (),
                      lo_ieee_float_inf_value (), "Inf");
}

DEFALIAS (inf, Inf);

/*
%!assert (inf (3), [Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf])
%!assert (inf (2, 3), [Inf, Inf, Inf; Inf, Inf, Inf])
%!assert (inf (3, 2), [Inf, Inf; Inf, Inf; Inf, Inf])
%!assert (size (inf (3, 4, 5)), [3, 4, 5])

%!assert (inf (3, "single"), single ([Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf]))
%!assert (inf (2, 3, "single"), single ([Inf, Inf, Inf; Inf, Inf, Inf]))
%!assert (inf (3, 2, "single"), single ([Inf, Inf; Inf, Inf; Inf, Inf]))
%!assert (size (inf (3, 4, 5, "single")), [3, 4, 5])

%!error (inf (3, "int8"))
%!error (inf (2, 3, "int8"))
%!error (inf (3, 2, "int8"))
%!error (inf (3, 4, 5, "int8"))
*/

DEFUN (NaN, args, ,
       "-*- texinfo -*-\n\
@c List other form of function in documentation index\n\
@findex nan\n\
\n\
@deftypefn  {Built-in Function} {} NaN\n\
@deftypefnx {Built-in Function} {} NaN (@var{n})\n\
@deftypefnx {Built-in Function} {} NaN (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} NaN (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} NaN (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the IEEE symbol NaN (Not a Number).\n\
\n\
NaN is the result of operations which do not produce a well defined numerical\n\
result.  Common operations which produce a NaN are arithmetic with infinity\n\
@tex\n\
($\\infty - \\infty$), zero divided by zero ($0/0$),\n\
@end tex\n\
@ifnottex\n\
(Inf - Inf), zero divided by zero (0/0),\n\
@end ifnottex\n\
and any operation involving another NaN value (5 + NaN).\n\
\n\
Note that NaN always compares not equal to NaN (NaN != NaN).  This behavior\n\
is specified by the IEEE standard for floating point arithmetic.  To find\n\
NaN values, use the @code{isnan} function.\n\
\n\
When called with no arguments, return a scalar with the value @samp{NaN}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{isnan, Inf}\n\
@end deftypefn")
{
  return fill_matrix (args, lo_ieee_nan_value (),
                      lo_ieee_float_nan_value (), "NaN");
}

DEFALIAS (nan, NaN);

/*
%!assert (NaN (3), [NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN])
%!assert (NaN (2, 3), [NaN, NaN, NaN; NaN, NaN, NaN])
%!assert (NaN (3, 2), [NaN, NaN; NaN, NaN; NaN, NaN])
%!assert (size (NaN (3, 4, 5)), [3, 4, 5])

%!assert (NaN (3, "single"), single ([NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN]))
%!assert (NaN (2, 3, "single"), single ([NaN, NaN, NaN; NaN, NaN, NaN]))
%!assert (NaN (3, 2, "single"), single ([NaN, NaN; NaN, NaN; NaN, NaN]))
%!assert (size (NaN (3, 4, 5, "single")), [3, 4, 5])

%!error (NaN (3, "int8"))
%!error (NaN (2, 3, "int8"))
%!error (NaN (3, 2, "int8"))
%!error (NaN (3, 4, 5, "int8"))
*/

DEFUN (e, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} e\n\
@deftypefnx {Built-in Function} {} e (@var{n})\n\
@deftypefnx {Built-in Function} {} e (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} e (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} e (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the base of natural logarithms.\n\
\n\
The constant\n\
@tex\n\
$e$ satisfies the equation $\\log (e) = 1$.\n\
@end tex\n\
@ifnottex\n\
@samp{e} satisfies the equation @code{log} (e) = 1.\n\
@end ifnottex\n\
\n\
When called with no arguments, return a scalar with the value @math{e}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{log, exp, pi, I}\n\
@end deftypefn")
{
#if defined (M_E)
  double e_val = M_E;
#else
  double e_val = exp (1.0);
#endif

  return fill_matrix (args, e_val, "e");
}

DEFUN (eps, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} eps\n\
@deftypefnx {Built-in Function} {} eps (@var{x})\n\
@deftypefnx {Built-in Function} {} eps (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} eps (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} eps (@dots{}, @var{class})\n\
Return a scalar, matrix or N-dimensional array whose elements are all eps,\n\
the machine precision.\n\
\n\
More precisely, @code{eps} is the relative spacing between any two adjacent\n\
numbers in the machine's floating point system.  This number is obviously\n\
system dependent.  On machines that support IEEE floating point arithmetic,\n\
@code{eps} is approximately\n\
@tex\n\
$2.2204\\times10^{-16}$ for double precision and $1.1921\\times10^{-7}$\n\
@end tex\n\
@ifnottex\n\
2.2204e-16 for double precision and 1.1921e-07\n\
@end ifnottex\n\
for single precision.\n\
\n\
When called with no arguments, return a scalar with the value\n\
@code{eps (1.0)}.\n\
\n\
Given a single argument @var{x}, return the distance between @var{x} and the\n\
next largest value.\n\
\n\
When called with more than one argument the first two arguments are taken as\n\
the number of rows and columns and any further arguments specify additional\n\
matrix dimensions.  The optional argument @var{class} specifies the return\n\
type and may be either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{realmax, realmin, intmax, bitmax}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin == 1 && ! args(0).is_string ())
    {
      if (args(0).is_single_type ())
        {
          Array<float> x = args(0).float_array_value ();

          if (! error_state)
            {
              Array<float> epsval (x.dims ());

              for (octave_idx_type i = 0; i < x.numel (); i++)
                {
                  float val = ::fabsf (x(i));
                  if (xisnan (val) || xisinf (val))
                    epsval(i) = lo_ieee_nan_value ();
                  else if (val < std::numeric_limits<float>::min ())
                    epsval(i) = powf (2.0, -149e0);
                  else
                    {
                      int expon;
                      gnulib::frexpf (val, &expon);
                      epsval(i) = std::pow (2.0f,
                                            static_cast<float> (expon - 24));
                    }
                }
              retval = epsval;
            }
        }
      else
        {
          Array<double> x = args(0).array_value ();

          if (! error_state)
            {
              Array<double> epsval (x.dims ());

              for (octave_idx_type i = 0; i < x.numel (); i++)
                {
                  double val = ::fabs (x(i));
                  if (xisnan (val) || xisinf (val))
                    epsval(i) = lo_ieee_nan_value ();
                  else if (val < std::numeric_limits<double>::min ())
                    epsval(i) = pow (2.0, -1074e0);
                  else
                    {
                      int expon;
                      gnulib::frexp (val, &expon);
                      epsval(i) = std::pow (2.0,
                                            static_cast<double> (expon - 53));
                    }
                  retval = epsval;
                }
            }
        }
    }
  else
    retval = fill_matrix (args, std::numeric_limits<double>::epsilon (),
                          std::numeric_limits<float>::epsilon (), "eps");

  return retval;
}

/*
%!assert (eps (1/2), 2^(-53))
%!assert (eps (1), 2^(-52))
%!assert (eps (2), 2^(-51))
%!assert (eps (realmax), 2^971)
%!assert (eps (0), 2^(-1074))
%!assert (eps (realmin/2), 2^(-1074))
%!assert (eps (realmin/16), 2^(-1074))
%!assert (eps (Inf), NaN)
%!assert (eps (NaN), NaN)
%!assert (eps ([1/2 1 2 realmax 0 realmin/2 realmin/16 Inf NaN]),
%!             [2^(-53) 2^(-52) 2^(-51) 2^971 2^(-1074) 2^(-1074) 2^(-1074) NaN NaN])
%!assert (eps (single (1/2)), single (2^(-24)))
%!assert (eps (single (1)), single (2^(-23)))
%!assert (eps (single (2)), single (2^(-22)))
%!assert (eps (realmax ("single")), single (2^104))
%!assert (eps (single (0)), single (2^(-149)))
%!assert (eps (realmin ("single")/2), single (2^(-149)))
%!assert (eps (realmin ("single")/16), single (2^(-149)))
%!assert (eps (single (Inf)), single (NaN))
%!assert (eps (single (NaN)), single (NaN))
%!assert (eps (single ([1/2 1 2 realmax("single") 0 realmin("single")/2 realmin("single")/16 Inf NaN])),
%!             single ([2^(-24) 2^(-23) 2^(-22) 2^104 2^(-149) 2^(-149) 2^(-149) NaN NaN]))

*/

DEFUN (pi, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} pi\n\
@deftypefnx {Built-in Function} {} pi (@var{n})\n\
@deftypefnx {Built-in Function} {} pi (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} pi (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} pi (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the ratio of the circumference of a circle to its\n\
@tex\n\
diameter($\\pi$).\n\
@end tex\n\
@ifnottex\n\
diameter.\n\
@end ifnottex\n\
\n\
Internally, @code{pi} is computed as @samp{4.0 * atan (1.0)}.\n\
\n\
When called with no arguments, return a scalar with the value of\n\
@tex\n\
$\\pi$.\n\
@end tex\n\
@ifnottex\n\
pi.\n\
@end ifnottex\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{e, I}\n\
@end deftypefn")
{
#if defined (M_PI)
  double pi_val = M_PI;
#else
  double pi_val = 4.0 * atan (1.0);
#endif

  return fill_matrix (args, pi_val, "pi");
}

DEFUN (realmax, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} realmax\n\
@deftypefnx {Built-in Function} {} realmax (@var{n})\n\
@deftypefnx {Built-in Function} {} realmax (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} realmax (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} realmax (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the largest floating point number that is representable.\n\
\n\
The actual value is system dependent.  On machines that support IEEE\n\
floating point arithmetic, @code{realmax} is approximately\n\
@tex\n\
$1.7977\\times10^{308}$ for double precision and $3.4028\\times10^{38}$\n\
@end tex\n\
@ifnottex\n\
1.7977e+308 for double precision and 3.4028e+38\n\
@end ifnottex\n\
for single precision.\n\
\n\
When called with no arguments, return a scalar with the value\n\
@code{realmax (@qcode{\"double\"})}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{realmin, intmax, bitmax, eps}\n\
@end deftypefn")
{
  return fill_matrix (args, std::numeric_limits<double>::max (),
                      std::numeric_limits<float>::max (), "realmax");
}

DEFUN (realmin, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} realmin\n\
@deftypefnx {Built-in Function} {} realmin (@var{n})\n\
@deftypefnx {Built-in Function} {} realmin (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} realmin (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} realmin (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the smallest normalized floating point number that is representable.\n\
\n\
The actual value is system dependent.  On machines that support\n\
IEEE floating point arithmetic, @code{realmin} is approximately\n\
@tex\n\
$2.2251\\times10^{-308}$ for double precision and $1.1755\\times10^{-38}$\n\
@end tex\n\
@ifnottex\n\
2.2251e-308 for double precision and 1.1755e-38\n\
@end ifnottex\n\
for single precision.\n\
\n\
When called with no arguments, return a scalar with the value\n\
@code{realmin (@qcode{\"double\"})}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{realmax, intmin, eps}\n\
@end deftypefn")
{
  return fill_matrix (args, std::numeric_limits<double>::min (),
                      std::numeric_limits<float>::min (), "realmin");
}

DEFUN (I, args, ,
       "-*- texinfo -*-\n\
@c List other forms of function in documentation index\n\
@findex i\n\
@findex j\n\
@findex J\n\
\n\
@deftypefn  {Built-in Function} {} I\n\
@deftypefnx {Built-in Function} {} I (@var{n})\n\
@deftypefnx {Built-in Function} {} I (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} I (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} I (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the pure imaginary unit, defined as\n\
@tex\n\
$\\sqrt{-1}$.\n\
@end tex\n\
@ifnottex\n\
@w{@code{sqrt (-1)}}.\n\
@end ifnottex\n\
\n\
I, and its equivalents i, j, and J, are functions so any of the names may\n\
be reused for other purposes (such as i for a counter variable).\n\
\n\
When called with no arguments, return a scalar with the value @math{i}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{e, pi, log, exp}\n\
@end deftypefn")
{
  return fill_matrix (args, Complex (0.0, 1.0), "I");
}

DEFALIAS (i, I);
DEFALIAS (J, I);
DEFALIAS (j, I);

DEFUN (NA, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} NA\n\
@deftypefnx {Built-in Function} {} NA (@var{n})\n\
@deftypefnx {Built-in Function} {} NA (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} NA (@var{n}, @var{m}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} NA (@dots{}, @var{class})\n\
Return a scalar, matrix, or N-dimensional array whose elements are all equal\n\
to the special constant used to designate missing values.\n\
\n\
Note that NA always compares not equal to NA (NA != NA).\n\
To find NA values, use the @code{isna} function.\n\
\n\
When called with no arguments, return a scalar with the value @samp{NA}.\n\
\n\
When called with a single argument, return a square matrix with the dimension\n\
specified.\n\
\n\
When called with more than one scalar argument the first two arguments are\n\
taken as the number of rows and columns and any further arguments specify\n\
additional matrix dimensions.\n\
\n\
The optional argument @var{class} specifies the return type and may be\n\
either @qcode{\"double\"} or @qcode{\"single\"}.\n\
@seealso{isna}\n\
@end deftypefn")
{
  return fill_matrix (args, lo_ieee_na_value (),
                      lo_ieee_float_na_value (), "NA");
}

/*
%!assert (single (NA ("double")), NA ("single"))
%!assert (double (NA ("single")), NA ("double"))
*/

DEFUN (false, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} false (@var{x})\n\
@deftypefnx {Built-in Function} {} false (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} false (@var{n}, @var{m}, @var{k}, @dots{})\n\
Return a matrix or N-dimensional array whose elements are all logical 0.\n\
\n\
If invoked with a single scalar integer argument, return a square\n\
matrix of the specified size.\n\
\n\
If invoked with two or more scalar integer arguments, or a vector of integer\n\
values, return an array with given dimensions.\n\
@seealso{true}\n\
@end deftypefn")
{
  return fill_matrix (args, false, "false");
}

DEFUN (true, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} true (@var{x})\n\
@deftypefnx {Built-in Function} {} true (@var{n}, @var{m})\n\
@deftypefnx {Built-in Function} {} true (@var{n}, @var{m}, @var{k}, @dots{})\n\
Return a matrix or N-dimensional array whose elements are all logical 1.\n\
\n\
If invoked with a single scalar integer argument, return a square\n\
matrix of the specified size.\n\
\n\
If invoked with two or more scalar integer arguments, or a vector of integer\n\
values, return an array with given dimensions.\n\
@seealso{false}\n\
@end deftypefn")
{
  return fill_matrix (args, true, "true");
}

template <class MT>
octave_value
identity_matrix (int nr, int nc)
{
  octave_value retval;

  typename MT::element_type one (1);

  if (nr == 1 && nc == 1)
    retval = one;
  else
    {
      dim_vector dims (nr, nc);

      typename MT::element_type zero (0);

      MT m (dims, zero);

      if (nr > 0 && nc > 0)
        {
          int n = std::min (nr, nc);

          for (int i = 0; i < n; i++)
            m(i,i) = one;
        }

      retval = m;
    }

  return retval;
}

#define INSTANTIATE_EYE(T) \
  template octave_value identity_matrix<T> (int, int)

INSTANTIATE_EYE (int8NDArray);
INSTANTIATE_EYE (uint8NDArray);
INSTANTIATE_EYE (int16NDArray);
INSTANTIATE_EYE (uint16NDArray);
INSTANTIATE_EYE (int32NDArray);
INSTANTIATE_EYE (uint32NDArray);
INSTANTIATE_EYE (int64NDArray);
INSTANTIATE_EYE (uint64NDArray);
INSTANTIATE_EYE (FloatNDArray);
INSTANTIATE_EYE (NDArray);
INSTANTIATE_EYE (boolNDArray);

static octave_value
identity_matrix (int nr, int nc, oct_data_conv::data_type dt)
{
  octave_value retval;

  // FIXME: perhaps this should be made extensible by using
  // the class name to lookup a function to call to create the new
  // value.

  if (! error_state)
    {
      switch (dt)
        {
        case oct_data_conv::dt_int8:
          retval = identity_matrix<int8NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_uint8:
          retval = identity_matrix<uint8NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_int16:
          retval = identity_matrix<int16NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_uint16:
          retval = identity_matrix<uint16NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_int32:
          retval = identity_matrix<int32NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_uint32:
          retval = identity_matrix<uint32NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_int64:
          retval = identity_matrix<int64NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_uint64:
          retval = identity_matrix<uint64NDArray> (nr, nc);
          break;

        case oct_data_conv::dt_single:
          retval = FloatDiagMatrix (nr, nc, 1.0f);
          break;

        case oct_data_conv::dt_double:
          retval = DiagMatrix (nr, nc, 1.0);
          break;

        case oct_data_conv::dt_logical:
          retval = identity_matrix<boolNDArray> (nr, nc);
          break;

        default:
          error ("eye: invalid class name");
          break;
        }
    }

  return retval;
}

#undef INT_EYE_MATRIX

DEFUN (eye, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} eye (@var{n})\n\
@deftypefnx {Built-in Function} {} eye (@var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {} eye ([@var{m} @var{n}])\n\
@deftypefnx {Built-in Function} {} eye (@dots{}, @var{class})\n\
Return an identity matrix.\n\
\n\
If invoked with a single scalar argument @var{n}, return a square\n\
@nospell{NxN} identity matrix.\n\
\n\
If supplied two scalar arguments (@var{m}, @var{n}), @code{eye} takes them\n\
to be the number of rows and columns.  If given a vector with two elements,\n\
@code{eye} uses the values of the elements as the number of rows and\n\
columns, respectively.  For example:\n\
\n\
@example\n\
@group\n\
eye (3)\n\
 @result{}  1  0  0\n\
     0  1  0\n\
     0  0  1\n\
@end group\n\
@end example\n\
\n\
The following expressions all produce the same result:\n\
\n\
@example\n\
@group\n\
eye (2)\n\
@equiv{}\n\
eye (2, 2)\n\
@equiv{}\n\
eye (size ([1, 2; 3, 4]))\n\
@end group\n\
@end example\n\
\n\
The optional argument @var{class}, allows @code{eye} to return an array of\n\
the specified type, like\n\
\n\
@example\n\
val = zeros (n,m, \"uint8\")\n\
@end example\n\
\n\
Calling @code{eye} with no arguments is equivalent to calling it with an\n\
argument of 1.  Any negative dimensions are treated as zero.  These odd\n\
definitions are for compatibility with @sc{matlab}.\n\
@seealso{speye, ones, zeros}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  // Check for type information.

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);

      if (error_state)
        return retval;
    }

  switch (nargin)
    {
    case 0:
      retval = identity_matrix (1, 1, dt);
      break;

    case 1:
      {
        octave_idx_type nr, nc;
        get_dimensions (args(0), "eye", nr, nc);

        if (! error_state)
          retval = identity_matrix (nr, nc, dt);
      }
      break;

    case 2:
      {
        octave_idx_type nr, nc;
        get_dimensions (args(0), args(1), "eye", nr, nc);

        if (! error_state)
          retval = identity_matrix (nr, nc, dt);
      }
      break;

    default:
      print_usage ();
      break;
    }

  return retval;
}

/*
%!assert (full (eye (3)), [1, 0, 0; 0, 1, 0; 0, 0, 1])
%!assert (full (eye (2, 3)), [1, 0, 0; 0, 1, 0])

%!assert (full (eye (3,"single")), single ([1, 0, 0; 0, 1, 0; 0, 0, 1]))
%!assert (full (eye (2, 3,"single")), single ([1, 0, 0; 0, 1, 0]))

%!assert (eye (3, "int8"), int8 ([1, 0, 0; 0, 1, 0; 0, 0, 1]))
%!assert (eye (2, 3, "int8"), int8 ([1, 0, 0; 0, 1, 0]))

%!error eye (1, 2, 3)
*/

template <class MT>
static octave_value
do_linspace (const octave_value& base, const octave_value& limit,
             octave_idx_type n)
{
  typedef typename MT::column_vector_type CVT;
  typedef typename MT::element_type T;

  octave_value retval;

  if (base.is_scalar_type ())
    {
      T bs = octave_value_extract<T> (base);
      if (limit.is_scalar_type ())
        {
          T ls = octave_value_extract<T> (limit);
          retval = linspace (bs, ls, n);
        }
      else
        {
          CVT lv = octave_value_extract<CVT> (limit);
          CVT bv (lv.length (), bs);
          retval = linspace (bv, lv, n);
        }
    }
  else
    {
      CVT bv = octave_value_extract<CVT> (base);
      if (limit.is_scalar_type ())
        {
          T ls = octave_value_extract<T> (limit);
          CVT lv (bv.length (), ls);
          retval = linspace (bv, lv, n);
        }
      else
        {
          CVT lv = octave_value_extract<CVT> (limit);
          retval = linspace (bv, lv, n);
        }
    }

  return retval;
}

DEFUN (linspace, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} linspace (@var{base}, @var{limit})\n\
@deftypefnx {Built-in Function} {} linspace (@var{base}, @var{limit}, @var{n})\n\
Return a row vector with @var{n} linearly spaced elements between\n\
@var{base} and @var{limit}.\n\
\n\
If the number of elements is greater than one, then the endpoints @var{base}\n\
and @var{limit} are always included in the range.  If @var{base} is greater\n\
than @var{limit}, the elements are stored in decreasing order.  If the\n\
number of points is not specified, a value of 100 is used.\n\
\n\
The @code{linspace} function always returns a row vector if both @var{base}\n\
and @var{limit} are scalars.  If one, or both, of them are column vectors,\n\
@code{linspace} returns a matrix.\n\
\n\
For compatibility with @sc{matlab}, return the second argument (@var{limit})\n\
if fewer than two values are requested.\n\
@seealso{logspace}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  octave_idx_type npoints = 100;

  if (nargin != 2 && nargin != 3)
    {
      print_usage ();
      return retval;
    }

  if (nargin == 3)
    {
      // Apparently undocumented Matlab.  If the third arg is an empty
      // numeric value, the number of points defaults to 1.

      octave_value arg_3 = args(2);

      if (arg_3.is_numeric_type () && arg_3.is_empty ())
        npoints = 1;
      else
        npoints = arg_3.idx_type_value ();
    }

  if (! error_state)
    {
      octave_value arg_1 = args(0);
      octave_value arg_2 = args(1);

      if (arg_1.is_single_type () || arg_2.is_single_type ())
        {
          if (arg_1.is_complex_type () || arg_2.is_complex_type ())
            retval = do_linspace<FloatComplexMatrix> (arg_1, arg_2, npoints);
          else
            retval = do_linspace<FloatMatrix> (arg_1, arg_2, npoints);

        }
      else
        {
          if (arg_1.is_complex_type () || arg_2.is_complex_type ())
            retval = do_linspace<ComplexMatrix> (arg_1, arg_2, npoints);
          else
            retval = do_linspace<Matrix> (arg_1, arg_2, npoints);
        }
    }
  else
    error ("linspace: N must be an integer");

  return retval;
}


/*
%!test
%! x1 = linspace (1, 2);
%! x2 = linspace (1, 2, 10);
%! x3 = linspace (1, -2, 10);
%! assert (size (x1) == [1, 100] && x1(1) == 1 && x1(100) == 2);
%! assert (size (x2) == [1, 10] && x2(1) == 1 && x2(10) == 2);
%! assert (size (x3) == [1, 10] && x3(1) == 1 && x3(10) == -2);

%! ##assert (linspace ([1, 2; 3, 4], 5, 6), linspace (1, 5, 6))

%!assert (linspace (0, 1, []), 1)

%!error linspace ()
%!error linspace (1, 2, 3, 4)
*/

// FIXME: should accept dimensions as separate args for N-d
// arrays as well as 1-d and 2-d arrays.

DEFUN (resize, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} resize (@var{x}, @var{m})\n\
@deftypefnx {Built-in Function} {} resize (@var{x}, @var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} resize (@var{x}, [@var{m} @var{n} @dots{}])\n\
Resize @var{x} cutting off elements as necessary.\n\
\n\
In the result, element with certain indices is equal to the corresponding\n\
element of @var{x} if the indices are within the bounds of @var{x};\n\
otherwise, the element is set to zero.\n\
\n\
In other words, the statement\n\
\n\
@example\n\
y = resize (x, dv)\n\
@end example\n\
\n\
@noindent\n\
is equivalent to the following code:\n\
\n\
@example\n\
@group\n\
y = zeros (dv, class (x));\n\
sz = min (dv, size (x));\n\
for i = 1:length (sz)\n\
  idx@{i@} = 1:sz(i);\n\
endfor\n\
y(idx@{:@}) = x(idx@{:@});\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
but is performed more efficiently.\n\
\n\
If only @var{m} is supplied, and it is a scalar, the dimension of the\n\
result is @var{m}-by-@var{m}.\n\
If @var{m}, @var{n}, @dots{} are all scalars, then the dimensions of\n\
the result are @var{m}-by-@var{n}-by-@dots{}.\n\
If given a vector as input, then the\n\
dimensions of the result are given by the elements of that vector.\n\
\n\
An object can be resized to more dimensions than it has;\n\
in such case the missing dimensions are assumed to be 1.\n\
Resizing an object to fewer dimensions is not possible.\n\
@seealso{reshape, postpad, prepad, cat}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin == 2)
    {
      Array<double> vec = args(1).vector_value ();
      int ndim = vec.length ();
      if (ndim == 1)
        {
          octave_idx_type m = static_cast<octave_idx_type> (vec(0));
          retval = args(0);
          retval = retval.resize (dim_vector (m, m), true);
        }
      else
        {
          dim_vector dv;
          dv.resize (ndim);
          for (int i = 0; i < ndim; i++)
            dv(i) = static_cast<octave_idx_type> (vec(i));
          retval = args(0);
          retval = retval.resize (dv, true);
        }
    }
  else if (nargin > 2)
    {
      dim_vector dv;
      dv.resize (nargin - 1);
      for (octave_idx_type i = 1; i < nargin; i++)
        dv(i-1) = static_cast<octave_idx_type> (args(i).scalar_value ());
      if (!error_state)
        {
          retval = args(0);
          retval = retval.resize (dv, true);
        }

    }
  else
    print_usage ();
  return retval;
}

// FIXME: should use octave_idx_type for dimensions.

DEFUN (reshape, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} reshape (@var{A}, @var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} reshape (@var{A}, [@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {} reshape (@var{A}, @dots{}, [], @dots{})\n\
@deftypefnx {Built-in Function} {} reshape (@var{A}, @var{size})\n\
Return a matrix with the specified dimensions (@var{m}, @var{n}, @dots{})\n\
whose elements are taken from the matrix @var{A}.\n\
\n\
The elements of the matrix are accessed in column-major order (like Fortran\n\
arrays are stored).\n\
\n\
The following code demonstrates reshaping a 1x4 row vector into a 2x2 square\n\
matrix.\n\
\n\
@example\n\
@group\n\
reshape ([1, 2, 3, 4], 2, 2)\n\
      @result{}  1  3\n\
          2  4\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Note that the total number of elements in the original matrix\n\
(@code{prod (size (@var{A}))}) must match the total number of elements\n\
in the new matrix (@code{prod ([@var{m} @var{n} @dots{}])}).\n\
\n\
A single dimension of the return matrix may be left unspecified and Octave\n\
will determine its size automatically.  An empty matrix ([]) is used to flag\n\
the unspecified dimension.\n\
@seealso{resize, vec, postpad, cat, squeeze}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector new_dims;

  if (nargin == 2)
    {
      Array<octave_idx_type> new_size = args(1).octave_idx_type_vector_value ();

      if (new_size.length () < 2)
        {
          error ("reshape: SIZE must have 2 or more dimensions");
          return retval;
        }

      new_dims = dim_vector::alloc (new_size.length ());

      for (octave_idx_type i = 0; i < new_size.length (); i++)
        {
          if (new_size(i) < 0)
            {
              error ("reshape: SIZE must be non-negative");
              break;
            }
          else
            new_dims(i) = new_size(i);
        }
    }
  else if (nargin > 2)
    {
      new_dims = dim_vector::alloc (nargin-1);
      int empty_dim = -1;

      for (int i = 1; i < nargin; i++)
        {
          if (args(i).is_empty ())
            {
              if (empty_dim > 0)
                {
                  error ("reshape: only a single dimension can be unknown");
                  break;
                }
              else
                {
                  empty_dim = i;
                  new_dims(i-1) = 1;
                }
            }
          else
            {
              new_dims(i-1) = args(i).idx_type_value ();

              if (error_state)
                break;
              else if (new_dims(i-1) < 0)
                {
                  error ("reshape: SIZE must be non-negative");
                  break;
                }
            }
        }

      if (! error_state && (empty_dim > 0))
        {
          octave_idx_type nel = new_dims.numel ();

          if (nel == 0)
            new_dims(empty_dim-1) = 0;
          else
            {
              octave_idx_type a_nel = args(0).numel ();
              octave_idx_type size_empty_dim = a_nel / nel;

              if (a_nel != size_empty_dim * nel)
                error ("reshape: SIZE is not divisible by the product of known dimensions (= %d)",
                       nel);
              else
                new_dims(empty_dim-1) = size_empty_dim;
            }
        }
    }
  else
    {
      print_usage ();
      return retval;
    }

  if (! error_state)
    retval = args(0).reshape (new_dims);

  return retval;
}

/*
%!assert (size (reshape (ones (4, 4), 2, 8)), [2, 8])
%!assert (size (reshape (ones (4, 4), 8, 2)), [8, 2])
%!assert (size (reshape (ones (15, 4), 1, 60)), [1, 60])
%!assert (size (reshape (ones (15, 4), 60, 1)), [60, 1])

%!assert (size (reshape (ones (4, 4, "single"), 2, 8)), [2, 8])
%!assert (size (reshape (ones (4, 4, "single"), 8, 2)), [8, 2])
%!assert (size (reshape (ones (15, 4, "single"), 1, 60)), [1, 60])
%!assert (size (reshape (ones (15, 4, "single"), 60, 1)), [60, 1])

%!test
%! s.a = 1;
%! fail ("reshape (s, 2, 3)", "can't reshape 1x1 array to 2x3 array");

%!error reshape ()
%!error reshape (1, 2, 3, 4)
%!error <SIZE must have 2 or more dimensions> reshape (1:3, 3)
%!error <SIZE must be non-negative> reshape (1:3, [3 -1])
%!error <only a single dimension can be unknown> reshape (1:3, 1,[],[],3)
%!error <SIZE must be non-negative> reshape (1:3, 3, -1)
%!error <SIZE is not divisible> reshape (1:3, 3, [], 2)
*/

DEFUN (vec, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{v} =} vec (@var{x})\n\
@deftypefnx {Built-in Function} {@var{v} =} vec (@var{x}, @var{dim})\n\
Return the vector obtained by stacking the columns of the matrix @var{x}\n\
one above the other.\n\
\n\
Without @var{dim} this is equivalent to @code{@var{x}(:)}.\n\
\n\
If @var{dim} is supplied, the dimensions of @var{v} are set to @var{dim}\n\
with all elements along the last dimension.  This is equivalent to\n\
@code{shiftdim (@var{x}(:), 1-@var{dim})}.\n\
@seealso{vech, resize, cat}\n\
@end deftypefn")
{
  octave_value retval;
  int dim = 1;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage () ;

  if (! error_state && nargin == 2)
    {
      dim = args(1).idx_type_value ();

      if (dim < 1)
        error ("vec: DIM must be greater than zero");
    }

  if (! error_state)
    {
      octave_value colon (octave_value::magic_colon_t);
      octave_value arg = args(0);
      retval = arg.single_subsref ("(", colon);


      if (! error_state && dim > 1)
        {
          dim_vector new_dims = dim_vector::alloc (dim);

          for (int i = 0; i < dim-1; i++)
            new_dims(i) = 1;

          new_dims(dim-1) = retval.numel ();

          retval = retval.reshape (new_dims);
        }
    }

  return retval;
}

/*
%!assert (vec ([1, 2; 3, 4]), [1; 3; 2; 4])
%!assert (vec ([1, 3, 2, 4]), [1; 3; 2; 4])
%!assert (vec ([1, 2, 3, 4], 2), [1, 2, 3, 4])
%!assert (vec ([1, 2; 3, 4]), vec ([1, 2; 3, 4], 1))
%!assert (vec ([1, 2; 3, 4], 1), [1; 3; 2; 4])
%!assert (vec ([1, 2; 3, 4], 2), [1, 3, 2, 4])
%!assert (vec ([1, 3; 2, 4], 3), reshape ([1, 2, 3, 4], 1, 1, 4))
%!assert (vec ([1, 3; 2, 4], 3), shiftdim (vec ([1, 3; 2, 4]), -2))

%!error vec ()
%!error vec (1, 2, 3)
%!error vec ([1, 2; 3, 4], 0)
*/

DEFUN (squeeze, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} squeeze (@var{x})\n\
Remove singleton dimensions from @var{x} and return the result.\n\
\n\
Note that for compatibility with @sc{matlab}, all objects have\n\
a minimum of two dimensions and row vectors are left unchanged.\n\
@seealso{reshape}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).squeeze ();
  else
    print_usage ();

  return retval;
}

DEFUN (full, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{FM} =} full (@var{SM})\n\
Return a full storage matrix from a sparse, diagonal, or permutation matrix,\n\
or a range.\n\
@seealso{sparse, issparse}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).full_value ();
  else
    print_usage ();

  return retval;
}

// Compute various norms of the vector X.

DEFUN (norm, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} norm (@var{A})\n\
@deftypefnx {Built-in Function} {} norm (@var{A}, @var{p})\n\
@deftypefnx {Built-in Function} {} norm (@var{A}, @var{p}, @var{opt})\n\
Compute the p-norm of the matrix @var{A}.\n\
\n\
If the second argument is missing, @w{@code{p = 2}} is assumed.\n\
\n\
If @var{A} is a matrix (or sparse matrix):\n\
\n\
@table @asis\n\
@item @var{p} = @code{1}\n\
1-norm, the largest column sum of the absolute values of @var{A}.\n\
\n\
@item @var{p} = @code{2}\n\
Largest singular value of @var{A}.\n\
\n\
@item @var{p} = @code{Inf} or @qcode{\"inf\"}\n\
@cindex infinity norm\n\
Infinity norm, the largest row sum of the absolute values of @var{A}.\n\
\n\
@item @var{p} = @qcode{\"fro\"}\n\
@cindex Frobenius norm\n\
Frobenius norm of @var{A}, @code{sqrt (sum (diag (@var{A}' * @var{A})))}.\n\
\n\
@item other @var{p}, @code{@var{p} > 1}\n\
@cindex general p-norm\n\
maximum @code{norm (A*x, p)} such that @code{norm (x, p) == 1}\n\
@end table\n\
\n\
If @var{A} is a vector or a scalar:\n\
\n\
@table @asis\n\
@item @var{p} = @code{Inf} or @qcode{\"inf\"}\n\
@code{max (abs (@var{A}))}.\n\
\n\
@item @var{p} = @code{-Inf}\n\
@code{min (abs (@var{A}))}.\n\
\n\
@item @var{p} = @qcode{\"fro\"}\n\
Frobenius norm of @var{A}, @code{sqrt (sumsq (abs (A)))}.\n\
\n\
@item @var{p} = 0\n\
Hamming norm - the number of nonzero elements.\n\
\n\
@item other @var{p}, @code{@var{p} > 1}\n\
p-norm of @var{A}, @code{(sum (abs (@var{A}) .^ @var{p})) ^ (1/@var{p})}.\n\
\n\
@item other @var{p} @code{@var{p} < 1}\n\
the p-pseudonorm defined as above.\n\
@end table\n\
\n\
If @var{opt} is the value @qcode{\"rows\"}, treat each row as a vector and\n\
compute its norm.  The result is returned as a column vector.\n\
Similarly, if @var{opt} is @qcode{\"columns\"} or @qcode{\"cols\"} then\n\
compute the norms of each column and return a row vector.\n\
@seealso{cond, svd}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin >= 1 && nargin <= 3)
    {
      octave_value x_arg = args(0);

      if (x_arg.ndims () == 2)
        {
          enum { sfmatrix, sfcols, sfrows, sffrob, sfinf } strflag = sfmatrix;
          if (nargin > 1 && args(nargin-1).is_string ())
            {
              std::string str = args(nargin-1).string_value ();
              if (str == "cols" || str == "columns")
                strflag = sfcols;
              else if (str == "rows")
                strflag = sfrows;
              else if (str == "fro")
                strflag = sffrob;
              else if (str == "inf")
                strflag = sfinf;
              else
                error ("norm: unrecognized option: %s", str.c_str ());
              // we've handled the last parameter, so act as if it was removed
              nargin --;
            }

          if (! error_state)
            {
              octave_value p_arg = (nargin > 1) ? args(1) : octave_value (2);

              if (p_arg.is_empty ())
                p_arg = octave_value (2);
              else if (p_arg.is_string ())
                {
                  std::string str = p_arg.string_value ();
                  if ((strflag == sfcols || strflag == sfrows))
                    {
                      if (str == "cols" || str == "columns" || str == "rows")
                        error ("norm: invalid combination of options");
                      else if (str == "fro")
                        p_arg = octave_value (2);
                      else if (str == "inf")
                        p_arg = octave_Inf;
                      else
                        error ("norm: unrecognized option: %s", str.c_str ());
                    }
                  else
                    error ("norm: invalid combination of options");
                }
              else if (! p_arg.is_scalar_type ())
                gripe_wrong_type_arg ("norm", p_arg, true);

              if (! error_state)
                {
                  switch (strflag)
                    {
                    case sfmatrix:
                      retval(0) = xnorm (x_arg, p_arg);
                      break;
                    case sfcols:
                      retval(0) = xcolnorms (x_arg, p_arg);
                      break;
                    case sfrows:
                      retval(0) = xrownorms (x_arg, p_arg);
                      break;
                    case sffrob:
                      retval(0) = xfrobnorm (x_arg);
                      break;
                    case sfinf:
                      retval(0) = xnorm (x_arg, octave_Inf);
                      break;
                    }
                }
            }
        }
      else
        error ("norm: only valid for 2-D objects");
    }
  else
    print_usage ();

  return retval;
}

/*
%!shared x
%! x = [1, -3, 4, 5, -7];
%!assert (norm (x,1), 20)
%!assert (norm (x,2), 10)
%!assert (norm (x,3), 8.24257059961711, -4*eps)
%!assert (norm (x,Inf), 7)
%!assert (norm (x,-Inf), 1)
%!assert (norm (x,"inf"), 7)
%!assert (norm (x,"fro"), 10, -eps)
%!assert (norm (x), 10)
%!assert (norm ([1e200, 1]), 1e200)
%!assert (norm ([3+4i, 3-4i, sqrt(31)]), 9, -4*eps)
%!shared m
%! m = magic (4);
%!assert (norm (m,1), 34)
%!assert (norm (m,2), 34, -eps)
%!assert (norm (m,Inf), 34)
%!assert (norm (m,"inf"), 34)
%!shared m2, flo, fhi
%! m2 = [1,2;3,4];
%! flo = 1e-300;
%! fhi = 1e+300;
%!assert (norm (flo*m2,"fro"), sqrt (30)*flo, -eps)
%!assert (norm (fhi*m2,"fro"), sqrt (30)*fhi, -eps)

%!shared x
%! x = single ([1, -3, 4, 5, -7]);
%!assert (norm (x,1), single (20))
%!assert (norm (x,2), single (10))
%!assert (norm (x,3), single (8.24257059961711), -4*eps ("single"))
%!assert (norm (x,Inf), single (7))
%!assert (norm (x,-Inf), single (1))
%!assert (norm (x,"inf"), single (7))
%!assert (norm (x,"fro"), single (10), -eps ("single"))
%!assert (norm (x), single (10))
%!assert (norm (single ([1e38, 1])), single (1e38))
%!assert (norm (single ([3+4i, 3-4i, sqrt(31)])), single (9), -4*eps ("single"))
%!shared m
%! m = single (magic (4));
%!assert (norm (m,1), single (34))
%!assert (norm (m,2), single (34), -eps ("single"))
%!assert (norm (m,Inf), single (34))
%!assert (norm (m,"inf"), single (34))
%!shared m2, flo, fhi
%! m2 = single ([1,2;3,4]);
%! flo = single (1e-300);
%! fhi = single (1e+300);
%!assert (norm (flo*m2,"fro"), single (sqrt (30)*flo), -eps ("single"))
%!assert (norm (fhi*m2,"fro"), single (sqrt (30)*fhi), -eps ("single"))

%!shared q
%! q = rand (1e3, 3);
%!assert (norm (q, 3, "rows"), sum (q.^3, 2).^(1/3), sqrt (eps));
%!assert (norm (q, "fro", "rows"), sum (q.^2, 2).^(1/2), sqrt (eps));
%!assert (norm (q, "fro", "rows"), sqrt (sumsq (q, 2)), sqrt (eps));
%!assert (norm (q, "fro", "cols"), sqrt (sumsq (q, 1)), sqrt (eps));
%!assert (norm (q, 3, "cols"), sum (q.^3, 1).^(1/3), sqrt (eps));
%!assert (norm (q, "inf", "rows"), norm (q, Inf, "rows"));
%!assert (norm (q, "inf", "cols"), norm (q, Inf, "cols"));
%!assert (norm (q, [], "rows"), norm (q, 2, "rows"));
%!assert (norm (q, [], "cols"), norm (q, 2, "cols"));

%!test
%! ## Test for norm returning NaN on sparse matrix (bug #30631)
%! A = sparse (2,2);
%! A(2,1) = 1;
%! assert (norm (A), 1);
*/

static octave_value
unary_op_defun_body (octave_value::unary_op op,
                     const octave_value_list& args)
{
  octave_value retval;
  if (args.length () == 1)
    retval = do_unary_op (op, args(0));
  else
    print_usage ();

  return retval;
}

DEFUN (not, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{z} =} not (@var{x})\n\
Return the logical NOT of @var{x}.\n\
\n\
This function is equivalent to the operator syntax @w{@code{! x}}.\n\
@seealso{and, or, xor}\n\
@end deftypefn")
{
  return unary_op_defun_body (octave_value::op_not, args);
}

DEFUN (uplus, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} uplus (@var{x})\n\
This function and @w{@tcode{+ x}} are equivalent.\n\
@seealso{uminus, plus, minus}\n\
@end deftypefn")
{
  return unary_op_defun_body (octave_value::op_uplus, args);
}

DEFUN (uminus, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} uminus (@var{x})\n\
This function and @w{@tcode{- x}} are equivalent.\n\
@seealso{uplus, minus}\n\
@end deftypefn")
{
  return unary_op_defun_body (octave_value::op_uminus, args);
}

DEFUN (transpose, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} transpose (@var{x})\n\
Return the transpose of @var{x}.\n\
\n\
This function and @tcode{x.'} are equivalent.\n\
@seealso{ctranspose}\n\
@end deftypefn")
{
  return unary_op_defun_body (octave_value::op_transpose, args);
}

/*
%!assert (2.', 2)
%!assert (2i.', 2i)
%!assert ([1:4].', [1;2;3;4])
%!assert ([1;2;3;4].', [1:4])
%!assert ([1,2;3,4].', [1,3;2,4])
%!assert ([1,2i;3,4].', [1,3;2i,4])

%!assert (transpose ([1,2;3,4]), [1,3;2,4])

%!assert (single (2).', single (2))
%!assert (single (2i).', single (2i))
%!assert (single ([1:4]).', single ([1;2;3;4]))
%!assert (single ([1;2;3;4]).', single ([1:4]))
%!assert (single ([1,2;3,4]).', single ([1,3;2,4]))
%!assert (single ([1,2i;3,4]).', single ([1,3;2i,4]))

%!assert (transpose (single ([1,2;3,4])), single ([1,3;2,4]))
*/

DEFUN (ctranspose, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ctranspose (@var{x})\n\
Return the complex conjugate transpose of @var{x}.\n\
\n\
This function and @tcode{x'} are equivalent.\n\
@seealso{transpose}\n\
@end deftypefn")
{
  return unary_op_defun_body (octave_value::op_hermitian, args);
}

/*
%!assert (2', 2)
%!assert (2i', -2i)
%!assert ([1:4]', [1;2;3;4])
%!assert ([1;2;3;4]', [1:4])
%!assert ([1,2;3,4]', [1,3;2,4])
%!assert ([1,2i;3,4]', [1,3;-2i,4])

%!assert (ctranspose ([1,2i;3,4]), [1,3;-2i,4])

%!assert (single (2)', single (2))
%!assert (single (2i)', single (-2i))
%!assert (single ([1:4])', single ([1;2;3;4]))
%!assert (single ([1;2;3;4])', single ([1:4]))
%!assert (single ([1,2;3,4])', single ([1,3;2,4]))
%!assert (single ([1,2i;3,4])', single ([1,3;-2i,4]))

%!assert (ctranspose (single ([1,2i;3,4])), single ([1,3;-2i,4]))
*/

static octave_value
binary_op_defun_body (octave_value::binary_op op,
                      const octave_value_list& args)
{
  octave_value retval;

  if (args.length () == 2)
    retval = do_binary_op (op, args(0), args(1));
  else
    print_usage ();

  return retval;
}

static octave_value
binary_assoc_op_defun_body (octave_value::binary_op op,
                            octave_value::assign_op aop,
                            const octave_value_list& args)
{
  octave_value retval;
  int nargin = args.length ();

  switch (nargin)
    {
    case 0:
      print_usage ();
      break;
    case 1:
      retval = args(0);
      break;
    case 2:
      retval = do_binary_op (op, args(0), args(1));
      break;
    default:
      retval = do_binary_op (op, args(0), args(1));
      for (int i = 2; i < nargin; i++)
        retval.assign (aop, args(i));
      break;
    }

  return retval;
}

DEFUN (plus, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} plus (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {} plus (@var{x1}, @var{x2}, @dots{})\n\
This function and @w{@tcode{x + y}} are equivalent.\n\
\n\
If more arguments are given, the summation is applied\n\
cumulatively from left to right:\n\
\n\
@example\n\
(@dots{}((x1 + x2) + x3) + @dots{})\n\
@end example\n\
\n\
At least one argument is required.\n\
@seealso{minus, uplus}\n\
@end deftypefn")
{
  return binary_assoc_op_defun_body (octave_value::op_add,
                                     octave_value::op_add_eq, args);
}

DEFUN (minus, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} minus (@var{x}, @var{y})\n\
This function and @w{@tcode{x - y}} are equivalent.\n\
@seealso{plus, uminus}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_sub, args);
}

DEFUN (mtimes, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} mtimes (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {} mtimes (@var{x1}, @var{x2}, @dots{})\n\
Return the matrix multiplication product of inputs.\n\
\n\
This function and @w{@tcode{x * y}} are equivalent.\n\
If more arguments are given, the multiplication is applied\n\
cumulatively from left to right:\n\
\n\
@example\n\
(@dots{}((x1 * x2) * x3) * @dots{})\n\
@end example\n\
\n\
At least one argument is required.\n\
@seealso{times, plus, minus, rdivide, mrdivide, mldivide, mpower}\n\
@end deftypefn")
{
  return binary_assoc_op_defun_body (octave_value::op_mul,
                                     octave_value::op_mul_eq, args);
}

DEFUN (mrdivide, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mrdivide (@var{x}, @var{y})\n\
Return the matrix right division of @var{x} and @var{y}.\n\
\n\
This function and @w{@tcode{x / y}} are equivalent.\n\
@seealso{mldivide, rdivide, plus, minus}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_div, args);
}

DEFUN (mpower, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mpower (@var{x}, @var{y})\n\
Return the matrix power operation of @var{x} raised to the @var{y} power.\n\
\n\
This function and @w{@tcode{x ^ y}} are equivalent.\n\
@seealso{power, mtimes, plus, minus}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_pow, args);
}

DEFUN (mldivide, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mldivide (@var{x}, @var{y})\n\
Return the matrix left division of @var{x} and @var{y}.\n\
\n\
This function and @w{@tcode{x @xbackslashchar{} y}} are equivalent.\n\
@seealso{mrdivide, ldivide, rdivide}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_ldiv, args);
}

DEFUN (lt, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} lt (@var{x}, @var{y})\n\
This function is equivalent to @w{@code{x < y}}.\n\
@seealso{le, eq, ge, gt, ne}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_lt, args);
}

DEFUN (le, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} le (@var{x}, @var{y})\n\
This function is equivalent to @w{@code{x <= y}}.\n\
@seealso{eq, ge, gt, ne, lt}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_le, args);
}

DEFUN (eq, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} eq (@var{x}, @var{y})\n\
Return true if the two inputs are equal.\n\
\n\
This function is equivalent to @w{@code{x == y}}.\n\
@seealso{ne, isequal, le, ge, gt, ne, lt}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_eq, args);
}

DEFUN (ge, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ge (@var{x}, @var{y})\n\
This function is equivalent to @w{@code{x >= y}}.\n\
@seealso{le, eq, gt, ne, lt}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_ge, args);
}

DEFUN (gt, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} gt (@var{x}, @var{y})\n\
This function is equivalent to @w{@code{x > y}}.\n\
@seealso{le, eq, ge, ne, lt}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_gt, args);
}

DEFUN (ne, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ne (@var{x}, @var{y})\n\
Return true if the two inputs are not equal.\n\
\n\
This function is equivalent to @w{@code{x != y}}.\n\
@seealso{eq, isequal, le, ge, lt}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_ne, args);
}

DEFUN (times, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} times (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {} times (@var{x1}, @var{x2}, @dots{})\n\
Return the element-by-element multiplication product of inputs.\n\
\n\
This function and @w{@tcode{x .* y}} are equivalent.\n\
If more arguments are given, the multiplication is applied\n\
cumulatively from left to right:\n\
\n\
@example\n\
(@dots{}((x1 .* x2) .* x3) .* @dots{})\n\
@end example\n\
\n\
At least one argument is required.\n\
@seealso{mtimes, rdivide}\n\
@end deftypefn")
{
  return binary_assoc_op_defun_body (octave_value::op_el_mul,
                                     octave_value::op_el_mul_eq, args);
}

DEFUN (rdivide, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rdivide (@var{x}, @var{y})\n\
Return the element-by-element right division of @var{x} and @var{y}.\n\
\n\
This function and @w{@tcode{x ./ y}} are equivalent.\n\
@seealso{ldivide, mrdivide, times, plus}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_el_div, args);
}

DEFUN (power, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} power (@var{x}, @var{y})\n\
Return the element-by-element operation of @var{x} raised to the\n\
@var{y} power.\n\
\n\
This function and @w{@tcode{x .^ y}} are equivalent.\n\
\n\
If several complex results are possible, returns the one with smallest\n\
non-negative argument (angle).  Use @code{realpow}, @code{realsqrt},\n\
@code{cbrt}, or @code{nthroot} if a real result is preferred.\n\
\n\
@seealso{mpower, realpow, realsqrt, cbrt, nthroot}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_el_pow, args);
}

DEFUN (ldivide, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ldivide (@var{x}, @var{y})\n\
Return the element-by-element left division of @var{x} and @var{y}.\n\
\n\
This function and @w{@tcode{x .@xbackslashchar{} y}} are equivalent.\n\
@seealso{rdivide, mldivide, times, plus}\n\
@end deftypefn")
{
  return binary_op_defun_body (octave_value::op_el_ldiv, args);
}

DEFUN (and, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{z} =} and (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {@var{z} =} and (@var{x1}, @var{x2}, @dots{})\n\
Return the logical AND of @var{x} and @var{y}.\n\
\n\
This function is equivalent to the operator syntax @w{@code{x & y}}.  If\n\
more than two arguments are given, the logical AND is applied cumulatively\n\
from left to right:\n\
\n\
@example\n\
(@dots{}((x1 & x2) & x3) & @dots{})\n\
@end example\n\
\n\
At least one argument is required.\n\
@seealso{or, not, xor}\n\
@end deftypefn")
{
  return binary_assoc_op_defun_body (octave_value::op_el_and,
                                     octave_value::op_el_and_eq, args);
}

DEFUN (or, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{z} =} or (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {@var{z} =} or (@var{x1}, @var{x2}, @dots{})\n\
Return the logical OR of @var{x} and @var{y}.\n\
\n\
This function is equivalent to the operator syntax @w{@code{x | y}}.  If\n\
more than two arguments are given, the logical OR is applied cumulatively\n\
from left to right:\n\
\n\
@example\n\
(@dots{}((x1 | x2) | x3) | @dots{})\n\
@end example\n\
\n\
At least one argument is required.\n\
@seealso{and, not, xor}\n\
@end deftypefn")
{
  return binary_assoc_op_defun_body (octave_value::op_el_or,
                                     octave_value::op_el_or_eq, args);
}

DEFUN (colon, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{r} =} colon (@var{base}, @var{limit})\n\
@deftypefnx {Built-in Function} {@var{r} =} colon (@var{base}, @var{increment}, @var{limit})\n\
Return the result of the colon expression corresponding to @var{base},\n\
@var{limit}, and optionally, @var{increment}.\n\
\n\
This function is equivalent to the operator syntax @w{@code{base : limit}}\n\
or @w{@code{base : increment : limit}}.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  switch (nargin)
    {
    case 2:
      retval = do_colon_op (args(0), args(1));
      break;

    case 3:
      retval = do_colon_op (args(0), args(1), args (2));
      break;

    default:
      print_usage ();
      break;
    }

  return retval;
}

static double tic_toc_timestamp = -1.0;

DEFUN (tic, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} tic ()\n\
@deftypefnx {Built-in Function} {@var{id} =} tic ()\n\
@deftypefnx {Built-in Function} {} toc ()\n\
@deftypefnx {Built-in Function} {} toc (@var{id})\n\
@deftypefnx {Built-in Function} {@var{val} =} toc (@dots{})\n\
Set or check a wall-clock timer.\n\
\n\
Calling @code{tic} without an output argument sets the internal timer state.\n\
Subsequent calls to @code{toc} return the number of seconds since the timer\n\
was set.\n\
For example,\n\
\n\
@example\n\
@group\n\
tic ();\n\
# many computations later@dots{}\n\
elapsed_time = toc ();\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will set the variable @code{elapsed_time} to the number of seconds since\n\
the most recent call to the function @code{tic}.\n\
\n\
If called with one output argument, @code{tic} returns a scalar\n\
of type @code{uint64} that may be later passed to @code{toc}.\n\
\n\
@example\n\
@group\n\
id = tic; sleep (5); toc (id)\n\
      @result{} 5.0010\n\
@end group\n\
@end example\n\
\n\
Calling @code{tic} and @code{toc} this way allows nested timing calls.\n\
\n\
If you are more interested in the CPU time that your process used, you\n\
should use the @code{cputime} function instead.  The @code{tic} and\n\
@code{toc} functions report the actual wall clock time that elapsed\n\
between the calls.  This may include time spent processing other jobs or\n\
doing nothing at all.\n\
@seealso{toc, cputime}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("tic: ignoring extra arguments");

  octave_time now;

  double tmp = now.double_value ();

  if (nargout > 0)
    {
      double ip = 0.0;
      double frac = modf (tmp, &ip);
      uint64_t microsecs = static_cast<uint64_t> (CLOCKS_PER_SEC * frac);
      microsecs += CLOCKS_PER_SEC * static_cast<uint64_t> (ip);
      retval = octave_uint64 (microsecs);
    }
  else
    tic_toc_timestamp = tmp;

  return retval;
}

DEFUN (toc, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} toc ()\n\
@deftypefnx {Built-in Function} {} toc (@var{id})\n\
@deftypefnx {Built-in Function} {@var{val} =} toc (@dots{})\n\
@seealso{tic, cputime}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  double start_time = tic_toc_timestamp;

  if (nargin > 1)
    print_usage ();
  else
    {
      if (nargin == 1)
        {
          octave_uint64 id = args(0).uint64_scalar_value ();

          if (! error_state)
            {
              uint64_t val = id.value ();

              start_time
                = (static_cast<double> (val / CLOCKS_PER_SEC)
                   + static_cast<double> (val % CLOCKS_PER_SEC)
                   / CLOCKS_PER_SEC);

              // FIXME: should we also check to see whether the start
              // time is after the beginning of this Octave session?
            }
          else
            error ("toc: invalid ID");
        }

      if (! error_state)
        {
          if (start_time < 0)
            error ("toc called before timer set");
          else
            {
              octave_time now;

              double tmp = now.double_value () - start_time;

              if (nargout > 0)
                retval = tmp;
              else
                octave_stdout << "Elapsed time is " << tmp << " seconds.\n";
            }
        }
    }

  return retval;
}

/*
%!shared id
%! id = tic ();
%!assert (isa (id, "uint64"))
%!assert (isa (toc (id), "double"))
*/

DEFUN (cputime, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{total}, @var{user}, @var{system}] =} cputime ();\n\
Return the CPU time used by your Octave session.\n\
\n\
The first output is the total time spent executing your process and is equal\n\
to the sum of second and third outputs, which are the number of CPU seconds\n\
spent executing in user mode and the number of CPU seconds spent executing\n\
in system mode, respectively.\n\
\n\
If your system does not have a way to report CPU time usage, @code{cputime}\n\
returns 0 for each of its output values.\n\
\n\
Note that because Octave used some CPU time to start, it is reasonable\n\
to check to see if @code{cputime} works by checking to see if the total\n\
CPU time used is nonzero.\n\
@seealso{tic, toc}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();
  double usr = 0.0;
  double sys = 0.0;

  if (nargin != 0)
    warning ("tic: ignoring extra arguments");

#if defined (HAVE_GETRUSAGE)

  struct rusage ru;

  getrusage (RUSAGE_SELF, &ru);

  usr = static_cast<double> (ru.ru_utime.tv_sec) +
        static_cast<double> (ru.ru_utime.tv_usec) * 1e-6;

  sys = static_cast<double> (ru.ru_stime.tv_sec) +
        static_cast<double> (ru.ru_stime.tv_usec) * 1e-6;

#else

  struct tms t;

  times (&t);

  unsigned long ticks;
  unsigned long seconds;
  unsigned long fraction;

  ticks = t.tms_utime + t.tms_cutime;
  fraction = ticks % CLOCKS_PER_SEC;
  seconds = ticks / CLOCKS_PER_SEC;

  usr = static_cast<double> (seconds) + static_cast<double>(fraction) /
        static_cast<double>(CLOCKS_PER_SEC);

  ticks = t.tms_stime + t.tms_cstime;
  fraction = ticks % CLOCKS_PER_SEC;
  seconds = ticks / CLOCKS_PER_SEC;

  sys = static_cast<double> (seconds) + static_cast<double>(fraction) /
        static_cast<double>(CLOCKS_PER_SEC);

#endif

  retval(2) = sys;
  retval(1) = usr;
  retval(0) = sys + usr;

  return retval;
}

DEFUN (sort, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{s}, @var{i}] =} sort (@var{x})\n\
@deftypefnx {Built-in Function} {[@var{s}, @var{i}] =} sort (@var{x}, @var{dim})\n\
@deftypefnx {Built-in Function} {[@var{s}, @var{i}] =} sort (@var{x}, @var{mode})\n\
@deftypefnx {Built-in Function} {[@var{s}, @var{i}] =} sort (@var{x}, @var{dim}, @var{mode})\n\
Return a copy of @var{x} with the elements arranged in increasing order.\n\
\n\
For matrices, @code{sort} orders the elements within columns\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
sort ([1, 2; 2, 3; 3, 1])\n\
   @result{}  1  1\n\
       2  2\n\
       3  3\n\
@end group\n\
@end example\n\
\n\
If the optional argument @var{dim} is given, then the matrix is sorted\n\
along the dimension defined by @var{dim}.  The optional argument @code{mode}\n\
defines the order in which the values will be sorted.  Valid values of\n\
@code{mode} are @qcode{\"ascend\"} or @qcode{\"descend\"}.\n\
\n\
The @code{sort} function may also be used to produce a matrix\n\
containing the original row indices of the elements in the sorted\n\
matrix.  For example:\n\
\n\
@example\n\
@group\n\
[s, i] = sort ([1, 2; 2, 3; 3, 1])\n\
  @result{} s = 1  1\n\
         2  2\n\
         3  3\n\
  @result{} i = 1  3\n\
         2  1\n\
         3  2\n\
@end group\n\
@end example\n\
\n\
For equal elements, the indices are such that equal elements are listed\n\
in the order in which they appeared in the original list.\n\
\n\
Sorting of complex entries is done first by magnitude\n\
(@w{@code{abs (@var{z})}}) and for any ties by phase angle\n\
(@w{@code{angle (z)}}).  For example:\n\
\n\
@example\n\
@group\n\
sort ([1+i; 1; 1-i])\n\
    @result{} 1 + 0i\n\
       1 - 1i\n\
       1 + 1i\n\
@end group\n\
@end example\n\
\n\
NaN values are treated as being greater than any other value and are sorted\n\
to the end of the list.\n\
\n\
The @code{sort} function may also be used to sort strings and cell arrays\n\
of strings, in which case ASCII dictionary order (uppercase 'A' precedes\n\
lowercase 'a') of the strings is used.\n\
\n\
The algorithm used in @code{sort} is optimized for the sorting of partially\n\
ordered lists.\n\
@seealso{sortrows, issorted}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  sortmode smode = ASCENDING;

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  bool return_idx = nargout > 1;

  octave_value arg = args(0);

  int dim = 0;
  if (nargin > 1)
    {
      if (args(1).is_string ())
        {
          std::string mode = args(1).string_value ();
          if (mode == "ascend")
            smode = ASCENDING;
          else if (mode == "descend")
            smode = DESCENDING;
          else
            {
              error ("sort: MODE must be either \"ascend\" or \"descend\"");
              return retval;
            }
        }
      else
        dim = args(1).nint_value () - 1;
    }

  if (nargin > 2)
    {
      if (args(1).is_string ())
        {
          print_usage ();
          return retval;
        }

      if (! args(2).is_string ())
        {
          error ("sort: MODE must be a string");
          return retval;
        }
      std::string mode = args(2).string_value ();
      if (mode == "ascend")
        smode = ASCENDING;
      else if (mode == "descend")
        smode = DESCENDING;
      else
        {
          error ("sort: MODE must be either \"ascend\" or \"descend\"");
          return retval;
        }
    }

  const dim_vector dv = arg.dims ();
  if (nargin == 1 || args(1).is_string ())
    {
      // Find first non singleton dimension
      dim = dv.first_non_singleton ();
    }
  else
    {
      if (dim < 0)
        {
          error ("sort: DIM must be a valid dimension");
          return retval;
        }
    }

  if (return_idx)
    {
      retval.resize (2);

      Array<octave_idx_type> sidx;

      retval(0) = arg.sort (sidx, dim, smode);
      retval(1) = idx_vector (sidx, dv(dim)); // No checking, extent is known.
    }
  else
    retval(0) = arg.sort (dim, smode);

  return retval;
}

/*
## Double
%!assert (sort ([NaN, 1, -1, 2, Inf]), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 1), [NaN, 1, -1, 2, Inf])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 3), [NaN, 1, -1, 2, Inf])
%!assert (sort ([NaN, 1, -1, 2, Inf], "ascend"), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2, "ascend"), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], "descend"), [NaN, Inf, 2, 1, -1])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2, "descend"), [NaN, Inf, 2, 1, -1])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4]), [3, 1, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4], 1), [3, 1, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4], 2), [1, 3, 5, 7; 2, 4, 6, 8])
%!assert (sort (1), 1)

%!test
%! [v, i] = sort ([NaN, 1, -1, Inf, 1]);
%! assert (v, [-1, 1, 1, Inf, NaN]);
%! assert (i, [3, 2, 5, 4, 1]);

## Complex
%!assert (sort ([NaN, 1i, -1, 2, Inf]), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 1), [NaN, 1i, -1, 2, Inf])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 3), [NaN, 1i, -1, 2, Inf])
%!assert (sort ([NaN, 1i, -1, 2, Inf], "ascend"), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2, "ascend"), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], "descend"), [NaN, Inf, 2, -1, 1i])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2, "descend"), [NaN, Inf, 2, -1, 1i])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4]), [3, 1i, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4], 1), [3, 1i, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4], 2), [1i, 3, 5, 7; 2, 4, 6, 8])
%!assert (sort (1i), 1i)

%!test
%! [v, i] = sort ([NaN, 1i, -1, Inf, 1, 1i]);
%! assert (v, [1, 1i, 1i, -1, Inf, NaN]);
%! assert (i, [5, 2, 6, 3, 4, 1]);

## Single
%!assert (sort (single ([NaN, 1, -1, 2, Inf])), single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 1), single ([NaN, 1, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 2), single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 3), single ([NaN, 1, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), "ascend"), single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 2, "ascend"), single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), "descend"), single ([NaN, Inf, 2, 1, -1]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 2, "descend"), single ([NaN, Inf, 2, 1, -1]))
%!assert (sort (single ([3, 1, 7, 5; 8, 2, 6, 4])), single ([3, 1, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1, 7, 5; 8, 2, 6, 4]), 1), single ([3, 1, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1, 7, 5; 8, 2, 6, 4]), 2), single ([1, 3, 5, 7; 2, 4, 6, 8]))
%!assert (sort (single (1)), single (1))

%!test
%! [v, i] = sort (single ([NaN, 1, -1, Inf, 1]));
%! assert (v, single ([-1, 1, 1, Inf, NaN]));
%! assert (i, [3, 2, 5, 4, 1]);

## Single Complex
%!assert (sort (single ([NaN, 1i, -1, 2, Inf])), single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 1), single ([NaN, 1i, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 2), single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 3), single ([NaN, 1i, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), "ascend"), single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 2, "ascend"), single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), "descend"), single ([NaN, Inf, 2, -1, 1i]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 2, "descend"), single ([NaN, Inf, 2, -1, 1i]))
%!assert (sort (single ([3, 1i, 7, 5; 8, 2, 6, 4])), single ([3, 1i, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1i, 7, 5; 8, 2, 6, 4]), 1), single ([3, 1i, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1i, 7, 5; 8, 2, 6, 4]), 2), single ([1i, 3, 5, 7; 2, 4, 6, 8]))
%!assert (sort (single (1i)), single (1i))

%!test
%! [v, i] = sort (single ([NaN, 1i, -1, Inf, 1, 1i]));
%! assert (v, single ([1, 1i, 1i, -1, Inf, NaN]));
%! assert (i, [5, 2, 6, 3, 4, 1]);

## Bool
%!assert (sort ([true, false, true, false]), [false, false, true, true])
%!assert (sort ([true, false, true, false], 1), [true, false, true, false])
%!assert (sort ([true, false, true, false], 2), [false, false, true, true])
%!assert (sort ([true, false, true, false], 3), [true, false, true, false])
%!assert (sort ([true, false, true, false], "ascend"), [false, false, true, true])
%!assert (sort ([true, false, true, false], 2, "ascend"), [false, false, true, true])
%!assert (sort ([true, false, true, false], "descend"), [true, true, false, false])
%!assert (sort ([true, false, true, false], 2, "descend"), [true, true, false, false])
%!assert (sort (true), true)

%!test
%! [v, i] = sort ([true, false, true, false]);
%! assert (v, [false, false, true, true]);
%! assert (i, [2, 4, 1, 3]);

## Sparse Double
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf])), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 1), sparse ([0, NaN, 1, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 3), sparse ([0, NaN, 1, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), "ascend"), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2, "ascend"), sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), "descend"), sparse ([NaN, Inf, 2, 1, 0, 0, -1]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2, "descend"), sparse ([NaN, Inf, 2, 1, 0, 0, -1]))

%!shared a
%! a = randn (10, 10);
%! a(a < 0) = 0;
%!assert (sort (sparse (a)), sparse (sort (a)))
%!assert (sort (sparse (a), 1), sparse (sort (a, 1)))
%!assert (sort (sparse (a), 2), sparse (sort (a, 2)))
%!test
%! [v, i] = sort (a);
%! [vs, is] = sort (sparse (a));
%! assert (vs, sparse (v));
%! assert (is, i);

## Sparse Complex
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf])), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 1), sparse ([0, NaN, 1i, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 3), sparse ([0, NaN, 1i, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), "ascend"), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2, "ascend"), sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), "descend"), sparse ([NaN, Inf, 2, -1, 1i, 0, 0]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2, "descend"), sparse ([NaN, Inf, 2, -1, 1i, 0, 0]))

%!shared a
%! a = randn (10, 10);
%! a(a < 0) = 0;
%! a = 1i * a;
%!assert (sort (sparse (a)), sparse (sort (a)))
%!assert (sort (sparse (a), 1), sparse (sort (a, 1)))
%!assert (sort (sparse (a), 2), sparse (sort (a, 2)))
%!test
%! [v, i] = sort (a);
%! [vs, is] = sort (sparse (a));
%! assert (vs, sparse (v));
%! assert (is, i);

## Sparse Bool
%!assert (sort (sparse ([true, false, true, false])), sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 1), sparse ([true, false, true, false]))
%!assert (sort (sparse ([true, false, true, false]), 2), sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 3), sparse ([true, false, true, false]))
%!assert (sort (sparse ([true, false, true, false]), "ascend"), sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 2, "ascend"), sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), "descend"), sparse ([true, true, false, false]))
%!assert (sort (sparse ([true, false, true, false]), 2, "descend"), sparse ([true, true, false, false]))

%!test
%! [v, i] = sort (sparse ([true, false, true, false]));
%! assert (v, sparse ([false, false, true, true]));
%! assert (i, [2, 4, 1, 3]);

## Cell string array
%!shared a, b, c
%! a = {"Alice", "Cecile", "Eric", "Barry", "David"};
%! b = {"Alice", "Barry", "Cecile", "David", "Eric"};
%! c = {"Eric", "David", "Cecile", "Barry", "Alice"};
%!assert (sort (a), b)
%!assert (sort (a, 1), a)
%!assert (sort (a, 2), b)
%!assert (sort (a, 3), a)
%!assert (sort (a, "ascend"), b)
%!assert (sort (a, 2, "ascend"), b)
%!assert (sort (a, "descend"), c)
%!assert (sort (a, 2, "descend"), c)

%!test
%! [v, i] = sort (a);
%! assert (i, [1, 4, 2, 5, 3]);

%!error sort ()
%!error sort (1, 2, 3, 4)
*/

// Sort the rows of the matrix @var{a} according to the order
// specified by @var{mode}, which can either be 'ascend' or 'descend'
// and return the index vector corresponding to the sort order.
//
// This function does not yet support sparse matrices.

DEFUN (__sort_rows_idx__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __sort_rows_idx__ (@var{a}, @var{mode})\n\
Undocumented internal function.\n\
@end deftypefn\n")
{
  octave_value retval;

  int nargin = args.length ();
  sortmode smode = ASCENDING;

  if (nargin < 1 || nargin > 2 || (nargin == 2 && ! args(1).is_string ()))
    {
      print_usage ();
      return retval;
    }

  if (nargin > 1)
    {
      std::string mode = args(1).string_value ();
      if (mode == "ascend")
        smode = ASCENDING;
      else if (mode == "descend")
        smode = DESCENDING;
      else
        {
          error ("__sort_rows_idx__: MODE must be either \"ascend\" or \"descend\"");
          return retval;
        }
    }

  octave_value arg = args(0);

  if (arg.is_sparse_type ())
    error ("__sort_rows_idx__: sparse matrices not yet supported");
  if (arg.ndims () == 2)
    {
      Array<octave_idx_type> idx = arg.sort_rows_idx (smode);

      retval = octave_value (idx, true, true);
    }
  else
    error ("__sort_rows_idx__: needs a 2-dimensional object");

  return retval;
}

static sortmode
get_sort_mode_option (const octave_value& arg, const char *argn)
{
  // FIXME: we initialize to UNSORTED here to avoid a GCC warning
  // about possibly using sortmode uninitialized.
  // FIXME: shouldn't these modes be scoped inside a class?
  sortmode smode = UNSORTED;

  if (arg.is_string ())
    {
      std::string mode = arg.string_value ();
      if (mode == "ascending")
        smode = ASCENDING;
      else if (mode == "descending")
        smode = DESCENDING;
      else if (mode == "either")
        smode = UNSORTED;
      else
        error ("issorted: MODE must be \"ascending\", \"descending\", or \"either\"");
    }
  else
    error ("issorted: expecting %s argument to be a string", argn);

  return smode;
}

DEFUN (issorted, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} issorted (@var{a})\n\
@deftypefnx {Built-in Function} {} issorted (@var{a}, @var{mode})\n\
@deftypefnx {Built-in Function} {} issorted (@var{a}, \"rows\", @var{mode})\n\
Return true if the array is sorted according to @var{mode}, which\n\
may be either @qcode{\"ascending\"}, @qcode{\"descending\"}, or\n\
@qcode{\"either\"}.\n\
\n\
By default,  @var{mode} is @qcode{\"ascending\"}.  NaNs are treated in the\n\
same manner as @code{sort}.\n\
\n\
If the optional argument @qcode{\"rows\"} is supplied, check whether\n\
the array is sorted by rows as output by the function @code{sortrows}\n\
(with no options).\n\
\n\
This function does not support sparse matrices.\n\
@seealso{sort, sortrows}\n\
@end deftypefn\n")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  bool by_rows = false;

  sortmode smode = ASCENDING;

  if (nargin > 1)
    {
      octave_value mode_arg;

      if (nargin == 3)
        smode = get_sort_mode_option (args(2), "third");

      if (args(1).is_string ())
        {
          std::string tmp = args(1).string_value ();
          if (tmp == "rows")
            by_rows = true;
          else
            smode = get_sort_mode_option (args(1), "second");
        }
      else
        error ("issorted: second argument must be a string");

      if (error_state)
        return retval;
    }

  octave_value arg = args(0);

  if (by_rows)
    {
      if (arg.is_sparse_type ())
        error ("issorted: sparse matrices not yet supported");
      if (arg.ndims () == 2)
        retval = arg.is_sorted_rows (smode) != UNSORTED;
      else
        error ("issorted: A must be a 2-dimensional object");
    }
  else
    {
      if (arg.dims ().is_vector ())
        retval = args(0).is_sorted (smode) != UNSORTED;
      else
        error ("issorted: needs a vector");
    }

  return retval;
}

/*
%!shared sm, um, sv, uv
%! sm = [1, 2; 3, 4];
%! um = [3, 1; 2, 4];
%! sv = [1, 2, 3, 4];
%! uv = [2, 1, 4, 3];
%!assert (issorted (sm, "rows"))
%!assert (!issorted (um, "rows"))
%!assert (issorted (sv))
%!assert (!issorted (uv))
%!assert (issorted (sv'))
%!assert (!issorted (uv'))
%!assert (issorted (sm, "rows", "ascending"))
%!assert (!issorted (um, "rows", "ascending"))
%!assert (issorted (sv, "ascending"))
%!assert (!issorted (uv, "ascending"))
%!assert (issorted (sv', "ascending"))
%!assert (!issorted (uv', "ascending"))
%!assert (!issorted (sm, "rows", "descending"))
%!assert (issorted (flipud (sm), "rows", "descending"))
%!assert (!issorted (sv, "descending"))
%!assert (issorted (fliplr (sv), "descending"))
%!assert (!issorted (sv', "descending"))
%!assert (issorted (fliplr (sv)', "descending"))
%!assert (!issorted (um, "rows", "either"))
%!assert (!issorted (uv, "either"))
%!assert (issorted (sm, "rows", "either"))
%!assert (issorted (flipud (sm), "rows", "either"))
%!assert (issorted (sv, "either"))
%!assert (issorted (fliplr (sv), "either"))
%!assert (issorted (sv', "either"))
%!assert (issorted (fliplr (sv)', "either"))
*/

DEFUN (nth_element, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} nth_element (@var{x}, @var{n})\n\
@deftypefnx {Built-in Function} {} nth_element (@var{x}, @var{n}, @var{dim})\n\
Select the n-th smallest element of a vector, using the ordering defined by\n\
@code{sort}.\n\
\n\
The result is equivalent to @code{sort(@var{x})(@var{n})}.\n\
\n\
@var{n} can also be a contiguous range, either ascending @code{l:u}\n\
or descending @code{u:-1:l}, in which case a range of elements is returned.\n\
\n\
If @var{x} is an array, @code{nth_element} operates along the dimension\n\
defined by @var{dim}, or the first non-singleton dimension if @var{dim} is\n\
not given.\n\
\n\
Programming Note: nth_element encapsulates the C++ standard library\n\
algorithms nth_element and partial_sort.  On average, the complexity of the\n\
operation is O(M*log(K)), where @w{@code{M = size (@var{x}, @var{dim})}} and\n\
@w{@code{K = length (@var{n})}}.  This function is intended for cases where\n\
the ratio K/M is small; otherwise, it may be better to use @code{sort}.\n\
@seealso{sort, min, max}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      octave_value argx = args(0);

      int dim = -1;
      if (nargin == 3)
        {
          dim = args(2).int_value (true) - 1;
          if (dim < 0)
            error ("nth_element: DIM must be a valid dimension");
        }
      if (dim < 0)
        dim = argx.dims ().first_non_singleton ();

      idx_vector n = args(1).index_vector ();

      if (error_state)
        return retval;

      switch (argx.builtin_type ())
        {
        case btyp_double:
          retval = argx.array_value ().nth_element (n, dim);
          break;
        case btyp_float:
          retval = argx.float_array_value ().nth_element (n, dim);
          break;
        case btyp_complex:
          retval = argx.complex_array_value ().nth_element (n, dim);
          break;
        case btyp_float_complex:
          retval = argx.float_complex_array_value ().nth_element (n, dim);
          break;
#define MAKE_INT_BRANCH(X) \
        case btyp_ ## X: \
          retval = argx.X ## _array_value ().nth_element (n, dim); \
          break;

        MAKE_INT_BRANCH (int8);
        MAKE_INT_BRANCH (int16);
        MAKE_INT_BRANCH (int32);
        MAKE_INT_BRANCH (int64);
        MAKE_INT_BRANCH (uint8);
        MAKE_INT_BRANCH (uint16);
        MAKE_INT_BRANCH (uint32);
        MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH
        default:
          if (argx.is_cellstr ())
            retval = argx.cellstr_value ().nth_element (n, dim);
          else
            gripe_wrong_type_arg ("nth_element", argx);
        }
    }
  else
    print_usage ();

  return retval;
}

template <class NDT>
static NDT
do_accumarray_sum (const idx_vector& idx, const NDT& vals,
                   octave_idx_type n = -1)
{
  typedef typename NDT::element_type T;
  if (n < 0)
    n = idx.extent (0);
  else if (idx.extent (n) > n)
    error ("accumarray: index out of range");

  NDT retval (dim_vector (n, 1), T ());

  if (vals.numel () == 1)
    retval.idx_add (idx, vals (0));
  else if (vals.numel () == idx.length (n))
    retval.idx_add (idx, vals);
  else
    error ("accumarray: dimensions mismatch");

  return retval;
}

DEFUN (__accumarray_sum__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __accumarray_sum__ (@var{idx}, @var{vals}, @var{n})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin >= 2 && nargin <= 3 && args(0).is_numeric_type ())
    {
      idx_vector idx = args(0).index_vector ();
      octave_idx_type n = -1;
      if (nargin == 3)
        n = args(2).idx_type_value (true);

      if (! error_state)
        {
          octave_value vals = args(1);
          if (vals.is_range ())
            {
              Range r = vals.range_value ();
              if (r.inc () == 0)
                vals = r.base ();
            }

          if (vals.is_single_type ())
            {
              if (vals.is_complex_type ())
                retval = do_accumarray_sum (idx,
                                            vals.float_complex_array_value (),
                                            n);
              else
                retval = do_accumarray_sum (idx, vals.float_array_value (), n);
            }
          else if (vals.is_numeric_type () || vals.is_bool_type ())
            {
              if (vals.is_complex_type ())
                retval = do_accumarray_sum (idx,
                                            vals.complex_array_value (),
                                            n);
              else
                retval = do_accumarray_sum (idx, vals.array_value (), n);
            }
          else
            gripe_wrong_type_arg ("accumarray", vals);
        }
    }
  else
    print_usage ();

  return retval;
}

template <class NDT>
static NDT
do_accumarray_minmax (const idx_vector& idx, const NDT& vals,
                      octave_idx_type n, bool ismin,
                      const typename NDT::element_type& zero_val)
{
  typedef typename NDT::element_type T;
  if (n < 0)
    n = idx.extent (0);
  else if (idx.extent (n) > n)
    error ("accumarray: index out of range");

  NDT retval (dim_vector (n, 1), zero_val);

  // Pick minimizer or maximizer.
  void (MArray<T>::*op) (const idx_vector&, const MArray<T>&) =
    ismin ? (&MArray<T>::idx_min) : (&MArray<T>::idx_max);

  octave_idx_type l = idx.length (n);
  if (vals.numel () == 1)
    (retval.*op) (idx, NDT (dim_vector (l, 1), vals(0)));
  else if (vals.numel () == l)
    (retval.*op) (idx, vals);
  else
    error ("accumarray: dimensions mismatch");

  return retval;
}

static octave_value_list
do_accumarray_minmax_fun (const octave_value_list& args,
                          bool ismin)
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin >= 3 && nargin <= 4 && args(0).is_numeric_type ())
    {
      idx_vector idx = args(0).index_vector ();
      octave_idx_type n = -1;
      if (nargin == 4)
        n = args(3).idx_type_value (true);

      if (! error_state)
        {
          octave_value vals = args(1);
          octave_value zero = args(2);

          switch (vals.builtin_type ())
            {
            case btyp_double:
              retval = do_accumarray_minmax (idx, vals.array_value (), n, ismin,
                                             zero.double_value ());
              break;
            case btyp_float:
              retval = do_accumarray_minmax (idx, vals.float_array_value (), n,
                                             ismin, zero.float_value ());
              break;
            case btyp_complex:
              retval = do_accumarray_minmax (idx, vals.complex_array_value (),
                                             n, ismin, zero.complex_value ());
              break;
            case btyp_float_complex:
              retval = do_accumarray_minmax (idx,
                                             vals.float_complex_array_value (),
                                             n, ismin,
                                             zero.float_complex_value ());
              break;
#define MAKE_INT_BRANCH(X) \
            case btyp_ ## X: \
              retval = do_accumarray_minmax (idx, vals.X ## _array_value (), \
                                             n, ismin, \
                                             zero.X ## _scalar_value ()); \
              break;

            MAKE_INT_BRANCH (int8);
            MAKE_INT_BRANCH (int16);
            MAKE_INT_BRANCH (int32);
            MAKE_INT_BRANCH (int64);
            MAKE_INT_BRANCH (uint8);
            MAKE_INT_BRANCH (uint16);
            MAKE_INT_BRANCH (uint32);
            MAKE_INT_BRANCH (uint64);
#undef MAKE_INT_BRANCH
            case btyp_bool:
              retval = do_accumarray_minmax (idx, vals.array_value (), n, ismin,
                                             zero.bool_value ());
              break;
            default:
              gripe_wrong_type_arg ("accumarray", vals);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__accumarray_min__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __accumarray_min__ (@var{idx}, @var{vals}, @var{zero}, @var{n})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  return do_accumarray_minmax_fun (args, true);
}

DEFUN (__accumarray_max__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __accumarray_max__ (@var{idx}, @var{vals}, @var{zero}, @var{n})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  return do_accumarray_minmax_fun (args, false);
}

template <class NDT>
static NDT
do_accumdim_sum (const idx_vector& idx, const NDT& vals,
                 int dim = -1, octave_idx_type n = -1)
{
  typedef typename NDT::element_type T;
  if (n < 0)
    n = idx.extent (0);
  else if (idx.extent (n) > n)
    error ("accumdim: index out of range");

  dim_vector vals_dim = vals.dims ();
  dim_vector rdv = vals_dim;

  if (dim < 0)
    dim = vals.dims ().first_non_singleton ();
  else if (dim >= rdv.length ())
    rdv.resize (dim+1, 1);

  rdv(dim) = n;

  NDT retval (rdv, T ());

  if (idx.length () != vals_dim(dim))
    error ("accumdim: dimension mismatch");

  retval.idx_add_nd (idx, vals, dim);

  return retval;
}

DEFUN (__accumdim_sum__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __accumdim_sum__ (@var{idx}, @var{vals}, @var{dim}, @var{n})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin >= 2 && nargin <= 4 && args(0).is_numeric_type ())
    {
      idx_vector idx = args(0).index_vector ();
      int dim = -1;
      if (nargin >= 3)
        dim = args(2).int_value () - 1;

      octave_idx_type n = -1;
      if (nargin == 4)
        n = args(3).idx_type_value (true);

      if (! error_state)
        {
          octave_value vals = args(1);

          if (vals.is_single_type ())
            {
              if (vals.is_complex_type ())
                retval = do_accumdim_sum (idx,
                                          vals.float_complex_array_value (),
                                          dim, n);
              else
                retval = do_accumdim_sum (idx, vals.float_array_value (),
                                          dim, n);
            }
          else if (vals.is_numeric_type () || vals.is_bool_type ())
            {
              if (vals.is_complex_type ())
                retval = do_accumdim_sum (idx, vals.complex_array_value (),
                                          dim, n);
              else
                retval = do_accumdim_sum (idx, vals.array_value (), dim, n);
            }
          else
            gripe_wrong_type_arg ("accumdim", vals);
        }
    }
  else
    print_usage ();

  return retval;
}

template <class NDT>
static NDT
do_merge (const Array<bool>& mask,
          const NDT& tval, const NDT& fval)
{
  typedef typename NDT::element_type T;
  dim_vector dv = mask.dims ();
  NDT retval (dv);

  bool tscl = tval.numel () == 1;
  bool fscl = fval.numel () == 1;

  if ((! tscl && tval.dims () != dv)
      || (! fscl && fval.dims () != dv))
    error ("merge: MASK, TVAL, and FVAL dimensions must match");
  else
    {
      T *rv = retval.fortran_vec ();
      octave_idx_type n = retval.numel ();

      const T *tv = tval.data ();
      const T *fv = fval.data ();
      const bool *mv = mask.data ();

      if (tscl)
        {
          if (fscl)
            {
              T ts = tv[0];
              T fs = fv[0];
              for (octave_idx_type i = 0; i < n; i++)
                rv[i] = mv[i] ? ts : fs;
            }
          else
            {
              T ts = tv[0];
              for (octave_idx_type i = 0; i < n; i++)
                rv[i] = mv[i] ? ts : fv[i];
            }
        }
      else
        {
          if (fscl)
            {
              T fs = fv[0];
              for (octave_idx_type i = 0; i < n; i++)
                rv[i] = mv[i] ? tv[i] : fs;
            }
          else
            {
              for (octave_idx_type i = 0; i < n; i++)
                rv[i] = mv[i] ? tv[i] : fv[i];
            }
        }
    }

  return retval;
}

#define MAKE_INT_BRANCH(INTX) \
  else if (tval.is_ ## INTX ## _type () && fval.is_ ## INTX ## _type ()) \
    { \
      retval = do_merge (mask, \
                         tval.INTX ## _array_value (), \
                         fval.INTX ## _array_value ()); \
    }

DEFUN (merge, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} merge (@var{mask}, @var{tval}, @var{fval})\n\
@deftypefnx {Built-in Function} {} ifelse (@var{mask}, @var{tval}, @var{fval})\n\
Merge elements of @var{true_val} and @var{false_val}, depending on the\n\
value of @var{mask}.\n\
\n\
If @var{mask} is a logical scalar, the other two arguments can be arbitrary\n\
values.  Otherwise, @var{mask} must be a logical array, and @var{tval},\n\
@var{fval} should be arrays of matching class, or cell arrays.  In the\n\
scalar mask case, @var{tval} is returned if @var{mask} is true, otherwise\n\
@var{fval} is returned.\n\
\n\
In the array mask case, both @var{tval} and @var{fval} must be either\n\
scalars or arrays with dimensions equal to @var{mask}.  The result is\n\
constructed as follows:\n\
\n\
@example\n\
@group\n\
result(mask) = tval(mask);\n\
result(! mask) = fval(! mask);\n\
@end group\n\
@end example\n\
\n\
@var{mask} can also be arbitrary numeric type, in which case it is first\n\
converted to logical.\n\
@seealso{logical, diff}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin == 3 && (args(0).is_bool_type () || args(0).is_numeric_type ()))
    {
      octave_value mask_val = args(0);

      if (mask_val.is_scalar_type ())
        retval = mask_val.is_true () ? args(1) : args(2);
      else
        {
          boolNDArray mask = mask_val.bool_array_value ();
          octave_value tval = args(1);
          octave_value fval = args(2);
          if (tval.is_double_type () && fval.is_double_type ())
            {
              if (tval.is_complex_type () || fval.is_complex_type ())
                retval = do_merge (mask,
                                   tval.complex_array_value (),
                                   fval.complex_array_value ());
              else
                retval = do_merge (mask,
                                   tval.array_value (),
                                   fval.array_value ());
            }
          else if (tval.is_single_type () && fval.is_single_type ())
            {
              if (tval.is_complex_type () || fval.is_complex_type ())
                retval = do_merge (mask,
                                   tval.float_complex_array_value (),
                                   fval.float_complex_array_value ());
              else
                retval = do_merge (mask,
                                   tval.float_array_value (),
                                   fval.float_array_value ());
            }
          else if (tval.is_string () && fval.is_string ())
            {
              bool sq_string = tval.is_sq_string () || fval.is_sq_string ();
              retval = octave_value (do_merge (mask,
                                               tval.char_array_value (),
                                               fval.char_array_value ()),
                                     sq_string ? '\'' : '"');
            }
          else if (tval.is_cell () && fval.is_cell ())
            {
              retval = do_merge (mask,
                                 tval.cell_value (),
                                 fval.cell_value ());
            }

          MAKE_INT_BRANCH (int8)
          MAKE_INT_BRANCH (int16)
          MAKE_INT_BRANCH (int32)
          MAKE_INT_BRANCH (int64)
          MAKE_INT_BRANCH (uint8)
          MAKE_INT_BRANCH (uint16)
          MAKE_INT_BRANCH (uint32)
          MAKE_INT_BRANCH (uint64)

          else
            error ("merge: cannot merge %s with %s with array mask",
                   tval.class_name ().c_str (),
                   fval.class_name ().c_str ());
        }
    }
  else
    print_usage ();

  return retval;
}

DEFALIAS (ifelse, merge);

#undef MAKE_INT_BRANCH

template <class SparseT>
static SparseT
do_sparse_diff (const SparseT& array, octave_idx_type order,
                int dim)
{
  SparseT retval = array;
  if (dim == 1)
    {
      octave_idx_type k = retval.columns ();
      while (order > 0 && k > 0)
        {
          idx_vector col1 (':'), col2 (':'), sl1 (1, k), sl2 (0, k-1);
          retval = SparseT (retval.index (col1, sl1))
                 - SparseT (retval.index (col2, sl2));
          assert (retval.columns () == k-1);
          order--;
          k--;
        }
    }
  else
    {
      octave_idx_type k = retval.rows ();
      while (order > 0 && k > 0)
        {
          idx_vector col1 (':'), col2 (':'), sl1 (1, k), sl2 (0, k-1);
          retval = SparseT (retval.index (sl1, col1))
                 - SparseT (retval.index (sl2, col2));
          assert (retval.rows () == k-1);
          order--;
          k--;
        }
    }

  return retval;
}

static octave_value
do_diff (const octave_value& array, octave_idx_type order,
         int dim = -1)
{
  octave_value retval;

  const dim_vector& dv = array.dims ();
  if (dim == -1)
    {
      dim = array.dims ().first_non_singleton ();

      // Bother Matlab. This behavior is really wicked.
      if (dv(dim) <= order)
        {
          if (dv(dim) == 1)
            retval = array.resize (dim_vector (0, 0));
          else
            {
              retval = array;
              while (order > 0)
                {
                  if (dim == dv.length ())
                    {
                      retval = do_diff (array, order, dim - 1);
                      order = 0;
                    }
                  else if (dv(dim) == 1)
                    dim++;
                  else
                    {
                      retval = do_diff (array, dv(dim) - 1, dim);
                      order -= dv(dim) - 1;
                      dim++;
                    }
                }
            }

          return retval;
        }
    }

  if (array.is_integer_type ())
    {
      if (array.is_int8_type ())
        retval = array.int8_array_value ().diff (order, dim);
      else if (array.is_int16_type ())
        retval = array.int16_array_value ().diff (order, dim);
      else if (array.is_int32_type ())
        retval = array.int32_array_value ().diff (order, dim);
      else if (array.is_int64_type ())
        retval = array.int64_array_value ().diff (order, dim);
      else if (array.is_uint8_type ())
        retval = array.uint8_array_value ().diff (order, dim);
      else if (array.is_uint16_type ())
        retval = array.uint16_array_value ().diff (order, dim);
      else if (array.is_uint32_type ())
        retval = array.uint32_array_value ().diff (order, dim);
      else if (array.is_uint64_type ())
        retval = array.uint64_array_value ().diff (order, dim);
      else
        panic_impossible ();
    }
  else if (array.is_sparse_type ())
    {
      if (array.is_complex_type ())
        retval = do_sparse_diff (array.sparse_complex_matrix_value (),
                                 order, dim);
      else
        retval = do_sparse_diff (array.sparse_matrix_value (), order, dim);
    }
  else if (array.is_single_type ())
    {
      if (array.is_complex_type ())
        retval = array.float_complex_array_value ().diff (order, dim);
      else
        retval = array.float_array_value ().diff (order, dim);
    }
  else
    {
      if (array.is_complex_type ())
        retval = array.complex_array_value ().diff (order, dim);
      else
        retval = array.array_value ().diff (order, dim);
    }

  return retval;
}

DEFUN (diff, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} diff (@var{x})\n\
@deftypefnx {Built-in Function} {} diff (@var{x}, @var{k})\n\
@deftypefnx {Built-in Function} {} diff (@var{x}, @var{k}, @var{dim})\n\
If @var{x} is a vector of length @math{n}, @w{@code{diff (@var{x})}} is the\n\
vector of first differences\n\
@tex\n\
 $x_2 - x_1, \\ldots{}, x_n - x_{n-1}$.\n\
@end tex\n\
@ifnottex\n\
 @var{x}(2) - @var{x}(1), @dots{}, @var{x}(n) - @var{x}(n-1).\n\
@end ifnottex\n\
\n\
If @var{x} is a matrix, @w{@code{diff (@var{x})}} is the matrix of column\n\
differences along the first non-singleton dimension.\n\
\n\
The second argument is optional.  If supplied,\n\
@w{@code{diff (@var{x}, @var{k})}}, where @var{k} is a non-negative integer,\n\
returns the @var{k}-th differences.  It is possible that @var{k} is larger\n\
than the first non-singleton dimension of the matrix.  In this case,\n\
@code{diff} continues to take the differences along the next\n\
non-singleton dimension.\n\
\n\
The dimension along which to take the difference can be explicitly\n\
stated with the optional variable @var{dim}.  In this case the\n\
@var{k}-th order differences are calculated along this dimension.\n\
In the case where @var{k} exceeds @w{@code{size (@var{x}, @var{dim})}}\n\
an empty matrix is returned.\n\
@seealso{sort, merge}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin < 1 || nargin > 3)
    print_usage ();
  else if (! (args(0).is_numeric_type () || args(0).is_bool_type ()))
    error ("diff: X must be numeric or logical");

  if (! error_state)
    {
      int dim = -1;
      octave_idx_type order = 1;
      if (nargin > 1)
        {
          if (args(1).is_scalar_type ())
            order = args(1).idx_type_value (true, false);
          else if (! args(1).is_zero_by_zero ())
            error ("order K must be a scalar or []");
          if (! error_state && order < 0)
            error ("order K must be non-negative");
        }

      if (nargin > 2)
        {
          dim = args(2).int_value (true, false);
          if (! error_state && (dim < 1 || dim > args(0).ndims ()))
            error ("DIM must be a valid dimension");
          else
            dim -= 1;
        }

      if (! error_state)
        retval = do_diff (args(0), order, dim);
    }

  return retval;
}

/*
%!assert (diff ([1, 2, 3, 4]), [1, 1, 1])
%!assert (diff ([1, 3, 7, 19], 2), [2, 8])
%!assert (diff ([1, 2; 5, 4; 8, 7; 9, 6; 3, 1]), [4, 2; 3, 3; 1, -1; -6, -5])
%!assert (diff ([1, 2; 5, 4; 8, 7; 9, 6; 3, 1], 3), [-1, -5; -5, 0])
%!assert (isempty (diff (1)))

%!error diff ()
%!error diff (1, 2, 3, 4)
%!error diff ("foo")
%!error diff ([1, 2; 3, 4], -1)
*/

template <class T>
static Array<T>
do_repelems (const Array<T>& src, const Array<octave_idx_type>& rep)
{
  Array<T> retval;

  assert (rep.ndims () == 2 && rep.rows () == 2);

  octave_idx_type n = rep.columns ();
  octave_idx_type l = 0;
  for (octave_idx_type i = 0; i < n; i++)
    {
      octave_idx_type k = rep(1, i);
      if (k < 0)
        {
          error ("repelems: second row must contain non-negative numbers");
          return retval;
        }

      l += k;
    }

  retval.clear (1, l);
  T *dest = retval.fortran_vec ();
  l = 0;
  for (octave_idx_type i = 0; i < n; i++)
    {
      octave_idx_type k = rep(1, i);
      std::fill_n (dest, k, src.checkelem (rep(0, i) - 1));
      dest += k;
    }

  return retval;
}

DEFUN (repelems, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} repelems (@var{x}, @var{r})\n\
Construct a vector of repeated elements from @var{x}.\n\
\n\
@var{r} is a 2x@var{N} integer matrix specifying which elements to repeat and\n\
how often to repeat each element.  Entries in the first row, @var{r}(1,j),\n\
select an element to repeat.  The corresponding entry in the second row,\n\
@var{r}(2,j), specifies the repeat count.  If @var{x} is a matrix then the\n\
columns of @var{x} are imagined to be stacked on top of each other for\n\
purposes of the selection index.  A row vector is always returned.\n\
\n\
Conceptually the result is calculated as follows:\n\
\n\
@example\n\
@group\n\
y = [];\n\
for i = 1:columns (@var{r})\n\
  y = [y, @var{x}(@var{r}(1,i)*ones(1, @var{r}(2,i)))];\n\
endfor\n\
@end group\n\
@end example\n\
@seealso{repmat, cat}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      octave_value x = args(0);

      const Matrix rm = args(1).matrix_value ();
      if (error_state)
        return retval;
      else if (rm.rows () != 2 || rm.ndims () != 2)
        {
          error ("repelems: R must be a matrix with two rows");
          return retval;
        }
      else
        {
          NoAlias< Array<octave_idx_type> > r (rm.dims ());

          for (octave_idx_type i = 0; i < rm.numel (); i++)
            {
              octave_idx_type rx = rm(i);
              if (static_cast<double> (rx) != rm(i))
                {
                  error ("repelems: R must be a matrix of integers");
                  return retval;
                }

              r(i) = rx;
            }

          switch (x.builtin_type ())
            {
#define BTYP_BRANCH(X, EX) \
            case btyp_ ## X: \
              retval = do_repelems (x.EX ## _value (), r); \
              break;

              BTYP_BRANCH (double, array);
              BTYP_BRANCH (float, float_array);
              BTYP_BRANCH (complex, complex_array);
              BTYP_BRANCH (float_complex, float_complex_array);
              BTYP_BRANCH (bool, bool_array);
              BTYP_BRANCH (char, char_array);

              BTYP_BRANCH (int8,  int8_array);
              BTYP_BRANCH (int16, int16_array);
              BTYP_BRANCH (int32, int32_array);
              BTYP_BRANCH (int64, int64_array);
              BTYP_BRANCH (uint8,  uint8_array);
              BTYP_BRANCH (uint16, uint16_array);
              BTYP_BRANCH (uint32, uint32_array);
              BTYP_BRANCH (uint64, uint64_array);

              BTYP_BRANCH (cell, cell);
              //BTYP_BRANCH (struct, map);//FIXME
#undef BTYP_BRANCH

            default:
              gripe_wrong_type_arg ("repelems", x);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (base64_encode, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{s} =} base64_encode (@var{x})\n\
Encode a double matrix or array @var{x} into the base64 format string\n\
@var{s}.\n\
\n\
@seealso{base64_decode}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else
    {
      if (! args(0).is_numeric_type ())
        error ("base64_encode: encoding is supported only for numeric arrays");
      else if (args(0).is_complex_type ()
               || args(0).is_sparse_type ())
        error ("base64_encode: encoding complex or sparse data is not supported");
      else if (args(0).is_integer_type ())
        {
#define MAKE_INT_BRANCH(X)                                              \
          if (args(0).is_ ## X ## _type ())                             \
            {                                                           \
              const X##NDArray in = args(0).  X## _array_value ();      \
              size_t inlen =                                            \
                in.numel () * sizeof (X## _t) / sizeof (char);          \
              const char* inc =                                         \
                reinterpret_cast<const char*> (in.data ());             \
              char* out;                                                \
              if (! error_state                                         \
                  && octave_base64_encode (inc, inlen, &out))           \
                {                                                       \
                  retval(0) = octave_value (out);                       \
                  ::free (out);                                         \
                }                                                       \
            }

          MAKE_INT_BRANCH(int8)
          else MAKE_INT_BRANCH(int16)
          else MAKE_INT_BRANCH(int32)
          else MAKE_INT_BRANCH(int64)
          else MAKE_INT_BRANCH(uint8)
          else MAKE_INT_BRANCH(uint16)
          else MAKE_INT_BRANCH(uint32)
          else MAKE_INT_BRANCH(uint64)
#undef MAKE_INT_BRANCH

          else
            panic_impossible ();
        }
      else if (args(0).is_single_type ())
        {
          const Array<float> in = args(0).float_array_value ();
          size_t inlen;
          inlen = in.numel () * sizeof (float) / sizeof (char);
          const char*  inc;
          inc = reinterpret_cast<const char*> (in.data ());
          char* out;
          if (! error_state
              && octave_base64_encode (inc, inlen, &out))
            {
              retval(0) = octave_value (out);
              ::free (out);
            }
        }
      else
        {
          const Array<double> in = args(0).array_value ();
          size_t inlen;
          inlen = in.numel () * sizeof (double) / sizeof (char);
          const char*  inc;
          inc = reinterpret_cast<const char*> (in.data ());
          char* out;
          if (! error_state
              && octave_base64_encode (inc, inlen, &out))
            {
              retval(0) = octave_value (out);
              ::free (out);
            }
        }
    }
  return retval;
}

/*
%!assert (base64_encode (single (pi)), "2w9JQA==")
%!assert (base64_encode (uint8 ([0 0 0])), "AAAA")
%!assert (base64_encode (uint16 ([0 0 0])), "AAAAAAAA")
%!assert (base64_encode (uint32 ([0 0 0])), "AAAAAAAAAAAAAAAA")
%!assert (base64_encode (uint64 ([0 0 0])), "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
%!assert (base64_encode (uint8 ([255 255 255])), "////")

%!error base64_encode ()
%!error base64_encode (1,2)
%!error base64_encode ("A string")
%!error base64_encode ({"A cell array"})
%!error base64_encode (struct ())
*/

DEFUN (base64_decode, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{x} =} base64_decode (@var{s})\n\
@deftypefnx {Built-in Function} {@var{x} =} base64_decode (@var{s}, @var{dims})\n\
Decode the double matrix or array @var{x} from the base64 encoded string\n\
@var{s}.\n\
\n\
The optional input parameter @var{dims} should be a vector containing the\n\
dimensions of the decoded array.\n\
@seealso{base64_encode}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();
  else
    {
      dim_vector dims;

      if (nargin > 1)
        {
          const Array<octave_idx_type> size =
            args(1).octave_idx_type_vector_value ();

          if (! error_state)
            {
              dims = dim_vector::alloc (size.length ());
              for (octave_idx_type i = 0; i < size.length (); i++)
                dims(i) = size(i);
            }
        }

      const std::string str = args(0).string_value ();

      if (! error_state)
        {
          Array<double> res = octave_base64_decode (str);

          if (nargin > 1)
            res = res.reshape (dims);

          retval = res;
        }
    }

  return retval;
}

/*
%!assert (base64_decode (base64_encode (pi)), pi)
%!
%!test
%! in   = randn (10);
%! outv = base64_decode (base64_encode (in));
%! outm = base64_decode (base64_encode (in), size (in));
%! assert (outv, in(:).');
%! assert (outm, in);

%!error base64_decode ()
%!error base64_decode (1,2,3)
%!error base64_decode (1, "this is not a valid set of dimensions")
%!error <input was not valid base64> base64_decode (1)
%!error <input was not valid base64> base64_decode ("AQ=")
%!error <incorrect input size> base64_decode ("AQ==")
*/
