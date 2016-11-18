/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 2010 Jaroslav Hajek, Jordi Gutiérrez Hermoso

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

#include "dNDArray.h"
#include "CNDArray.h"
#include "fNDArray.h"
#include "fCNDArray.h"
#include "lo-mappers.h"
#include "oct-binmap.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

static double
simple_gcd (double a, double b)
{
  if (! xisinteger (a) || ! xisinteger (b))
    (*current_liboctave_error_handler)
      ("gcd: all values must be integers");

  double aa = fabs (a);
  double bb = fabs (b);

  while (bb != 0)
    {
      double tt = fmod (aa, bb);
      aa = bb;
      bb = tt;
    }

  return aa;
}

// Don't use the Complex and FloatComplex typedefs because we need to
// refer to the actual float precision FP in the body (and when gcc
// implements template aliases from C++0x, can do a small fix here).
template <typename FP>
static void
divide (const std::complex<FP>& a, const std::complex<FP>& b,
        std::complex<FP>& q, std::complex<FP>& r)
{
  FP qr = gnulib::floor ((a/b).real () + 0.5);
  FP qi = gnulib::floor ((a/b).imag () + 0.5);

  q = std::complex<FP> (qr, qi);

  r = a - q*b;
}

template <typename FP>
static std::complex<FP>
simple_gcd (const std::complex<FP>& a, const std::complex<FP>& b)
{
  if (! xisinteger (a.real ()) || ! xisinteger (a.imag ())
      || ! xisinteger (b.real ()) || ! xisinteger (b.imag ()))
    (*current_liboctave_error_handler)
      ("gcd: all complex parts must be integers");

  std::complex<FP> aa = a;
  std::complex<FP> bb = b;

  if (abs (aa) < abs (bb))
    std::swap (aa, bb);

  while (abs (bb) != 0)
    {
      std::complex<FP> qq, rr;
      divide (aa, bb, qq, rr);
      aa = bb;
      bb = rr;
    }

  return aa;
}

template <class T>
static octave_int<T>
simple_gcd (const octave_int<T>& a, const octave_int<T>& b)
{
  T aa = a.abs ().value ();
  T bb = b.abs ().value ();

  while (bb != 0)
    {
      T tt = aa % bb;
      aa = bb;
      bb = tt;
    }

  return aa;
}

static double
extended_gcd (double a, double b, double& x, double& y)
{
  if (! xisinteger (a) || ! xisinteger (b))
    (*current_liboctave_error_handler)
      ("gcd: all values must be integers");

  double aa = fabs (a);
  double bb = fabs (b);

  double xx, lx, yy, ly;
  xx = 0, lx = 1;
  yy = 1, ly = 0;

  while (bb != 0)
    {
      double qq = gnulib::floor (aa / bb);
      double tt = fmod (aa, bb);

      aa = bb;
      bb = tt;

      double tx = lx - qq*xx;
      lx = xx;
      xx = tx;

      double ty = ly - qq*yy;
      ly = yy;
      yy = ty;
    }

  x = a >= 0 ? lx : -lx;
  y = b >= 0 ? ly : -ly;

  return aa;
}

template <typename FP>
static std::complex<FP>
extended_gcd (const std::complex<FP>& a, const std::complex<FP>& b,
              std::complex<FP>& x, std::complex<FP>& y)
{
  if (! xisinteger (a.real ()) || ! xisinteger (a.imag ())
      || ! xisinteger (b.real ()) || ! xisinteger (b.imag ()))
    (*current_liboctave_error_handler)
      ("gcd: all complex parts must be integers");

  std::complex<FP> aa = a;
  std::complex<FP> bb = b;
  bool swapped = false;
  if (abs (aa) < abs (bb))
    {
      std::swap (aa, bb);
      swapped = true;
    }

  std::complex<FP> xx, lx, yy, ly;
  xx = 0, lx = 1;
  yy = 1, ly = 0;

  while (abs(bb) != 0)
    {
      std::complex<FP> qq, rr;
      divide (aa, bb, qq, rr);
      aa = bb;
      bb = rr;

      std::complex<FP> tx = lx - qq*xx;
      lx = xx;
      xx = tx;

      std::complex<FP> ty = ly - qq*yy;
      ly = yy;
      yy = ty;
    }

  x = lx;
  y = ly;

  if (swapped)
    std::swap (x, y);

  return aa;
}

template <class T>
static octave_int<T>
extended_gcd (const octave_int<T>& a, const octave_int<T>& b,
              octave_int<T>& x, octave_int<T>& y)
{
  T aa = a.abs ().value ();
  T bb = b.abs ().value ();
  T xx, lx, yy, ly;
  xx = 0, lx = 1;
  yy = 1, ly = 0;

  while (bb != 0)
    {
      T qq = aa / bb;
      T tt = aa % bb;
      aa = bb;
      bb = tt;

      T tx = lx - qq*xx;
      lx = xx;
      xx = tx;

      T ty = ly - qq*yy;
      ly = yy;
      yy = ty;
    }

  x = octave_int<T> (lx) * a.signum ();
  y = octave_int<T> (ly) * b.signum ();

  return aa;
}

template<class NDA>
static octave_value
do_simple_gcd (const octave_value& a, const octave_value& b)
{
  typedef typename NDA::element_type T;
  octave_value retval;

  if (a.is_scalar_type () && b.is_scalar_type ())
    {
      // Optimize scalar case.
      T aa = octave_value_extract<T> (a);
      T bb = octave_value_extract<T> (b);
      retval = simple_gcd (aa, bb);
    }
  else
    {
      NDA aa = octave_value_extract<NDA> (a);
      NDA bb = octave_value_extract<NDA> (b);
      retval = binmap<T> (aa, bb, simple_gcd, "gcd");
    }

  return retval;
}

// Dispatcher
static octave_value
do_simple_gcd (const octave_value& a, const octave_value& b)
{
  octave_value retval;
  builtin_type_t btyp = btyp_mixed_numeric (a.builtin_type (),
                                            b.builtin_type ());
  switch (btyp)
    {
    case btyp_double:
      if (a.is_sparse_type () && b.is_sparse_type ())
        {
          retval = do_simple_gcd<SparseMatrix> (a, b);
          break;
        }
      // fall through!

    case btyp_float:
      retval = do_simple_gcd<NDArray> (a, b);
      break;

#define MAKE_INT_BRANCH(X) \
    case btyp_ ## X: \
      retval = do_simple_gcd<X ## NDArray> (a, b); \
      break

    MAKE_INT_BRANCH (int8);
    MAKE_INT_BRANCH (int16);
    MAKE_INT_BRANCH (int32);
    MAKE_INT_BRANCH (int64);
    MAKE_INT_BRANCH (uint8);
    MAKE_INT_BRANCH (uint16);
    MAKE_INT_BRANCH (uint32);
    MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

    case btyp_complex:
      retval = do_simple_gcd<ComplexNDArray> (a, b);
      break;

    case btyp_float_complex:
      retval = do_simple_gcd<FloatComplexNDArray> (a, b);
      break;

    default:
      error ("gcd: invalid class combination for gcd: %s and %s\n",
             a.class_name ().c_str (), b.class_name ().c_str ());
    }

  if (btyp == btyp_float)
    retval = retval.float_array_value ();

  return retval;
}

template<class NDA>
static octave_value
do_extended_gcd (const octave_value& a, const octave_value& b,
                 octave_value& x, octave_value& y)
{
  typedef typename NDA::element_type T;
  octave_value retval;

  if (a.is_scalar_type () && b.is_scalar_type ())
    {
      // Optimize scalar case.
      T aa = octave_value_extract<T> (a);
      T bb = octave_value_extract<T> (b);
      T xx, yy;
      retval = extended_gcd (aa, bb, xx, yy);
      x = xx;
      y = yy;
    }
  else
    {
      NDA aa = octave_value_extract<NDA> (a);
      NDA bb = octave_value_extract<NDA> (b);

      dim_vector dv = aa.dims ();
      if (aa.numel () == 1)
        dv = bb.dims ();
      else if (bb.numel () != 1 && bb.dims () != dv)
        gripe_nonconformant ("gcd", a.dims (), b.dims ());

      NDA gg (dv), xx (dv), yy (dv);

      const T *aptr = aa.fortran_vec ();
      const T *bptr = bb.fortran_vec ();

      bool inca = aa.numel () != 1;
      bool incb = bb.numel () != 1;

      T *gptr = gg.fortran_vec ();
      T *xptr = xx.fortran_vec ();
      T *yptr = yy.fortran_vec ();

      octave_idx_type n = gg.numel ();
      for (octave_idx_type i = 0; i < n; i++)
        {
          octave_quit ();

          *gptr++ = extended_gcd (*aptr, *bptr, *xptr++, *yptr++);

          aptr += inca;
          bptr += incb;
        }

      x = xx;
      y = yy;

      retval = gg;
    }

  return retval;
}

// Dispatcher
static octave_value
do_extended_gcd (const octave_value& a, const octave_value& b,
                 octave_value& x, octave_value& y)
{
  octave_value retval;

  builtin_type_t btyp = btyp_mixed_numeric (a.builtin_type (),
                                            b.builtin_type ());
  switch (btyp)
    {
    case btyp_double:
    case btyp_float:
      retval = do_extended_gcd<NDArray> (a, b, x, y);
      break;

#define MAKE_INT_BRANCH(X) \
    case btyp_ ## X: \
      retval = do_extended_gcd<X ## NDArray> (a, b, x, y); \
      break

    MAKE_INT_BRANCH (int8);
    MAKE_INT_BRANCH (int16);
    MAKE_INT_BRANCH (int32);
    MAKE_INT_BRANCH (int64);
    MAKE_INT_BRANCH (uint8);
    MAKE_INT_BRANCH (uint16);
    MAKE_INT_BRANCH (uint32);
    MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

    case btyp_complex:
      retval = do_extended_gcd<ComplexNDArray> (a, b, x, y);
      break;

    case btyp_float_complex:
      retval = do_extended_gcd<FloatComplexNDArray> (a, b, x, y);
      break;

    default:
      error ("gcd: invalid class combination for gcd: %s and %s\n",
             a.class_name ().c_str (), b.class_name ().c_str ());
    }

  // For consistency.
  if (! error_state && a.is_sparse_type () && b.is_sparse_type ())
    {
      retval = retval.sparse_matrix_value ();
      x = x.sparse_matrix_value ();
      y = y.sparse_matrix_value ();
    }

  if (btyp == btyp_float)
    {
      retval = retval.float_array_value ();
      x = x.float_array_value ();
      y = y.float_array_value ();
    }

  return retval;
}

DEFUN (gcd, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{g} =} gcd (@var{a1}, @var{a2}, @dots{})\n\
@deftypefnx {Built-in Function} {[@var{g}, @var{v1}, @dots{}] =} gcd (@var{a1}, @var{a2}, @dots{})\n\
Compute the greatest common divisor of @var{a1}, @var{a2}, @dots{}.\n\
\n\
If more than one argument is given then all arguments must be the same size\n\
or scalar.  In this case the greatest common divisor is calculated for each\n\
element individually.  All elements must be ordinary or Gaussian (complex)\n\
integers.  Note that for Gaussian integers, the gcd is only unique up to a\n\
phase factor (multiplication by 1, -1, i, or -i), so an arbitrary greatest\n\
common divisor among the four possible is returned.\n\
\n\
Optional return arguments @var{v1}, @dots{}, contain integer vectors such\n\
that,\n\
\n\
@tex\n\
$g = v_1 a_1 + v_2 a_2 + \\cdots$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@var{g} = @var{v1} .* @var{a1} + @var{v2} .* @var{a2} + @dots{}\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
Example code:\n\
\n\
@example\n\
@group\n\
gcd ([15, 9], [20, 18])\n\
   @result{}  5  9\n\
@end group\n\
@end example\n\
\n\
@seealso{lcm, factor, isprime}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 1)
    {
      if (nargout > 1)
        {
          retval.resize (nargin + 1);

          retval(0) = do_extended_gcd (args(0), args(1), retval(1), retval(2));

          for (int j = 2; j < nargin; j++)
            {
              octave_value x;
              retval(0) = do_extended_gcd (retval(0), args(j),
                                           x, retval(j+1));
              for (int i = 0; i < j; i++)
                retval(i+1).assign (octave_value::op_el_mul_eq, x);

              if (error_state)
                break;
            }
        }
      else
        {
          retval(0) = do_simple_gcd (args(0), args(1));

          for (int j = 2; j < nargin; j++)
            {
              retval(0) = do_simple_gcd (retval(0), args(j));

              if (error_state)
                break;
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (gcd (200, 300, 50, 35), 5)
%!assert (gcd (int16 (200), int16 (300), int16 (50), int16 (35)), int16 (5))
%!assert (gcd (uint64 (200), uint64 (300), uint64 (50), uint64 (35)), uint64 (5))
%!assert (gcd (18-i, -29+3i), -3-4i)

%!test
%! p = [953 967];
%! u = [953 + i*971, 967 + i*977];
%! [d, k(1), k(2)] = gcd (p(1), p(2));
%! [z, w(1), w(2)] = gcd (u(1), u(2));
%! assert (d, 1)
%! assert (sum (p.*k), d)
%! assert (abs (z), sqrt (2))
%! assert (abs (sum (u.*w)), sqrt (2))

%!error <all values must be integers> gcd (1/2, 2);
%!error <all complex parts must be integers> gcd (e + i*pi, 1);

%!error gcd ()

%!test
%! s.a = 1;
%! fail ("gcd (s)");
*/
