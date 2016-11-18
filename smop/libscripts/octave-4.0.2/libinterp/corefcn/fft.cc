/*

Copyright (C) 1997-2015 David Bateman
Copyright (C) 1996-1997 John W. Eaton

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

#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#if defined (HAVE_FFTW)
#define FFTSRC "@sc{fftw}"
#else
#define FFTSRC "@sc{fftpack}"
#endif

static octave_value
do_fft (const octave_value_list &args, const char *fcn, int type)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);
  dim_vector dims = arg.dims ();
  octave_idx_type n_points = -1;
  int dim = -1;

  if (nargin > 1)
    {
      if (! args(1).is_empty ())
        {
          double dval = args(1).double_value ();
          if (xisnan (dval))
            error ("%s: number of points (N) cannot be NaN", fcn);
          else
            {
              n_points = NINTbig (dval);
              if (n_points < 0)
                error ("%s: number of points (N) must be greater than zero",
                       fcn);
            }
        }
    }

  if (error_state)
    return retval;

  if (nargin > 2)
    {
      double dval = args(2).double_value ();
      if (xisnan (dval))
        error ("%s: DIM cannot be NaN", fcn);
      else if (dval < 1 || dval > dims.length ())
        error ("%s: DIM must be a valid dimension along which to perform FFT",
               fcn);
      else
        // to be safe, cast it back to int since dim is an int
        dim = NINT (dval) - 1;
    }

  if (error_state)
    return retval;

  for (octave_idx_type i = 0; i < dims.length (); i++)
    if (dims(i) < 0)
      return retval;

  if (dim < 0)
    {
      for (octave_idx_type i = 0; i < dims.length (); i++)
        if (dims(i) > 1)
          {
            dim = i;
            break;
          }

      // And if the first argument is scalar?
      if (dim < 0)
        dim = 1;
    }

  if (n_points < 0)
    n_points = dims (dim);
  else
    dims (dim) = n_points;

  if (dims.any_zero () || n_points == 0)
    {
      if (arg.is_single_type ())
        return octave_value (FloatNDArray (dims));
      else
        return octave_value (NDArray (dims));
    }

  if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          FloatNDArray nda = arg.float_array_value ();

          if (! error_state)
            {
              nda.resize (dims, 0.0);
              retval = (type != 0 ? nda.ifourier (dim) : nda.fourier (dim));
            }
        }
      else
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          if (! error_state)
            {
              cnda.resize (dims, 0.0);
              retval = (type != 0 ? cnda.ifourier (dim) : cnda.fourier (dim));
            }
        }
    }
  else
    {
      if (arg.is_real_type ())
        {
          NDArray nda = arg.array_value ();

          if (! error_state)
            {
              nda.resize (dims, 0.0);
              retval = (type != 0 ? nda.ifourier (dim) : nda.fourier (dim));
            }
        }
      else if (arg.is_complex_type ())
        {
          ComplexNDArray cnda = arg.complex_array_value ();

          if (! error_state)
            {
              cnda.resize (dims, 0.0);
              retval = (type != 0 ? cnda.ifourier (dim) : cnda.fourier (dim));
            }
        }
      else
        {
          gripe_wrong_type_arg (fcn, arg);
        }
    }

  return retval;
}

/*
%!assert (fft ([]), [])
%!assert (fft (zeros (10,0)), zeros (10,0))
%!assert (fft (zeros (0,10)), zeros (0,10))
%!assert (fft (0), 0)
%!assert (fft (1), 1)
%!assert (fft (ones (2,2)), [2,2; 0,0])
%!assert (fft (eye (2,2)), [1,1; 1,-1])

%!assert (fft (single ([])), single ([]))
%!assert (fft (zeros (10,0,"single")), zeros (10,0,"single"))
%!assert (fft (zeros (0,10,"single")), zeros (0,10,"single"))
%!assert (fft (single (0)), single (0))
%!assert (fft (single (1)), single (1))
%!assert (fft (ones (2,2,"single")), single ([2,2; 0,0]))
%!assert (fft (eye (2,2,"single")), single ([1,1; 1,-1]))

%!error (fft ())
*/


DEFUN (fft, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} fft (@var{x})\n\
@deftypefnx {Built-in Function} {} fft (@var{x}, @var{n})\n\
@deftypefnx {Built-in Function} {} fft (@var{x}, @var{n}, @var{dim})\n\
Compute the discrete Fourier transform of @var{A} using\n\
a Fast Fourier Transform (FFT) algorithm.\n\
\n\
The FFT is calculated along the first non-singleton dimension of the\n\
array.  Thus if @var{x} is a matrix, @code{fft (@var{x})} computes the\n\
FFT for each column of @var{x}.\n\
\n\
If called with two arguments, @var{n} is expected to be an integer\n\
specifying the number of elements of @var{x} to use, or an empty\n\
matrix to specify that its value should be ignored.  If @var{n} is\n\
larger than the dimension along which the FFT is calculated, then\n\
@var{x} is resized and padded with zeros.  Otherwise, if @var{n} is\n\
smaller than the dimension along which the FFT is calculated, then\n\
@var{x} is truncated.\n\
\n\
If called with three arguments, @var{dim} is an integer specifying the\n\
dimension of the matrix along which the FFT is performed\n\
@seealso{ifft, fft2, fftn, fftw}\n\
@end deftypefn")
{
  return do_fft (args, "fft", 0);
}


DEFUN (ifft, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} ifft (@var{x})\n\
@deftypefnx {Built-in Function} {} ifft (@var{x}, @var{n})\n\
@deftypefnx {Built-in Function} {} ifft (@var{x}, @var{n}, @var{dim})\n\
Compute the inverse discrete Fourier transform of @var{A}\n\
using a Fast Fourier Transform (FFT) algorithm.\n\
\n\
The inverse FFT is calculated along the first non-singleton dimension\n\
of the array.  Thus if @var{x} is a matrix, @code{fft (@var{x})} computes\n\
the inverse FFT for each column of @var{x}.\n\
\n\
If called with two arguments, @var{n} is expected to be an integer\n\
specifying the number of elements of @var{x} to use, or an empty\n\
matrix to specify that its value should be ignored.  If @var{n} is\n\
larger than the dimension along which the inverse FFT is calculated, then\n\
@var{x} is resized and padded with zeros.  Otherwise, if @var{n} is\n\
smaller than the dimension along which the inverse FFT is calculated,\n\
then @var{x} is truncated.\n\
\n\
If called with three arguments, @var{dim} is an integer specifying the\n\
dimension of the matrix along which the inverse FFT is performed\n\
@seealso{fft, ifft2, ifftn, fftw}\n\
@end deftypefn")
{
  return do_fft (args, "ifft", 1);
}

/*
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! N = 64;
%! n = 4;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos (n*t);
%! S = fft (s);
%!
%! answer = zeros (size (t));
%! answer(n+1) = N/2;
%! answer(N-n+1) = N/2;
%!
%! assert (S, answer, 4*N*eps);

%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! N = 64;
%! n = 7;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos (n*t);
%!
%! S = zeros (size (t));
%! S(n+1) = N/2;
%! S(N-n+1) = N/2;
%!
%! assert (ifft (S), s, 4*N*eps);

%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! N = 64;
%! n = 4;
%! t = single (2*pi*(0:1:N-1)/N);
%! s = cos (n*t);
%! S = fft (s);
%!
%! answer = zeros (size (t), "single");
%! answer(n+1) = N/2;
%! answer(N-n+1) = N/2;
%!
%! assert (S, answer, 4*N*eps ("single"));

%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! N = 64;
%! n = 7;
%! t = 2*pi*(0:1:N-1)/N;
%! s = cos (n*t);
%!
%! S = zeros (size (t), "single");
%! S(n+1) = N/2;
%! S(N-n+1) = N/2;
%!
%! assert (ifft (S), s, 4*N*eps ("single"));
*/
