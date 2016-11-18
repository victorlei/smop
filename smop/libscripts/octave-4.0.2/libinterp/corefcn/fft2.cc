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

// This function should be merged with Fifft.

#if defined (HAVE_FFTW)
#define FFTSRC "@sc{fftw}"
#else
#define FFTSRC "@sc{fftpack}"
#endif

static octave_value
do_fft2 (const octave_value_list &args, const char *fcn, int type)
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
  octave_idx_type n_rows = -1;

  if (nargin > 1)
    {
      double dval = args(1).double_value ();
      if (xisnan (dval))
        error ("%s: number of rows (N) cannot be NaN", fcn);
      else
        {
          n_rows = NINTbig (dval);
          if (n_rows < 0)
            error ("%s: number of rows (N) must be greater than zero", fcn);
        }
    }

  if (error_state)
    return retval;

  octave_idx_type n_cols = -1;
  if (nargin > 2)
    {
      double dval = args(2).double_value ();
      if (xisnan (dval))
        error ("%s: number of columns (M) cannot be NaN", fcn);
      else
        {
          n_cols = NINTbig (dval);
          if (n_cols < 0)
            error ("%s: number of columns (M) must be greater than zero", fcn);
        }
    }

  if (error_state)
    return retval;

  for (int i = 0; i < dims.length (); i++)
    if (dims(i) < 0)
      return retval;

  if (n_rows < 0)
    n_rows = dims (0);
  else
    dims (0) = n_rows;

  if (n_cols < 0)
    n_cols = dims (1);
  else
    dims (1) = n_cols;

  if (dims.all_zero () || n_rows == 0 || n_cols == 0)
    {
      if (arg.is_single_type ())
        return octave_value (FloatMatrix ());
      else
        return octave_value (Matrix ());
    }

  if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          FloatNDArray nda = arg.float_array_value ();

          if (! error_state)
            {
              nda.resize (dims, 0.0);
              retval = (type != 0 ? nda.ifourier2d () : nda.fourier2d ());
            }
        }
      else
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          if (! error_state)
            {
              cnda.resize (dims, 0.0);
              retval = (type != 0 ? cnda.ifourier2d () : cnda.fourier2d ());
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
              retval = (type != 0 ? nda.ifourier2d () : nda.fourier2d ());
            }
        }
      else if (arg.is_complex_type ())
        {
          ComplexNDArray cnda = arg.complex_array_value ();

          if (! error_state)
            {
              cnda.resize (dims, 0.0);
              retval = (type != 0 ? cnda.ifourier2d () : cnda.fourier2d ());
            }
        }
      else
        {
          gripe_wrong_type_arg (fcn, arg);
        }
    }

  return retval;
}

DEFUN (fft2, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} fft2 (@var{A})\n\
@deftypefnx {Built-in Function} {} fft2 (@var{A}, @var{m}, @var{n})\n\
Compute the two-dimensional discrete Fourier transform of @var{A} using\n\
a Fast Fourier Transform (FFT) algorithm.\n\
\n\
The optional arguments @var{m} and @var{n} may be used specify the number of\n\
rows and columns of @var{A} to use.  If either of these is larger than the\n\
size of @var{A}, @var{A} is resized and padded with zeros.\n\
\n\
If @var{A} is a multi-dimensional matrix, each two-dimensional sub-matrix\n\
of @var{A} is treated separately.\n\
@seealso{ifft2, fft, fftn, fftw}\n\
@end deftypefn")
{
  return do_fft2 (args, "fft2", 0);
}


DEFUN (ifft2, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} ifft2 (@var{A})\n\
@deftypefnx {Built-in Function} {} ifft2 (@var{A}, @var{m}, @var{n})\n\
Compute the inverse two-dimensional discrete Fourier transform of @var{A}\n\
using a Fast Fourier Transform (FFT) algorithm.\n\
\n\
The optional arguments @var{m} and @var{n} may be used specify the number of\n\
rows and columns of @var{A} to use.  If either of these is larger than the\n\
size of @var{A}, @var{A} is resized and padded with zeros.\n\
\n\
If @var{A} is a multi-dimensional matrix, each two-dimensional sub-matrix\n\
of @var{A} is treated separately\n\
@seealso{fft2, ifft, ifftn, fftw}\n\
@end deftypefn")
{
  return do_fft2 (args, "ifft2", 1);
}

/*
%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! M = 16;
%! N = 8;
%!
%! m = 5;
%! n = 3;
%!
%! x = 2*pi*(0:1:M-1)/M;
%! y = 2*pi*(0:1:N-1)/N;
%! sx = cos (m*x);
%! sy = sin (n*y);
%! s = kron (sx',sy);
%! S = fft2 (s);
%! answer = kron (fft (sx)', fft (sy));
%! assert (S, answer, 4*M*N*eps);

%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! M = 12;
%! N = 7;
%!
%! m = 3;
%! n = 2;
%!
%! x = 2*pi*(0:1:M-1)/M;
%! y = 2*pi*(0:1:N-1)/N;
%!
%! sx = cos (m*x);
%! sy = cos (n*y);
%!
%! S = kron (fft (sx)', fft (sy));
%! answer = kron (sx', sy);
%! s = ifft2 (S);
%!
%! assert (s, answer, 30*eps);


%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! M = 16;
%! N = 8;
%!
%! m = 5;
%! n = 3;
%!
%! x = 2*pi*(0:1:M-1)/M;
%! y = 2*pi*(0:1:N-1)/N;
%! sx = single (cos (m*x));
%! sy = single (sin (n*y));
%! s = kron (sx', sy);
%! S = fft2 (s);
%! answer = kron (fft (sx)', fft (sy));
%! assert (S, answer, 4*M*N*eps ("single"));

%% Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
%%         Comalco Research and Technology
%%         02 May 2000
%!test
%! M = 12;
%! N = 7;
%!
%! m = 3;
%! n = 2;
%!
%! x = single (2*pi*(0:1:M-1)/M);
%! y = single (2*pi*(0:1:N-1)/N);
%!
%! sx = cos (m*x);
%! sy = cos (n*y);
%!
%! S = kron (fft (sx)', fft (sy));
%! answer = kron (sx', sy);
%! s = ifft2 (S);
%!
%! assert (s, answer, 30*eps ("single"));
*/
