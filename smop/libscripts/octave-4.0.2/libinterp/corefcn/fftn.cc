/*

Copyright (C) 2004-2015 David Bateman

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
do_fftn (const octave_value_list &args, const char *fcn, int type)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);
  dim_vector dims = arg.dims ();

  for (int i = 0; i < dims.length (); i++)
    if (dims(i) < 0)
      return retval;

  if (nargin > 1)
    {
      Matrix val = args(1).matrix_value ();
      if (val.rows () > val.columns ())
        val = val.transpose ();

      if (error_state || val.columns () != dims.length () || val.rows () != 1)
        error ("%s: SIZE must be a vector of length dim", fcn);
      else
        {
          for (int i = 0; i < dims.length (); i++)
            {
              if (xisnan (val(i,0)))
                error ("%s: SIZE has invalid NaN entries", fcn);
              else if (NINTbig (val(i,0)) < 0)
                error ("%s: all dimensions in SIZE must be greater than zero",
                       fcn);
              else
                {
                  dims(i) = NINTbig(val(i,0));
                }
            }
        }
    }

  if (error_state)
    return retval;

  if (dims.all_zero ())
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
              retval = (type != 0 ? nda.ifourierNd () : nda.fourierNd ());
            }
        }
      else
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          if (! error_state)
            {
              cnda.resize (dims, 0.0);
              retval = (type != 0 ? cnda.ifourierNd () : cnda.fourierNd ());
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
              retval = (type != 0 ? nda.ifourierNd () : nda.fourierNd ());
            }
        }
      else if (arg.is_complex_type ())
        {
          ComplexNDArray cnda = arg.complex_array_value ();

          if (! error_state)
            {
              cnda.resize (dims, 0.0);
              retval = (type != 0 ? cnda.ifourierNd () : cnda.fourierNd ());
            }
        }
      else
        {
          gripe_wrong_type_arg (fcn, arg);
        }
    }

  return retval;
}

DEFUN (fftn, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} fftn (@var{A})\n\
@deftypefnx {Built-in Function} {} fftn (@var{A}, @var{size})\n\
Compute the N-dimensional discrete Fourier transform of @var{A} using\n\
a Fast Fourier Transform (FFT) algorithm.\n\
\n\
The optional vector argument @var{size} may be used specify the dimensions\n\
of the array to be used.  If an element of @var{size} is smaller than the\n\
corresponding dimension of @var{A}, then the dimension of @var{A} is\n\
truncated prior to performing the FFT@.  Otherwise, if an element of\n\
@var{size} is larger than the corresponding dimension then @var{A} is\n\
resized and padded with zeros.\n\
@seealso{ifftn, fft, fft2, fftw}\n\
@end deftypefn")
{
  return do_fftn (args, "fftn", 0);
}

DEFUN (ifftn, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} ifftn (@var{A})\n\
@deftypefnx {Built-in Function} {} ifftn (@var{A}, @var{size})\n\
Compute the inverse N-dimensional discrete Fourier transform of @var{A}\n\
using a Fast Fourier Transform (FFT) algorithm.\n\
\n\
The optional vector argument @var{size} may be used specify the dimensions\n\
of the array to be used.  If an element of @var{size} is smaller than the\n\
corresponding dimension of @var{A}, then the dimension of @var{A} is\n\
truncated prior to performing the inverse FFT@.  Otherwise, if an element of\n\
@var{size} is larger than the corresponding dimension then @var{A} is\n\
resized and padded with zeros.\n\
@seealso{fftn, ifft, ifft2, fftw}\n\
@end deftypefn")
{
  return do_fftn (args, "ifftn", 1);
}
