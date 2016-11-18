/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#include "defun.h"
#include "ov.h"
#include "pager.h"
#include "error.h"
#include "gripes.h"

#include "oct-spparms.h"

DEFUN (spparms, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} { } spparms ()\n\
@deftypefnx {Built-in Function} {@var{vals} =} spparms ()\n\
@deftypefnx {Built-in Function} {[@var{keys}, @var{vals}] =} spparms ()\n\
@deftypefnx {Built-in Function} {@var{val} =} spparms (@var{key})\n\
@deftypefnx {Built-in Function} { } spparms (@var{vals})\n\
@deftypefnx {Built-in Function} { } spparms (\"default\")\n\
@deftypefnx {Built-in Function} { } spparms (\"tight\")\n\
@deftypefnx {Built-in Function} { } spparms (@var{key}, @var{val})\n\
Query or set the parameters used by the sparse solvers and factorization\n\
functions.\n\
\n\
The first four calls above get information about the current settings, while\n\
the others change the current settings.  The parameters are stored as pairs\n\
of keys and values, where the values are all floats and the keys are one of\n\
the following strings:\n\
\n\
@table @samp\n\
@item spumoni\n\
Printing level of debugging information of the solvers (default 0)\n\
\n\
@item ths_rel\n\
Included for compatibility.  Not used.  (default 1)\n\
\n\
@item ths_abs\n\
Included for compatibility.  Not used.  (default 1)\n\
\n\
@item exact_d\n\
Included for compatibility.  Not used.  (default 0)\n\
\n\
@item supernd\n\
Included for compatibility.  Not used.  (default 3)\n\
\n\
@item rreduce\n\
Included for compatibility.  Not used.  (default 3)\n\
\n\
@item wh_frac\n\
Included for compatibility.  Not used.  (default 0.5)\n\
\n\
@item autommd\n\
Flag whether the LU/QR and the '\\' and '/' operators will automatically\n\
use the sparsity preserving mmd functions (default 1)\n\
\n\
@item autoamd\n\
Flag whether the LU and the '\\' and '/' operators will automatically\n\
use the sparsity preserving amd functions (default 1)\n\
\n\
@item piv_tol\n\
The pivot tolerance of the @sc{umfpack} solvers (default 0.1)\n\
\n\
@item sym_tol\n\
The pivot tolerance of the @sc{umfpack} symmetric solvers (default 0.001)\n\
\n\
@item bandden\n\
The density of nonzero elements in a banded matrix before it is treated\n\
by the @sc{lapack} banded solvers (default 0.5)\n\
\n\
@item umfpack\n\
Flag whether the @sc{umfpack} or mmd solvers are used for the LU, '\\' and\n\
'/' operations (default 1)\n\
@end table\n\
\n\
The value of individual keys can be set with\n\
@code{spparms (@var{key}, @var{val})}.\n\
The default values can be restored with the special keyword\n\
@qcode{\"default\"}.  The special keyword @qcode{\"tight\"} can be used to\n\
set the mmd solvers to attempt a sparser solution at the potential cost of\n\
longer running time.\n\
@seealso{chol, colamd, lu, qr, symamd}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin == 0)
    {
      if (nargout == 0)
        octave_sparse_params::print_info (octave_stdout, "");
      else if (nargout == 1)
        retval(0) =  octave_sparse_params::get_vals ();
      else if (nargout == 2)
        {
          retval(1) = octave_sparse_params::get_vals ();
          retval(0) = octave_sparse_params::get_keys ();
        }
      else
        error ("spparms: too many output arguments");
    }
  else if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string str = args(0).string_value ();
          int len = str.length ();
          for (int i = 0; i < len; i++)
            str[i] = tolower (str[i]);

          if (str == "defaults" || str == "default")
            {
              // FIXME: deprecated in 4.0, remove "defaults" for 4.4 release
              static bool warned = false;
              if (! warned && str == "defaults")
                {
                  warning ("spparms: use \"default\" instead of \"defaults\"");
                  warned = true;
                }
              octave_sparse_params::defaults ();
            }
          else if (str == "tight")
            octave_sparse_params::tight ();
          else
            {
              double val = octave_sparse_params::get_key (str);
              if (xisnan (val))
                error ("spparms: KEY not recognized");
              else
                retval(0) = val;
            }
        }
      else
        {
          NDArray vals = args(0).array_value ();

          if (error_state)
            error ("spparms: input must be a string or a vector");
          else if (vals.numel () > OCTAVE_SPARSE_CONTROLS_SIZE)
            error ("spparms: too many elements in vector VALS");
          else
            octave_sparse_params::set_vals (vals);
        }
    }
  else if (nargin == 2)
    {
      if (args(0).is_string ())
        {
          std::string str = args(0).string_value ();

          double val = args(1).double_value ();

          if (error_state)
            error ("spparms: second argument must be a real scalar");
          else if (str == "umfpack")
            warning ("spparms: request to disable umfpack solvers ignored");
          else if (!octave_sparse_params::set_key (str, val))
            error ("spparms: KEY not found");
        }
      else
        error ("spparms: first argument must be a string");
    }
  else
    error ("spparms: too many input arguments");

  return retval;
}

/*
%!test
%! old_vals = spparms ();  # save state
%! spparms ("default");
%! vals = spparms ();
%! assert (vals, [0 1 1 0 3 3 0.5 1.0 1.0 0.1 0.5 1.0 0.001]');
%! [keys, vals] = spparms ();
%! assert (rows (keys), 13);
%! assert (keys(2,:), "ths_rel");
%! assert (vals, [0 1 1 0 3 3 0.5 1.0 1.0 0.1 0.5 1.0 0.001]');
%! spparms ([3 2 1]);
%! assert (spparms ()(1:3), [3, 2, 1]');
%! assert (spparms ("ths_rel"), 2);
%! spparms ("exact_d", 5);
%! assert (spparms ("exact_d"), 5);
%! spparms (old_vals);     # restore state

%% Test input validation
%!error <too many input arguments> spparms (1, 2, 3)
%!error <too many output arguments> [x, y, z] = spparms ()
%!error <KEY not recognized> spparms ("UNKNOWN_KEY")
%!#error <input must be a string> spparms ({1, 2, 3})
%!error spparms ({1, 2, 3})
%!error <too many elements in vector VALS> spparms (ones (14, 1))
%!error <first argument must be a string> spparms (1, 1)
%!#error <second argument must be a real scalar> spparms ("ths_rel", "hello")
%!error spparms ("ths_rel", "hello")
%!error <KEY not found> spparms ("UNKNOWN_KEY", 1)
*/
