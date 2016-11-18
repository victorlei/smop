/*

Copyright (C) 2006-2015 David Bateman

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

#include <algorithm>

#include "oct-fftw.h"

#include "defun-dld.h"
#include "error.h"
#include "ov.h"

#include "gripes.h"

DEFUN_DLD (fftw, args, ,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{method} =} fftw (\"planner\")\n\
@deftypefnx {Loadable Function} {} fftw (\"planner\", @var{method})\n\
@deftypefnx {Loadable Function} {@var{wisdom} =} fftw (\"dwisdom\")\n\
@deftypefnx {Loadable Function} {} fftw (\"dwisdom\", @var{wisdom})\n\
@deftypefnx {Loadable Function} {} fftw (\"threads\", @var{nthreads})\n\
@deftypefnx {Loadable Function} {@var{nthreads} =} fftw (\"threads\")\n\
\n\
Manage @sc{fftw} wisdom data.\n\
\n\
Wisdom data can be used to significantly accelerate the calculation of the\n\
FFTs, but implies an initial cost in its calculation.  When the @sc{fftw}\n\
libraries are initialized, they read a system wide wisdom file (typically in\n\
@file{/etc/fftw/wisdom}), allowing wisdom to be shared between applications\n\
other than Octave.  Alternatively, the @code{fftw} function can be used to\n\
import wisdom.  For example,\n\
\n\
@example\n\
@var{wisdom} = fftw (\"dwisdom\")\n\
@end example\n\
\n\
@noindent\n\
will save the existing wisdom used by Octave to the string @var{wisdom}.\n\
This string can then be saved to a file and restored using the @code{save}\n\
and @code{load} commands respectively.  This existing wisdom can be\n\
re-imported as follows\n\
\n\
@example\n\
fftw (\"dwisdom\", @var{wisdom})\n\
@end example\n\
\n\
If @var{wisdom} is an empty string, then the wisdom used is cleared.\n\
\n\
During the calculation of Fourier transforms further wisdom is generated.\n\
The fashion in which this wisdom is generated is also controlled by\n\
the @code{fftw} function.  There are five different manners in which the\n\
wisdom can be treated:\n\
\n\
@table @asis\n\
@item @qcode{\"estimate\"}\n\
Specifies that no run-time measurement of the optimal means of\n\
calculating a particular is performed, and a simple heuristic is used\n\
to pick a (probably sub-optimal) plan.  The advantage of this method is\n\
that there is little or no overhead in the generation of the plan, which\n\
is appropriate for a Fourier transform that will be calculated once.\n\
\n\
@item @qcode{\"measure\"}\n\
In this case a range of algorithms to perform the transform is considered\n\
and the best is selected based on their execution time.\n\
\n\
@item @qcode{\"patient\"}\n\
Similar to @qcode{\"measure\"}, but a wider range of algorithms is\n\
considered.\n\
\n\
@item @qcode{\"exhaustive\"}\n\
Like @qcode{\"measure\"}, but all possible algorithms that may be used to\n\
treat the transform are considered.\n\
\n\
@item @qcode{\"hybrid\"}\n\
As run-time measurement of the algorithm can be expensive, this is a\n\
compromise where @qcode{\"measure\"} is used for transforms up to the size\n\
of 8192 and beyond that the @qcode{\"estimate\"} method is used.\n\
@end table\n\
\n\
The default method is @qcode{\"estimate\"}.  The current method can\n\
be queried with\n\
\n\
@example\n\
@var{method} = fftw (\"planner\")\n\
@end example\n\
\n\
@noindent\n\
or set by using\n\
\n\
@example\n\
fftw (\"planner\", @var{method})\n\
@end example\n\
\n\
Note that calculated wisdom will be lost when restarting Octave.  However,\n\
the wisdom data can be reloaded if it is saved to a file as described\n\
above.  Saved wisdom files should not be used on different platforms since\n\
they will not be efficient and the point of calculating the wisdom is lost.\n\
\n\
The number of threads used for computing the plans and executing the\n\
transforms can be set with\n\
\n\
@example\n\
fftw (\"threads\", @var{NTHREADS})\n\
@end example\n\
\n\
Note that octave must be compiled with multi-threaded @sc{fftw} support for\n\
this feature.  The number of processors available to the current process is\n\
used per default.\n\
\n\
@seealso{fft, ifft, fft2, ifft2, fftn, ifftn}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

#if defined (HAVE_FFTW)
  if (args(0).is_string ())
    {
      std::string arg0 = args(0).string_value ();

      if (arg0 == "planner")
        {
          if (nargin == 2)  //planner setter
            {
              if (args(1).is_string ())
                {
                  // Use STL function to convert to lower case
                  std::transform (arg0.begin (), arg0.end (), arg0.begin (),
                                  tolower);
                  std::string arg1 = args(1).string_value ();
                  if (!error_state)
                    {
                      std::transform (arg1.begin (), arg1.end (),
                                      arg1.begin (), tolower);
                      octave_fftw_planner::FftwMethod meth
                        = octave_fftw_planner::UNKNOWN;
                      octave_float_fftw_planner::FftwMethod methf
                        = octave_float_fftw_planner::UNKNOWN;

                      if (arg1 == "estimate")
                        {
                          meth = octave_fftw_planner::ESTIMATE;
                          methf = octave_float_fftw_planner::ESTIMATE;
                        }
                      else if (arg1 == "measure")
                        {
                          meth = octave_fftw_planner::MEASURE;
                          methf = octave_float_fftw_planner::MEASURE;
                        }
                      else if (arg1 == "patient")
                        {
                          meth = octave_fftw_planner::PATIENT;
                          methf = octave_float_fftw_planner::PATIENT;
                        }
                      else if (arg1 == "exhaustive")
                        {
                          meth = octave_fftw_planner::EXHAUSTIVE;
                          methf = octave_float_fftw_planner::EXHAUSTIVE;
                        }
                      else if (arg1 == "hybrid")
                        {
                          meth = octave_fftw_planner::HYBRID;
                          methf = octave_float_fftw_planner::HYBRID;
                        }
                      else
                        error ("fftw: unrecognized planner METHOD");

                      if (!error_state)
                        {
                          meth = octave_fftw_planner::method (meth);
                          octave_float_fftw_planner::method (methf);

                          if (meth == octave_fftw_planner::MEASURE)
                            retval = octave_value ("measure");
                          else if (meth == octave_fftw_planner::PATIENT)
                            retval = octave_value ("patient");
                          else if (meth == octave_fftw_planner::EXHAUSTIVE)
                            retval = octave_value ("exhaustive");
                          else if (meth == octave_fftw_planner::HYBRID)
                            retval = octave_value ("hybrid");
                          else
                            retval = octave_value ("estimate");
                        }
                    }
                }
              else
                error ("fftw: planner expects a string value as METHOD");
            }
          else //planner getter
            {
              octave_fftw_planner::FftwMethod meth =
                octave_fftw_planner::method ();

              if (meth == octave_fftw_planner::MEASURE)
                retval = octave_value ("measure");
              else if (meth == octave_fftw_planner::PATIENT)
                retval = octave_value ("patient");
              else if (meth == octave_fftw_planner::EXHAUSTIVE)
                retval = octave_value ("exhaustive");
              else if (meth == octave_fftw_planner::HYBRID)
                retval = octave_value ("hybrid");
              else
                retval = octave_value ("estimate");
            }
        }
      else if (arg0 == "dwisdom")
        {
          if (nargin == 2)  //dwisdom setter
            {
              if (args(1).is_string ())
                {
                  // Use STL function to convert to lower case
                  std::transform (arg0.begin (), arg0.end (), arg0.begin (),
                                  tolower);
                  std::string arg1 = args(1).string_value ();
                  if (!error_state)
                    {
                      char *str = fftw_export_wisdom_to_string ();

                      if (arg1.length () < 1)
                        fftw_forget_wisdom ();
                      else if (! fftw_import_wisdom_from_string (arg1.c_str ()))
                        error ("fftw: could not import supplied WISDOM");

                      if (!error_state)
                        retval = octave_value (std::string (str));

                      free (str);
                    }
                }
            }
          else //dwisdom getter
            {
              char *str = fftw_export_wisdom_to_string ();
              retval = octave_value (std::string (str));
              free (str);
            }
        }
      else if (arg0 == "swisdom")
        {
          //swisdom uses fftwf_ functions (float), dwisdom fftw_ (real)
          if (nargin == 2)  //swisdom setter
            {
              if (args(1).is_string ())
                {
                  // Use STL function to convert to lower case
                  std::transform (arg0.begin (), arg0.end (), arg0.begin (),
                                  tolower);
                  std::string arg1 = args(1).string_value ();
                  if (!error_state)
                    {
                      char *str = fftwf_export_wisdom_to_string ();

                      if (arg1.length () < 1)
                        fftwf_forget_wisdom ();
                      else if (! fftwf_import_wisdom_from_string (arg1.c_str ()))
                        error ("fftw: could not import supplied WISDOM");

                      if (!error_state)
                        retval = octave_value (std::string (str));

                      free (str);
                    }
                }
            }
          else //swisdom getter
            {
              char *str = fftwf_export_wisdom_to_string ();
              retval = octave_value (std::string (str));
              free (str);
            }
        }
      else if (arg0 == "threads")
        {
          if (nargin == 2)  //threads setter
            {
              if (args(1).is_real_scalar ())
                {
                  int nthreads = args(1).int_value();
                  if (nthreads >= 1)
                    {
#if defined (HAVE_FFTW3_THREADS)
                      octave_fftw_planner::threads (nthreads);
#else
                      gripe_disabled_feature ("fftw", "multithreaded FFTW");
#endif
#if defined (HAVE_FFTW3F_THREADS)
                      octave_float_fftw_planner::threads (nthreads);
#else
                      gripe_disabled_feature ("fftw", "multithreaded FFTW");
#endif
                    }
                  else
                    error ("fftw: number of threads must be >=1");
                }
              else
                error ("fftw: setting threads needs one integer argument");
            }
          else //threads getter
#if defined (HAVE_FFTW3_THREADS)
            retval = octave_value (octave_fftw_planner::threads());
#else
            retval = 1;
#endif
        }
      else
        error ("fftw: unrecognized argument");
    }
  else
    error ("fftw: unrecognized argument");
#else

  gripe_disabled_feature ("fftw", "the FFTW3 planner");

#endif

  return retval;
}

/*
%!testif HAVE_FFTW
%! def_method = fftw ("planner");
%! unwind_protect
%!   method = "estimate";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "measure";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "patient";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "exhaustive";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "hybrid";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%! unwind_protect_cleanup
%!   fftw ("planner", def_method);
%! end_unwind_protect

%!testif HAVE_FFTW
%! def_dwisdom = fftw ("dwisdom");
%! def_swisdom = fftw ("swisdom");
%! unwind_protect
%!   wisdom = fftw ("dwisdom");
%!   assert (ischar (wisdom));
%!   fftw ("dwisdom", wisdom);
%!   assert (fftw ("dwisdom"), wisdom);
%!   wisdom = fftw ("swisdom");
%!   assert (ischar (wisdom));
%!   fftw ("swisdom", wisdom);
%!   assert (fftw ("swisdom"), wisdom);
%! unwind_protect_cleanup
%!   fftw ("dwisdom", def_dwisdom);
%!   fftw ("swisdom", def_swisdom);
%! end_unwind_protect

%!testif HAVE_FFTW3_THREADS
%! n = fftw ("threads");
%! unwind_protect
%!   fftw ("threads", 3);
%!   assert (fftw ("threads"), 3);
%! unwind_protect_cleanup
%!   fftw ("threads", n);
%! end_unwind_protect

%!error <Invalid call to fftw> fftw ();
%!error <Invalid call to fftw> fftw ("planner", "estimate", "measure");
%!error fftw (3);
%!error fftw ("invalid");
%!error fftw ("planner", "invalid");
%!error fftw ("planner", 2);
%!error fftw ("dwisdom", "invalid");
%!error fftw ("swisdom", "invalid");
%!error fftw ("threads", "invalid");
%!error fftw ("threads", -3);
 */
