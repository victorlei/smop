
/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#include <ctime>
#if defined (HAVE_UNORDERED_MAP)
#include <unordered_map>
#elif defined (HAVE_TR1_UNORDERED_MAP)
#include <tr1/unordered_map>
#endif
#include <string>

#include "f77-fcn.h"
#include "lo-mappers.h"
#include "oct-rand.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"
#include "ov-re-mat.h"

/*
%!shared __random_statistical_tests__
%! ## Flag whether the statistical tests should be run in "make check" or not
%! __random_statistical_tests__ = 0;
*/

static octave_value
do_rand (const octave_value_list& args, int nargin, const char *fcn,
         const std::string& distribution, bool additional_arg = false)
{
  octave_value retval;
  NDArray a;
  int idx = 0;
  dim_vector dims;
  bool is_single = false;

  unwind_protect frame;
  // Restore current distribution on any exit.
  frame.add_fcn (octave_rand::distribution,
                 octave_rand::distribution ());

  octave_rand::distribution (distribution);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string s_arg = args(nargin-1).string_value ();

      if (s_arg == "single")
        {
          is_single = true;
          nargin--;
        }
      else if (s_arg == "double")
        nargin--;
    }

  if (additional_arg)
    {
      if (nargin == 0)
        {
          error ("%s: expecting at least one argument", fcn);
          goto done;
        }
      else if (args(0).is_string ())
        additional_arg = false;
      else
        {
          a = args(0).array_value ();
          if (error_state)
            {
              error ("%s: expecting scalar or matrix arguments", fcn);
              goto done;
            }
          idx++;
          nargin--;
        }
    }

  switch (nargin)
    {
    case 0:
      {
        if (additional_arg)
          dims = a.dims ();
        else
          {
            dims.resize (2);

            dims(0) = 1;
            dims(1) = 1;
          }
        goto gen_matrix;
      }
      break;

    case 1:
      {
        octave_value tmp = args(idx);

        if (tmp.is_string ())
          {
            std::string s_arg = tmp.string_value ();

            if (s_arg == "dist")
              {
                retval = octave_rand::distribution ();
              }
            else if (s_arg == "seed")
              {
                retval = octave_rand::seed ();
              }
            else if (s_arg == "state" || s_arg == "twister")
              {
                retval = octave_rand::state (fcn);
              }
            else if (s_arg == "uniform")
              {
                octave_rand::uniform_distribution ();
              }
            else if (s_arg == "normal")
              {
                octave_rand::normal_distribution ();
              }
            else if (s_arg == "exponential")
              {
                octave_rand::exponential_distribution ();
              }
            else if (s_arg == "poisson")
              {
                octave_rand::poisson_distribution ();
              }
            else if (s_arg == "gamma")
              {
                octave_rand::gamma_distribution ();
              }
            else
              error ("%s: unrecognized string argument", fcn);
          }
        else if (tmp.is_scalar_type ())
          {
            double dval = tmp.double_value ();

            if (xisnan (dval))
              {
                error ("%s: NaN is invalid matrix dimension", fcn);
              }
            else
              {
                dims.resize (2);

                dims(0) = NINTbig (tmp.double_value ());
                dims(1) = NINTbig (tmp.double_value ());

                if (! error_state)
                  goto gen_matrix;
              }
          }
        else if (tmp.is_range ())
          {
            Range r = tmp.range_value ();

            if (r.all_elements_are_ints ())
              {
                octave_idx_type n = r.nelem ();

                dims.resize (n);

                octave_idx_type base = NINTbig (r.base ());
                octave_idx_type incr = NINTbig (r.inc ());

                for (octave_idx_type i = 0; i < n; i++)
                  {
                    //Negative dimensions are treated as zero for Matlab
                    //compatibility
                    dims(i) = base >= 0 ? base : 0;
                    base += incr;
                  }

                goto gen_matrix;

              }
            else
              error ("%s: all elements of range must be integers",
                     fcn);
          }
        else if (tmp.is_matrix_type ())
          {
            Array<int> iv = tmp.int_vector_value (true);

            if (! error_state)
              {
                octave_idx_type len = iv.length ();

                dims.resize (len);

                for (octave_idx_type i = 0; i < len; i++)
                  {
                    //Negative dimensions are treated as zero for Matlab
                    //compatibility
                    octave_idx_type elt = iv(i);
                    dims(i) = elt >=0 ? elt : 0;
                  }

                goto gen_matrix;
              }
            else
              error ("%s: expecting integer vector", fcn);
          }
        else
          {
            gripe_wrong_type_arg ("rand", tmp);
            return retval;
          }
      }
      break;

    default:
      {
        octave_value tmp = args(idx);

        if (nargin == 2 && tmp.is_string ())
          {
            std::string ts = tmp.string_value ();

            if (ts == "seed")
              {
                if (args(idx+1).is_real_scalar ())
                  {
                    double d = args(idx+1).double_value ();

                    if (! error_state)
                      octave_rand::seed (d);
                  }
                else if (args(idx+1).is_string ()
                         && args(idx+1).string_value () == "reset")
                  octave_rand::reset ();
                else
                  error ("%s: seed must be a real scalar", fcn);
              }
            else if (ts == "state" || ts == "twister")
              {
                if (args(idx+1).is_string ()
                    && args(idx+1).string_value () == "reset")
                  octave_rand::reset (fcn);
                else
                  {
                    ColumnVector s =
                      ColumnVector (args(idx+1).vector_value(false, true));

                    if (! error_state)
                      octave_rand::state (s, fcn);
                  }
              }
            else
              error ("%s: unrecognized string argument", fcn);
          }
        else
          {
            dims.resize (nargin);

            for (int i = 0; i < nargin; i++)
              {
                octave_idx_type elt = args(idx+i).int_value ();
                if (error_state)
                  {
                    error ("%s: expecting integer arguments", fcn);
                    goto done;
                  }
                //Negative is zero for Matlab compatibility
                dims(i) = elt >= 0 ? elt : 0;
              }

            goto gen_matrix;
          }
      }
      break;
    }

done:

  return retval;

gen_matrix:

  dims.chop_trailing_singletons ();

  if (is_single)
    {
      if (additional_arg)
        {
          if (a.length () == 1)
            return octave_rand::float_nd_array (dims, a(0));
          else
            {
              if (a.dims () != dims)
                {
                  error ("%s: mismatch in argument size", fcn);
                  return retval;
                }
              octave_idx_type len = a.length ();
              FloatNDArray m (dims);
              float *v = m.fortran_vec ();
              for (octave_idx_type i = 0; i < len; i++)
                v[i] = octave_rand::float_scalar (a(i));
              return m;
            }
        }
      else
        return octave_rand::float_nd_array (dims);
    }
  else
    {
      if (additional_arg)
        {
          if (a.length () == 1)
            return octave_rand::nd_array (dims, a(0));
          else
            {
              if (a.dims () != dims)
                {
                  error ("%s: mismatch in argument size", fcn);
                  return retval;
                }
              octave_idx_type len = a.length ();
              NDArray m (dims);
              double *v = m.fortran_vec ();
              for (octave_idx_type i = 0; i < len; i++)
                v[i] = octave_rand::scalar (a(i));
              return m;
            }
        }
      else
        return octave_rand::nd_array (dims);
    }
}

DEFUN (rand, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} rand (@var{n})\n\
@deftypefnx {Built-in Function} {} rand (@var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} rand ([@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {@var{v} =} rand (\"state\")\n\
@deftypefnx {Built-in Function} {} rand (\"state\", @var{v})\n\
@deftypefnx {Built-in Function} {} rand (\"state\", \"reset\")\n\
@deftypefnx {Built-in Function} {@var{v} =} rand (\"seed\")\n\
@deftypefnx {Built-in Function} {} rand (\"seed\", @var{v})\n\
@deftypefnx {Built-in Function} {} rand (\"seed\", \"reset\")\n\
@deftypefnx {Built-in Function} {} rand (@dots{}, \"single\")\n\
@deftypefnx {Built-in Function} {} rand (@dots{}, \"double\")\n\
Return a matrix with random elements uniformly distributed on the\n\
interval (0, 1).\n\
\n\
The arguments are handled the same as the arguments for @code{eye}.\n\
\n\
You can query the state of the random number generator using the form\n\
\n\
@example\n\
v = rand (\"state\")\n\
@end example\n\
\n\
This returns a column vector @var{v} of length 625.  Later, you can restore\n\
the random number generator to the state @var{v} using the form\n\
\n\
@example\n\
rand (\"state\", v)\n\
@end example\n\
\n\
@noindent\n\
You may also initialize the state vector from an arbitrary vector of length\n\
@leq{} 625 for @var{v}.  This new state will be a hash based on the value of\n\
@var{v}, not @var{v} itself.\n\
\n\
By default, the generator is initialized from @code{/dev/urandom} if it is\n\
available, otherwise from CPU time, wall clock time, and the current\n\
fraction of a second.  Note that this differs from @sc{matlab}, which\n\
always initializes the state to the same state at startup.  To obtain\n\
behavior comparable to @sc{matlab}, initialize with a deterministic state\n\
vector in Octave's startup files (@pxref{Startup Files}).\n\
\n\
To compute the pseudo-random sequence, @code{rand} uses the Mersenne\n\
Twister with a period of @math{2^{19937}-1}\n\
(See @nospell{M. Matsumoto and T. Nishimura},\n\
@cite{Mersenne Twister: A 623-dimensionally equidistributed uniform\n\
pseudorandom number generator},\n\
ACM Trans. on Modeling and Computer Simulation Vol. 8, No. 1, pp. 3--30,\n\
January 1998,\n\
@url{http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html}).\n\
Do @strong{not} use for cryptography without securely hashing several\n\
returned values together, otherwise the generator state can be learned after\n\
reading 624 consecutive values.\n\
\n\
Older versions of Octave used a different random number generator.\n\
The new generator is used by default as it is significantly faster than the\n\
old generator, and produces random numbers with a significantly longer cycle\n\
time.  However, in some circumstances it might be desirable to obtain the\n\
same random sequences as produced by the old generators.  To do this the\n\
keyword @qcode{\"seed\"} is used to specify that the old generators should\n\
be used, as in\n\
\n\
@example\n\
rand (\"seed\", val)\n\
@end example\n\
\n\
@noindent\n\
which sets the seed of the generator to @var{val}.  The seed of the\n\
generator can be queried with\n\
\n\
@example\n\
s = rand (\"seed\")\n\
@end example\n\
\n\
However, it should be noted that querying the seed will not cause\n\
@code{rand} to use the old generators, only setting the seed will.  To cause\n\
@code{rand} to once again use the new generators, the keyword\n\
@qcode{\"state\"} should be used to reset the state of the @code{rand}.\n\
\n\
The state or seed of the generator can be reset to a new random value using\n\
the @qcode{\"reset\"} keyword.\n\
\n\
The class of the value returned can be controlled by a trailing\n\
@qcode{\"double\"} or @qcode{\"single\"} argument.  These are the only valid\n\
classes.\n\
@seealso{randn, rande, randg, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = do_rand (args, nargin, "rand", "uniform");

  return retval;
}

// FIXME: The old generator (selected when "seed" is set) will not
// work properly if compiled to use 64-bit integers.

/*
%!test  # "state" can be a scalar
%! rand ("state", 12);  x = rand (1,4);
%! rand ("state", 12);  y = rand (1,4);
%! assert (x, y);
%!test  # "state" can be a vector
%! rand ("state", [12,13]);  x = rand (1,4);
%! rand ("state", [12;13]);  y = rand (1,4);
%! assert (x, y);
%!test  # querying "state" doesn't disturb sequence
%! rand ("state", 12);  rand (1,2);  x = rand (1,2);
%! rand ("state", 12);  rand (1,2);
%! s = rand ("state");  y = rand (1,2);
%! assert (x, y);
%! rand ("state", s);  z = rand (1,2);
%! assert (x, z);
%!test  # "seed" must be a scalar
%! rand ("seed", 12);  x = rand (1,4);
%! rand ("seed", 12);  y = rand (1,4);
%! assert (x, y);
%!error <seed must be a real scalar> rand ("seed", [12,13])
%!test  # querying "seed" returns a value which can be used later
%! s = rand ("seed");  x = rand (1,2);
%! rand ("seed", s);  y = rand (1,2);
%! assert (x, y);
%!test  # querying "seed" doesn't disturb sequence
%! rand ("seed", 12);  rand (1,2);  x = rand (1,2);
%! rand ("seed", 12);  rand (1,2);
%! s = rand ("seed");  y = rand (1,2);
%! assert (x, y);
%! rand ("seed", s);  z = rand (1,2);
%! assert (x, z);
*/

/*
%!test
%! ## Test fixed state
%! rand ("state", 1);
%! assert (rand (1,6), [0.1343642441124013 0.8474337369372327 0.763774618976614 0.2550690257394218 0.495435087091941 0.4494910647887382], 1e-6);
%!test
%! ## Test fixed seed
%! rand ("seed", 1);
%! assert (rand (1,6), [0.8668024251237512 0.9126510815694928 0.09366085007786751 0.1664607301354408 0.7408077004365623 0.7615650338120759], 1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   rand ("state", 12);
%!   x = rand (100000, 1);
%!   assert (max (x) < 1);   #*** Please report this!!! ***
%!   assert (min (x) > 0);   #*** Please report this!!! ***
%!   assert (mean (x), 0.5, 0.0024);
%!   assert (var (x), 1/48, 0.0632);
%!   assert (skewness (x), 0, 0.012);
%!   assert (kurtosis (x), -6/5, 0.0094);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   rand ("seed", 12);
%!   x = rand (100000, 1);
%!   assert (max (x) < 1);   #*** Please report this!!! ***
%!   assert (min (x) > 0);   #*** Please report this!!! ***
%!   assert (mean (x), 0.5, 0.0024);
%!   assert (var (x), 1/48, 0.0632);
%!   assert (skewness (x), 0, 0.012);
%!   assert (kurtosis (x), -6/5, 0.0094);
%! endif
*/

/*
%!# Test out-of-range values as rand() seeds.  See oct-rand.cc: double2uint32().
%!function v = __rand_sample__ (initval)
%!  rand ("state", initval);
%!  v = rand (1, 6);
%!endfunction
%!
%!assert (__rand_sample__ (0), __rand_sample__ (2^32))
%!assert (__rand_sample__ (-2), __rand_sample__ (2^32-2))
%!assert (__rand_sample__ (Inf), __rand_sample__ (NaN))
%!assert (! isequal (__rand_sample__ (-1), __rand_sample__ (-2)))
*/

static std::string current_distribution = octave_rand::distribution ();

DEFUN (randn, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} randn (@var{n})\n\
@deftypefnx {Built-in Function} {} randn (@var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} randn ([@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {@var{v} =} randn (\"state\")\n\
@deftypefnx {Built-in Function} {} randn (\"state\", @var{v})\n\
@deftypefnx {Built-in Function} {} randn (\"state\", \"reset\")\n\
@deftypefnx {Built-in Function} {@var{v} =} randn (\"seed\")\n\
@deftypefnx {Built-in Function} {} randn (\"seed\", @var{v})\n\
@deftypefnx {Built-in Function} {} randn (\"seed\", \"reset\")\n\
@deftypefnx {Built-in Function} {} randn (@dots{}, \"single\")\n\
@deftypefnx {Built-in Function} {} randn (@dots{}, \"double\")\n\
Return a matrix with normally distributed random elements having zero mean\n\
and variance one.\n\
\n\
The arguments are handled the same as the arguments for @code{rand}.\n\
\n\
By default, @code{randn} uses the @nospell{Marsaglia and Tsang}\n\
``Ziggurat technique'' to transform from a uniform to a normal distribution.\n\
\n\
The class of the value returned can be controlled by a trailing\n\
@qcode{\"double\"} or @qcode{\"single\"} argument.  These are the only valid\n\
classes.\n\
\n\
Reference: @nospell{G. Marsaglia and W.W. Tsang},\n\
@cite{Ziggurat Method for Generating Random Variables},\n\
J. Statistical Software, vol 5, 2000,\n\
@url{http://www.jstatsoft.org/v05/i08/}\n\
\n\
@seealso{rand, rande, randg, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = do_rand (args, nargin, "randn", "normal");

  return retval;
}

/*
%!test
%! ## Test fixed state
%! randn ("state", 1);
%! assert (randn (1, 6), [-2.666521678978671 -0.7381719971724564 1.507903992673601 0.6019427189162239 -0.450661261143348 -0.7054431351574116], 1e-6);
%!test
%! ## Test fixed seed
%! randn ("seed", 1);
%! assert (randn (1, 6), [-1.039402365684509 -1.25938892364502 0.1968704611063004 0.3874166905879974 -0.5976632833480835 -0.6615074276924133], 1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randn ("state", 12);
%!   x = randn (100000, 1);
%!   assert (mean (x), 0, 0.01);
%!   assert (var (x), 1, 0.02);
%!   assert (skewness (x), 0, 0.02);
%!   assert (kurtosis (x), 0, 0.04);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randn ("seed", 12);
%!   x = randn (100000, 1);
%!   assert (mean (x), 0, 0.01);
%!   assert (var (x), 1, 0.02);
%!   assert (skewness (x), 0, 0.02);
%!   assert (kurtosis (x), 0, 0.04);
%! endif
*/

DEFUN (rande, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} rande (@var{n})\n\
@deftypefnx {Built-in Function} {} rande (@var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} rande ([@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {@var{v} =} rande (\"state\")\n\
@deftypefnx {Built-in Function} {} rande (\"state\", @var{v})\n\
@deftypefnx {Built-in Function} {} rande (\"state\", \"reset\")\n\
@deftypefnx {Built-in Function} {@var{v} =} rande (\"seed\")\n\
@deftypefnx {Built-in Function} {} rande (\"seed\", @var{v})\n\
@deftypefnx {Built-in Function} {} rande (\"seed\", \"reset\")\n\
@deftypefnx {Built-in Function} {} rande (@dots{}, \"single\")\n\
@deftypefnx {Built-in Function} {} rande (@dots{}, \"double\")\n\
Return a matrix with exponentially distributed random elements.\n\
\n\
The arguments are handled the same as the arguments for @code{rand}.\n\
\n\
By default, @code{rande} uses the @nospell{Marsaglia and Tsang}\n\
``Ziggurat technique'' to transform from a uniform to an exponential\n\
distribution.\n\
\n\
The class of the value returned can be controlled by a trailing\n\
@qcode{\"double\"} or @qcode{\"single\"} argument.  These are the only valid\n\
classes.\n\
\n\
Reference: @nospell{G. Marsaglia and W.W. Tsang},\n\
@cite{Ziggurat Method for Generating Random Variables},\n\
J. Statistical Software, vol 5, 2000,\n\
@url{http://www.jstatsoft.org/v05/i08/}\n\
\n\
@seealso{rand, randn, randg, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = do_rand (args, nargin, "rande", "exponential");

  return retval;
}

/*
%!test
%! ## Test fixed state
%! rande ("state", 1);
%! assert (rande (1, 6), [3.602973885835625 0.1386190677555021 0.6743112889616958 0.4512830847258422 0.7255744741233175 0.3415969205292291], 1e-6);
%!test
%! ## Test fixed seed
%! rande ("seed", 1);
%! assert (rande (1, 6), [0.06492075175653866 1.717980206012726 0.4816154008731246 0.5231300676241517 0.103910739364359 1.668931916356087], 1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally
%!   rande ("state", 1);
%!   x = rande (100000, 1);
%!   assert (min (x) > 0);   # *** Please report this!!! ***
%!   assert (mean (x), 1, 0.01);
%!   assert (var (x), 1, 0.03);
%!   assert (skewness (x), 2, 0.06);
%!   assert (kurtosis (x), 6, 0.7);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally
%!   rande ("seed", 1);
%!   x = rande (100000, 1);
%!   assert (min (x)>0);   # *** Please report this!!! ***
%!   assert (mean (x), 1, 0.01);
%!   assert (var (x), 1, 0.03);
%!   assert (skewness (x), 2, 0.06);
%!   assert (kurtosis (x), 6, 0.7);
%! endif
*/

DEFUN (randg, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} randg (@var{n})\n\
@deftypefnx {Built-in Function} {} randg (@var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} randg ([@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {@var{v} =} randg (\"state\")\n\
@deftypefnx {Built-in Function} {} randg (\"state\", @var{v})\n\
@deftypefnx {Built-in Function} {} randg (\"state\", \"reset\")\n\
@deftypefnx {Built-in Function} {@var{v} =} randg (\"seed\")\n\
@deftypefnx {Built-in Function} {} randg (\"seed\", @var{v})\n\
@deftypefnx {Built-in Function} {} randg (\"seed\", \"reset\")\n\
@deftypefnx {Built-in Function} {} randg (@dots{}, \"single\")\n\
@deftypefnx {Built-in Function} {} randg (@dots{}, \"double\")\n\
Return a matrix with @code{gamma (@var{a},1)} distributed random elements.\n\
\n\
The arguments are handled the same as the arguments for @code{rand}, except\n\
for the argument @var{a}.\n\
\n\
This can be used to generate many distributions:\n\
\n\
@table @asis\n\
@item @code{gamma (a, b)} for @code{a > -1}, @code{b > 0}\n\
\n\
@example\n\
r = b * randg (a)\n\
@end example\n\
\n\
@item @code{beta (a, b)} for @code{a > -1}, @code{b > -1}\n\
\n\
@example\n\
@group\n\
r1 = randg (a, 1)\n\
r = r1 / (r1 + randg (b, 1))\n\
@end group\n\
@end example\n\
\n\
@item @code{Erlang (a, n)}\n\
\n\
@example\n\
r = a * randg (n)\n\
@end example\n\
\n\
@item @code{chisq (df)} for @code{df > 0}\n\
\n\
@example\n\
r = 2 * randg (df / 2)\n\
@end example\n\
\n\
@item @code{t (df)} for @code{0 < df < inf} (use randn if df is infinite)\n\
\n\
@example\n\
r = randn () / sqrt (2 * randg (df / 2) / df)\n\
@end example\n\
\n\
@item @code{F (n1, n2)} for @code{0 < n1}, @code{0 < n2}\n\
\n\
@example\n\
@group\n\
## r1 equals 1 if n1 is infinite\n\
r1 = 2 * randg (n1 / 2) / n1\n\
## r2 equals 1 if n2 is infinite\n\
r2 = 2 * randg (n2 / 2) / n2\n\
r = r1 / r2\n\n\
@end group\n\
@end example\n\
\n\
@item negative @code{binomial (n, p)} for @code{n > 0}, @code{0 < p <= 1}\n\
\n\
@example\n\
r = randp ((1 - p) / p * randg (n))\n\
@end example\n\
\n\
@item non-central @code{chisq (df, L)}, for @code{df >= 0} and @code{L > 0}\n\
(use chisq if @code{L = 0})\n\
\n\
@example\n\
@group\n\
r = randp (L / 2)\n\
r(r > 0) = 2 * randg (r(r > 0))\n\
r(df > 0) += 2 * randg (df(df > 0)/2)\n\
@end group\n\
@end example\n\
\n\
@item @code{Dirichlet (a1, @dots{} ak)}\n\
\n\
@example\n\
@group\n\
r = (randg (a1), @dots{}, randg (ak))\n\
r = r / sum (r)\n\
@end group\n\
@end example\n\
\n\
@end table\n\
\n\
The class of the value returned can be controlled by a trailing\n\
@qcode{\"double\"} or @qcode{\"single\"} argument.  These are the only valid\n\
classes.\n\
@seealso{rand, randn, rande, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1)
    error ("randg: insufficient arguments");
  else
    retval = do_rand (args, nargin, "randg", "gamma", true);

  return retval;
}

/*
%!test
%! randg ("state", 12)
%! assert (randg ([-inf, -1, 0, inf, nan]), [nan, nan, nan, nan, nan]); # *** Please report

%!test
%! ## Test fixed state
%! randg ("state", 1);
%! assert (randg (0.1, 1, 6), [0.0103951513331241 8.335671459898252e-05 0.00138691397249762 0.000587308416993855 0.495590518784736 2.3921917414795e-12], 1e-6);
%!test
%! ## Test fixed state
%! randg ("state", 1);
%! assert (randg (0.95, 1, 6), [3.099382433255327 0.3974529788871218 0.644367450750855 1.143261091802246 1.964111762696822 0.04011915547957939], 1e-6);
%!test
%! ## Test fixed state
%! randg ("state", 1);
%! assert (randg (1, 1, 6), [0.2273389379645993 1.288822625058359 0.2406335209340746 1.218869553370733 1.024649860162554 0.09631230343599533], 1e-6);
%!test
%! ## Test fixed state
%! randg ("state", 1);
%! assert (randg (10, 1, 6), [3.520369644331133 15.15369864472106 8.332112081991205 8.406211067432674 11.81193475187611 10.88792728177059], 1e-5);
%!test
%! ## Test fixed state
%! randg ("state", 1);
%! assert (randg (100, 1, 6), [75.34570255262264 115.4911985594699 95.23493031356388 95.48926019250911 106.2397448229803 103.4813150404118], 1e-4);
%!test
%! ## Test fixed seed
%! randg ("seed", 1);
%! assert (randg (0.1, 1, 6), [0.07144210487604141 0.460641473531723 0.4749028384685516 0.06823389977216721 0.000293838675133884 1.802567535340305e-12], 1e-6);
%!test
%! ## Test fixed seed
%! randg ("seed", 1);
%! assert (randg (0.95, 1, 6), [1.664905071258545 1.879976987838745 1.905677795410156 0.9948706030845642 0.5606933236122131 0.0766092911362648], 1e-6);
%!test
%! ## Test fixed seed
%! randg ("seed", 1);
%! assert (randg (1, 1, 6), [0.03512085229158401 0.6488978862762451 0.8114678859710693 0.1666885763406754 1.60791552066803 1.90356981754303], 1e-6);
%!test
%! ## Test fixed seed
%! randg ("seed", 1);
%! assert (randg (10, 1, 6), [6.566435813903809 10.11648464202881 10.73162078857422 7.747178077697754 6.278522491455078 6.240195751190186], 1e-5);
%!test
%! ## Test fixed seed
%! randg ("seed", 1);
%! assert (randg (100, 1, 6), [89.40208435058594 101.4734725952148 103.4020004272461 93.62763214111328 88.33104705810547 88.1871337890625], 1e-4);
%!test
%! ## Test out-of-bounds values produce NaN w/old-style generators & floats
%! randg ("seed", 1);
%! result = randg ([-2 Inf], "single");
%! assert (result, single ([NaN NaN]));

%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 0.1;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.01);
%!   assert (skewness (x), 2/sqrt (a), 1);
%!   assert (kurtosis (x), 6/a,        50);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 0.95;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 1;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 10;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.1);
%!   assert (var (x),      a,          0.5);
%!   assert (skewness (x), 2/sqrt (a), 0.1);
%!   assert (kurtosis (x), 6/a,        0.5);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 100;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.2);
%!   assert (var (x),      a,          2);
%!   assert (skewness (x), 2/sqrt (a), 0.05);
%!   assert (kurtosis (x), 6/a,        0.2);
%! endif
%!test
%! randg ("seed", 12);
%!assert (randg ([-inf, -1, 0, inf, nan]), [nan, nan, nan, nan, nan]) # *** Please report
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 0.1;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.01);
%!   assert (skewness (x), 2/sqrt (a), 1);
%!   assert (kurtosis (x), 6/a,        50);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 0.95;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 1;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 10;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.1);
%!   assert (var (x),      a,          0.5);
%!   assert (skewness (x), 2/sqrt (a), 0.1);
%!   assert (kurtosis (x), 6/a,        0.5);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 100;
%!   x = randg (a, 100000, 1);
%!   assert (mean (x),     a,          0.2);
%!   assert (var (x),      a,          2);
%!   assert (skewness (x), 2/sqrt (a), 0.05);
%!   assert (kurtosis (x), 6/a,        0.2);
%! endif
*/

DEFUN (randp, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} randp (@var{l}, @var{n})\n\
@deftypefnx {Built-in Function} {} randp (@var{l}, @var{m}, @var{n}, @dots{})\n\
@deftypefnx {Built-in Function} {} randp (@var{l}, [@var{m} @var{n} @dots{}])\n\
@deftypefnx {Built-in Function} {@var{v} =} randp (\"state\")\n\
@deftypefnx {Built-in Function} {} randp (\"state\", @var{v})\n\
@deftypefnx {Built-in Function} {} randp (\"state\", \"reset\")\n\
@deftypefnx {Built-in Function} {@var{v} =} randp (\"seed\")\n\
@deftypefnx {Built-in Function} {} randp (\"seed\", @var{v})\n\
@deftypefnx {Built-in Function} {} randp (\"seed\", \"reset\")\n\
@deftypefnx {Built-in Function} {} randp (@dots{}, \"single\")\n\
@deftypefnx {Built-in Function} {} randp (@dots{}, \"double\")\n\
Return a matrix with Poisson distributed random elements with mean value\n\
parameter given by the first argument, @var{l}.\n\
\n\
The arguments are handled the same as the arguments for @code{rand}, except\n\
for the argument @var{l}.\n\
\n\
Five different algorithms are used depending on the range of @var{l} and\n\
whether or not @var{l} is a scalar or a matrix.\n\
\n\
@table @asis\n\
@item For scalar @var{l} @leq{} 12, use direct method.\n\
W.H. Press, et al., @cite{Numerical Recipes in C},\n\
Cambridge University Press, 1992.\n\
\n\
@item For scalar @var{l} > 12, use rejection method.[1]\n\
W.H. Press, et al., @cite{Numerical Recipes in C},\n\
Cambridge University Press, 1992.\n\
\n\
@item For matrix @var{l} @leq{} 10, use inversion method.[2]\n\
@nospell{E. Stadlober, et al., WinRand source code}, available via FTP.\n\
\n\
@item For matrix @var{l} > 10, use patchwork rejection method.\n\
@nospell{E. Stadlober, et al., WinRand source code}, available via FTP, or\n\
@nospell{H. Zechner}, @cite{Efficient sampling from continuous and discrete\n\
unimodal distributions}, Doctoral Dissertation, 156pp., Technical\n\
University @nospell{Graz}, Austria, 1994.\n\
\n\
@item For @var{l} > 1e8, use normal approximation.\n\
@nospell{L. Montanet}, et al., @cite{Review of Particle Properties},\n\
Physical Review D 50 p1284, 1994.\n\
@end table\n\
\n\
The class of the value returned can be controlled by a trailing\n\
@qcode{\"double\"} or @qcode{\"single\"} argument.  These are the only valid\n\
classes.\n\
@seealso{rand, randn, rande, randg}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1)
    error ("randp: insufficient arguments");
  else
    retval = do_rand (args, nargin, "randp", "poisson", true);

  return retval;
}

/*
%!test
%! randp ("state", 12);
%! assert (randp ([-inf, -1, 0, inf, nan]), [nan, nan, 0, nan, nan]);   # *** Please report
%!test
%! ## Test fixed state
%! randp ("state", 1);
%! assert (randp (5, 1, 6), [5 5 3 7 7 3])
%!test
%! ## Test fixed state
%! randp ("state", 1);
%! assert (randp (15, 1, 6), [13 15 8 18 18 15])
%!test
%! ## Test fixed state
%! randp ("state", 1);
%! assert (randp (1e9, 1, 6), [999915677 999976657 1000047684 1000019035 999985749 999977692], -1e-6)
%!test
%! ## Test fixed state
%! randp ("seed", 1);
%! %%assert (randp (5, 1, 6), [8 2 3 6 6 8])
%! assert (randp (5, 1, 5), [8 2 3 6 6])
%!test
%! ## Test fixed state
%! randp ("seed", 1);
%! assert (randp (15, 1, 6), [15 16 12 10 10 12])
%!test
%! ## Test fixed state
%! randp ("seed", 1);
%! assert (randp (1e9, 1, 6), [1000006208 1000012224 999981120 999963520 999963072 999981440], -1e-6)
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("state", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a (1), 100000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("state", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a(1)*ones (100000, 1), 100000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
%!test
%! randp ("seed", 12);
%! assert (randp ([-inf, -1, 0, inf, nan]), [nan, nan, 0, nan, nan]);   # *** Please report
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("seed", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a(1), 100000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("seed", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a(1)*ones (100000, 1), 100000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
*/

DEFUN (randperm, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} randperm (@var{n})\n\
@deftypefnx {Built-in Function} {} randperm (@var{n}, @var{m})\n\
Return a row vector containing a random permutation of @code{1:@var{n}}.\n\
\n\
If @var{m} is supplied, return @var{m} unique entries, sampled without\n\
replacement from @code{1:@var{n}}.\n\
\n\
The complexity is O(@var{n}) in memory and O(@var{m}) in time, unless\n\
@var{m} < @var{n}/5, in which case O(@var{m}) memory is used as well.  The\n\
randomization is performed using rand().  All permutations are equally\n\
likely.\n\
@seealso{perms}\n\
@end deftypefn")
{

#ifdef USE_UNORDERED_MAP_WITH_TR1
using std::tr1::unordered_map;
#else
using std::unordered_map;
#endif

  int nargin = args.length ();
  octave_value retval;

  if (nargin == 1 || nargin == 2)
    {
      octave_idx_type n, m;

      n = args(0).idx_type_value (true);

      if (nargin == 2)
        m = args(1).idx_type_value (true);
      else
        m = n;

      if (m < 0 || n < 0)
        error ("randperm: M and N must be non-negative");

      if (m > n)
        error ("randperm: M must be less than or equal to N");

      // Quick and dirty heuristic to decide if we allocate or not the
      // whole vector for tracking the truncated shuffle.
      bool short_shuffle = m < n/5;

      if (! error_state)
        {
          // Generate random numbers.
          NDArray r = octave_rand::nd_array (dim_vector (1, m));
          double *rvec = r.fortran_vec ();

          octave_idx_type idx_len = short_shuffle ? m : n;
          Array<octave_idx_type> idx;
          try
            {
              idx = Array<octave_idx_type> (dim_vector (1, idx_len));
            }
          catch (std::bad_alloc)
            {
              // Looks like n is too big and short_shuffle is false.
              // Let's try again, but this time with the alternative.
              idx_len = m;
              short_shuffle = true;
              idx = Array<octave_idx_type> (dim_vector (1, idx_len));
            }

          octave_idx_type *ivec = idx.fortran_vec ();

          for (octave_idx_type i = 0; i < idx_len; i++)
            ivec[i] = i;

          if (short_shuffle)
            {
              unordered_map<octave_idx_type, octave_idx_type> map (m);

              // Perform the Knuth shuffle only keeping track of moved
              // entries in the map
              for (octave_idx_type i = 0; i < m; i++)
                {
                  octave_idx_type k = i +
                                      gnulib::floor (rvec[i] * (n - i));

                  //For shuffling first m entries, no need to use extra
                  //storage
                  if (k < m)
                    {
                      std::swap (ivec[i], ivec[k]);
                    }
                  else
                    {
                      if (map.find (k) == map.end ())
                        map[k] = k;

                      std::swap (ivec[i], map[k]);
                    }
                }
            }
          else
            {

              // Perform the Knuth shuffle of the first m entries
              for (octave_idx_type i = 0; i < m; i++)
                {
                  octave_idx_type k = i +
                                      gnulib::floor (rvec[i] * (n - i));
                  std::swap (ivec[i], ivec[k]);
                }
            }

          // Convert to doubles, reusing r.
          for (octave_idx_type i = 0; i < m; i++)
            rvec[i] = ivec[i] + 1;

          if (m < n)
            idx.resize (dim_vector (1, m));

          // Now create an array object with a cached idx_vector.
          retval = new octave_matrix (r, idx_vector (idx));
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (sort (randperm (20)), 1:20)
%!assert (length (randperm (20,10)), 10)

## Test biggish N (bug #39378)
%!assert (length (randperm (30000^2, 100000)), 100000)

%!test
%! rand ("seed", 0);
%! for i = 1:100
%!   p = randperm (305, 30);
%!   assert (length (unique (p)), 30);
%! endfor
*/
