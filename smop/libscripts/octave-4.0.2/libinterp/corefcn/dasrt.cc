/*

Copyright (C) 2002-2015 John W. Eaton

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

#include <iostream>
#include <string>

#include "DASRT.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "ov-cell.h"
#include "pager.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "DASRT-opts.cc"

// Global pointers for user defined function required by dasrt.
static octave_function *dasrt_f;
static octave_function *dasrt_j;
static octave_function *dasrt_cf;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;
static bool warned_cf_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
dasrt_user_f (const ColumnVector& x, const ColumnVector& xdot,
              double t, octave_idx_type&)
{
  ColumnVector retval;

  assert (x.capacity () == xdot.capacity ());

  octave_value_list args;

  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (dasrt_f)
    {
      octave_value_list tmp = dasrt_f->do_multi_index_op (1, args);

      if (error_state)
        {
          gripe_user_supplied_eval ("dasrt");
          return retval;
        }

      if (tmp.length () > 0 && tmp(0).is_defined ())
        {
          if (! warned_fcn_imaginary && tmp(0).is_complex_type ())
            {
              warning ("dasrt: ignoring imaginary part returned from user-supplied function");
              warned_fcn_imaginary = true;
            }

          retval = ColumnVector (tmp(0).vector_value ());

          if (error_state || retval.length () == 0)
            gripe_user_supplied_eval ("dasrt");
        }
      else
        gripe_user_supplied_eval ("dasrt");
    }

  return retval;
}

static ColumnVector
dasrt_user_cf (const ColumnVector& x, double t)
{
  ColumnVector retval;

  octave_value_list args;

  args(1) = t;
  args(0) = x;

  if (dasrt_cf)
    {
      octave_value_list tmp = dasrt_cf->do_multi_index_op (1, args);

      if (error_state)
        {
          gripe_user_supplied_eval ("dasrt");
          return retval;
        }

      if (tmp.length () > 0 && tmp(0).is_defined ())
        {
          if (! warned_cf_imaginary && tmp(0).is_complex_type ())
            {
              warning ("dasrt: ignoring imaginary part returned from user-supplied constraint function");
              warned_cf_imaginary = true;
            }

          retval = ColumnVector (tmp(0).vector_value ());

          if (error_state || retval.length () == 0)
            gripe_user_supplied_eval ("dasrt");
        }
      else
        gripe_user_supplied_eval ("dasrt");
    }

  return retval;
}

static Matrix
dasrt_user_j (const ColumnVector& x, const ColumnVector& xdot,
              double t, double cj)
{
  Matrix retval;

  assert (x.capacity () == xdot.capacity ());

  octave_value_list args;

  args(3) = cj;
  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (dasrt_j)
    {
      octave_value_list tmp = dasrt_j->do_multi_index_op (1, args);

      if (error_state)
        {
          gripe_user_supplied_eval ("dasrt");
          return retval;
        }

      int tlen = tmp.length ();
      if (tlen > 0 && tmp(0).is_defined ())
        {
          if (! warned_jac_imaginary && tmp(0).is_complex_type ())
            {
              warning ("dasrt: ignoring imaginary part returned from user-supplied jacobian function");
              warned_jac_imaginary = true;
            }

          retval = tmp(0).matrix_value ();

          if (error_state || retval.length () == 0)
            gripe_user_supplied_eval ("dasrt");
        }
      else
        gripe_user_supplied_eval ("dasrt");
    }

  return retval;
}

#define DASRT_ABORT \
  return retval

#define DASRT_ABORT1(msg) \
  do \
    { \
      ::error ("dasrt: " msg); \
      DASRT_ABORT; \
    } \
  while (0)

#define DASRT_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("dasrt: " fmt, arg); \
      DASRT_ABORT; \
    } \
  while (0)

DEFUN (dasrt, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{x}, @var{xdot}, @var{t_out}, @var{istat}, @var{msg}] =} dasrt (@var{fcn}, [], @var{x_0}, @var{xdot_0}, @var{t})\n\
@deftypefnx {Built-in Function} {@dots{} =} dasrt (@var{fcn}, @var{g}, @var{x_0}, @var{xdot_0}, @var{t})\n\
@deftypefnx {Built-in Function} {@dots{} =} dasrt (@var{fcn}, [], @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})\n\
@deftypefnx {Built-in Function} {@dots{} =} dasrt (@var{fcn}, @var{g}, @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})\n\
Solve the set of differential-algebraic equations\n\
@tex\n\
$$ 0 = f (x, \\dot{x}, t) $$\n\
with\n\
$$ x(t_0) = x_0, \\dot{x}(t_0) = \\dot{x}_0 $$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
0 = f (x, xdot, t)\n\
@end example\n\
\n\
@noindent\n\
with\n\
\n\
@example\n\
x(t_0) = x_0, xdot(t_0) = xdot_0\n\
@end example\n\
\n\
@end ifnottex\n\
with functional stopping criteria (root solving).\n\
\n\
The solution is returned in the matrices @var{x} and @var{xdot},\n\
with each row in the result matrices corresponding to one of the\n\
elements in the vector @var{t_out}.  The first element of @var{t}\n\
should be @math{t_0} and correspond to the initial state of the\n\
system @var{x_0} and its derivative @var{xdot_0}, so that the first\n\
row of the output @var{x} is @var{x_0} and the first row\n\
of the output @var{xdot} is @var{xdot_0}.\n\
\n\
The vector @var{t} provides an upper limit on the length of the\n\
integration.  If the stopping condition is met, the vector\n\
@var{t_out} will be shorter than @var{t}, and the final element of\n\
@var{t_out} will be the point at which the stopping condition was met,\n\
and may not correspond to any element of the vector @var{t}.\n\
\n\
The first argument, @var{fcn}, is a string, inline, or function handle\n\
that names the function @math{f} to call to compute the vector of\n\
residuals for the set of equations.  It must have the form\n\
\n\
@example\n\
@var{res} = f (@var{x}, @var{xdot}, @var{t})\n\
@end example\n\
\n\
@noindent\n\
in which @var{x}, @var{xdot}, and @var{res} are vectors, and @var{t} is a\n\
scalar.\n\
\n\
If @var{fcn} is a two-element string array or a two-element cell array\n\
of strings, inline functions, or function handles, the first element names\n\
the function @math{f} described above, and the second element names a\n\
function to compute the modified Jacobian\n\
\n\
@tex\n\
$$\n\
J = {\\partial f \\over \\partial x}\n\
  + c {\\partial f \\over \\partial \\dot{x}}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
      df       df\n\
jac = -- + c ------\n\
      dx     d xdot\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
The modified Jacobian function must have the form\n\
\n\
@example\n\
@group\n\
\n\
@var{jac} = j (@var{x}, @var{xdot}, @var{t}, @var{c})\n\
\n\
@end group\n\
@end example\n\
\n\
The optional second argument names a function that defines the\n\
constraint functions whose roots are desired during the integration.\n\
This function must have the form\n\
\n\
@example\n\
@var{g_out} = g (@var{x}, @var{t})\n\
@end example\n\
\n\
@noindent\n\
and return a vector of the constraint function values.\n\
If the value of any of the constraint functions changes sign, @sc{dasrt}\n\
will attempt to stop the integration at the point of the sign change.\n\
\n\
If the name of the constraint function is omitted, @code{dasrt} solves\n\
the same problem as @code{daspk} or @code{dassl}.\n\
\n\
Note that because of numerical errors in the constraint functions\n\
due to round-off and integration error, @sc{dasrt} may return false\n\
roots, or return the same root at two or more nearly equal values of\n\
@var{T}.  If such false roots are suspected, the user should consider\n\
smaller error tolerances or higher precision in the evaluation of the\n\
constraint functions.\n\
\n\
If a root of some constraint function defines the end of the problem,\n\
the input to @sc{dasrt} should nevertheless allow integration to a\n\
point slightly past that root, so that @sc{dasrt} can locate the root\n\
by interpolation.\n\
\n\
The third and fourth arguments to @code{dasrt} specify the initial\n\
condition of the states and their derivatives, and the fourth argument\n\
specifies a vector of output times at which the solution is desired,\n\
including the time corresponding to the initial condition.\n\
\n\
The set of initial states and derivatives are not strictly required to\n\
be consistent.  In practice, however, @sc{dassl} is not very good at\n\
determining a consistent set for you, so it is best if you ensure that\n\
the initial values result in the function evaluating to zero.\n\
\n\
The sixth argument is optional, and may be used to specify a set of\n\
times that the DAE solver should not integrate past.  It is useful for\n\
avoiding difficulties with singularities and points where there is a\n\
discontinuity in the derivative.\n\
\n\
After a successful computation, the value of @var{istate} will be\n\
greater than zero (consistent with the Fortran version of @sc{dassl}).\n\
\n\
If the computation is not successful, the value of @var{istate} will be\n\
less than zero and @var{msg} will contain additional information.\n\
\n\
You can use the function @code{dasrt_options} to set optional\n\
parameters for @code{dasrt}.\n\
@seealso{dasrt_options, daspk, dasrt, lsode}\n\
@end deftypefn")
{
  octave_value_list retval;

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;
  warned_cf_imaginary = false;

  unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    DASRT_ABORT1 ("invalid recursive call");

  int argp = 0;

  int nargin = args.length ();

  if (nargin < 4 || nargin > 6)
    {
      print_usage ();
      return retval;
    }

  std::string fcn_name, fname, jac_name, jname;
  dasrt_f = 0;
  dasrt_j = 0;
  dasrt_cf = 0;

  // Check all the arguments.  Are they the right animals?

  // Here's where I take care of f and j in one shot:

  octave_value f_arg = args(0);

  if (f_arg.is_cell ())
    {
      Cell c = f_arg.cell_value ();
      if (c.length () == 1)
        f_arg = c(0);
      else if (c.length () == 2)
        {
          if (c(0).is_function_handle () || c(0).is_inline_function ())
            dasrt_f = c(0).function_value ();
          else
            {
              fcn_name = unique_symbol_name ("__dasrt_fcn__");
              fname = "function y = ";
              fname.append (fcn_name);
              fname.append (" (x, xdot, t) y = ");
              dasrt_f = extract_function (c(0), "dasrt", fcn_name, fname,
                                          "; endfunction");
            }

          if (dasrt_f)
            {
              if (c(1).is_function_handle () || c(1).is_inline_function ())
                dasrt_j = c(1).function_value ();
              else
                {
                  jac_name = unique_symbol_name ("__dasrt_jac__");
                  jname = "function jac = ";
                  jname.append (jac_name);
                  jname.append (" (x, xdot, t, cj) jac = ");
                  dasrt_j = extract_function (c(1), "dasrt", jac_name, jname,
                                              "; endfunction");

                  if (!dasrt_j)
                    {
                      if (fcn_name.length ())
                        clear_function (fcn_name);
                      dasrt_f = 0;
                    }
                }
            }
        }
      else
        DASRT_ABORT1 ("incorrect number of elements in cell array");
    }

  if (!dasrt_f && ! f_arg.is_cell ())
    {
      if (f_arg.is_function_handle () || f_arg.is_inline_function ())
        dasrt_f = f_arg.function_value ();
      else
        {
          switch (f_arg.rows ())
            {
            case 1:
              fcn_name = unique_symbol_name ("__dasrt_fcn__");
              fname = "function y = ";
              fname.append (fcn_name);
              fname.append (" (x, xdot, t) y = ");
              dasrt_f = extract_function (f_arg, "dasrt", fcn_name, fname,
                                          "; endfunction");
              break;

            case 2:
              {
                string_vector tmp = args(0).all_strings ();

                if (! error_state)
                  {
                    fcn_name = unique_symbol_name ("__dasrt_fcn__");
                    fname = "function y = ";
                    fname.append (fcn_name);
                    fname.append (" (x, xdot, t) y = ");
                    dasrt_f = extract_function (tmp(0), "dasrt", fcn_name,
                                                fname, "; endfunction");

                    if (dasrt_f)
                      {
                        jac_name = unique_symbol_name ("__dasrt_jac__");
                        jname = "function jac = ";
                        jname.append (jac_name);
                        jname.append (" (x, xdot, t, cj) jac = ");
                        dasrt_j = extract_function (tmp(1), "dasrt", jac_name,
                                                    jname, "; endfunction");

                        if (! dasrt_j)
                          dasrt_f = 0;
                      }
                  }
              }
              break;

            default:
              DASRT_ABORT1
                ("first arg should be a string or 2-element string array");
            }
        }
    }

  if (error_state || (! dasrt_f))
    DASRT_ABORT;

  DAERTFunc func (dasrt_user_f);

  argp++;

  if (args(1).is_function_handle () || args(1).is_inline_function ())
    {
      dasrt_cf = args(1).function_value ();

      if (! dasrt_cf)
        DASRT_ABORT1 ("expecting function name as argument 2");

      argp++;

      func.set_constraint_function (dasrt_user_cf);
    }
  else if (args(1).is_string ())
    {
      dasrt_cf = is_valid_function (args(1), "dasrt", true);
      if (! dasrt_cf)
        DASRT_ABORT1 ("expecting function name as argument 2");

      argp++;

      func.set_constraint_function (dasrt_user_cf);
    }

  ColumnVector state (args(argp++).vector_value ());

  if (error_state)
    DASRT_ABORT2 ("expecting state vector as argument %d", argp);

  ColumnVector stateprime (args(argp++).vector_value ());

  if (error_state)
    DASRT_ABORT2 ("expecting time derivative of state vector as argument %d",
                  argp);

  ColumnVector out_times (args(argp++).vector_value ());

  if (error_state)
    DASRT_ABORT2 ("expecting output time vector as %s argument %d", argp);

  double tzero = out_times (0);

  ColumnVector crit_times;

  bool crit_times_set = false;

  if (argp < nargin)
    {
      crit_times = ColumnVector (args(argp++).vector_value ());

      if (error_state)
        DASRT_ABORT2 ("expecting critical time vector as argument %d", argp);

      crit_times_set = true;
    }

  if (dasrt_j)
    func.set_jacobian_function (dasrt_user_j);

  DASRT_result output;

  DASRT dae = DASRT (state, stateprime, tzero, func);

  dae.set_options (dasrt_opts);

  if (crit_times_set)
    output = dae.integrate (out_times, crit_times);
  else
    output = dae.integrate (out_times);

  if (fcn_name.length ())
    clear_function (fcn_name);
  if (jac_name.length ())
    clear_function (jac_name);

  if (! error_state)
    {
      std::string msg = dae.error_message ();

      retval(4) = msg;
      retval(3) = static_cast<double> (dae.integration_state ());

      if (dae.integration_ok ())
        {
          retval(2) = output.times ();
          retval(1) = output.deriv ();
          retval(0) = output.state ();
        }
      else
        {
          retval(2) = Matrix ();
          retval(1) = Matrix ();
          retval(0) = Matrix ();

          if (nargout < 4)
            error ("dasrt: %s", msg.c_str ());
        }
    }

  return retval;
}
