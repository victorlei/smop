/*

Copyright (C) 1996-2015 John W. Eaton

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

#include <string>

#include <iomanip>
#include <iostream>

#include "DASSL.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "ov-cell.h"
#include "pager.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "DASSL-opts.cc"

// Global pointer for user defined function required by dassl.
static octave_function *dassl_fcn;

// Global pointer for optional user defined jacobian function.
static octave_function *dassl_jac;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

ColumnVector
dassl_user_function (const ColumnVector& x, const ColumnVector& xdot,
                     double t, octave_idx_type& ires)
{
  ColumnVector retval;

  assert (x.capacity () == xdot.capacity ());

  octave_value_list args;

  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (dassl_fcn)
    {
      octave_value_list tmp = dassl_fcn->do_multi_index_op (1, args);

      if (error_state)
        {
          gripe_user_supplied_eval ("dassl");
          return retval;
        }

      int tlen = tmp.length ();
      if (tlen > 0 && tmp(0).is_defined ())
        {
          if (! warned_fcn_imaginary && tmp(0).is_complex_type ())
            {
              warning ("dassl: ignoring imaginary part returned from user-supplied function");
              warned_fcn_imaginary = true;
            }

          retval = ColumnVector (tmp(0).vector_value ());

          if (tlen > 1)
            ires = tmp(1).int_value ();

          if (error_state || retval.length () == 0)
            gripe_user_supplied_eval ("dassl");
        }
      else
        gripe_user_supplied_eval ("dassl");
    }

  return retval;
}

Matrix
dassl_user_jacobian (const ColumnVector& x, const ColumnVector& xdot,
                     double t, double cj)
{
  Matrix retval;

  assert (x.capacity () == xdot.capacity ());

  octave_value_list args;

  args(3) = cj;
  args(2) = t;
  args(1) = xdot;
  args(0) = x;

  if (dassl_jac)
    {
      octave_value_list tmp = dassl_jac->do_multi_index_op (1, args);

      if (error_state)
        {
          gripe_user_supplied_eval ("dassl");
          return retval;
        }

      int tlen = tmp.length ();
      if (tlen > 0 && tmp(0).is_defined ())
        {
          if (! warned_jac_imaginary && tmp(0).is_complex_type ())
            {
              warning ("dassl: ignoring imaginary part returned from user-supplied jacobian function");
              warned_jac_imaginary = true;
            }

          retval = tmp(0).matrix_value ();

          if (error_state || retval.length () == 0)
            gripe_user_supplied_eval ("dassl");
        }
      else
        gripe_user_supplied_eval ("dassl");
    }

  return retval;
}

#define DASSL_ABORT() \
  return retval

#define DASSL_ABORT1(msg) \
  do \
    { \
      ::error ("dassl: " msg); \
      DASSL_ABORT (); \
    } \
  while (0)

#define DASSL_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("dassl: " fmt, arg); \
      DASSL_ABORT (); \
    } \
  while (0)

DEFUN (dassl, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{x}, @var{xdot}, @var{istate}, @var{msg}] =} dassl (@var{fcn}, @var{x_0}, @var{xdot_0}, @var{t}, @var{t_crit})\n\
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
The solution is returned in the matrices @var{x} and @var{xdot},\n\
with each row in the result matrices corresponding to one of the\n\
elements in the vector @var{t}.  The first element of @var{t}\n\
should be @math{t_0} and correspond to the initial state of the\n\
system @var{x_0} and its derivative @var{xdot_0}, so that the first\n\
row of the output @var{x} is @var{x_0} and the first row\n\
of the output @var{xdot} is @var{xdot_0}.\n\
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
The second and third arguments to @code{dassl} specify the initial\n\
condition of the states and their derivatives, and the fourth argument\n\
specifies a vector of output times at which the solution is desired,\n\
including the time corresponding to the initial condition.\n\
\n\
The set of initial states and derivatives are not strictly required to\n\
be consistent.  In practice, however, @sc{dassl} is not very good at\n\
determining a consistent set for you, so it is best if you ensure that\n\
the initial values result in the function evaluating to zero.\n\
\n\
The fifth argument is optional, and may be used to specify a set of\n\
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
You can use the function @code{dassl_options} to set optional\n\
parameters for @code{dassl}.\n\
@seealso{daspk, dasrt, lsode}\n\
@end deftypefn")
{
  octave_value_list retval;

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;

  unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    DASSL_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin > 3 && nargin < 6 && nargout < 5)
    {
      std::string fcn_name, fname, jac_name, jname;
      dassl_fcn = 0;
      dassl_jac = 0;

      octave_value f_arg = args(0);

      if (f_arg.is_cell ())
        {
          Cell c = f_arg.cell_value ();
          if (c.length () == 1)
            f_arg = c(0);
          else if (c.length () == 2)
            {
              if (c(0).is_function_handle () || c(0).is_inline_function ())
                dassl_fcn = c(0).function_value ();
              else
                {
                  fcn_name = unique_symbol_name ("__dassl_fcn__");
                  fname = "function y = ";
                  fname.append (fcn_name);
                  fname.append (" (x, xdot, t) y = ");
                  dassl_fcn = extract_function (c(0), "dassl", fcn_name, fname,
                                                "; endfunction");
                }

              if (dassl_fcn)
                {
                  if (c(1).is_function_handle () || c(1).is_inline_function ())
                    dassl_jac = c(1).function_value ();
                  else
                    {
                      jac_name = unique_symbol_name ("__dassl_jac__");
                      jname = "function jac = ";
                      jname.append (jac_name);
                      jname.append (" (x, xdot, t, cj) jac = ");
                      dassl_jac = extract_function (c(1), "dassl", jac_name,
                                                    jname, "; endfunction");

                      if (!dassl_jac)
                        {
                          if (fcn_name.length ())
                            clear_function (fcn_name);
                          dassl_fcn = 0;
                        }
                    }
                }
            }
          else
            DASSL_ABORT1 ("incorrect number of elements in cell array");
        }

      if (!dassl_fcn && ! f_arg.is_cell ())
        {
          if (f_arg.is_function_handle () || f_arg.is_inline_function ())
            dassl_fcn = f_arg.function_value ();
          else
            {
              switch (f_arg.rows ())
                {
                case 1:
                  do
                    {
                      fcn_name = unique_symbol_name ("__dassl_fcn__");
                      fname = "function y = ";
                      fname.append (fcn_name);
                      fname.append (" (x, xdot, t) y = ");
                      dassl_fcn = extract_function (f_arg, "dassl", fcn_name,
                                                    fname, "; endfunction");
                    }
                  while (0);
                  break;

                case 2:
                  {
                    string_vector tmp = f_arg.all_strings ();

                    if (! error_state)
                      {
                        fcn_name = unique_symbol_name ("__dassl_fcn__");
                        fname = "function y = ";
                        fname.append (fcn_name);
                        fname.append (" (x, xdot, t) y = ");
                        dassl_fcn = extract_function (tmp(0), "dassl", fcn_name,
                                                      fname, "; endfunction");

                        if (dassl_fcn)
                          {
                            jac_name = unique_symbol_name ("__dassl_jac__");
                            jname = "function jac = ";
                            jname.append (jac_name);
                            jname.append (" (x, xdot, t, cj) jac = ");
                            dassl_jac = extract_function (tmp(1), "dassl",
                                                          jac_name, jname,
                                                          "; endfunction");

                            if (!dassl_jac)
                              {
                                if (fcn_name.length ())
                                  clear_function (fcn_name);
                                dassl_fcn = 0;
                              }
                          }
                      }
                  }
                }
            }
        }

      if (error_state || ! dassl_fcn)
        DASSL_ABORT ();

      ColumnVector state = ColumnVector (args(1).vector_value ());

      if (error_state)
        DASSL_ABORT1 ("expecting state vector as second argument");

      ColumnVector deriv (args(2).vector_value ());

      if (error_state)
        DASSL_ABORT1 ("expecting derivative vector as third argument");

      ColumnVector out_times (args(3).vector_value ());

      if (error_state)
        DASSL_ABORT1 ("expecting output time vector as fourth argument");

      ColumnVector crit_times;
      int crit_times_set = 0;
      if (nargin > 4)
        {
          crit_times = ColumnVector (args(4).vector_value ());

          if (error_state)
            DASSL_ABORT1 ("expecting critical time vector as fifth argument");

          crit_times_set = 1;
        }

      if (state.capacity () != deriv.capacity ())
        DASSL_ABORT1 ("x and xdot must have the same size");

      double tzero = out_times (0);

      DAEFunc func (dassl_user_function);
      if (dassl_jac)
        func.set_jacobian_function (dassl_user_jacobian);

      DASSL dae (state, deriv, tzero, func);

      dae.set_options (dassl_opts);

      Matrix output;
      Matrix deriv_output;

      if (crit_times_set)
        output = dae.integrate (out_times, deriv_output, crit_times);
      else
        output = dae.integrate (out_times, deriv_output);

      if (fcn_name.length ())
        clear_function (fcn_name);
      if (jac_name.length ())
        clear_function (jac_name);

      if (! error_state)
        {
          std::string msg = dae.error_message ();

          retval(3) = msg;
          retval(2) = static_cast<double> (dae.integration_state ());

          if (dae.integration_ok ())
            {
              retval(1) = deriv_output;
              retval(0) = output;
            }
          else
            {
              retval(1) = Matrix ();
              retval(0) = Matrix ();

              if (nargout < 3)
                error ("dassl: %s", msg.c_str ());
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
## dassl-1.m
##
## Test dassl() function
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         20 May 1998
##
## Problem
##
##    y1' = -y2,   y1(0) = 1
##    y2' =  y1,   y2(0) = 0
##
## Solution
##
##    y1(t) = cos(t)
##    y2(t) = sin(t)
##
%!function res = __f (x, xdot, t)
%!  res = [xdot(1)+x(2); xdot(2)-x(1)];
%!endfunction

%!test
%!
%! x0 = [1; 0];
%! xdot0 = [0; 1];
%! t = (0:1:10)';
%!
%! tol = 100 * dassl_options ("relative tolerance");
%!
%! [x, xdot] = dassl ("__f", x0, xdot0, t);
%!
%! y = [cos(t), sin(t)];
%!
%! assert (x, y, tol);

## dassl-2.m
##
## Test dassl() function
##
## Author: David Billinghurst (David.Billinghurst@riotinto.com.au)
##         Comalco Research and Technology
##         20 May 1998
##
## Based on SLATEC quick check for DASSL by Linda Petzold
##
## Problem
##
##   x1' + 10*x1 = 0,   x1(0) = 1
##   x1  + x2    = 1,   x2(0) = 0
##
##
## Solution
##
##  x1(t) = exp(-10*t)
##  x2(t) = 1 - x(1)
##
%!function res = __f (x, xdot, t)
%!  res = [xdot(1)+10*x(1); x(1)+x(2)-1];
%!endfunction

%!test
%!
%! x0 = [1; 0];
%! xdot0 = [-10; 10];
%! t = (0:0.2:1)';
%!
%! tol = 500 * dassl_options ("relative tolerance");
%!
%! [x, xdot] = dassl ("__f", x0, xdot0, t);
%!
%! y = [exp(-10*t), 1-exp(-10*t)];
%!
%! assert (x, y, tol);

%!test
%! old_tol = dassl_options ("absolute tolerance");
%! dassl_options ("absolute tolerance", eps);
%! assert (dassl_options ("absolute tolerance") == eps);
%! ## Restore old value of tolerance
%! dassl_options ("absolute tolerance", old_tol);

%!error dassl_options ("foo", 1, 2)
*/
