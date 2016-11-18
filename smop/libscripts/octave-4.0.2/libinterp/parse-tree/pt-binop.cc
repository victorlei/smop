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

#include "error.h"
#include "defun.h"
#include "oct-obj.h"
#include "ov.h"
#include "profiler.h"
#include "pt-binop.h"
#include "pt-bp.h"
#include "pt-walk.h"
#include "variables.h"

// TRUE means we mark | and & expressions for braindead short-circuit
// behavior.
static bool Vdo_braindead_shortcircuit_evaluation = true;

// Binary expressions.

octave_value_list
tree_binary_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("binary operator '%s': invalid number of output arguments",
           oper () . c_str ());
  else
    retval = rvalue1 (nargout);

  return retval;
}

void
tree_binary_expression::matlab_style_short_circuit_warning (const char *op)
{
  warning_with_id ("Octave:possible-matlab-short-circuit-operator",
                   "Matlab-style short-circuit operation performed for operator %s",
                   op);

  braindead_shortcircuit_warning_issued = true;
}

octave_value
tree_binary_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (Vdo_braindead_shortcircuit_evaluation
      && eligible_for_braindead_shortcircuit)
    {
      if (op_lhs)
        {
          octave_value a = op_lhs->rvalue1 ();

          if (! error_state)
            {
              if (a.ndims () == 2 && a.rows () == 1 && a.columns () == 1)
                {
                  bool result = false;

                  bool a_true = a.is_true ();

                  if (! error_state)
                    {
                      if (a_true)
                        {
                          if (etype == octave_value::op_el_or)
                            {
                              matlab_style_short_circuit_warning ("|");
                              result = true;
                              goto done;
                            }
                        }
                      else
                        {
                          if (etype == octave_value::op_el_and)
                            {
                              matlab_style_short_circuit_warning ("&");
                              goto done;
                            }
                        }

                      if (op_rhs)
                        {
                          octave_value b = op_rhs->rvalue1 ();

                          if (! error_state)
                            result = b.is_true ();
                        }

                    done:

                      if (! error_state)
                        return octave_value (result);
                    }
                }
            }
        }
    }

  if (op_lhs)
    {
      octave_value a = op_lhs->rvalue1 ();

      if (! error_state && a.is_defined () && op_rhs)
        {
          octave_value b = op_rhs->rvalue1 ();

          if (! error_state && b.is_defined ())
            {
              BEGIN_PROFILER_BLOCK (tree_binary_expression)

              // Note: The profiler does not catch the braindead
              // short-circuit evaluation code above, but that should be
              // ok. The evaluation of operands and the operator itself
              // is entangled and it's not clear where to start/stop
              // timing the operator to make it reasonable.

              retval = ::do_binary_op (etype, a, b);

              if (error_state)
                retval = octave_value ();

              END_PROFILER_BLOCK
            }
        }
    }

  return retval;
}

std::string
tree_binary_expression::oper (void) const
{
  return octave_value::binary_op_as_string (etype);
}

tree_expression *
tree_binary_expression::dup (symbol_table::scope_id scope,
                             symbol_table::context_id context) const
{
  tree_binary_expression *new_be
    = new tree_binary_expression (op_lhs ? op_lhs->dup (scope, context) : 0,
                                  op_rhs ? op_rhs->dup (scope, context) : 0,
                                  line (), column (), etype);

  new_be->copy_base (*this);

  return new_be;
}

void
tree_binary_expression::accept (tree_walker& tw)
{
  tw.visit_binary_expression (*this);
}

// Boolean expressions.

octave_value_list
tree_boolean_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("binary operator '%s': invalid number of output arguments",
           oper () . c_str ());
  else
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_boolean_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state)
    return retval;

  bool result = false;

  // This evaluation is not caught by the profiler, since we can't find
  // a reasonable place where to time. Note that we don't want to
  // include evaluation of LHS or RHS into the timing, but this is
  // entangled together with short-circuit evaluation here.

  if (op_lhs)
    {
      octave_value a = op_lhs->rvalue1 ();

      if (! error_state)
        {
          bool a_true = a.is_true ();

          if (! error_state)
            {
              if (a_true)
                {
                  if (etype == bool_or)
                    {
                      result = true;
                      goto done;
                    }
                }
              else
                {
                  if (etype == bool_and)
                    goto done;
                }

              if (op_rhs)
                {
                  octave_value b = op_rhs->rvalue1 ();

                  if (! error_state)
                    result = b.is_true ();
                }

            done:

              if (! error_state)
                retval = octave_value (result);
            }
        }
    }

  return retval;
}

std::string
tree_boolean_expression::oper (void) const
{
  std::string retval = "<unknown>";

  switch (etype)
    {
    case bool_and:
      retval = "&&";
      break;

    case bool_or:
      retval = "||";
      break;

    default:
      break;
    }

  return retval;
}

tree_expression *
tree_boolean_expression::dup (symbol_table::scope_id scope,
                              symbol_table::context_id context) const
{
  tree_boolean_expression *new_be
    = new tree_boolean_expression (op_lhs ? op_lhs->dup (scope, context) : 0,
                                   op_rhs ? op_rhs->dup (scope, context) : 0,
                                   line (), column (), etype);

  new_be->copy_base (*this);

  return new_be;
}

DEFUN (do_braindead_shortcircuit_evaluation, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} do_braindead_shortcircuit_evaluation ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} do_braindead_shortcircuit_evaluation (@var{new_val})\n\
@deftypefnx {Built-in Function} {} do_braindead_shortcircuit_evaluation (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave will\n\
do short-circuit evaluation of @samp{|} and @samp{&} operators inside the\n\
conditions of if or while statements.\n\
\n\
This feature is only provided for compatibility with @sc{matlab} and should\n\
not be used unless you are porting old code that relies on this feature.\n\
\n\
To obtain short-circuit behavior for logical expressions in new programs,\n\
you should always use the @samp{&&} and @samp{||} operators.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  static bool warned = false;
  if (! warned)
    {
      warned = true;
      warning_with_id ("Octave:deprecated-function",
                       "do_braindead_shortcircuit_evaluation is obsolete and will be removed from a future version of Octave");
    }

  return SET_INTERNAL_VARIABLE (do_braindead_shortcircuit_evaluation);
}

/*
%!test
%! x = 0;
%! do_braindead_shortcircuit_evaluation (0);
%! if (1 | (x = 1))
%! endif
%! assert (x, 1);
%! do_braindead_shortcircuit_evaluation (1);
%! if (1 | (x = 0))
%! endif
%! assert (x, 1);
*/
