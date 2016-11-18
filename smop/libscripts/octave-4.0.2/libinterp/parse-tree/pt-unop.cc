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
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "profiler.h"
#include "pt-bp.h"
#include "pt-unop.h"
#include "pt-walk.h"

// Unary expressions.

std::string
tree_unary_expression::oper (void) const
{
  return octave_value::unary_op_as_string (etype);
}

// Prefix expressions.

octave_value_list
tree_prefix_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("prefix operator '%s': invalid number of output arguments",
           oper () . c_str ());
  else
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_prefix_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == octave_value::op_incr || etype == octave_value::op_decr)
        {
          octave_lvalue ref = op->lvalue ();

          if (! error_state)
            {
              BEGIN_PROFILER_BLOCK (tree_prefix_expression)

              ref.do_unary_op (etype);

              if (! error_state)
                retval = ref.value ();

              END_PROFILER_BLOCK
            }
        }
      else
        {
          octave_value val = op->rvalue1 ();

          if (! error_state && val.is_defined ())
            {
              BEGIN_PROFILER_BLOCK (tree_prefix_expression)

              // Attempt to do the operation in-place if it is unshared
              // (a temporary expression).
              if (val.get_count () == 1)
                retval = val.do_non_const_unary_op (etype);
              else
                retval = ::do_unary_op (etype, val);

              if (error_state)
                retval = octave_value ();

              END_PROFILER_BLOCK
            }
        }
    }

  return retval;
}

tree_expression *
tree_prefix_expression::dup (symbol_table::scope_id scope,
                             symbol_table::context_id context) const
{
  tree_prefix_expression *new_pe
    = new tree_prefix_expression (op ? op->dup (scope, context) : 0,
                                  line (), column (), etype);

  new_pe->copy_base (*this);

  return new_pe;
}

void
tree_prefix_expression::accept (tree_walker& tw)
{
  tw.visit_prefix_expression (*this);
}

// Postfix expressions.

octave_value_list
tree_postfix_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("postfix operator '%s': invalid number of output arguments",
           oper () . c_str ());
  else
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_postfix_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state)
    return retval;

  if (op)
    {
      if (etype == octave_value::op_incr || etype == octave_value::op_decr)
        {
          octave_lvalue ref = op->lvalue ();

          if (! error_state)
            {
              retval = ref.value ();

              BEGIN_PROFILER_BLOCK (tree_postfix_expression)

              ref.do_unary_op (etype);

              END_PROFILER_BLOCK
            }
        }
      else
        {
          octave_value val = op->rvalue1 ();

          if (! error_state && val.is_defined ())
            {
              BEGIN_PROFILER_BLOCK (tree_postfix_expression)

              retval = ::do_unary_op (etype, val);

              if (error_state)
                retval = octave_value ();

              END_PROFILER_BLOCK
            }
        }
    }

  return retval;
}

tree_expression *
tree_postfix_expression::dup (symbol_table::scope_id scope,
                              symbol_table::context_id context) const
{
  tree_postfix_expression *new_pe
    = new tree_postfix_expression (op ? op->dup (scope, context) : 0,
                                   line (), column (), etype);

  new_pe->copy_base (*this);

  return new_pe;
}

void
tree_postfix_expression::accept (tree_walker& tw)
{
  tw.visit_postfix_expression (*this);
}
