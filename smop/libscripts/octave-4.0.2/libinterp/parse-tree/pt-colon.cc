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
#include "pager.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-colon.h"
#include "pt-walk.h"

// Colon expressions.

tree_colon_expression *
tree_colon_expression::append (tree_expression *t)
{
  tree_colon_expression *retval = 0;

  if (op_base)
    {
      if (op_limit)
        {
          if (op_increment)
            ::error ("invalid colon expression");
          else
            {
              // Stupid syntax:
              //
              // base : limit
              // base : increment : limit

              op_increment = op_limit;
              op_limit = t;
            }
        }
      else
        op_limit = t;

      retval = this;
    }
  else
    ::error ("invalid colon expression");

  return retval;
}

octave_value_list
tree_colon_expression::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for colon expression");
  else
    retval = rvalue1 (nargout);

  return retval;
}

octave_value
tree_colon_expression::rvalue1 (int)
{
  octave_value retval;

  if (error_state || ! op_base || ! op_limit)
    return retval;

  octave_value ov_base = op_base->rvalue1 ();

  if (error_state || ov_base.is_undefined ())
    eval_error ("invalid base value in colon expression");
  else
    {
      octave_value ov_limit = op_limit->rvalue1 ();

      if (error_state || ov_limit.is_undefined ())
        eval_error ("invalid limit value in colon expression");
      else if (ov_base.is_object () || ov_limit.is_object ())
        {
          octave_value_list tmp1;

          if (op_increment)
            {
              octave_value ov_increment = op_increment->rvalue1 ();

              if (error_state || ov_increment.is_undefined ())
                eval_error ("invalid increment value in colon expression");
              else
                {
                  tmp1(2) = ov_limit;
                  tmp1(1) = ov_increment;
                  tmp1(0) = ov_base;
                }
            }
          else
            {
              tmp1(1) = ov_limit;
              tmp1(0) = ov_base;
            }

          if (!error_state)
            {
              octave_value fcn = symbol_table::find_function ("colon", tmp1);

              if (fcn.is_defined ())
                {
                  octave_value_list tmp2 = fcn.do_multi_index_op (1, tmp1);

                  if (! error_state)
                    retval = tmp2 (0);
                }
              else
                ::error ("can not find overloaded colon function");
            }
        }
      else
        {
          octave_value ov_increment = 1.0;

          if (op_increment)
            {
              ov_increment = op_increment->rvalue1 ();

              if (error_state || ov_increment.is_undefined ())
                eval_error ("invalid increment value in colon expression");
            }

          if (! error_state)
            retval = do_colon_op (ov_base, ov_increment, ov_limit,
                                  is_for_cmd_expr ());
        }
    }

  return retval;
}

void
tree_colon_expression::eval_error (const std::string& s) const
{
  ::error ("%s", s.c_str ());
}

int
tree_colon_expression::line (void) const
{
  return (op_base ? op_base->line ()
          : (op_increment ? op_increment->line ()
             : (op_limit ? op_limit->line ()
                : -1)));
}

int
tree_colon_expression::column (void) const
{
  return (op_base ? op_base->column ()
          : (op_increment ? op_increment->column ()
             : (op_limit ? op_limit->column ()
                : -1)));
}

tree_expression *
tree_colon_expression::dup (symbol_table::scope_id scope,
                            symbol_table::context_id context) const
{
  tree_colon_expression *new_ce = new
    tree_colon_expression (op_base ? op_base->dup (scope, context) : 0,
                           op_limit ? op_limit->dup (scope, context) : 0,
                           op_increment ? op_increment->dup (scope, context)
                                        : 0,
                           line (), column ());

  new_ce->copy_base (*new_ce);

  return new_ce;
}

void
tree_colon_expression::accept (tree_walker& tw)
{
  tw.visit_colon_expression (*this);
}
