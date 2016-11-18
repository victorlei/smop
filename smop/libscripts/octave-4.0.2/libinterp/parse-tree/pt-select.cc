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
#include "ov.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-select.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "Cell.h"
#include "ov-typeinfo.h"

// If clauses.

tree_if_clause::~tree_if_clause (void)
{
  delete expr;
  delete list;
  delete lead_comm;
}

tree_if_clause *
tree_if_clause::dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const
{
  return new tree_if_clause (expr ? expr->dup (scope, context) : 0,
                             list ? list->dup (scope, context) : 0,
                             lead_comm ? lead_comm->dup () : 0);
}

void
tree_if_clause::accept (tree_walker& tw)
{
  tw.visit_if_clause (*this);
}

// List of if commands.

tree_if_command_list *
tree_if_command_list::dup (symbol_table::scope_id scope,
                           symbol_table::context_id context) const
{
  tree_if_command_list *new_icl = new tree_if_command_list ();

  for (const_iterator p = begin (); p != end (); p++)
    {
      const tree_if_clause *elt = *p;

      new_icl->append (elt ? elt->dup (scope, context) : 0);
    }

  return new_icl;
}

void
tree_if_command_list::accept (tree_walker& tw)
{
  tw.visit_if_command_list (*this);
}

// If.

tree_if_command::~tree_if_command (void)
{
  delete list;
  delete lead_comm;
  delete trail_comm;
}

tree_command *
tree_if_command::dup (symbol_table::scope_id scope,
                      symbol_table::context_id context) const
{
  return new tree_if_command (list ? list->dup (scope, context) : 0,
                              lead_comm ? lead_comm->dup () : 0,
                              trail_comm ? trail_comm->dup () : 0,
                              line (), column ());
}

void
tree_if_command::accept (tree_walker& tw)
{
  tw.visit_if_command (*this);
}

// Switch cases.

tree_switch_case::~tree_switch_case (void)
{
  delete label;
  delete list;
  delete lead_comm;
}


bool
tree_switch_case::label_matches (const octave_value& val)
{
  octave_value label_value = label->rvalue1 ();

  if (! error_state && label_value.is_defined ())
    {
      if (label_value.is_cell ())
        {
          Cell cell (label_value.cell_value ());

          for (octave_idx_type i = 0; i < cell.rows (); i++)
            {
              for (octave_idx_type j = 0; j < cell.columns (); j++)
                {
                  bool match = val.is_equal (cell(i,j));

                  if (error_state)
                    return false;
                  else if (match)
                    return true;
                }
            }
        }
      else
        {
          bool match = val.is_equal (label_value);

          if (error_state)
            return false;
          else
            return match;
        }
    }

  return false;
}

tree_switch_case *
tree_switch_case::dup (symbol_table::scope_id scope,
                       symbol_table::context_id context) const
{
  return new tree_switch_case (label ? label->dup (scope, context) : 0,
                               list ? list->dup (scope, context) : 0,
                               lead_comm ? lead_comm->dup () : 0);
}

void
tree_switch_case::accept (tree_walker& tw)
{
  tw.visit_switch_case (*this);
}

// List of switch cases.

tree_switch_case_list *
tree_switch_case_list::dup (symbol_table::scope_id scope,
                            symbol_table::context_id context) const
{
  tree_switch_case_list *new_scl = new tree_switch_case_list ();

  for (const_iterator p = begin (); p != end (); p++)
    {
      const tree_switch_case *elt = *p;

      new_scl->append (elt ? elt->dup (scope, context) : 0);
    }

  return new_scl;
}

void
tree_switch_case_list::accept (tree_walker& tw)
{
  tw.visit_switch_case_list (*this);
}

// Switch.

tree_switch_command::~tree_switch_command (void)
{
  delete expr;
  delete list;
  delete lead_comm;
  delete trail_comm;
}

tree_command *
tree_switch_command::dup (symbol_table::scope_id scope,
                          symbol_table::context_id context) const
{
  return new tree_switch_command (expr ? expr->dup (scope, context) : 0,
                                  list ? list->dup (scope, context) : 0,
                                  lead_comm ? lead_comm->dup () : 0,
                                  trail_comm ? trail_comm->dup () : 0,
                                  line (), column ());
}

void
tree_switch_command::accept (tree_walker& tw)
{
  tw.visit_switch_command (*this);
}
