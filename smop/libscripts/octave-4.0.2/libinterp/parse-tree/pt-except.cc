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

#include "quit.h"

#include "error.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-bp.h"
#include "pt-cmd.h"
#include "pt-except.h"
#include "pt-exp.h"
#include "pt-id.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "variables.h"

// Simple exception handling.

tree_try_catch_command::~tree_try_catch_command (void)
{
  delete expr_id;
  delete try_code;
  delete catch_code;
  delete lead_comm;
  delete mid_comm;
  delete trail_comm;
}

tree_command *
tree_try_catch_command::dup (symbol_table::scope_id scope,
                             symbol_table::context_id context) const
{
  return new
    tree_try_catch_command (try_code ? try_code->dup (scope, context) : 0,
                            catch_code ? catch_code->dup (scope, context) : 0,
                            expr_id ? expr_id->dup (scope, context) : 0,
                            lead_comm ? lead_comm->dup () : 0,
                            mid_comm ? mid_comm->dup () : 0,
                            trail_comm ? trail_comm->dup () : 0,
                            line (), column ());
}

void
tree_try_catch_command::accept (tree_walker& tw)
{
  tw.visit_try_catch_command (*this);
}

// Simple exception handling.

tree_unwind_protect_command::~tree_unwind_protect_command (void)
{
  delete unwind_protect_code;
  delete cleanup_code;
  delete lead_comm;
  delete mid_comm;
  delete trail_comm;
}

tree_command *
tree_unwind_protect_command::dup (symbol_table::scope_id scope,
                                  symbol_table::context_id context) const
{
  return new tree_unwind_protect_command
    (unwind_protect_code ? unwind_protect_code->dup (scope, context) : 0,
     cleanup_code ? cleanup_code->dup (scope, context) : 0,
     lead_comm ? lead_comm->dup () : 0,
     mid_comm ? mid_comm->dup () : 0,
     trail_comm ? trail_comm->dup () : 0,
     line (), column ());
}

void
tree_unwind_protect_command::accept (tree_walker& tw)
{
  tw.visit_unwind_protect_command (*this);
}
