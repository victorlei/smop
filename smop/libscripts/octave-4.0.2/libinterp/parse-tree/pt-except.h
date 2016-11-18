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

#if !defined (octave_pt_except_h)
#define octave_pt_except_h 1

class tree_statement_list;

class tree_walker;

#include "comment-list.h"
#include "pt-cmd.h"
#include "pt-id.h"
#include "symtab.h"

// Simple exception handling.

class
tree_try_catch_command : public tree_command
{
public:

  tree_try_catch_command (int l = -1, int c = -1)
    : tree_command (l, c), try_code (0), catch_code (0), expr_id (0),
      lead_comm (0), mid_comm (0), trail_comm (0) { }

  tree_try_catch_command (tree_statement_list *tc, tree_statement_list *cc,
                          tree_identifier *id,
                          octave_comment_list *cl = 0,
                          octave_comment_list *cm = 0,
                          octave_comment_list *ct = 0,
                          int l = -1, int c = -1)
    : tree_command (l, c), try_code (tc), catch_code (cc), expr_id (id),
      lead_comm (cl), mid_comm (cm), trail_comm (ct) { }

  ~tree_try_catch_command (void);

  tree_identifier *identifier (void) { return expr_id; }

  tree_statement_list *body (void) { return try_code; }

  tree_statement_list *cleanup (void) { return catch_code; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *middle_comment (void) { return mid_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // The first block of code to attempt to execute.
  tree_statement_list *try_code;

  // The code to execute if an error occurs in the first block.
  tree_statement_list *catch_code;

  // Identifier to modify.
  tree_identifier *expr_id;

  // Comment preceding TRY token.
  octave_comment_list *lead_comm;

  // Comment preceding CATCH token.
  octave_comment_list *mid_comm;

  // Comment preceding END_TRY_CATCH token.
  octave_comment_list *trail_comm;

  // No copying!

  tree_try_catch_command (const tree_try_catch_command&);

  tree_try_catch_command& operator = (const tree_try_catch_command&);
};

// Simple exception handling.

class
tree_unwind_protect_command : public tree_command
{
public:

  tree_unwind_protect_command (int l = -1, int c = -1)
    : tree_command (l, c), unwind_protect_code (0), cleanup_code (0),
      lead_comm (0), mid_comm (0), trail_comm (0) { }

  tree_unwind_protect_command (tree_statement_list *tc,
                               tree_statement_list *cc,
                               octave_comment_list *cl = 0,
                               octave_comment_list *cm = 0,
                               octave_comment_list *ct = 0,
                               int l = -1, int c = -1)
    : tree_command (l, c), unwind_protect_code (tc), cleanup_code (cc),
      lead_comm (cl), mid_comm (cm), trail_comm (ct) { }

  ~tree_unwind_protect_command (void);

  tree_statement_list *body (void) { return unwind_protect_code; }

  tree_statement_list *cleanup (void) { return cleanup_code; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *middle_comment (void) { return mid_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // The first body of code to attempt to execute.
  tree_statement_list *unwind_protect_code;

  // The body of code to execute no matter what happens in the first
  // body of code.
  tree_statement_list *cleanup_code;

  // Comment preceding UNWIND_PROTECT token.
  octave_comment_list *lead_comm;

  // Comment preceding UNWIND_PROTECT_CLEANUP token.
  octave_comment_list *mid_comm;

  // Comment preceding END_UNWIND_PROTECT token.
  octave_comment_list *trail_comm;

  // No copying!

  tree_unwind_protect_command (const tree_unwind_protect_command&);

  tree_unwind_protect_command& operator = (const tree_unwind_protect_command&);
};

#endif
