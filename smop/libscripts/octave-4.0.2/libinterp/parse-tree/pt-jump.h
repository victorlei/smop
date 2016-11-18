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

#if !defined (octave_pt_jump_h)
#define octave_pt_jump_h 1

class tree_walker;

#include "pt-cmd.h"
#include "symtab.h"

// Break.

class
tree_break_command : public tree_command
{
public:

  tree_break_command (int l = -1, int c = -1)
    : tree_command (l, c) { }

  ~tree_break_command (void) { }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  static int breaking;

private:

  // No copying!

  tree_break_command (const tree_break_command&);

  tree_break_command& operator = (const tree_break_command&);
};

// Continue.

class
tree_continue_command : public tree_command
{
public:

  tree_continue_command (int l = -1, int c = -1)
    : tree_command (l, c) { }

  ~tree_continue_command (void) { }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  static int continuing;

private:

  // No copying!

  tree_continue_command (const tree_continue_command&);

  tree_continue_command& operator = (const tree_continue_command&);
};

// Return.

class
tree_return_command : public tree_command
{
public:

  tree_return_command (int l = -1, int c = -1)
    : tree_command (l, c) { }

  ~tree_return_command (void) { }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  static int returning;

private:

  // No copying!

  tree_return_command (const tree_return_command&);

  tree_return_command& operator = (const tree_return_command&);
};

#endif
