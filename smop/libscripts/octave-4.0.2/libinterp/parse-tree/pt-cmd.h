/*

Copyright (C) 1994-2015 John W. Eaton

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

#if !defined (octave_pt_cmd_h)
#define octave_pt_cmd_h 1

#include <string>

class tree_walker;

#include "ov-fcn.h"
#include "pt.h"
#include "pt-bp.h"
#include "symtab.h"

// A base class for commands.

class
tree_command : public tree
{
public:

  tree_command (int l = -1, int c = -1)
    : tree (l, c) { }

  virtual ~tree_command (void) { }

  virtual tree_command *dup (symbol_table::scope_id,
                             symbol_table::context_id context) const = 0;

private:

  // No copying!

  tree_command (const tree_command&);

  tree_command& operator = (const tree_command&);
};

// No-op.

class
tree_no_op_command : public tree_command
{
public:

  tree_no_op_command (const std::string& cmd = "no_op", bool e = false,
                      int l = -1, int c = -1)
    : tree_command (l, c), eof (e), orig_cmd (cmd) { }

  ~tree_no_op_command (void) { }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  bool is_end_of_fcn_or_script (void) const
  {
    return (orig_cmd == "endfunction" || orig_cmd == "endscript");
  }

  bool is_end_of_file (void) const { return eof; }

  std::string original_command (void) { return orig_cmd; }

private:

  bool eof;

  std::string orig_cmd;

  // No copying!

  tree_no_op_command (const tree_no_op_command&);

  tree_no_op_command& operator = (const tree_no_op_command&);
};

// Function definition.

class
tree_function_def : public tree_command
{
public:

  tree_function_def (octave_function *f, int l = -1, int c = -1)
    : tree_command (l, c), fcn (f) { }

  ~tree_function_def (void) { }

  tree_command *dup (symbol_table::scope_id scope,
                     symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  octave_value function (void) { return fcn; }

private:

  octave_value fcn;

  tree_function_def (const octave_value& v, int l = -1, int c = -1)
    : tree_command (l, c), fcn (v) { }

  // No copying!

  tree_function_def (const tree_function_def&);

  tree_function_def& operator = (const tree_function_def&);
};

#endif
