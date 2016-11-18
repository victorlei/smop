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

#if !defined (octave_pt_id_h)
#define octave_pt_id_h 1

#include <iosfwd>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;

class tree_walker;

#include "oct-lvalue.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "symtab.h"

// Symbols from the symbol table.

class
tree_identifier : public tree_expression
{
  friend class tree_index_expression;

public:

  tree_identifier (int l = -1, int c = -1)
    : tree_expression (l, c) { }

  tree_identifier (const symbol_table::symbol_record& s,
                   int l = -1, int c = -1,
                   symbol_table::scope_id sc = symbol_table::current_scope ())
    : tree_expression (l, c), sym (s, sc) { }

  ~tree_identifier (void) { }

  bool has_magic_end (void) const { return (name () == "end"); }

  bool is_identifier (void) const { return true; }

  // The name doesn't change with scope, so use sym instead of
  // accessing it through sym so that this function may remain const.
  std::string name (void) const { return sym.name (); }

  bool is_defined (void) { return sym->is_defined (); }

  virtual bool is_variable (void) { return sym->is_variable (); }

  virtual bool is_black_hole (void) { return false; }

  // Try to find a definition for an identifier.  Here's how:
  //
  //   * If the identifier is already defined and is a function defined
  //     in an function file that has been modified since the last time
  //     we parsed it, parse it again.
  //
  //   * If the identifier is not defined, try to find a builtin
  //     variable or an already compiled function with the same name.
  //
  //   * If the identifier is still undefined, try looking for an
  //     function file to parse.
  //
  //   * On systems that support dynamic linking, we prefer .oct files,
  //     then .mex files, then .m files.

  octave_value
  do_lookup (const octave_value_list& args = octave_value_list ())
  {
    return sym->find (args);
  }

  void mark_global (void) { sym->mark_global (); }

  void mark_as_static (void) { sym->init_persistent (); }

  void mark_as_formal_parameter (void) { sym->mark_formal (); }

  // We really need to know whether this symbol referst to a variable
  // or a function, but we may not know that yet.

  bool lvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout)
  {
    return rvalue (nargout, 0);
  }

  octave_value_list rvalue (int nargout,
                            const std::list<octave_lvalue> *lvalue_list);

  octave_lvalue lvalue (void);

  void eval_undefined_error (void);

  void static_workspace_error (void)
  {
    ::error ("can not add variable \"%s\" to a static workspace",
             name ().c_str ());
  }

  tree_identifier *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  symbol_table::symbol_reference symbol (void) const
  {
    return sym;
  }
private:

  // The symbol record that this identifier references.
  symbol_table::symbol_reference sym;

  // No copying!

  tree_identifier (const tree_identifier&);

  tree_identifier& operator = (const tree_identifier&);
};

class tree_black_hole : public tree_identifier
{
public:

  tree_black_hole (int l = -1, int c = -1)
    : tree_identifier (l, c) { }

  std::string name (void) const { return "~"; }

  bool is_variable (void) { return false; }

  bool is_black_hole (void) { return true; }

  tree_black_hole *dup (void) const
  { return new tree_black_hole; }

  octave_lvalue lvalue (void)
  {
    return octave_lvalue (); // black hole lvalue
  }
};

#endif
