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

#if !defined (octave_oct_lvalue_h)
#define octave_oct_lvalue_h 1

class octave_value;
class octave_value_list;

#include <string>

#include "oct-obj.h"
#include "pt-idx.h"
#include "symtab.h"

class
octave_lvalue
{
public:

  octave_lvalue (const symbol_table::symbol_reference& s
                   = symbol_table::symbol_reference ())
    : sym (s), type (), idx (), nel (1)
  { }

  octave_lvalue (const octave_lvalue& vr)
    : sym (vr.sym), type (vr.type), idx (vr.idx), nel (vr.nel)
  { }

  octave_lvalue& operator = (const octave_lvalue& vr)
  {
    if (this != &vr)
      {
        sym = vr.sym;
        type = vr.type;
        idx = vr.idx;
        nel = vr.nel;
      }

    return *this;
  }

  ~octave_lvalue (void) { }

  bool is_black_hole (void) const { return sym.is_black_hole (); }

  bool is_defined (void) const
  {
    return ! is_black_hole () && sym->is_defined ();
  }

  bool is_undefined (void) const
  {
    return is_black_hole () || sym->is_undefined ();
  }

  bool is_map (void) const
  {
    return value().is_map ();
  }

  void define (const octave_value& v) { sym->assign (v); }

  void assign (octave_value::assign_op, const octave_value&);

  void numel (octave_idx_type n) { nel = n; }

  octave_idx_type numel (void) const { return nel; }

  void set_index (const std::string& t, const std::list<octave_value_list>& i);

  void clear_index (void) { type = std::string (); idx.clear (); }

  void do_unary_op (octave_value::unary_op op);

  octave_value value (void) const;

private:

  symbol_table::symbol_reference sym;

  std::string type;

  std::list<octave_value_list> idx;

  octave_idx_type nel;
};

#endif
