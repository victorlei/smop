/*

Copyright (C) 2002-2015 John W. Eaton

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

#if !defined (octave_ov_cs_list_h)
#define octave_ov_cs_list_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "Cell.h"
#include "error.h"
#include "oct-obj.h"
#include "ov-typeinfo.h"

class tree_walker;

// Lists.

class
octave_cs_list : public octave_base_value
{
public:

  octave_cs_list (void)
    : octave_base_value (), lst () { }

  octave_cs_list (const octave_value_list& l)
    : octave_base_value (), lst (l) { }

  octave_cs_list (const Cell& c);

  octave_cs_list (const octave_cs_list& l)
    : octave_base_value (), lst (l.lst) { }

  ~octave_cs_list (void) { }

  octave_base_value *clone (void) const { return new octave_cs_list (*this); }
  octave_base_value *empty_clone (void) const { return new octave_cs_list (); }

  dim_vector dims (void) const { return dim_vector (1, lst.length ()); }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_cs_list (void) const { return true; }

  octave_value_list list_value (void) const { return lst; }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx, int);

private:

  // The list of Octave values.
  octave_value_list lst;


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
