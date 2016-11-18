/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#if !defined (octave_oct_obj_h)
#define octave_oct_obj_h 1

#include <string>
#include <vector>

#include "str-vec.h"
#include "Array.h"

#include "ov.h"
#include "Cell.h"

class
OCTINTERP_API
octave_value_list
{
public:

  octave_value_list (void)
    : data (), names () { }

  explicit octave_value_list (octave_idx_type n)
    : data (dim_vector (1, n)), names () { }

  octave_value_list (octave_idx_type n, const octave_value& val)
    : data (dim_vector (1, n), val), names () { }

  octave_value_list (const octave_value& tc)
    : data (dim_vector (1, 1), tc), names () { }

  octave_value_list (const Array<octave_value>& d)
    : data (d.as_row ()), names () { }

  octave_value_list (const Cell& tc)
    : data (tc.as_row ()), names () { }

  octave_value_list (const octave_value_list& obj)
    : data (obj.data), names (obj.names) { }

  // Concatenation constructor.
  octave_value_list (const std::list<octave_value_list>&);

  ~octave_value_list (void) { }

  octave_value_list& operator = (const octave_value_list& obj)
  {
    if (this != &obj)
      {
        data = obj.data;
        names = obj.names;
      }

    return *this;
  }

  Array<octave_value> array_value (void) const { return data; }

  Cell cell_value (void) const { return array_value (); }

  // Assignment will resize on range errors.

  octave_value& operator () (octave_idx_type n) { return elem (n); }

  const octave_value& operator () (octave_idx_type n) const { return elem (n); }

  octave_idx_type length (void) const { return data.length (); }

  bool empty (void) const { return length () == 0; }

  void resize (octave_idx_type n, const octave_value& rfv = octave_value ())
  {
    data.resize (dim_vector (1, n), rfv);
  }

  octave_value_list& prepend (const octave_value& val);

  octave_value_list& append (const octave_value& val);

  octave_value_list& append (const octave_value_list& lst);

  octave_value_list& reverse (void);

  octave_value_list
  slice (octave_idx_type offset, octave_idx_type len, bool tags = false) const
  {
    // linear_slice uses begin/end indices instead of offset and
    // length.  Avoid calling with upper bound out of range.
    // linear_slice handles the case of len < 0.

    octave_value_list retval
      = data.linear_slice (offset, std::min (offset + len, length ()));

    if (tags && len > 0 && names.length () > 0)
      retval.names = names.linear_slice (offset, std::min (offset + len,
                                                           names.length ()));

    return retval;
  }

  octave_value_list
  splice (octave_idx_type offset, octave_idx_type len,
          const octave_value_list& lst = octave_value_list ()) const;

  bool all_strings_p (void) const;

  bool all_scalars (void) const;

  bool any_cell (void) const;

  bool has_magic_colon (void) const;

  string_vector make_argv (const std::string& = std::string ()) const;

  void stash_name_tags (const string_vector& nm) { names = nm; }

  string_vector name_tags (void) const { return names; }

  void make_storable_values (void);

  octave_value& xelem (octave_idx_type i)
  {
    return data.xelem (i);
  }

  void clear (void)
  {
    data.clear ();
  }

private:

  Array<octave_value> data;

  // This list of strings can be used to tag each element of data with
  // a name.  By default, it is empty.
  string_vector names;

  octave_value& elem (octave_idx_type n)
  {
    if (n >= length ())
      resize (n + 1);

    return data(n);
  }

  const octave_value& elem (octave_idx_type n) const
  { return data(n); }

};

// Make it easy to build argument lists for built-in functions or for
// returning values.

inline octave_value_list
ovl (const octave_value& a0)
{
  octave_value_list retval;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1)
{
  octave_value_list retval;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2)
{
  octave_value_list retval;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3)
{
  octave_value_list retval;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3,
     const octave_value& a4)
{
  octave_value_list retval;
  retval(4) = a4;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3,
     const octave_value& a4, const octave_value& a5)
{
  octave_value_list retval;
  retval(5) = a5;
  retval(4) = a4;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3,
     const octave_value& a4, const octave_value& a5,
     const octave_value& a6)
{
  octave_value_list retval;
  retval(6) = a6;
  retval(5) = a5;
  retval(4) = a4;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3,
     const octave_value& a4, const octave_value& a5,
     const octave_value& a6, const octave_value& a7)
{
  octave_value_list retval;
  retval(7) = a7;
  retval(6) = a6;
  retval(5) = a5;
  retval(4) = a4;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3,
     const octave_value& a4, const octave_value& a5,
     const octave_value& a6, const octave_value& a7,
     const octave_value& a8)
{
  octave_value_list retval;
  retval(8) = a8;
  retval(7) = a7;
  retval(6) = a6;
  retval(5) = a5;
  retval(4) = a4;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

inline octave_value_list
ovl (const octave_value& a0, const octave_value& a1,
     const octave_value& a2, const octave_value& a3,
     const octave_value& a4, const octave_value& a5,
     const octave_value& a6, const octave_value& a7,
     const octave_value& a8, const octave_value& a9)
{
  octave_value_list retval;
  retval(9) = a9;
  retval(8) = a8;
  retval(7) = a7;
  retval(6) = a6;
  retval(5) = a5;
  retval(4) = a4;
  retval(3) = a3;
  retval(2) = a2;
  retval(1) = a1;
  retval(0) = a0;
  return retval;
}

#endif
