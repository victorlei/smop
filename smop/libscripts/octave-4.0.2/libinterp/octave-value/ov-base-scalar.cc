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

#include <iostream>

#include "oct-obj.h"
#include "ov-base.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-base-scalar.h"
#include "pr-output.h"

template <class ST>
octave_value
octave_base_scalar<ST>::subsref (const std::string& type,
                                 const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

template <class ST>
octave_value
octave_base_scalar<ST>::subsasgn (const std::string& type,
                                  const std::list<octave_value_list>& idx,
                                  const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
        if (type.length () == 1)
          retval = numeric_assign (type, idx, rhs);
        else
          {
            std::string nm = type_name ();
            error ("in indexed assignment of %s, last rhs index must be ()",
                   nm.c_str ());
          }
      }
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <class ST>
dim_vector
octave_base_scalar<ST>::dims (void) const
{
  static dim_vector dv (1, 1);
  return dv;
}

template <class ST>
octave_value
octave_base_scalar<ST>::permute (const Array<int>& vec, bool inv) const
{
  return Array<ST> (dim_vector (1, 1), scalar).permute (vec, inv);
}

template <class ST>
octave_value
octave_base_scalar<ST>::reshape (const dim_vector& new_dims) const
{
  return Array<ST> (dim_vector (1, 1), scalar).reshape (new_dims);
}

template <class ST>
octave_value
octave_base_scalar<ST>::diag (octave_idx_type k) const
{
  return Array<ST> (dim_vector (1, 1), scalar).diag (k);
}

template <class ST>
octave_value
octave_base_scalar<ST>::diag (octave_idx_type m, octave_idx_type n) const
{
  return Array<ST> (dim_vector (1, 1), scalar).diag (m, n);
}

template <class ST>
bool
octave_base_scalar<ST>::is_true (void) const
{
  bool retval = false;

  if (xisnan (scalar))
    gripe_nan_to_logical_conversion ();
  else
    retval = (scalar != ST ());

  return retval;
}

template <class ST>
void
octave_base_scalar<ST>::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class ST>
void
octave_base_scalar<ST>::print_raw (std::ostream& os,
                                   bool pr_as_read_syntax) const
{
  indent (os);
  octave_print_internal (os, scalar, pr_as_read_syntax);
}

template <class ST>
bool
octave_base_scalar<ST>::print_name_tag (std::ostream& os,
                                        const std::string& name) const
{
  indent (os);
  os << name << " = ";
  return false;
}

template <class ST>
void
octave_base_scalar<ST>::short_disp (std::ostream& os) const
{
  std::ostringstream buf;
  octave_print_internal (buf, scalar);
  std::string tmp = buf.str ();
  size_t pos = tmp.find_first_not_of (" ");
  if (pos != std::string::npos)
    os << tmp.substr (pos);
  else if (! tmp.empty ())
    os << tmp[0];
}

template <class ST>
octave_value
octave_base_scalar<ST>::fast_elem_extract (octave_idx_type n) const
{
  return (n == 0) ? octave_value (scalar) : octave_value ();
}

template <class ST>
bool
octave_base_scalar<ST>::fast_elem_insert_self (void *where,
                                               builtin_type_t btyp) const
{

  // Don't use builtin_type () here to avoid an extra VM call.
  if (btyp == class_to_btyp<ST>::btyp)
    {
      *(reinterpret_cast<ST *>(where)) = scalar;
      return true;
    }
  else
    return false;
}
