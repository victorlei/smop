/*

Copyright (C) 1999-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#include "idx-vector.h"

#include "Cell.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"

Cell::Cell (const octave_value_list& ovl)
  : Array<octave_value> (ovl.cell_value ())
{
}

Cell::Cell (const string_vector& sv, bool trim)
  : Array<octave_value> ()
{
  octave_idx_type n = sv.length ();

  if (n > 0)
    {
      resize (dim_vector (n, 1));

      for (octave_idx_type i = 0; i < n; i++)
        {
          std::string s = sv[i];

          if (trim)
            {
              size_t pos = s.find_last_not_of (' ');

              s = (pos == std::string::npos) ? "" : s.substr (0, pos+1);
            }

          elem(i,0) = s;
        }
    }
}

Cell::Cell (const std::list<std::string>& lst)
  : Array<octave_value> ()
{
  size_t n = lst.size ();

  if (n > 0)
    {
      resize (dim_vector (n, 1));

      octave_idx_type i = 0;

      for (std::list<std::string>::const_iterator it = lst.begin ();
           it != lst.end (); it++)
        {
          elem(i++,0) = *it;
        }
    }
}

Cell::Cell (const Array<std::string>& sa)
  : Array<octave_value> (sa.dims ())
{
  octave_idx_type n = sa.numel ();

  octave_value *dst = fortran_vec ();
  const std::string *src = sa.data ();

  for (octave_idx_type i = 0; i < n; i++)
    dst[i] = src[i];
}

// Set size to DV, filling with [].  Then fill with as many elements of
// SV as possible.

Cell::Cell (const dim_vector& dv, const string_vector& sv, bool trim)
  : Array<octave_value> (dv, Matrix ())
{
  octave_idx_type n = sv.length ();

  if (n > 0)
    {
      octave_idx_type m = numel ();

      octave_idx_type len = n > m ? m : n;

      for (octave_idx_type i = 0; i < len; i++)
        {
          std::string s = sv[i];

          if (trim)
            {
              size_t pos = s.find_last_not_of (' ');

              s = (pos == std::string::npos) ? "" : s.substr (0, pos+1);
            }

          elem(i) = s;
        }
    }
}

bool
Cell::is_cellstr (void) const
{
  bool retval = true;

  octave_idx_type n = numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (! elem(i).is_string ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

Array<std::string>
Cell::cellstr_value (void) const
{
  Array<std::string> retval (dims ());

  octave_idx_type n = numel ();

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = elem (i).string_value ();

  return retval;
}

Cell
Cell::index (const octave_value_list& idx_arg, bool resize_ok) const
{
  Cell retval;

  octave_idx_type n = idx_arg.length ();

  switch (n)
    {
    case 0:
      retval = *this;
      break;

    case 1:
      {
        idx_vector i = idx_arg(0).index_vector ();

        if (! error_state)
          retval = Array<octave_value>::index (i, resize_ok, Matrix ());
      }
      break;

    case 2:
      {
        idx_vector i = idx_arg(0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx_arg(1).index_vector ();

            if (! error_state)
              retval = Array<octave_value>::index (i, j, resize_ok, Matrix ());
          }
      }
      break;

    default:
      {
        Array<idx_vector> iv (dim_vector (n, 1));

        for (octave_idx_type i = 0; i < n; i++)
          {
            iv(i) = idx_arg(i).index_vector ();

            if (error_state)
              break;
          }

        if (!error_state)
          retval = Array<octave_value>::index (iv, resize_ok, Matrix ());
      }
      break;
    }

  return retval;
}

/*
%!test
%! a = {"foo", "bar"};
%! assert (a(), a)
*/

void
Cell::assign (const octave_value_list& idx_arg, const Cell& rhs,
              const octave_value& fill_val)

{
  octave_idx_type len = idx_arg.length ();

  Array<idx_vector> ra_idx (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    ra_idx(i) = idx_arg(i).index_vector ();

  Array<octave_value>::assign (ra_idx, rhs, fill_val);
}

void
Cell::delete_elements (const octave_value_list& idx_arg)

{
  octave_idx_type len = idx_arg.length ();

  Array<idx_vector> ra_idx (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    ra_idx.xelem (i) = idx_arg(i).index_vector ();

  Array<octave_value>::delete_elements (ra_idx);
}

octave_idx_type
Cell::nnz (void) const
{
  gripe_wrong_type_arg ("nnz", "cell array");
  return -1;
}

/*
%!error <wrong type argument 'cell array'> nnz ({0, 1, 2})
%!error <wrong type argument 'cell array'> nnz (cell ())
%!error <wrong type argument 'cell array'> nnz ({"foo", "bar"})
*/

Cell
Cell::column (octave_idx_type i) const
{
  Cell retval;

  if (ndims () < 3)
    {
      if (i < 0 || i >= cols ())
        error ("invalid column selection");
      else
        {
          octave_idx_type nr = rows ();

          retval.resize (dim_vector (nr, 1));

          for (octave_idx_type j = 0; j < nr; j++)
            retval.xelem (j) = elem (j, i);
        }
    }
  else
    error ("Cell::column: requires 2-d cell array");

  return retval;
}

Cell
Cell::concat (const Cell& rb, const Array<octave_idx_type>& ra_idx)
{
  return insert (rb, ra_idx);
}

Cell&
Cell::insert (const Cell& a, octave_idx_type r, octave_idx_type c)
{
  Array<octave_value>::insert (a, r, c);
  return *this;
}

Cell&
Cell::insert (const Cell& a, const Array<octave_idx_type>& ra_idx)
{
  Array<octave_value>::insert (a, ra_idx);
  return *this;
}

Cell
Cell::map (ctype_mapper fcn) const
{
  Cell retval (dims ());
  octave_value *r = retval.fortran_vec ();

  const octave_value *p = data ();

  for (octave_idx_type i = 0; i < numel (); i++)
    r[i] = ((p++)->*fcn) ();

  return retval;
}

octave_value
Cell::resize_fill_value (void) const
{
  static octave_value rfv = octave_value (Matrix ());
  return rfv;
}

Cell
Cell::diag (octave_idx_type k) const
{
  return Array<octave_value>::diag (k);
}

Cell
Cell::diag (octave_idx_type m, octave_idx_type n) const
{
  return Array<octave_value>::diag (m, n);
}
