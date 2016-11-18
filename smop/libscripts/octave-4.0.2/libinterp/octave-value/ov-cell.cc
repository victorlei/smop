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

#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>
#include <queue>

#include "Array-util.h"
#include "byte-swap.h"
#include "lo-utils.h"
#include "quit.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "error.h"
#include "mxarray.h"
#include "ov-cell.h"
#include "oct-obj.h"
#include "oct-hdf5.h"
#include "unwind-prot.h"
#include "utils.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"
#include "ov-scalar.h"
#include "gripes.h"

#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

// Cell is able to handle octave_value indexing by itself, so just forward
// everything.

template <>
octave_value
octave_base_matrix<Cell>::do_index_op (const octave_value_list& idx,
                                       bool resize_ok)
{
  return matrix.index (idx, resize_ok);
}

template <>
void
octave_base_matrix<Cell>::assign (const octave_value_list& idx, const Cell& rhs)
{
  matrix.assign (idx, rhs);
}

template <>
void
octave_base_matrix<Cell>::assign (const octave_value_list& idx,
                                  octave_value rhs)
{
  // FIXME: Really?
  if (rhs.is_cell ())
    matrix.assign (idx, rhs.cell_value ());
  else
    matrix.assign (idx, Cell (rhs));
}

template <>
void
octave_base_matrix<Cell>::delete_elements (const octave_value_list& idx)
{
  matrix.delete_elements (idx);
}

// FIXME: this list of specializations is becoming so long that we should
// really ask whether octave_cell should inherit from octave_base_matrix at all.

template <>
octave_value
octave_base_matrix<Cell>::fast_elem_extract (octave_idx_type n) const
{
  if (n < matrix.numel ())
    return Cell (matrix(n));
  else
    return octave_value ();
}

template <>
bool
octave_base_matrix<Cell>::fast_elem_insert (octave_idx_type n,
                                            const octave_value& x)
{
  const octave_cell *xrep =
    dynamic_cast<const octave_cell *> (&x.get_rep ());

  bool retval = xrep && xrep->matrix.numel () == 1 && n < matrix.numel ();
  if (retval)
    matrix(n) = xrep->matrix(0);

  return retval;
}

template class octave_base_matrix<Cell>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_cell, "cell", "cell");

static void
gripe_failed_assignment (void)
{
  error ("assignment to cell array failed");
}

octave_value_list
octave_cell::subsref (const std::string& type,
                      const std::list<octave_value_list>& idx,
                      int nargout,
                      const std::list<octave_lvalue> *lvalue_list)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      retval(0) = do_index_op (idx.front ());
      break;

    case '{':
      {
        octave_value tmp = do_index_op (idx.front ());

        if (! error_state)
          {
            Cell tcell = tmp.cell_value ();

            if (tcell.length () == 1)
              retval(0) = tcell(0,0);
            else
              retval = octave_value (octave_value_list (tcell), true);
          }
      }
      break;

    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.

  if (idx.size () > 1)
    retval = (lvalue_list
              ? retval(0).next_subsref (nargout, type, idx, lvalue_list)
              : retval(0).next_subsref (nargout, type, idx));

  return retval;
}

octave_value
octave_cell::subsref (const std::string& type,
                      const std::list<octave_value_list>& idx,
                      bool auto_add)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front (), auto_add);
      break;

    case '{':
      {
        octave_value tmp = do_index_op (idx.front (), auto_add);

        if (! error_state)
          {
            const Cell tcell = tmp.cell_value ();

            if (tcell.length () == 1)
              retval = tcell(0,0);
            else
              retval = octave_value (octave_value_list (tcell), true);
          }
      }
      break;

    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.

  if (idx.size () > 1)
    retval = retval.next_subsref (auto_add, type, idx);

  return retval;
}

octave_value
octave_cell::subsasgn (const std::string& type,
                       const std::list<octave_value_list>& idx,
                       const octave_value& rhs)
{
  octave_value retval;

  int n = type.length ();

  octave_value t_rhs = rhs;

  clear_cellstr_cache ();

  if (idx.front ().empty ())
    {
      error ("missing index in indexed assignment");
      return retval;
    }

  if (n > 1)
    {
      switch (type[0])
        {
        case '(':
          {
            if (is_empty () && type[1] == '.')
              {
                // Allow conversion of empty cell array to some other
                // type in cases like
                //
                //  x = {}; x(i).f = rhs

                octave_value tmp = octave_value::empty_conv (type, rhs);

                return tmp.subsasgn (type, idx, rhs);
              }
            else
              {
                octave_value tmp = do_index_op (idx.front (), true);

                if (! tmp.is_defined ())
                  tmp = octave_value::empty_conv (type.substr (1), rhs);

                if (! error_state)
                  {
                    std::list<octave_value_list> next_idx (idx);

                    next_idx.erase (next_idx.begin ());

                    tmp.make_unique ();

                    t_rhs = tmp.subsasgn (type.substr (1), next_idx, rhs);
                  }
              }
          }
          break;

        case '{':
          {
            matrix.make_unique ();
            Cell tmpc = matrix.index (idx.front (), true);

            if (! error_state)
              {
                std::list<octave_value_list> next_idx (idx);

                next_idx.erase (next_idx.begin ());

                std::string next_type = type.substr (1);

                if (tmpc.numel () == 1)
                  {
                    octave_value tmp = tmpc(0);
                    tmpc = Cell ();

                    if (! tmp.is_defined () || tmp.is_zero_by_zero ())
                      {
                        tmp = octave_value::empty_conv (type.substr (1), rhs);
                        tmp.make_unique (); // probably a no-op.
                      }
                    else
                      // optimization: ignore copy still stored inside array.
                      tmp.make_unique (1);

                    if (! error_state)
                      t_rhs = tmp.subsasgn (next_type, next_idx, rhs);
                  }
                else
                  gripe_indexed_cs_list ();
              }
          }
          break;

        case '.':
          {
            if (is_empty ())
              {
                // Do nothing; the next branch will handle it.
              }
            else
              {
                std::string nm = type_name ();
                error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
              }
          }
          break;

        default:
          panic_impossible ();
        }
    }

  if (! error_state)
    {
      switch (type[0])
        {
        case '(':
          {
            octave_value_list i = idx.front ();

            if (t_rhs.is_cell ())
              octave_base_matrix<Cell>::assign (i, t_rhs.cell_value ());
            else if (t_rhs.is_null_value ())
              octave_base_matrix<Cell>::delete_elements (i);
            else
              octave_base_matrix<Cell>::assign (i, Cell (t_rhs));

            if (! error_state)
              {
                count++;
                retval = octave_value (this);
              }
            else
              gripe_failed_assignment ();
          }
          break;

        case '{':
          {
            octave_value_list idxf = idx.front ();

            if (t_rhs.is_cs_list ())
              {
                Cell tmp_cell = Cell (t_rhs.list_value ());

                // Inquire the proper shape of the RHS.

                dim_vector didx = dims ().redim (idxf.length ());
                for (octave_idx_type k = 0; k < idxf.length (); k++)
                  if (! idxf(k).is_magic_colon ()) didx(k) = idxf(k).numel ();

                if (didx.numel () == tmp_cell.numel ())
                  tmp_cell = tmp_cell.reshape (didx);


                octave_base_matrix<Cell>::assign (idxf, tmp_cell);
              }
            else if (idxf.all_scalars ()
                     || do_index_op (idxf, true).numel () == 1)
              // Regularize a null matrix if stored into a cell.
              octave_base_matrix<Cell>::assign (idxf,
                                                Cell (t_rhs.storable_value ()));
            else if (! error_state)
              gripe_nonbraced_cs_list_assignment ();

            if (! error_state)
              {
                count++;
                retval = octave_value (this);
              }
            else
              gripe_failed_assignment ();
          }
          break;

        case '.':
          {
            if (is_empty ())
              {
                // Allow conversion of empty cell array to some other
                // type in cases like
                //
                //  x = {}; x.f = rhs

                octave_value tmp = octave_value::empty_conv (type, rhs);

                return tmp.subsasgn (type, idx, rhs);
              }
            else
              {
                std::string nm = type_name ();
                error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
              }
          }
          break;

        default:
          panic_impossible ();
        }
    }

  return retval;
}

bool
octave_cell::is_cellstr (void) const
{
  bool retval;
  if (cellstr_cache.get ())
    retval = true;
  else
    {
      retval = matrix.is_cellstr ();
      // Allocate empty cache to mark that this is indeed a cellstr.
      if (retval)
        cellstr_cache.reset (new Array<std::string> ());
    }

  return retval;
}

void
octave_cell::assign (const octave_value_list& idx, const Cell& rhs)
{
  clear_cellstr_cache ();
  octave_base_matrix<Cell>::assign (idx, rhs);
}

void
octave_cell::assign (const octave_value_list& idx, const octave_value& rhs)
{
  clear_cellstr_cache ();
  octave_base_matrix<Cell>::assign (idx, rhs);
}


void
octave_cell::delete_elements (const octave_value_list& idx)
{
  clear_cellstr_cache ();
  octave_base_matrix<Cell>::delete_elements (idx);
}

size_t
octave_cell::byte_size (void) const
{
  size_t retval = 0;

  for (octave_idx_type i = 0; i < numel (); i++)
    retval += matrix(i).byte_size ();

  return retval;
}

octave_value
octave_cell::sort (octave_idx_type dim, sortmode mode) const
{
  octave_value retval;

  if (is_cellstr ())
    {
      Array<std::string> tmp = cellstr_value ();

      tmp = tmp.sort (dim, mode);

      // We already have the cache.
      retval = new octave_cell (tmp);
    }
  else
    error ("sort: only cell arrays of character strings may be sorted");

  return retval;
}

octave_value
octave_cell::sort (Array<octave_idx_type> &sidx, octave_idx_type dim,
                   sortmode mode) const
{
  octave_value retval;

  if (is_cellstr ())
    {
      Array<std::string> tmp = cellstr_value ();

      tmp = tmp.sort (sidx, dim, mode);

      // We already have the cache.
      retval = new octave_cell (tmp);
    }
  else
    error ("sort: only cell arrays of character strings may be sorted");

  return retval;
}

sortmode
octave_cell::is_sorted (sortmode mode) const
{
  sortmode retval = UNSORTED;

  if (is_cellstr ())
    {
      Array<std::string> tmp = cellstr_value ();

      retval = tmp.is_sorted (mode);
    }
  else
    error ("issorted: A is not a cell array of strings");

  return retval;
}


Array<octave_idx_type>
octave_cell::sort_rows_idx (sortmode mode) const
{
  Array<octave_idx_type> retval;

  if (is_cellstr ())
    {
      Array<std::string> tmp = cellstr_value ();

      retval = tmp.sort_rows_idx (mode);
    }
  else
    error ("sortrows: only cell arrays of character strings may be sorted");

  return retval;
}

sortmode
octave_cell::is_sorted_rows (sortmode mode) const
{
  sortmode retval = UNSORTED;

  if (is_cellstr ())
    {
      Array<std::string> tmp = cellstr_value ();

      retval = tmp.is_sorted_rows (mode);
    }
  else
    error ("issorted: A is not a cell array of strings");

  return retval;
}

bool
octave_cell::is_true (void) const
{
  error ("invalid conversion from cell array to logical value");
  return false;
}

octave_value_list
octave_cell::list_value (void) const
{
  return octave_value_list (matrix);
}

string_vector
octave_cell::all_strings (bool pad) const
{
  string_vector retval;

  octave_idx_type nel = numel ();

  int n_elts = 0;

  octave_idx_type max_len = 0;

  std::queue<string_vector> strvec_queue;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      string_vector s = matrix(i).all_strings ();

      if (error_state)
        return retval;

      octave_idx_type s_len = s.length ();

      n_elts += s_len ? s_len : 1;

      octave_idx_type s_max_len = s.max_length ();

      if (s_max_len > max_len)
        max_len = s_max_len;

      strvec_queue.push (s);
    }

  retval = string_vector (n_elts);

  octave_idx_type k = 0;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      const string_vector s = strvec_queue.front ();
      strvec_queue.pop ();

      octave_idx_type s_len = s.length ();

      if (s_len)
        {
          for (octave_idx_type j = 0; j < s_len; j++)
            {
              std::string t = s[j];
              int t_len = t.length ();

              if (pad && max_len > t_len)
                t += std::string (max_len - t_len, ' ');

              retval[k++] = t;
            }
        }
      else if (pad)
        retval[k++] = std::string (max_len, ' ');
      else
        retval[k++] = std::string ();
    }

  return retval;
}

Array<std::string>
octave_cell::cellstr_value (void) const
{
  Array<std::string> retval;

  if (is_cellstr ())
    {
      if (cellstr_cache->is_empty ())
        *cellstr_cache = matrix.cellstr_value ();

      return *cellstr_cache;
    }
  else
    error ("invalid conversion from cell array to array of strings");

  return retval;
}

bool
octave_cell::print_as_scalar (void) const
{
  return true;
}

void
octave_cell::print (std::ostream& os, bool)
{
  print_raw (os);
}

void
octave_cell::print_raw (std::ostream& os, bool) const
{
  int nd = matrix.ndims ();

  if (nd == 2)
    {
      octave_idx_type nr = rows ();
      octave_idx_type nc = columns ();

      if (nr > 0 && nc > 0)
        {
          newline (os);
          indent (os);
          os << "{";
          newline (os);

          increment_indent_level ();

          for (octave_idx_type j = 0; j < nc; j++)
            {
              for (octave_idx_type i = 0; i < nr; i++)
                {
                  octave_quit ();

                  std::ostringstream buf;
                  buf << "[" << i+1 << "," << j+1 << "]";

                  octave_value val = matrix(i,j);

                  val.print_with_name (os, buf.str ());
                }
            }

          decrement_indent_level ();

          indent (os);
          os << "}";
          newline (os);
        }
      else
        {
          indent (os);
          os << "{}";
          if (Vprint_empty_dimensions)
            os << "(" << nr << "x" << nc << ")";
          newline (os);
        }
    }
  else
    {
      indent (os);
      dim_vector dv = matrix.dims ();
      os << "{" << dv.str () << " Cell Array}";
      newline (os);
    }
}

void
octave_cell::short_disp (std::ostream& os) const
{
  os << (matrix.is_empty () ? "{}" : "...");
}

#define CELL_ELT_TAG "<cell-element>"

bool
octave_cell::save_ascii (std::ostream& os)
{
  dim_vector d = dims ();
  if (d.length () > 2)
    {
      os << "# ndims: " << d.length () << "\n";

      for (int i = 0; i < d.length (); i++)
        os << " " << d (i);
      os << "\n";

      Cell tmp = cell_value ();

      for (octave_idx_type i = 0; i < d.numel (); i++)
        {
          octave_value o_val = tmp.elem (i);

          // Recurse to print sub-value.
          bool b = save_ascii_data (os, o_val, CELL_ELT_TAG, false, 0);

          if (! b)
            return ! os.fail ();
        }
    }
  else
    {
      // Keep this case, rather than use generic code above for backward
      // compatiability. Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
         << "# columns: " << columns () << "\n";

      Cell tmp = cell_value ();

      for (octave_idx_type j = 0; j < tmp.cols (); j++)
        {
          for (octave_idx_type i = 0; i < tmp.rows (); i++)
            {
              octave_value o_val = tmp.elem (i, j);

              // Recurse to print sub-value.
              bool b = save_ascii_data (os, o_val, CELL_ELT_TAG, false, 0);

              if (! b)
                return ! os.fail ();
            }

          os << "\n";
        }
    }

  return true;
}

bool
octave_cell::load_ascii (std::istream& is)
{
  bool success = true;

  clear_cellstr_cache ();

  string_vector keywords(2);

  keywords[0] = "ndims";
  keywords[1] = "rows";

  std::string kw;
  octave_idx_type val = 0;

  if (extract_keyword (is, keywords, kw, val, true))
    {
      if (kw == "ndims")
        {
          int mdims = static_cast<int> (val);

          if (mdims >= 0)
            {
              dim_vector dv;
              dv.resize (mdims);

              for (int i = 0; i < mdims; i++)
                is >> dv(i);

              Cell tmp(dv);

              for (octave_idx_type i = 0; i < dv.numel (); i++)
                {
                  octave_value t2;
                  bool dummy;

                  // recurse to read cell elements
                  std::string nm = read_ascii_data (is, std::string (),
                                                    dummy, t2, i);

                  if (nm == CELL_ELT_TAG)
                    {
                      if (is)
                        tmp.elem (i) = t2;
                    }
                  else
                    {
                      error ("load: cell array element had unexpected name");
                      success = false;
                      break;
                    }
                }

              if (is)
                matrix = tmp;
              else
                {
                  error ("load: failed to load matrix constant");
                  success = false;
                }
            }
          else
            {
              error ("load: failed to extract number of rows and columns");
              success = false;
            }
        }
      else if (kw == "rows")
        {
          octave_idx_type nr = val;
          octave_idx_type nc = 0;

          if (nr >= 0 && extract_keyword (is, "columns", nc) && nc >= 0)
            {
              if (nr > 0 && nc > 0)
                {
                  Cell tmp (nr, nc);

                  for (octave_idx_type j = 0; j < nc; j++)
                    {
                      for (octave_idx_type i = 0; i < nr; i++)
                        {
                          octave_value t2;
                          bool dummy;

                          // recurse to read cell elements
                          std::string nm = read_ascii_data (is, std::string (),
                                                            dummy, t2, i);

                          if (nm == CELL_ELT_TAG)
                            {
                              if (is)
                                tmp.elem (i, j) = t2;
                            }
                          else
                            {
                              error ("load: cell array element had unexpected name");
                              success = false;
                              goto cell_read_error;
                            }
                        }
                    }

                cell_read_error:

                  if (is)
                    matrix = tmp;
                  else
                    {
                      error ("load: failed to load cell element");
                      success = false;
                    }
                }
              else if (nr == 0 || nc == 0)
                matrix = Cell (nr, nc);
              else
                panic_impossible ();
            }
          else
            {
              error ("load: failed to extract number of rows and columns for cell array");
              success = false;
            }
        }
      else
        panic_impossible ();
    }
  else
    {
      error ("load: failed to extract number of rows and columns");
      success = false;
    }

  return success;
}

bool
octave_cell::save_binary (std::ostream& os, bool& save_as_floats)
{
  dim_vector d = dims ();
  if (d.length () < 1)
    return false;

  // Use negative value for ndims
  int32_t di = - d.length ();
  os.write (reinterpret_cast<char *> (&di), 4);
  for (int i = 0; i < d.length (); i++)
    {
      di = d(i);
      os.write (reinterpret_cast<char *> (&di), 4);
    }

  Cell tmp = cell_value ();

  for (octave_idx_type i = 0; i < d.numel (); i++)
    {
      octave_value o_val = tmp.elem (i);

      // Recurse to print sub-value.
      bool b = save_binary_data (os, o_val, CELL_ELT_TAG, "", 0,
                                 save_as_floats);

      if (! b)
        return false;
    }

  return true;
}

bool
octave_cell::load_binary (std::istream& is, bool swap,
                          oct_mach_info::float_format fmt)
{
  clear_cellstr_cache ();

  bool success = true;
  int32_t mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims >= 0)
    return false;

  mdims = -mdims;
  int32_t di;
  dim_vector dv;
  dv.resize (mdims);

  for (int i = 0; i < mdims; i++)
    {
      if (! is.read (reinterpret_cast<char *> (&di), 4))
        return false;
      if (swap)
        swap_bytes<4> (&di);
      dv(i) = di;
    }

  // Convert an array with a single dimension to be a row vector.
  // Octave should never write files like this, other software
  // might.

  if (mdims == 1)
    {
      mdims = 2;
      dv.resize (mdims);
      dv(1) = dv(0);
      dv(0) = 1;
    }

  octave_idx_type nel = dv.numel ();
  Cell tmp(dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      octave_value t2;
      bool dummy;
      std::string doc;

      // recurse to read cell elements
      std::string nm = read_binary_data (is, swap, fmt, std::string (),
                                         dummy, t2, doc);

      if (nm == CELL_ELT_TAG)
        {
          if (is)
            tmp.elem (i) = t2;
        }
      else
        {
          error ("load: cell array element had unexpected name");
          success = false;
          break;
        }
    }

  if (is)
    matrix = tmp;
  else
    {
      error ("load: failed to load matrix constant");
      success = false;
    }

  return success;
}

void *
octave_cell::mex_get_data (void) const
{
  clear_cellstr_cache ();
  return matrix.mex_get_data ();
}

bool
octave_cell::save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats)
{
#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  hsize_t rank = dv.length ();
  hid_t space_hid, data_hid, size_hid;
  space_hid = data_hid = size_hid = -1;

#if HAVE_HDF5_18
  data_hid = H5Gcreate (loc_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Gcreate (loc_id, name, 0);
#endif

  if (data_hid < 0)
    return false;

  // Have to save cell array shape, since can't have a
  // dataset of groups....

  space_hid = H5Screate_simple (1, &rank, 0);

  if (space_hid < 0)
    {
      H5Gclose (data_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (octave_idx_type, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (hsize_t i = 0; i < rank; i++)
    hdims[i] = dv(rank-i-1);

#if HAVE_HDF5_18
  size_hid = H5Dcreate (data_hid, "dims", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  size_hid = H5Dcreate (data_hid, "dims", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT);
#endif
  if (size_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (data_hid);
      return false;
    }

  if (H5Dwrite (size_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
                H5P_DEFAULT, hdims) < 0)
    {
      H5Dclose (size_hid);
      H5Sclose (space_hid);
      H5Gclose (data_hid);
      return false;
    }

  H5Dclose (size_hid);
  H5Sclose (space_hid);

  // Recursively add each element of the cell to this group.

  Cell tmp = cell_value ();

  octave_idx_type nel = dv.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      std::ostringstream buf;
      int digits = static_cast<int> (gnulib::floor (::log10 (static_cast<double>
                                     (nel)) + 1.0));
      buf << "_" << std::setw (digits) << std::setfill ('0') << i;
      std::string s = buf.str ();

      if (! add_hdf5_data (data_hid, tmp.elem (i), s.c_str (), "", false,
                           save_as_floats))
        {
          H5Gclose (data_hid);
          return false;
        }
    }

  H5Gclose (data_hid);

  return true;

#else
  gripe_save ("hdf5");
  return false;
#endif
}

bool
octave_cell::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  clear_cellstr_cache ();

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize (dv);
  if (empty)
    return (empty > 0);

#if HAVE_HDF5_18
  hid_t group_id = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  hid_t group_id = H5Gopen (loc_id, name);
#endif

  if (group_id < 0)
    return false;

#if HAVE_HDF5_18
  hid_t data_hid = H5Dopen (group_id, "dims", H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (group_id, "dims");
#endif
  hid_t space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);
  if (rank != 1)
    {
      H5Dclose (data_hid);
      H5Gclose (group_id);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  // Octave uses column-major, while HDF5 uses row-major ordering.

  dv.resize (hdims[0]);

  OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, hdims[0]);

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, tmp) < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_id);
      return false;
    }

  H5Dclose (data_hid);
  H5Gclose (group_id);

  for (hsize_t i = 0, j = hdims[0] - 1; i < hdims[0]; i++, j--)
    dv(j) = tmp[i];

  hdf5_callback_data dsub;

  herr_t retval2 = -1;

  Cell m (dv);

  int current_item = 0;

  hsize_t num_obj = 0;
#if HAVE_HDF5_18
  group_id = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  group_id = H5Gopen (loc_id, name);
#endif
  H5Gget_num_objs (group_id, &num_obj);
  H5Gclose (group_id);

  for (octave_idx_type i = 0; i < dv.numel (); i++)
    {

      if (current_item >= static_cast<int> (num_obj))
        retval2 = -1;
      else
        retval2 = H5Giterate (loc_id, name, &current_item,
                              hdf5_read_next_data, &dsub);

      if (retval2 <= 0)
        break;

      octave_value ov = dsub.tc;
      m.elem (i) = ov;

    }

  if (retval2 >= 0)
    {
      matrix = m;
      retval = true;
    }

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

DEFUN (iscell, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscell (@var{x})\n\
Return true if @var{x} is a cell array object.\n\
@seealso{ismatrix, isstruct, iscellstr, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_cell ();
  else
    print_usage ();

  return retval;
}

DEFUN (cell, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} cell (@var{n})\n\
@deftypefnx {Built-in Function} {} cell (@var{m}, @var{n})\n\
@deftypefnx {Built-in Function} {} cell (@var{m}, @var{n}, @var{k}, @dots{})\n\
@deftypefnx {Built-in Function} {} cell ([@var{m} @var{n} @dots{}])\n\
Create a new cell array object.\n\
\n\
If invoked with a single scalar integer argument, return a square\n\
@nospell{NxN} cell array.  If invoked with two or more scalar integer\n\
arguments, or a vector of integer values, return an array with the given\n\
dimensions.\n\
@seealso{cellstr, mat2cell, num2cell, struct2cell}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector dims;

  switch (nargin)
    {
    case 0:
      dims = dim_vector (0, 0);
      break;

    case 1:
      get_dimensions (args(0), "cell", dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            dims(i) = args(i).is_empty () ? 0 : args(i).nint_value ();

            if (error_state)
              {
                error ("cell: expecting scalar arguments");
                break;
              }
          }
      }
      break;
    }

  if (! error_state)
    {
      dims.chop_trailing_singletons ();

      check_dimensions (dims, "cell");

      if (! error_state)
        retval = Cell (dims, Matrix ());
    }

  return retval;
}

DEFUN (iscellstr, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscellstr (@var{cell})\n\
Return true if every element of the cell array @var{cell} is a character\n\
string.\n\
@seealso{ischar}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_cellstr ();
  else
    print_usage ();

  return retval;
}

// Note that since Fcellstr calls Fiscellstr, we need to have
// Fiscellstr defined first (to provide a declaration) and also we
// should keep it in the same file (so we don't have to provide a
// declaration) and so we don't have to use feval to call it.

DEFUN (cellstr, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{cstr} =} cellstr (@var{strmat})\n\
Create a new cell array object from the elements of the string array\n\
@var{strmat}.\n\
\n\
Each row of @var{strmat} becomes an element of @var{cstr}.  Any trailing\n\
spaces in a row are deleted before conversion.\n\
\n\
To convert back from a cellstr to a character array use @code{char}.\n\
@seealso{cell, char}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_value_list tmp = Fiscellstr (args, 1);

      if (tmp(0).is_true ())
        retval = args(0);
      else
        {
          string_vector s = args(0).all_strings ();

          if (! error_state)
            retval = (s.is_empty ()
                      ? Cell (octave_value (std::string ()))
                      : Cell (s, true));
          else
            error ("cellstr: argument STRING must be a 2-D character array");
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (struct2cell, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{c} =} struct2cell (@var{s})\n\
Create a new cell array from the objects stored in the struct object.\n\
\n\
If @var{f} is the number of fields in the structure, the resulting cell\n\
array will have a dimension vector corresponding to\n\
@code{[@var{f} size(@var{s})]}.  For example:\n\
\n\
@example\n\
@group\n\
s = struct (\"name\", @{\"Peter\", \"Hannah\", \"Robert\"@},\n\
           \"age\", @{23, 16, 3@});\n\
c = struct2cell (s)\n\
   @result{} c = @{2x1x3 Cell Array@}\n\
c(1,1,:)(:)\n\
   @result{}\n\
      @{\n\
        [1,1] = Peter\n\
        [2,1] = Hannah\n\
        [3,1] = Robert\n\
      @}\n\
c(2,1,:)(:)\n\
   @result{}\n\
      @{\n\
        [1,1] = 23\n\
        [2,1] = 16\n\
        [3,1] = 3\n\
      @}\n\
@end group\n\
@end example\n\
\n\
@seealso{cell2struct, fieldnames}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      const octave_map m = args(0).map_value ();

      if (! error_state)
        {
          const dim_vector m_dv = m.dims ();

          octave_idx_type num_fields = m.nfields ();

          // The resulting dim_vector should have dimensions:
          // [numel(fields) size(struct)]
          // except if the struct is a column vector.

          dim_vector result_dv;
          if (m_dv (m_dv.length () - 1) == 1)
            result_dv.resize (m_dv.length ());
          else
            result_dv.resize (m_dv.length () + 1); // Add 1 for the fields.

          result_dv(0) = num_fields;

          for (int i = 1; i < result_dv.length (); i++)
            result_dv(i) = m_dv(i-1);

          NoAlias<Cell> c (result_dv);

          octave_idx_type n_elts = m.numel ();

          // Fill c in one sweep. Note that thanks to octave_map structure,
          // we don't need a key lookup at all.
          for (octave_idx_type j = 0; j < n_elts; j++)
            for (octave_idx_type i = 0; i < num_fields; i++)
              c(i,j) = m.contents(i)(j);

          retval = c;
        }
      else
        error ("struct2cell: argument S must be a structure");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! keys = cellstr (char (floor (rand (11,10)*24+65)))';
%! vals = cellfun (@(x) mat2cell (rand (19,1), ones (19,1), 1), ...
%!          mat2cell ([1:11]', ones (11,1), 1), "uniformoutput", false)';
%! s = struct ([keys; vals]{:});
%! t = cell2struct ([vals{:}], keys, 2);
%! assert (s, t);
%! assert (struct2cell (s), [vals{:}]');
%! assert (fieldnames (s), keys');
*/

mxArray *
octave_cell::as_mxArray (void) const
{
  mxArray *retval = new mxArray (dims ());

  mxArray **elts = static_cast<mxArray **> (retval->get_data ());

  mwSize nel = numel ();

  const octave_value *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    elts[i] = new mxArray (p[i]);

  return retval;
}

octave_value
octave_cell::map (unary_mapper_t umap) const
{
  switch (umap)
    {
#define FORWARD_MAPPER(UMAP) \
    case umap_ ## UMAP: \
      return matrix.UMAP ()
    FORWARD_MAPPER (xisalnum);
    FORWARD_MAPPER (xisalpha);
    FORWARD_MAPPER (xisascii);
    FORWARD_MAPPER (xiscntrl);
    FORWARD_MAPPER (xisdigit);
    FORWARD_MAPPER (xisgraph);
    FORWARD_MAPPER (xislower);
    FORWARD_MAPPER (xisprint);
    FORWARD_MAPPER (xispunct);
    FORWARD_MAPPER (xisspace);
    FORWARD_MAPPER (xisupper);
    FORWARD_MAPPER (xisxdigit);
    FORWARD_MAPPER (xtoascii);
    FORWARD_MAPPER (xtolower);
    FORWARD_MAPPER (xtoupper);

    default:
      return octave_base_value::map (umap);
    }
}
