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

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "mxarray.h"
#include "oct-lvalue.h"
#include "oct-hdf5.h"
#include "ov-struct.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "Array-util.h"
#include "oct-locbuf.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-hdf5.h"
#include "ls-utils.h"
#include "pr-output.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(octave_struct, "struct", "struct");

// How many levels of structure elements should we print?
static int Vstruct_levels_to_print = 2;

// TRUE means print struct array contents, up to the number of levels
// specified by struct_levels_to_print.
static bool Vprint_struct_array_contents = false;

octave_base_value *
octave_struct::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (numel () == 1)
    retval = new octave_scalar_struct (map.checkelem (0));

  return retval;
}

Cell
octave_struct::dotref (const octave_value_list& idx, bool auto_add)
{
  Cell retval;

  assert (idx.length () == 1);

  std::string nm = idx(0).string_value ();

  octave_map::const_iterator p = map.seek (nm);

  if (p != map.end ())
    retval = map.contents (p);
  else if (auto_add)
    retval = (numel () == 0) ? Cell (dim_vector (1, 1)) : Cell (dims ());
  else
    error_with_id ("Octave:invalid-indexing",
                   "structure has no member '%s'", nm.c_str ());

  return retval;
}

#if 0
static void
gripe_invalid_index1 (void)
{
  error ("invalid index for structure array");
}
#endif

static void
gripe_invalid_index_for_assignment (void)
{
  error ("invalid index for structure array assignment");
}

static void
gripe_invalid_index_type (const std::string& nm, char t)
{
  error ("%s cannot be indexed with %c", nm.c_str (), t);
}

static void
gripe_failed_assignment (void)
{
  error ("assignment to structure element failed");
}

static void
maybe_warn_invalid_field_name (const std::string& key, const char *who)
{
  if (! valid_identifier (key))
    {
      if (who)
        warning_with_id ("Octave:language-extension",
                         "%s: invalid structure field name '%s'",
                         who, key.c_str ());
      else
        warning_with_id ("Octave:language-extension",
                         "invalid structure field name '%s'",
                         key.c_str ());
    }
}

octave_value_list
octave_struct::subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        int nargout)
{
  octave_value_list retval;

  int skip = 1;

  switch (type[0])
    {
    case '(':
      {
        if (type.length () > 1 && type[1] == '.')
          {
            std::list<octave_value_list>::const_iterator p = idx.begin ();
            octave_value_list key_idx = *++p;

            const Cell tmp = dotref (key_idx);

            if (! error_state)
              {
                const Cell t = tmp.index (idx.front ());

                retval(0) = (t.length () == 1) ? t(0) : octave_value (t, true);

                // We handled two index elements, so tell
                // next_subsref to skip both of them.

                skip++;
              }
          }
        else
          retval(0) = do_index_op (idx.front ());
      }
      break;

    case '.':
      {
        if (map.numel () > 0)
          {
            const Cell t = dotref (idx.front ());

            retval(0) = (t.length () == 1) ? t(0) : octave_value (t, true);
          }
      }
      break;

    case '{':
      gripe_invalid_index_type (type_name (), type[0]);
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx, skip);

  return retval;
}

octave_value
octave_struct::subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add)
{
  octave_value retval;

  int skip = 1;

  switch (type[0])
    {
    case '(':
      {
        if (type.length () > 1 && type[1] == '.')
          {
            std::list<octave_value_list>::const_iterator p = idx.begin ();
            octave_value_list key_idx = *++p;

            const Cell tmp = dotref (key_idx, auto_add);

            if (! error_state)
              {
                const Cell t = tmp.index (idx.front (), auto_add);

                retval = (t.length () == 1) ? t(0) : octave_value (t, true);

                // We handled two index elements, so tell
                // next_subsref to skip both of them.

                skip++;
              }
          }
        else
          retval = do_index_op (idx.front (), auto_add);
      }
      break;

    case '.':
      {
        if (map.numel () > 0)
          {
            const Cell t = dotref (idx.front (), auto_add);

            retval = (t.length () == 1) ? t(0) : octave_value (t, true);
          }
      }
      break;

    case '{':
      gripe_invalid_index_type (type_name (), type[0]);
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.

  if (idx.size () > 1)
    retval = retval.next_subsref (auto_add, type, idx, skip);

  return retval;
}

/*
%!test
%! x(1).a.a = 1;
%! x(2).a.a = 2;
%! assert (size (x), [1, 2]);
%! assert (x(1).a.a, 1);
%! assert (x(2).a.a, 2);
*/

octave_value
octave_struct::numeric_conv (const octave_value& val,
                             const std::string& type)
{
  octave_value retval;

  if (type.length () > 0 && type[0] == '.' && ! val.is_map ())
    retval = octave_map ();
  else
    retval = val;

  return retval;
}

octave_value
octave_struct::subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs)
{
  octave_value retval;

  int n = type.length ();

  octave_value t_rhs = rhs;

  if (idx.front ().empty ())
    {
      error ("missing index in indexed assignment");
      return retval;
    }

  if (n > 1 && ! (type.length () == 2 && type[0] == '(' && type[1] == '.'))
    {
      switch (type[0])
        {
        case '(':
          {
            if (type.length () > 1 && type[1] == '.')
              {
                std::list<octave_value_list>::const_iterator p = idx.begin ();
                octave_value_list t_idx = *p;

                octave_value_list key_idx = *++p;

                assert (key_idx.length () == 1);

                std::string key = key_idx(0).string_value ();

                maybe_warn_invalid_field_name (key, "subsasgn");

                if (error_state)
                  return retval;

                std::list<octave_value_list> next_idx (idx);

                // We handled two index elements, so subsasgn to
                // needs to skip both of them.

                next_idx.erase (next_idx.begin ());
                next_idx.erase (next_idx.begin ());

                std::string next_type = type.substr (2);

                Cell tmpc (1, 1);
                octave_map::iterator pkey = map.seek (key);
                if (pkey != map.end ())
                  {
                    map.contents (pkey).make_unique ();
                    tmpc = map.contents (pkey).index (idx.front (), true);
                  }

                // FIXME: better code reuse?
                //        cf. octave_cell::subsasgn and the case below.
                if (! error_state)
                  {
                    if (tmpc.numel () == 1)
                      {
                        octave_value& tmp = tmpc(0);

                        bool orig_undefined = tmp.is_undefined ();

                        if (orig_undefined || tmp.is_zero_by_zero ())
                          {
                            tmp = octave_value::empty_conv (next_type, rhs);
                            tmp.make_unique (); // probably a no-op.
                          }
                        else
                          // optimization: ignore the copy
                          // still stored inside our map.
                          tmp.make_unique (1);

                        if (! error_state)
                          t_rhs =
                            (orig_undefined
                               ? tmp.undef_subsasgn (next_type, next_idx, rhs)
                               : tmp.subsasgn (next_type, next_idx, rhs));
                      }
                    else
                      gripe_indexed_cs_list ();
                  }
              }
            else
              gripe_invalid_index_for_assignment ();
          }
          break;

        case '.':
          {
            octave_value_list key_idx = idx.front ();

            assert (key_idx.length () == 1);

            std::string key = key_idx(0).string_value ();

            maybe_warn_invalid_field_name (key, "subsasgn");

            if (error_state)
              return retval;

            std::list<octave_value_list> next_idx (idx);

            next_idx.erase (next_idx.begin ());

            std::string next_type = type.substr (1);

            Cell tmpc (1, 1);
            octave_map::iterator pkey = map.seek (key);
            if (pkey != map.end ())
              {
                map.contents (pkey).make_unique ();
                tmpc = map.contents (pkey);
              }

            // FIXME: better code reuse?
            if (! error_state)
              {
                if (tmpc.numel () == 1)
                  {
                    octave_value& tmp = tmpc(0);

                    bool orig_undefined = tmp.is_undefined ();

                    if (orig_undefined || tmp.is_zero_by_zero ())
                      {
                        tmp = octave_value::empty_conv (next_type, rhs);
                        tmp.make_unique (); // probably a no-op.
                      }
                    else
                      // optimization: ignore the copy
                      // still stored inside our map.
                      tmp.make_unique (1);

                    if (! error_state)
                      t_rhs = (orig_undefined
                               ? tmp.undef_subsasgn (next_type, next_idx, rhs)
                               : tmp.subsasgn (next_type, next_idx, rhs));
                  }
                else
                  gripe_indexed_cs_list ();
              }
          }
          break;

        case '{':
          gripe_invalid_index_type (type_name (), type[0]);
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
            if (n > 1 && type[1] == '.')
              {
                std::list<octave_value_list>::const_iterator p = idx.begin ();
                octave_value_list key_idx = *++p;
                octave_value_list idxf = idx.front ();

                assert (key_idx.length () == 1);

                std::string key = key_idx(0).string_value ();

                maybe_warn_invalid_field_name (key, "subsasgn");

                if (error_state)
                  return retval;

                if (! error_state)
                  {
                    if (t_rhs.is_cs_list ())
                      {
                        Cell tmp_cell = Cell (t_rhs.list_value ());

                        // Inquire the proper shape of the RHS.

                        dim_vector didx = dims ().redim (idxf.length ());
                        for (octave_idx_type k = 0; k < idxf.length (); k++)
                          if (! idxf(k).is_magic_colon ())
                            didx(k) = idxf(k).numel ();

                        if (didx.numel () == tmp_cell.numel ())
                          tmp_cell = tmp_cell.reshape (didx);


                        map.assign (idxf, key, tmp_cell);

                        if (! error_state)
                          {
                            count++;
                            retval = octave_value (this);
                          }
                        else
                          gripe_failed_assignment ();
                      }
                    else
                      {
                        const octave_map& cmap =
                          const_cast<const octave_map &> (map);
                        // cast to const reference, avoid forced key insertion.
                        if (idxf.all_scalars ()
                            || cmap.contents (key).index (idxf, true).numel ()
                               == 1)
                          {
                            map.assign (idxf,
                                        key, Cell (t_rhs.storable_value ()));
                            if (! error_state)
                              {
                                count++;
                                retval = octave_value (this);
                              }
                            else
                              gripe_failed_assignment ();
                          }
                        else if (! error_state)
                          gripe_nonbraced_cs_list_assignment ();
                      }
                  }
                else
                  gripe_failed_assignment ();
              }
            else
              {
                if (t_rhs.is_map () || t_rhs.is_object ())
                  {
                    octave_map rhs_map = t_rhs.map_value ();

                    if (! error_state)
                      {
                        map.assign (idx.front (), rhs_map);

                        if (! error_state)
                          {
                            count++;
                            retval = octave_value (this);
                          }
                        else
                          gripe_failed_assignment ();
                      }
                    else
                      error ("invalid structure assignment");
                  }
                else
                  {
                    if (t_rhs.is_null_value ())
                      {
                        map.delete_elements (idx.front ());

                        if (! error_state)
                          {
                            count++;
                            retval = octave_value (this);
                          }
                        else
                          gripe_failed_assignment ();
                      }
                    else
                      error ("invalid structure assignment");
                  }
              }
          }
          break;

        case '.':
          {
            octave_value_list key_idx = idx.front ();

            assert (key_idx.length () == 1);

            std::string key = key_idx(0).string_value ();

            maybe_warn_invalid_field_name (key, "subsasgn");

            if (error_state)
              return retval;

            if (t_rhs.is_cs_list ())
              {
                Cell tmp_cell = Cell (t_rhs.list_value ());

                // The shape of the RHS is irrelevant, we just want
                // the number of elements to agree and to preserve the
                // shape of the left hand side of the assignment.

                if (numel () == tmp_cell.numel ())
                  tmp_cell = tmp_cell.reshape (dims ());

                map.setfield (key, tmp_cell);
              }
            else
              {
                Cell tmp_cell(1, 1);
                tmp_cell(0) = t_rhs.storable_value ();
                map.setfield (key, tmp_cell);
              }

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
          gripe_invalid_index_type (type_name (), type[0]);
          break;

        default:
          panic_impossible ();
        }
    }
  else
    gripe_failed_assignment ();

  retval.maybe_mutate ();

  return retval;
}

octave_value
octave_struct::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // octave_map handles indexing itself.
  return map.index (idx, resize_ok);
}

size_t
octave_struct::byte_size (void) const
{
  // Neglect the size of the fieldnames.

  size_t retval = 0;

  for (octave_map::const_iterator p = map.begin (); p != map.end (); p++)
    {
      std::string key = map.key (p);

      octave_value val = octave_value (map.contents (p));

      retval += val.byte_size ();
    }

  return retval;
}

void
octave_struct::print (std::ostream& os, bool)
{
  print_raw (os);
}

void
octave_struct::print_raw (std::ostream& os, bool) const
{
  unwind_protect frame;

  frame.protect_var (Vstruct_levels_to_print);

  if (Vstruct_levels_to_print >= 0)
    {
      bool max_depth_reached = Vstruct_levels_to_print-- == 0;

      bool print_fieldnames_only
        = (max_depth_reached || ! Vprint_struct_array_contents);

      increment_indent_level ();

      newline (os);
      indent (os);
      dim_vector dv = dims ();
      os << dv.str () << " struct array containing the fields:";
      newline (os);

      increment_indent_level ();

      string_vector key_list = map.fieldnames ();

      for (octave_idx_type i = 0; i < key_list.length (); i++)
        {
          std::string key = key_list[i];

          Cell val = map.contents (key);

          newline (os);

          if (print_fieldnames_only)
            {
              indent (os);
              os << key;
            }
          else
            {
              octave_value tmp (val);
              tmp.print_with_name (os, key);
            }
        }

      if (print_fieldnames_only)
        newline (os);

      decrement_indent_level ();
      decrement_indent_level ();
    }
  else
    {
      indent (os);
      os << "<structure>";
      newline (os);
    }
}

bool
octave_struct::print_name_tag (std::ostream& os, const std::string& name) const
{
  bool retval = false;

  indent (os);

  if (Vstruct_levels_to_print < 0)
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      retval = true;
    }

  return retval;
}

static bool
scalar (const dim_vector& dims)
{
  return dims.length () == 2 && dims (0) == 1 && dims (1) == 1;
}


bool
octave_struct::save_ascii (std::ostream& os)
{
  octave_map m = map_value ();

  octave_idx_type nf = m.nfields ();

  const dim_vector dv = dims ();

  os << "# ndims: " << dv.length () << "\n";

  for (int i = 0; i < dv.length (); i++)
    os << " " << dv (i);
  os << "\n";

  os << "# length: " << nf << "\n";

  // Iterating over the list of keys will preserve the order of the
  // fields.
  string_vector keys = m.fieldnames ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      std::string key = keys(i);

      octave_value val = map.contents (key);

      bool b = save_ascii_data (os, val, key, false, 0);

      if (! b)
        return ! os.fail ();
    }

  return true;
}

bool
octave_struct::load_ascii (std::istream& is)
{
  octave_idx_type len = 0;
  dim_vector dv (1, 1);
  bool success = true;

  // KLUGE: earlier Octave versions did not save extra dimensions with struct,
  // and as a result did not preserve dimensions for empty structs.
  // The default dimensions were 1x1, which we want to preserve.
  string_vector keywords(2);

  keywords[0] = "ndims";
  keywords[1] = "length";

  std::string kw;

  if (extract_keyword (is, keywords, kw, len, true))
    {
      if (kw == keywords[0])
        {
          int mdims = std::max (static_cast<int> (len), 2);
          dv.resize (mdims);
          for (int i = 0; i < mdims; i++)
            is >> dv(i);

          success = extract_keyword (is, keywords[1], len);
        }
    }
  else
    success = false;

  if (success && len >= 0)
    {
      if (len > 0)
        {
          octave_map m (dv);

          for (octave_idx_type j = 0; j < len; j++)
            {
              octave_value t2;
              bool dummy;

              // recurse to read cell elements
              std::string nm
                = read_ascii_data (is, std::string (), dummy, t2, j);

              if (!is)
                break;

              Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);

              if (error_state)
                {
                  error ("load: internal error loading struct elements");
                  return false;
                }

              m.setfield (nm, tcell);
            }

          if (is)
            map = m;
          else
            {
              error ("load: failed to load structure");
              success = false;
            }
        }
      else if (len == 0)
        map = octave_map (dv);
      else
        panic_impossible ();
    }
  else
    {
      error ("load: failed to extract number of elements in structure");
      success = false;
    }

  return success;
}

bool
octave_struct::save_binary (std::ostream& os, bool& save_as_floats)
{
  octave_map m = map_value ();

  octave_idx_type nf = m.nfields ();

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

  int32_t len = nf;
  os.write (reinterpret_cast<char *> (&len), 4);

  // Iterating over the list of keys will preserve the order of the
  // fields.
  string_vector keys = m.fieldnames ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      std::string key = keys(i);

      octave_value val = map.contents (key);

      bool b = save_binary_data (os, val, key, "", 0, save_as_floats);

      if (! b)
        return ! os.fail ();
    }

  return true;
}

bool
octave_struct::load_binary (std::istream& is, bool swap,
                            oct_mach_info::float_format fmt)
{
  bool success = true;
  int32_t len;
  if (! is.read (reinterpret_cast<char *> (&len), 4))
    return false;
  if (swap)
    swap_bytes<4> (&len);

  dim_vector dv (1, 1);

  if (len < 0)
    {
      // We have explicit dimensions.
      int mdims = -len;

      int32_t di;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
        {
          if (! is.read (reinterpret_cast<char *> (&di), 4))
            return false;
          if (swap)
            swap_bytes<4> (&di);
          dv(i) = di;
        }

      if (! is.read (reinterpret_cast<char *> (&len), 4))
        return false;
      if (swap)
        swap_bytes<4> (&len);
    }

  if (len > 0)
    {
      octave_map m (dv);

      for (octave_idx_type j = 0; j < len; j++)
        {
          octave_value t2;
          bool dummy;
          std::string doc;

          // recurse to read cell elements
          std::string nm = read_binary_data (is, swap, fmt, std::string (),
                                             dummy, t2, doc);

          if (!is)
            break;

          Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);

          if (error_state)
            {
              error ("load: internal error loading struct elements");
              return false;
            }

          m.setfield (nm, tcell);
        }

      if (is)
        map = m;
      else
        {
          error ("load: failed to load structure");
          success = false;
        }
    }
  else if (len == 0)
    map = octave_map (dv);
  else
    success = false;

  return success;
}

bool
octave_struct::save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats)
{
#if defined (HAVE_HDF5)

  hid_t data_hid = -1;

#if HAVE_HDF5_18
  data_hid = H5Gcreate (loc_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (data_hid < 0) return false;

  // recursively add each element of the structure to this group
  octave_map m = map_value ();

  octave_idx_type nf = m.nfields ();

  // Iterating over the list of keys will preserve the order of the
  // fields.
  string_vector keys = m.fieldnames ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      std::string key = keys(i);

      octave_value val = map.contents (key);

      bool retval2 = add_hdf5_data (data_hid, val, key, "", false,
                                    save_as_floats);

      if (! retval2)
        break;
    }

  H5Gclose (data_hid);

  return true;

#else
  gripe_save ("hdf5");
  return false;
#endif
}

bool
octave_struct::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hdf5_callback_data dsub;

  herr_t retval2 = 0;
  octave_map m (dim_vector (1, 1));
  int current_item = 0;
  hsize_t num_obj = 0;
#if HAVE_HDF5_18
  hid_t group_id = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  hid_t group_id = H5Gopen (loc_id, name);
#endif
  H5Gget_num_objs (group_id, &num_obj);
  H5Gclose (group_id);

  // FIXME: fields appear to be sorted alphabetically on loading.
  // Why is that happening?

  while (current_item < static_cast<int> (num_obj)
         && (retval2 = H5Giterate (loc_id, name, &current_item,
                                   hdf5_read_next_data, &dsub)) > 0)
    {
      octave_value t2 = dsub.tc;

      Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);

      if (error_state)
        {
          error ("load: internal error loading struct elements");
          return false;
        }

      m.setfield (dsub.name, tcell);

    }

  if (retval2 >= 0)
    {
      map = m;
      retval = true;
    }

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_struct::as_mxArray (void) const
{
  int nf = nfields ();
  string_vector kv = map_keys ();

  OCTAVE_LOCAL_BUFFER (const char *, f, nf);

  for (int i = 0; i < nf; i++)
    f[i] = kv[i].c_str ();

  mxArray *retval = new mxArray (dims (), nf, f);

  mxArray **elts = static_cast<mxArray **> (retval->get_data ());

  mwSize nel = numel ();

  mwSize ntot = nf * nel;

  for (int i = 0; i < nf; i++)
    {
      Cell c = map.contents (kv[i]);

      const octave_value *p = c.data ();

      mwIndex k = 0;
      for (mwIndex j = i; j < ntot; j += nf)
        elts[j] = new mxArray (p[k++]);
    }

  return retval;
}

octave_value
octave_struct::fast_elem_extract (octave_idx_type n) const
{
  if (n < map.numel ())
    return map.checkelem (n);
  else
    return octave_value ();
}

bool
octave_struct::fast_elem_insert (octave_idx_type n,
                                 const octave_value& x)
{
  bool retval = false;

  if (n < map.numel ())
    {
      // To avoid copying the scalar struct, it just stores a pointer to
      // itself.
      const octave_scalar_map *sm_ptr;
      void *here = reinterpret_cast<void *>(&sm_ptr);
      return (x.get_rep ().fast_elem_insert_self (here, btyp_struct)
              && map.fast_elem_insert (n, *sm_ptr));
    }

  return retval;
}

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(octave_scalar_struct, "scalar struct",
                                    "struct");

octave_value
octave_scalar_struct::dotref (const octave_value_list& idx, bool auto_add)
{
  octave_value retval;

  assert (idx.length () == 1);

  std::string nm = idx(0).string_value ();

  maybe_warn_invalid_field_name (nm, "subsref");

  if (error_state)
    return retval;

  retval = map.getfield (nm);

  if (! auto_add && retval.is_undefined ())
    error_with_id ("Octave:invalid-indexing",
                   "structure has no member '%s'", nm.c_str ());

  return retval;
}

octave_value
octave_scalar_struct::subsref (const std::string& type,
                               const std::list<octave_value_list>& idx)
{
  octave_value retval;

  if (type[0] == '.')
    {
      int skip = 1;

      retval = dotref (idx.front ());

      if (idx.size () > 1)
        retval = retval.next_subsref (type, idx, skip);
    }
  else
    retval = to_array ().subsref (type, idx);

  return retval;
}

octave_value_list
octave_scalar_struct::subsref (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               int nargout)
{
  octave_value_list retval;

  if (type[0] == '.')
    {
      int skip = 1;

      retval(0) = dotref (idx.front ());

      if (idx.size () > 1)
        retval = retval(0).next_subsref (nargout, type, idx, skip);
    }
  else
    retval = to_array ().subsref (type, idx, nargout);

  return retval;
}

octave_value
octave_scalar_struct::subsref (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               bool auto_add)
{
  octave_value retval;

  if (type[0] == '.')
    {
      int skip = 1;

      retval = dotref (idx.front (), auto_add);

      if (idx.size () > 1)
        retval = retval.next_subsref (auto_add, type, idx, skip);
    }
  else
    retval = to_array ().subsref (type, idx, auto_add);

  return retval;
}

/*
%!test
%! x(1).a.a = 1;
%! x(2).a.a = 2;
%! assert (size (x), [1, 2]);
%! assert (x(1).a.a, 1);
%! assert (x(2).a.a, 2);
*/

octave_value
octave_scalar_struct::numeric_conv (const octave_value& val,
                                    const std::string& type)
{
  octave_value retval;

  if (type.length () > 0 && type[0] == '.' && ! val.is_map ())
    retval = octave_map ();
  else
    retval = val;

  return retval;
}

octave_value
octave_scalar_struct::subsasgn (const std::string& type,
                                const std::list<octave_value_list>& idx,
                                const octave_value& rhs)
{
  octave_value retval;

  if (idx.front ().empty ())
    {
      error ("missing index in indexed assignment");
      return retval;
    }

  if (type[0] == '.')
    {
      int n = type.length ();

      octave_value t_rhs = rhs;

      octave_value_list key_idx = idx.front ();

      assert (key_idx.length () == 1);

      std::string key = key_idx(0).string_value ();

      maybe_warn_invalid_field_name (key, "subsasgn");

      if (error_state)
        return retval;

      if (n > 1)
        {
          std::list<octave_value_list> next_idx (idx);

          next_idx.erase (next_idx.begin ());

          std::string next_type = type.substr (1);

          octave_value tmp;
          octave_map::iterator pkey = map.seek (key);
          if (pkey != map.end ())
            {
              map.contents (pkey).make_unique ();
              tmp = map.contents (pkey);
            }

          if (! error_state)
            {
              bool orig_undefined = tmp.is_undefined ();

              if (orig_undefined || tmp.is_zero_by_zero ())
                {
                  tmp = octave_value::empty_conv (next_type, rhs);
                  tmp.make_unique (); // probably a no-op.
                }
              else
                // optimization: ignore the copy still stored inside our map.
                tmp.make_unique (1);

              if (! error_state)
                t_rhs = (orig_undefined
                         ? tmp.undef_subsasgn (next_type, next_idx, rhs)
                         : tmp.subsasgn (next_type, next_idx, rhs));
            }
        }

      if (! error_state)
        map.setfield (key, t_rhs.storable_value ());
      else
        gripe_failed_assignment ();

      count++;
      retval = this;
    }
  else
    {
      // Forward this case to octave_struct.
      octave_value tmp (new octave_struct (octave_map (map)));
      retval = tmp.subsasgn (type, idx, rhs);
    }

  return retval;
}

octave_value
octave_scalar_struct::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // octave_map handles indexing itself.
  return octave_map (map).index (idx, resize_ok);
}

size_t
octave_scalar_struct::byte_size (void) const
{
  // Neglect the size of the fieldnames.

  size_t retval = 0;

  for (octave_map::const_iterator p = map.begin (); p != map.end (); p++)
    {
      std::string key = map.key (p);

      octave_value val = octave_value (map.contents (p));

      retval += val.byte_size ();
    }

  return retval;
}

void
octave_scalar_struct::print (std::ostream& os, bool)
{
  print_raw (os);
}

void
octave_scalar_struct::print_raw (std::ostream& os, bool) const
{
  unwind_protect frame;

  frame.protect_var (Vstruct_levels_to_print);

  if (Vstruct_levels_to_print >= 0)
    {
      bool max_depth_reached = Vstruct_levels_to_print-- == 0;

      bool print_fieldnames_only = max_depth_reached;

      increment_indent_level ();

      if (! Vcompact_format)
        newline (os);

      indent (os);
      os << "scalar structure containing the fields:";
      newline (os);
      if (! Vcompact_format)
        newline (os);

      increment_indent_level ();

      string_vector key_list = map.fieldnames ();

      for (octave_idx_type i = 0; i < key_list.length (); i++)
        {
          std::string key = key_list[i];

          octave_value val = map.contents (key);

          if (print_fieldnames_only)
            {
              indent (os);
              os << key;
              dim_vector dv = val.dims ();
              os << ": " << dv.str () << " " << val.type_name ();
              newline (os);
            }
          else
            val.print_with_name (os, key);
        }

      decrement_indent_level ();
      decrement_indent_level ();
    }
  else
    {
      indent (os);
      os << "<structure>";
      newline (os);
    }
}

bool
octave_scalar_struct::print_name_tag (std::ostream& os,
                                      const std::string& name) const
{
  bool retval = false;

  indent (os);

  if (Vstruct_levels_to_print < 0)
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      retval = true;
    }

  return retval;
}

bool
octave_scalar_struct::save_ascii (std::ostream& os)
{
  octave_map m = map_value ();

  octave_idx_type nf = m.nfields ();

  const dim_vector dv = dims ();

  os << "# ndims: " << dv.length () << "\n";

  for (int i = 0; i < dv.length (); i++)
    os << " " << dv (i);
  os << "\n";

  os << "# length: " << nf << "\n";

  // Iterating over the list of keys will preserve the order of the
  // fields.
  string_vector keys = m.fieldnames ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      std::string key = keys(i);

      octave_value val = map.contents (key);

      bool b = save_ascii_data (os, val, key, false, 0);

      if (! b)
        return ! os.fail ();
    }

  return true;
}

bool
octave_scalar_struct::load_ascii (std::istream& is)
{
  bool success = true;
  octave_idx_type len = 0;

  if (extract_keyword (is, "length", len) && len >= 0)
    {
      if (len > 0)
        {
          octave_scalar_map m;

          for (octave_idx_type j = 0; j < len; j++)
            {
              octave_value t2;
              bool dummy;

              // recurse to read cell elements
              std::string nm
                = read_ascii_data (is, std::string (), dummy, t2, j);

              if (!is)
                break;

              if (error_state)
                {
                  error ("load: internal error loading struct elements");
                  return false;
                }

              m.setfield (nm, t2);
            }

          if (is)
            map = m;
          else
            {
              error ("load: failed to load structure");
              success = false;
            }
        }
      else if (len == 0)
        map = octave_scalar_map ();
      else
        panic_impossible ();
    }
  else
    {
      error ("load: failed to extract number of elements in structure");
      success = false;
    }

  return success;
}

bool
octave_scalar_struct::save_binary (std::ostream& os, bool& save_as_floats)
{
  octave_map m = map_value ();

  octave_idx_type nf = m.nfields ();

  int32_t len = nf;
  os.write (reinterpret_cast<char *> (&len), 4);

  // Iterating over the list of keys will preserve the order of the
  // fields.
  string_vector keys = m.fieldnames ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      std::string key = keys(i);

      octave_value val = map.contents (key);

      bool b = save_binary_data (os, val, key, "", 0, save_as_floats);

      if (! b)
        return ! os.fail ();
    }

  return true;
}

bool
octave_scalar_struct::load_binary (std::istream& is, bool swap,
                                   oct_mach_info::float_format fmt)
{
  bool success = true;
  int32_t len;
  if (! is.read (reinterpret_cast<char *> (&len), 4))
    return false;
  if (swap)
    swap_bytes<4> (&len);

  dim_vector dv (1, 1);

  if (len > 0)
    {
      octave_scalar_map m;

      for (octave_idx_type j = 0; j < len; j++)
        {
          octave_value t2;
          bool dummy;
          std::string doc;

          // recurse to read cell elements
          std::string nm = read_binary_data (is, swap, fmt, std::string (),
                                             dummy, t2, doc);

          if (!is)
            break;

          if (error_state)
            {
              error ("load: internal error loading struct elements");
              return false;
            }

          m.setfield (nm, t2);
        }

      if (is)
        map = m;
      else
        {
          error ("load: failed to load structure");
          success = false;
        }
    }
  else if (len == 0)
    map = octave_scalar_map ();
  else
    success = false;

  return success;
}

bool
octave_scalar_struct::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                 bool save_as_floats)
{
#if defined (HAVE_HDF5)

  hid_t data_hid = -1;

#if HAVE_HDF5_18
  data_hid = H5Gcreate (loc_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (data_hid < 0) return false;

  // recursively add each element of the structure to this group
  octave_scalar_map m = scalar_map_value ();

  octave_idx_type nf = m.nfields ();

  // Iterating over the list of keys will preserve the order of the
  // fields.
  string_vector keys = m.fieldnames ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      std::string key = keys(i);

      octave_value val = map.contents (key);

      bool retval2 = add_hdf5_data (data_hid, val, key, "", false,
                                    save_as_floats);

      if (! retval2)
        break;
    }

  H5Gclose (data_hid);

  return true;

#else
  gripe_save ("hdf5");
  return false;
#endif
}

bool
octave_scalar_struct::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hdf5_callback_data dsub;

  herr_t retval2 = 0;
  octave_scalar_map m;
  int current_item = 0;
  hsize_t num_obj = 0;
#if HAVE_HDF5_18
  hid_t group_id = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  hid_t group_id = H5Gopen (loc_id, name);
#endif
  H5Gget_num_objs (group_id, &num_obj);
  H5Gclose (group_id);

  // FIXME: fields appear to be sorted alphabetically on loading.
  // Why is that happening?

  while (current_item < static_cast<int> (num_obj)
         && (retval2 = H5Giterate (loc_id, name, &current_item,
                                   hdf5_read_next_data, &dsub)) > 0)
    {
      octave_value t2 = dsub.tc;

      if (error_state)
        {
          error ("load: internal error loading struct elements");
          return false;
        }

      m.setfield (dsub.name, t2);

    }

  if (retval2 >= 0)
    {
      map = m;
      retval = true;
    }

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_scalar_struct::as_mxArray (void) const
{
  int nf = nfields ();
  string_vector kv = map_keys ();

  OCTAVE_LOCAL_BUFFER (const char *, f, nf);

  for (int i = 0; i < nf; i++)
    f[i] = kv[i].c_str ();

  mxArray *retval = new mxArray (dims (), nf, f);

  mxArray **elts = static_cast<mxArray **> (retval->get_data ());

  mwSize nel = numel ();

  mwSize ntot = nf * nel;

  for (int i = 0; i < nf; i++)
    {
      Cell c = map.contents (kv[i]);

      const octave_value *p = c.data ();

      mwIndex k = 0;
      for (mwIndex j = i; j < ntot; j += nf)
        elts[j] = new mxArray (p[k++]);
    }

  return retval;
}


octave_value
octave_scalar_struct::to_array (void)
{
  return new octave_struct (octave_map (map));
}

bool
octave_scalar_struct::fast_elem_insert_self (void *where,
                                             builtin_type_t btyp) const
{

  if (btyp == btyp_struct)
    {
      *(reinterpret_cast<const octave_scalar_map **>(where)) = &map;
      return true;
    }
  else
    return false;
}

DEFUN (struct, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{s} =} struct ()\n\
@deftypefnx {Built-in Function} {@var{s} =} struct (@var{field1}, @var{value1}, @var{field2}, @var{value2}, @dots{})\n\
@deftypefnx {Built-in Function} {@var{s} =} struct (@var{obj})\n\
\n\
Create a scalar or array structure and initialize its values.\n\
\n\
The @var{field1}, @var{field2}, @dots{} variables are strings specifying the\n\
names of the fields and the @var{value1}, @var{value2}, @dots{} variables\n\
can be of any type.\n\
\n\
If the values are cell arrays, create a structure array and initialize its\n\
values.  The dimensions of each cell array of values must match.  Singleton\n\
cells and non-cell values are repeated so that they fill the entire array. \n\
If the cells are empty, create an empty structure array with the specified\n\
field names.\n\
\n\
If the argument is an object, return the underlying struct.\n\
\n\
Observe that the syntax is optimized for struct @strong{arrays}.  Consider\n\
the following examples:\n\
\n\
@example\n\
@group\n\
struct (\"foo\", 1)\n\
  @result{} scalar structure containing the fields:\n\
    foo =  1\n\
\n\
struct (\"foo\", @{@})\n\
  @result{} 0x0 struct array containing the fields:\n\
    foo\n\
\n\
struct (\"foo\", @{ @{@} @})\n\
  @result{} scalar structure containing the fields:\n\
    foo = @{@}(0x0)\n\
\n\
struct (\"foo\", @{1, 2, 3@})\n\
  @result{} 1x3 struct array containing the fields:\n\
    foo\n\
\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
The first case is an ordinary scalar struct---one field, one value.  The\n\
second produces an empty struct array with one field and no values, since\n\
being passed an empty cell array of struct array values.  When the value is\n\
a cell array containing a single entry, this becomes a scalar struct with\n\
that single entry as the value of the field.  That single entry happens\n\
to be an empty cell array.\n\
\n\
Finally, if the value is a non-scalar cell array, then @code{struct}\n\
produces a struct @strong{array}.\n\
@seealso{cell2struct, fieldnames, getfield, setfield, rmfield, isfield, orderfields, isstruct, structfun}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  // struct ([]) returns an empty struct.

  // struct (empty_matrix) returns an empty struct with the same
  // dimensions as the empty matrix.

  // Note that struct () creates a 1x1 struct with no fields for
  // compatibility with Matlab.

  if (nargin == 1 && args(0).is_map ())
    return args(0);

  if (nargin == 1 && args(0).is_object ())
    {
      retval = args(0).map_value ();

      return retval;
    }

  if ((nargin == 1 || nargin == 2)
      && args(0).is_empty () && args(0).is_real_matrix ())
    {
      Cell fields;

      if (nargin == 2)
        {
          if (args(1).is_cellstr ())
            retval = octave_map (args(0).dims (), args(1).cellstr_value ());
          else
            error ("struct: expecting cell array of field names as second argument");
        }
      else
        retval = octave_map (args(0).dims ());

      return retval;
    }

  // Check for "field", VALUE pairs.

  for (int i = 0; i < nargin; i += 2)
    {
      if (! args(i).is_string () || i + 1 >= nargin)
        {
          error ("struct: expecting alternating \"field\", VALUE pairs");
          return retval;
        }
    }

  // Check that the dimensions of the values correspond.

  dim_vector dims (1, 1);

  int first_dimensioned_value = 0;

  for (int i = 1; i < nargin; i += 2)
    {
      if (args(i).is_cell ())
        {
          dim_vector argdims (args(i).dims ());

          if (! scalar (argdims))
            {
              if (! first_dimensioned_value)
                {
                  dims = argdims;
                  first_dimensioned_value = i + 1;
                }
              else if (dims != argdims)
                {
                  error ("struct: dimensions of parameter %d do not match those of parameter %d",
                         first_dimensioned_value, i+1);
                  return retval;
                }
            }
        }
    }

  // Create the return value.

  octave_map map (dims);

  for (int i = 0; i < nargin; i+= 2)
    {
      // Get key.

      std::string key (args(i).string_value ());

      if (error_state)
        return retval;

      maybe_warn_invalid_field_name (key, "struct");

      if (error_state)
        return retval;

      // Value may be v, { v }, or { v1, v2, ... }
      // In the first two cases, we need to create a cell array of
      // the appropriate dimensions filled with v.  In the last case,
      // the cell array has already been determined to be of the
      // correct dimensions.

      if (args(i+1).is_cell ())
        {
          const Cell c (args(i+1).cell_value ());

          if (error_state)
            return retval;

          if (scalar (c.dims ()))
            map.setfield (key, Cell (dims, c(0)));
          else
            map.setfield (key, c);
        }
      else
        map.setfield (key, Cell (dims, args(i+1)));

      if (error_state)
        return retval;
    }

  return octave_value (map);
}

/*
%!shared x
%! x(1).a=1;  x(2).a=2;  x(1).b=3;  x(2).b=3;
%!assert (struct ("a",1, "b",3), x(1))
%!assert (isempty (x([])))
%!assert (isempty (struct ("a",{}, "b",{})))
%!assert (struct ("a",{1,2}, "b",{3,3}), x)
%!assert (struct ("a",{1,2}, "b",3), x)
%!assert (struct ("a",{1,2}, "b",{3}), x)
%!assert (struct ("b",3, "a",{1,2}), x)
%!assert (struct ("b",{3}, "a",{1,2}), x)
%!test x = struct ([]);
%!assert (size (x), [0,0])
%!assert (isstruct (x))
%!assert (isempty (fieldnames (x)))
%!fail ('struct ("a",{1,2},"b",{1,2,3})', 'dimensions of parameter 2 do not match those of parameter 4')
%!fail ('struct (1,2,3,4)', 'struct: expecting alternating "field", VALUE pairs')
%!fail ('struct ("1",2,"3")', 'struct: expecting alternating "field", VALUE pairs')
*/

DEFUN (isstruct, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isstruct (@var{x})\n\
Return true if @var{x} is a structure or a structure array.\n\
@seealso{ismatrix, iscell, isa}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_map ();
  else
    print_usage ();

  return retval;
}

DEFUN (__fieldnames__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} __fieldnames__ (@var{struct})\n\
@deftypefnx {Built-in Function} {} __fieldnames__ (@var{obj})\n\
Internal function.\n\
\n\
Implements @code{fieldnames()} for structures and Octave objects.\n\
@seealso{fieldnames}\n\
@end deftypefn")
{
  octave_value retval;

  // Input validation has already been done in fieldnames.m.
  octave_value arg = args(0);

  octave_map m = arg.map_value ();

  string_vector keys = m.fieldnames ();

  if (keys.length () == 0)
    retval = Cell (0, 1);
  else
    retval = Cell (keys);

  return retval;
}

DEFUN (isfield, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} isfield (@var{x}, \"@var{name}\")\n\
@deftypefnx {Built-in Function} {} isfield (@var{x}, @var{name})\n\
Return true if the @var{x} is a structure and it includes an element named\n\
@var{name}.\n\
\n\
If @var{name} is a cell array of strings then a logical array of equal\n\
dimension is returned.\n\
@seealso{fieldnames}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      retval = false;

      if (args(0).is_map ())
        {
          octave_map m = args(0).map_value ();

          // FIXME: should this work for all types that can do
          // structure reference operations?

          if (args(1).is_string ())
            {
              std::string key = args(1).string_value ();

              retval = m.isfield (key);
            }
          else if (args(1).is_cell ())
            {
              Cell c = args(1).cell_value ();
              boolNDArray bm (c.dims ());
              octave_idx_type n = bm.numel ();

              for (octave_idx_type i = 0; i < n; i++)
                {
                  if (c(i).is_string ())
                    {
                      std::string key = c(i).string_value ();

                      bm(i) = m.isfield (key);
                    }
                  else
                    bm(i) = false;
                }

              retval = bm;
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (numfields, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} numfields (@var{s})\n\
Return the number of fields of the structure @var{s}.\n\
@seealso{fieldnames}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_map ())
    {
      retval = static_cast<double> (args(0).nfields ());
    }
  else
    print_usage ();

  return retval;
}

/*
## test isfield
%!test
%! x(3).d=1;  x(2).a=2;  x(1).b=3;  x(2).c=3;
%! assert (isfield (x, "b"));
%!assert (isfield (struct ("a", "1"), "a"))
%!assert (isfield ({1}, "c"), false)
%!assert (isfield (struct ("a", "1"), 10), false)
%!assert (isfield (struct ("a", "b"), "a "), false)
%!assert (isfield (struct ("a", 1, "b", 2), {"a", "c"}), [true, false])
*/

DEFUN (cell2struct, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} cell2struct (@var{cell}, @var{fields})\n\
@deftypefnx {Built-in Function} {} cell2struct (@var{cell}, @var{fields}, @var{dim})\n\
Convert @var{cell} to a structure.\n\
\n\
The number of fields in @var{fields} must match the number of elements in\n\
@var{cell} along dimension @var{dim}, that is\n\
@code{numel (@var{fields}) == size (@var{cell}, @var{dim})}.  If @var{dim}\n\
is omitted, a value of 1 is assumed.\n\
\n\
@example\n\
@group\n\
A = cell2struct (@{\"Peter\", \"Hannah\", \"Robert\";\n\
                   185, 170, 168@},\n\
                 @{\"Name\",\"Height\"@}, 1);\n\
A(1)\n\
   @result{}\n\
      @{\n\
        Name   = Peter\n\
        Height = 185\n\
      @}\n\
\n\
@end group\n\
@end example\n\
@seealso{struct2cell, cell2mat, struct}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      if (! args(0).is_cell ())
        {
          error ("cell2struct: argument CELL must be of type cell");
          return retval;
        }

      if (! (args(1).is_cellstr () || args(1).is_char_matrix ()))
        {
          error ("cell2struct: FIELDS must be a cell array of strings or a character matrix");
          return retval;
        }

      const Cell vals = args(0).cell_value ();
      const Array<std::string> fields = args(1).cellstr_value ();

      octave_idx_type ext = 0;

      int dim = 0;

      if (nargin == 3)
        {
          if (args(2).is_real_scalar ())
            {
              dim = nargin == 2 ? 0 : args(2).int_value () - 1;

              if (error_state)
                return retval;
            }
          else
            {
              error ("cell2struct: DIM must be a real scalar");
              return retval;
            }
        }

      if (dim < 0)
        {
          error ("cell2struct: DIM must be a valid dimension");
          return retval;
        }

      ext = vals.ndims () > dim ? vals.dims ()(dim) : 1;

      if (ext != fields.numel ())
        {
          error ("cell2struct: number of FIELDS does not match dimension");
          return retval;
        }

      int nd = std::max (dim+1, vals.ndims ());
      // result dimensions.
      dim_vector rdv = vals.dims ().redim (nd);

      assert (ext == rdv(dim));
      if (nd == 2)
        {
          rdv(0) = rdv(1-dim);
          rdv(1) = 1;
        }
      else
        {
          for (int i =  dim + 1; i < nd; i++)
            rdv(i-1) = rdv(i);

          rdv.resize (nd-1);
        }

      octave_map map (rdv);
      Array<idx_vector> ia (dim_vector (nd, 1), idx_vector::colon);

      for (octave_idx_type i = 0; i < ext; i++)
        {
          ia(dim) = i;
          map.setfield (fields(i), vals.index (ia).reshape (rdv));
        }

      retval = map;
    }
  else
    print_usage ();

  return retval;
}

/*
## test cell2struct versus struct2cell
%!test
%! keys = cellstr (char (floor (rand (100,10)*24+65)))';
%! vals = mat2cell (rand (100,1), ones (100,1), 1)';
%! s = struct ([keys; vals]{:});
%! t = cell2struct (vals, keys, 2);
%! assert (s, t);
%! assert (struct2cell (s), vals');
%! assert (fieldnames (s), keys');

%!assert (cell2struct ({1; 2}, {"a"; "b"}), struct ("a", 1, "b", 2));

%!assert (cell2struct ({}, {"f"}, 3), struct ("f", {}));
*/


// So we can call Fcellstr directly.
extern octave_value_list Fcellstr (const octave_value_list& args, int);

DEFUN (rmfield, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{sout} =} rmfield (@var{s}, \"@var{f}\")\n\
@deftypefnx {Built-in Function} {@var{sout} =} rmfield (@var{s}, @var{f})\n\
Return a @emph{copy} of the structure (array) @var{s} with the field @var{f}\n\
removed.\n\
\n\
If @var{f} is a cell array of strings or a character array, remove each of\n\
the named fields.\n\
@seealso{orderfields, fieldnames, isfield}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_map m = args(0).map_value ();

      octave_value_list fval = Fcellstr (args(1), 1);

      if (! error_state)
        {
          Cell fcell = fval(0).cell_value ();

          for (int i = 0; i < fcell.numel (); i++)
            {
              std::string key = fcell(i).string_value ();

              if (m.isfield (key))
                m.rmfield (key);
              else
                {
                  error ("rmfield: structure does not contain field %s",
                         key.c_str ());

                  break;
                }
            }

          if (! error_state)
            retval = m;
        }
    }
  else
    print_usage ();

  return retval;
}

/*
## test rmfield
%!shared x
%! x(3).d=1;  x(2).a=2;  x(1).b=3;  x(2).c=3;  x(6).f="abc123";
%!
%!test
%! y = rmfield (x, "c");
%! assert (fieldnames (y), {"d"; "a"; "b"; "f"});
%! assert (size (y), [1, 6]);
%!test
%! y = rmfield (x, {"a", "f"});
%! assert (fieldnames (y), {"d"; "b"; "c"});
%! assert (size (y), [1, 6]);
*/

DEFUN (struct_levels_to_print, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} struct_levels_to_print ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} struct_levels_to_print (@var{new_val})\n\
@deftypefnx {Built-in Function} {} struct_levels_to_print (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the number of\n\
structure levels to display.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{print_struct_array_contents}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (struct_levels_to_print, -1,
                                            std::numeric_limits<int>::max ());
}

DEFUN (print_struct_array_contents, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} print_struct_array_contents ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} print_struct_array_contents (@var{new_val})\n\
@deftypefnx {Built-in Function} {} print_struct_array_contents (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies whether to print struct\n\
array contents.\n\
\n\
If true, values of struct array elements are printed.  This variable does\n\
not affect scalar structures whose elements are always printed.  In both\n\
cases, however, printing will be limited to the number of levels specified\n\
by @var{struct_levels_to_print}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{struct_levels_to_print}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (print_struct_array_contents);
}
