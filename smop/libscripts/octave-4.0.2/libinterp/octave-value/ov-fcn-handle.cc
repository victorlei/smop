/*

Copyright (C) 2003-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.
Copyright (C) 2010 Jaroslav Hajek

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
#include <sstream>
#include <vector>

#include "file-ops.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-hdf5.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pr-output.h"
#include "pt-pr-code.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-assign.h"
#include "pt-arg-list.h"
#include "variables.h"
#include "parse.h"
#include "unwind-prot.h"
#include "defaults.h"
#include "file-stat.h"
#include "load-path.h"
#include "oct-env.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
                                     "function handle",
                                     "function_handle");

const std::string octave_fcn_handle::anonymous ("@<anonymous>");

octave_fcn_handle::octave_fcn_handle (const octave_value& f,
                                      const std::string& n)
  : fcn (f), nm (n), has_overloads (false)
{
  octave_user_function *uf = fcn.user_function_value (true);

  if (uf && nm != anonymous)
    symbol_table::cache_name (uf->scope (), nm);

  if (uf && uf->is_nested_function ())
    ::error ("handles to nested functions are not yet supported");
}

octave_value_list
octave_fcn_handle::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int nargout)
{
  return octave_fcn_handle::subsref (type, idx, nargout, 0);
}

octave_value_list
octave_fcn_handle::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int nargout,
                            const std::list<octave_lvalue>* lvalue_list)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
        int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

        retval = do_multi_index_op (tmp_nargout, idx.front (),
                                    idx.size () == 1 ? lvalue_list : 0);
      }
      break;

    case '{':
    case '.':
      {
        std::string tnm = type_name ();
        error ("%s cannot be indexed with %c", tnm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

octave_value_list
octave_fcn_handle::do_multi_index_op (int nargout,
                                      const octave_value_list& args)
{
  return do_multi_index_op (nargout, args, 0);
}

octave_value_list
octave_fcn_handle::do_multi_index_op (int nargout,
                                      const octave_value_list& args,
                                      const std::list<octave_lvalue>* lvalue_list)
{
  octave_value_list retval;

  out_of_date_check (fcn, std::string (), false);

  if (has_overloads)
    {
      // Possibly overloaded function.
      octave_value ov_fcn;

      // Compute dispatch type.
      builtin_type_t btyp;
      std::string dispatch_type = get_dispatch_type (args, btyp);

      // Retrieve overload.
      if (btyp != btyp_unknown)
        {
          out_of_date_check (builtin_overloads[btyp], dispatch_type, false);
          ov_fcn = builtin_overloads[btyp];
        }
      else
        {
          str_ov_map::iterator it = overloads.find (dispatch_type);

          if (it == overloads.end ())
            {
              // Try parent classes too.

              std::list<std::string> plist
                = symbol_table::parent_classes (dispatch_type);

              std::list<std::string>::const_iterator pit = plist.begin ();

              while (pit != plist.end ())
                {
                  std::string pname = *pit;

                  std::string fnm = fcn_name ();

                  octave_value ftmp = symbol_table::find_method (fnm, pname);

                  if (ftmp.is_defined ())
                    {
                      set_overload (pname, ftmp);

                      out_of_date_check (ftmp, pname, false);
                      ov_fcn = ftmp;

                      break;
                    }

                  pit++;
                }
            }
          else
            {
              out_of_date_check (it->second, dispatch_type, false);
              ov_fcn = it->second;
            }
        }

      if (ov_fcn.is_defined ())
        retval = ov_fcn.do_multi_index_op (nargout, args, lvalue_list);
      else if (fcn.is_defined ())
        retval = fcn.do_multi_index_op (nargout, args, lvalue_list);
      else
        error ("%s: no method for class %s",
               nm.c_str (), dispatch_type.c_str ());
    }
  else
    {
      // Non-overloaded function (anonymous, subfunction, private function).
      if (fcn.is_defined ())
        retval = fcn.do_multi_index_op (nargout, args, lvalue_list);
      else
        error ("%s: no longer valid function handle", nm.c_str ());
    }

  return retval;
}

dim_vector
octave_fcn_handle::dims (void) const
{
  static dim_vector dv (1, 1);
  return dv;
}

bool
octave_fcn_handle::is_equal_to (const octave_fcn_handle& h) const
{
  bool retval = fcn.is_copy_of (h.fcn) && (has_overloads == h.has_overloads);
  retval = retval && (overloads.size () == h.overloads.size ());

  if (retval && has_overloads)
    {
      for (int i = 0; i < btyp_num_types && retval; i++)
        retval = builtin_overloads[i].is_copy_of (h.builtin_overloads[i]);

      str_ov_map::const_iterator iter = overloads.begin ();
      str_ov_map::const_iterator hiter = h.overloads.begin ();
      for (; iter != overloads.end () && retval; iter++, hiter++)
        retval = (iter->first == hiter->first)
                 && (iter->second.is_copy_of (hiter->second));
    }

  return retval;
}

bool
octave_fcn_handle::set_fcn (const std::string &octaveroot,
                            const std::string& fpath)
{
  bool success = true;

  if (octaveroot.length () != 0
      && fpath.length () >= octaveroot.length ()
      && fpath.substr (0, octaveroot.length ()) == octaveroot
      && OCTAVE_EXEC_PREFIX != octaveroot)
    {
      // First check if just replacing matlabroot is enough
      std::string str = OCTAVE_EXEC_PREFIX +
                        fpath.substr (octaveroot.length ());
      file_stat fs (str);

      if (fs.exists ())
        {
          size_t xpos = str.find_last_of (file_ops::dir_sep_chars ());

          std::string dir_name = str.substr (0, xpos);

          octave_function *xfcn
            = load_fcn_from_file (str, dir_name, "", "", nm);

          if (xfcn)
            {
              octave_value tmp (xfcn);

              fcn = octave_value (new octave_fcn_handle (tmp, nm));
            }
          else
            {
              error ("function handle points to non-existent function");
              success = false;
            }
        }
      else
        {
          // Next just search for it anywhere in the system path
          string_vector names(3);
          names(0) = nm + ".oct";
          names(1) = nm + ".mex";
          names(2) = nm + ".m";

          dir_path p (load_path::system_path ());

          str = octave_env::make_absolute (p.find_first_of (names));

          size_t xpos = str.find_last_of (file_ops::dir_sep_chars ());

          std::string dir_name = str.substr (0, xpos);

          octave_function *xfcn = load_fcn_from_file (str, dir_name, "", "", nm);

          if (xfcn)
            {
              octave_value tmp (xfcn);

              fcn = octave_value (new octave_fcn_handle (tmp, nm));
            }
          else
            {
              error ("function handle points to non-existent function");
              success = false;
            }
        }
    }
  else
    {
      if (fpath.length () > 0)
        {
          size_t xpos = fpath.find_last_of (file_ops::dir_sep_chars ());

          std::string dir_name = fpath.substr (0, xpos);

          octave_function *xfcn = load_fcn_from_file (fpath, dir_name, "", "", nm);

          if (xfcn)
            {
              octave_value tmp (xfcn);

              fcn = octave_value (new octave_fcn_handle (tmp, nm));
            }
          else
            {
              error ("function handle points to non-existent function");
              success = false;
            }
        }
      else
        {
          fcn = symbol_table::find_function (nm);

          if (! fcn.is_function ())
            {
              error ("function handle points to non-existent function");
              success = false;
            }
        }
    }

  return success;
}

bool
octave_fcn_handle::save_ascii (std::ostream& os)
{
  if (nm == anonymous)
    {
      os << nm << "\n";

      print_raw (os, true);
      os << "\n";

      if (fcn.is_undefined ())
        return false;

      octave_user_function *f = fcn.user_function_value ();

      std::list<symbol_table::symbol_record> vars
        = symbol_table::all_variables (f->scope (), 0);

      size_t varlen = vars.size ();

      if (varlen > 0)
        {
          os << "# length: " << varlen << "\n";

          for (std::list<symbol_table::symbol_record>::const_iterator
               p = vars.begin (); p != vars.end (); p++)
            {
              if (! save_ascii_data (os, p->varval (0), p->name (), false, 0))
                return ! os.fail ();
            }
        }
    }
  else
    {
      octave_function *f = function_value ();
      std::string fnm = f ? f->fcn_file_name () : std::string ();

      os << "# octaveroot: " << OCTAVE_EXEC_PREFIX << "\n";
      if (! fnm.empty ())
        os << "# path: " << fnm << "\n";
      os << nm << "\n";
    }

  return true;
}

bool
octave_fcn_handle::load_ascii (std::istream& is)
{
  bool success = true;

  std::streampos pos = is.tellg ();
  std::string octaveroot = extract_keyword (is, "octaveroot", true);
  if (octaveroot.length () == 0)
    {
      is.seekg (pos);
      is.clear ();
    }
  pos = is.tellg ();
  std::string fpath = extract_keyword (is, "path", true);
  if (fpath.length () == 0)
    {
      is.seekg (pos);
      is.clear ();
    }

  is >> nm;

  if (nm == anonymous)
    {
      skip_preceeding_newline (is);

      std::string buf;

      if (is)
        {

          // Get a line of text whitespace characters included, leaving
          // newline in the stream.
          buf = read_until_newline (is, true);

        }

      pos = is.tellg ();

      unwind_protect_safe frame;

      // Set up temporary scope to use for evaluating the text that
      // defines the anonymous function.

      symbol_table::scope_id local_scope = symbol_table::alloc_scope ();
      frame.add_fcn (symbol_table::erase_scope, local_scope);

      symbol_table::set_scope (local_scope);

      octave_call_stack::push (local_scope, 0);
      frame.add_fcn (octave_call_stack::pop);

      octave_idx_type len = 0;

      if (extract_keyword (is, "length", len, true) && len >= 0)
        {
          if (len > 0)
            {
              for (octave_idx_type i = 0; i < len; i++)
                {
                  octave_value t2;
                  bool dummy;

                  std::string name
                    = read_ascii_data (is, std::string (), dummy, t2, i);

                  if (!is)
                    {
                      error ("load: failed to load anonymous function handle");
                      break;
                    }

                  symbol_table::assign (name, t2, local_scope, 0);
                }
            }
        }
      else
        {
          is.seekg (pos);
          is.clear ();
        }

      if (is && success)
        {
          int parse_status;
          octave_value anon_fcn_handle =
            eval_string (buf, true, parse_status);

          if (parse_status == 0)
            {
              octave_fcn_handle *fh =
                anon_fcn_handle.fcn_handle_value ();

              if (fh)
                {
                  fcn = fh->fcn;

                  octave_user_function *uf = fcn.user_function_value (true);

                  if (uf)
                    symbol_table::cache_name (uf->scope (), nm);
                }
              else
                success = false;
            }
          else
            success = false;
        }
      else
        success = false;
    }
  else
    success = set_fcn (octaveroot, fpath);

  return success;
}

bool
octave_fcn_handle::save_binary (std::ostream& os, bool& save_as_floats)
{
  if (nm == anonymous)
    {
      std::ostringstream nmbuf;

      if (fcn.is_undefined ())
        return false;

      octave_user_function *f = fcn.user_function_value ();

      std::list<symbol_table::symbol_record> vars
        = symbol_table::all_variables (f->scope (), 0);

      size_t varlen = vars.size ();

      if (varlen > 0)
        nmbuf << nm << " " << varlen;
      else
        nmbuf << nm;

      std::string buf_str = nmbuf.str ();
      int32_t tmp = buf_str.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (buf_str.c_str (), buf_str.length ());

      std::ostringstream buf;
      print_raw (buf, true);
      std::string stmp = buf.str ();
      tmp = stmp.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (stmp.c_str (), stmp.length ());

      if (varlen > 0)
        {
          for (std::list<symbol_table::symbol_record>::const_iterator
               p = vars.begin (); p != vars.end (); p++)
            {
              if (! save_binary_data (os, p->varval (0), p->name (),
                                      "", 0, save_as_floats))
                return ! os.fail ();
            }
        }
    }
  else
    {
      std::ostringstream nmbuf;

      octave_function *f = function_value ();
      std::string fnm = f ? f->fcn_file_name () : std::string ();

      nmbuf << nm << "\n" << OCTAVE_EXEC_PREFIX << "\n" << fnm;

      std::string buf_str = nmbuf.str ();
      int32_t tmp = buf_str.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (buf_str.c_str (), buf_str.length ());
    }

  return true;
}

bool
octave_fcn_handle::load_binary (std::istream& is, bool swap,
                                oct_mach_info::float_format fmt)
{
  bool success = true;

  int32_t tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;
  if (swap)
    swap_bytes<4> (&tmp);

  OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
  // is.get (ctmp1, tmp+1, 0); caused is.eof () to be true though
  // effectively not reading over file end
  is.read (ctmp1, tmp);
  ctmp1[tmp] = 0;
  nm = std::string (ctmp1);

  if (! is)
    return false;

  size_t anl = anonymous.length ();

  if (nm.length () >= anl && nm.substr (0, anl) == anonymous)
    {
      octave_idx_type len = 0;

      if (nm.length () > anl)
        {
          std::istringstream nm_is (nm.substr (anl));
          nm_is >> len;
          nm = nm.substr (0, anl);
        }

      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
      // is.get (ctmp2, tmp+1, 0); caused is.eof () to be true though
      // effectively not reading over file end
      is.read (ctmp2, tmp);
      ctmp2[tmp] = 0;

      unwind_protect_safe frame;

      // Set up temporary scope to use for evaluating the text that
      // defines the anonymous function.

      symbol_table::scope_id local_scope = symbol_table::alloc_scope ();
      frame.add_fcn (symbol_table::erase_scope, local_scope);

      symbol_table::set_scope (local_scope);

      octave_call_stack::push (local_scope, 0);
      frame.add_fcn (octave_call_stack::pop);

      if (len > 0)
        {
          for (octave_idx_type i = 0; i < len; i++)
            {
              octave_value t2;
              bool dummy;
              std::string doc;

              std::string name =
                read_binary_data (is, swap, fmt, std::string (),
                                  dummy, t2, doc);

              if (!is)
                {
                  error ("load: failed to load anonymous function handle");
                  break;
                }

              symbol_table::assign (name, t2, local_scope);
            }
        }

      if (is && success)
        {
          int parse_status;
          octave_value anon_fcn_handle =
            eval_string (ctmp2, true, parse_status);

          if (parse_status == 0)
            {
              octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();

              if (fh)
                {
                  fcn = fh->fcn;

                  octave_user_function *uf = fcn.user_function_value (true);

                  if (uf)
                    symbol_table::cache_name (uf->scope (), nm);
                }
              else
                success = false;
            }
          else
            success = false;
        }
    }
  else
    {
      std::string octaveroot;
      std::string fpath;

      if (nm.find_first_of ("\n") != std::string::npos)
        {
          size_t pos1 = nm.find_first_of ("\n");
          size_t pos2 = nm.find_first_of ("\n", pos1 + 1);
          octaveroot = nm.substr (pos1 + 1, pos2 - pos1 - 1);
          fpath = nm.substr (pos2 + 1);
          nm = nm.substr (0, pos1);
        }

      success = set_fcn (octaveroot, fpath);
    }

  return success;
}

bool
octave_fcn_handle::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                              bool save_as_floats)
{
#if defined (HAVE_HDF5)

  bool retval = true;

  hid_t group_hid = -1;
#if HAVE_HDF5_18
  group_hid = H5Gcreate (loc_id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    return false;

  hid_t space_hid, data_hid, type_hid;
  space_hid = data_hid = type_hid = -1;

  // attach the type of the variable
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, nm.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, 2);
  hdims[0] = 0;
  hdims[1] = 0;
  space_hid = H5Screate_simple (0 , hdims, 0);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid, H5P_DEFAULT);
#endif
  if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
                                H5P_DEFAULT, nm.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  if (nm == anonymous)
    {
      std::ostringstream buf;
      print_raw (buf, true);
      std::string stmp = buf.str ();

      // attach the type of the variable
      H5Tset_size (type_hid, stmp.length () + 1);
      if (type_hid < 0)
        {
          H5Sclose (space_hid);
          H5Gclose (group_hid);
          return false;
        }

#if HAVE_HDF5_18
      data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
      data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
                            H5P_DEFAULT);
#endif
      if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
                                    H5P_DEFAULT, stmp.c_str ()) < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Dclose (data_hid);

      octave_user_function *f = fcn.user_function_value ();

      std::list<symbol_table::symbol_record> vars
        = symbol_table::all_variables (f->scope (), 0);

      size_t varlen = vars.size ();

      if (varlen > 0)
        {
          hid_t as_id = H5Screate (H5S_SCALAR);

          if (as_id >= 0)
            {
#if HAVE_HDF5_18
              hid_t a_id = H5Acreate (group_hid, "SYMBOL_TABLE",
                                      H5T_NATIVE_IDX, as_id,
                                      H5P_DEFAULT, H5P_DEFAULT);

#else
              hid_t a_id = H5Acreate (group_hid, "SYMBOL_TABLE",
                                      H5T_NATIVE_IDX, as_id, H5P_DEFAULT);
#endif

              if (a_id >= 0)
                {
                  retval = (H5Awrite (a_id, H5T_NATIVE_IDX, &varlen) >= 0);

                  H5Aclose (a_id);
                }
              else
                retval = false;

              H5Sclose (as_id);
            }
          else
            retval = false;
#if HAVE_HDF5_18
          data_hid = H5Gcreate (group_hid, "symbol table",
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
          data_hid = H5Gcreate (group_hid, "symbol table", 0);
#endif
          if (data_hid < 0)
            {
              H5Sclose (space_hid);
              H5Tclose (type_hid);
              H5Gclose (group_hid);
              return false;
            }

          for (std::list<symbol_table::symbol_record>::const_iterator
               p = vars.begin (); p != vars.end (); p++)
            {
              if (! add_hdf5_data (data_hid, p->varval (0), p->name (),
                                   "", false, save_as_floats))
                break;
            }
          H5Gclose (data_hid);
        }
    }
  else
    {
      std::string octaveroot = OCTAVE_EXEC_PREFIX;

      octave_function *f = function_value ();
      std::string fpath = f ? f->fcn_file_name () : std::string ();

      H5Sclose (space_hid);
      hdims[0] = 1;
      hdims[1] = octaveroot.length ();
      space_hid = H5Screate_simple (0 , hdims, 0);
      if (space_hid < 0)
        {
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (type_hid);
      type_hid = H5Tcopy (H5T_C_S1);
      H5Tset_size (type_hid, octaveroot.length () + 1);
#if HAVE_HDF5_18
      hid_t a_id = H5Acreate (group_hid, "OCTAVEROOT",
                              type_hid, space_hid, H5P_DEFAULT, H5P_DEFAULT);
#else
      hid_t a_id = H5Acreate (group_hid, "OCTAVEROOT",
                              type_hid, space_hid, H5P_DEFAULT);
#endif

      if (a_id >= 0)
        {
          retval = (H5Awrite (a_id, type_hid, octaveroot.c_str ()) >= 0);

          H5Aclose (a_id);
        }
      else
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Sclose (space_hid);
      hdims[0] = 1;
      hdims[1] = fpath.length ();
      space_hid = H5Screate_simple (0 , hdims, 0);
      if (space_hid < 0)
        {
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (type_hid);
      type_hid = H5Tcopy (H5T_C_S1);
      H5Tset_size (type_hid, fpath.length () + 1);

#if HAVE_HDF5_18
      a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT);
#else
      a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid, H5P_DEFAULT);
#endif

      if (a_id >= 0)
        {
          retval = (H5Awrite (a_id, type_hid, fpath.c_str ()) >= 0);

          H5Aclose (a_id);
        }
      else
        retval = false;
    }

  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;

#else
  gripe_save ("hdf5");
  return false;
#endif
}

bool
octave_fcn_handle::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
#if defined (HAVE_HDF5)

  bool success = true;

  hid_t group_hid, data_hid, space_hid, type_hid, type_class_hid, st_id;
  hsize_t rank;
  int slen;

#if HAVE_HDF5_18
  group_hid = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0)
    return false;

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "nm", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nm");
#endif

  if (data_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  type_hid = H5Dget_type (data_hid);
  type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, nm_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, nm_tmp) < 0)
    {
      H5Tclose (st_id);
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);
  nm = nm_tmp;

  if (nm == anonymous)
    {
#if HAVE_HDF5_18
      data_hid = H5Dopen (group_hid, "fcn", H5P_DEFAULT);
#else
      data_hid = H5Dopen (group_hid, "fcn");
#endif

      if (data_hid < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (type_hid);
      type_hid = H5Dget_type (data_hid);
      type_class_hid = H5Tget_class (type_hid);

      if (type_class_hid != H5T_STRING)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Sclose (space_hid);
      space_hid = H5Dget_space (data_hid);
      rank = H5Sget_simple_extent_ndims (space_hid);

      if (rank != 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      slen = H5Tget_size (type_hid);
      if (slen < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      OCTAVE_LOCAL_BUFFER (char, fcn_tmp, slen);

      // create datatype for (null-terminated) string to read into:
      st_id = H5Tcopy (H5T_C_S1);
      H5Tset_size (st_id, slen);

      if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, fcn_tmp) < 0)
        {
          H5Tclose (st_id);
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }
      H5Tclose (st_id);
      H5Dclose (data_hid);

      octave_idx_type len = 0;

      // we have to pull some shenanigans here to make sure
      // HDF5 doesn't print out all sorts of error messages if we
      // call H5Aopen for a non-existing attribute

      H5E_auto_t err_func;
      void *err_func_data;

      // turn off error reporting temporarily, but save the error
      // reporting function:
#if HAVE_HDF5_18
      H5Eget_auto (H5E_DEFAULT, &err_func, &err_func_data);
      H5Eset_auto (H5E_DEFAULT, 0, 0);
#else
      H5Eget_auto (&err_func, &err_func_data);
      H5Eset_auto (0, 0);
#endif

      hid_t attr_id = H5Aopen_name (group_hid, "SYMBOL_TABLE");

      if (attr_id >= 0)
        {
          if (H5Aread (attr_id, H5T_NATIVE_IDX, &len) < 0)
            success = false;

          H5Aclose (attr_id);
        }

      // restore error reporting:
#if HAVE_HDF5_18
      H5Eset_auto (H5E_DEFAULT, err_func, err_func_data);
#else
      H5Eset_auto (err_func, err_func_data);
#endif

      unwind_protect_safe frame;

      // Set up temporary scope to use for evaluating the text that
      // defines the anonymous function.

      symbol_table::scope_id local_scope = symbol_table::alloc_scope ();
      frame.add_fcn (symbol_table::erase_scope, local_scope);

      symbol_table::set_scope (local_scope);

      octave_call_stack::push (local_scope, 0);
      frame.add_fcn (octave_call_stack::pop);

      if (len > 0 && success)
        {
          hsize_t num_obj = 0;
#if HAVE_HDF5_18
          data_hid = H5Gopen (group_hid, "symbol table", H5P_DEFAULT);
#else
          data_hid = H5Gopen (group_hid, "symbol table");
#endif
          H5Gget_num_objs (data_hid, &num_obj);
          H5Gclose (data_hid);

          if (num_obj != static_cast<hsize_t>(len))
            {
              error ("load: failed to load anonymous function handle");
              success = false;
            }

          if (! error_state)
            {
              hdf5_callback_data dsub;
              int current_item = 0;
              for (octave_idx_type i = 0; i < len; i++)
                {
                  if (H5Giterate (group_hid, "symbol table", &current_item,
                                  hdf5_read_next_data, &dsub) <= 0)
                    {
                      error ("load: failed to load anonymous function handle");
                      success = false;
                      break;
                    }

                  symbol_table::assign (dsub.name, dsub.tc, local_scope);
                }
            }
        }

      if (success)
        {
          int parse_status;
          octave_value anon_fcn_handle =
            eval_string (fcn_tmp, true, parse_status);

          if (parse_status == 0)
            {
              octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();

              if (fh)
                {
                  fcn = fh->fcn;

                  octave_user_function *uf = fcn.user_function_value (true);

                  if (uf)
                    symbol_table::cache_name (uf->scope (), nm);
                }
              else
                success = false;
            }
          else
            success = false;
        }

      frame.run ();
    }
  else
    {
      std::string octaveroot;
      std::string fpath;

      // we have to pull some shenanigans here to make sure
      // HDF5 doesn't print out all sorts of error messages if we
      // call H5Aopen for a non-existing attribute

      H5E_auto_t err_func;
      void *err_func_data;

      // turn off error reporting temporarily, but save the error
      // reporting function:
#if HAVE_HDF5_18
      H5Eget_auto (H5E_DEFAULT, &err_func, &err_func_data);
      H5Eset_auto (H5E_DEFAULT, 0, 0);
#else
      H5Eget_auto (&err_func, &err_func_data);
      H5Eset_auto (0, 0);
#endif

      hid_t attr_id = H5Aopen_name (group_hid, "OCTAVEROOT");
      if (attr_id >= 0)
        {
          H5Tclose (type_hid);
          type_hid = H5Aget_type (attr_id);
          type_class_hid = H5Tget_class (type_hid);

          if (type_class_hid != H5T_STRING)
            success = false;
          else
            {
              slen = H5Tget_size (type_hid);
              st_id = H5Tcopy (H5T_C_S1);
              H5Tset_size (st_id, slen);
              OCTAVE_LOCAL_BUFFER (char, root_tmp, slen);

              if (H5Aread (attr_id, st_id, root_tmp) < 0)
                success = false;
              else
                octaveroot = root_tmp;

              H5Tclose (st_id);
            }

          H5Aclose (attr_id);
        }

      if (success)
        {
          attr_id = H5Aopen_name (group_hid, "FILE");
          if (attr_id >= 0)
            {
              H5Tclose (type_hid);
              type_hid = H5Aget_type (attr_id);
              type_class_hid = H5Tget_class (type_hid);

              if (type_class_hid != H5T_STRING)
                success = false;
              else
                {
                  slen = H5Tget_size (type_hid);
                  st_id = H5Tcopy (H5T_C_S1);
                  H5Tset_size (st_id, slen);
                  OCTAVE_LOCAL_BUFFER (char, path_tmp, slen);

                  if (H5Aread (attr_id, st_id, path_tmp) < 0)
                    success = false;
                  else
                    fpath = path_tmp;

                  H5Tclose (st_id);
                }

              H5Aclose (attr_id);
            }
        }

      // restore error reporting:
#if HAVE_HDF5_18
      H5Eset_auto (H5E_DEFAULT, err_func, err_func_data);
#else
      H5Eset_auto (err_func, err_func_data);
#endif

      success = (success ? set_fcn (octaveroot, fpath) : success);
    }

  H5Tclose (type_hid);
  H5Sclose (space_hid);
  H5Gclose (group_hid);

  return success;

#else
  gripe_load ("hdf5");
  return false;
#endif
}

/*
%!test
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! hm = @version;
%! hdld = @svd;
%! hbi = @log2;
%! f2 = f;
%! g2 = g;
%! hm2 = hm;
%! hdld2 = hdld;
%! hbi2 = hbi;
%! modes = {"-text", "-binary"};
%! if (isfield (octave_config_info, "HAVE_HDF5")
%!     && octave_config_info ("HAVE_HDF5"))
%!   modes(end+1) = "-hdf5";
%! endif
%! for i = 1:numel (modes)
%!   mode = modes{i};
%!   nm = tempname ();
%!   unwind_protect
%!     f2 (1); # bug #33857
%!     save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!     clear f2 g2 hm2 hdld2 hbi2
%!     load (nm);
%!     assert (f (2), f2 (2));
%!     assert (g (2), g2 (2));
%!     assert (g (3), g2 (3));
%!     unlink (nm);
%!     save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endfor
*/

/*
%!function fcn_handle_save_recurse (n, mode, nm, f2, g2, hm2, hdld2, hbi2)
%!  if (n == 0)
%!    save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!  else
%!    fcn_handle_save_recurse (n - 1, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!  endif
%!endfunction
%!function [f2, g2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (n, nm)
%!  if (n == 0)
%!    load (nm)
%!  else
%!    [f2, g2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (n - 1, nm);
%!  endif
%!endfunction

Test for bug #35876
%!test
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! hm = @version;
%! hdld = @svd;
%! hbi = @log2;
%! f2 = f;
%! g2 = g;
%! hm2 = hm;
%! hdld2 = hdld;
%! hbi2 = hbi;
%! modes = {"-text", "-binary"};
%! if (isfield (octave_config_info, "HAVE_HDF5")
%!     && octave_config_info ("HAVE_HDF5"))
%!   modes(end+1) = "-hdf5";
%! endif
%! for i = 1:numel (modes)
%!   mode = modes{i};
%!   nm = tempname ();
%!   unwind_protect
%!     fcn_handle_save_recurse (2, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!     clear f2 g2 hm2 hdld2 hbi2
%!     [f2, f2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (2, nm);
%!     load (nm);
%!     assert (f (2), f2 (2));
%!     assert (g (2), g2 (2));
%!     assert (g (3), g2 (3));
%!     unlink (nm);
%!     fcn_handle_save_recurse (2, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endfor
*/

void
octave_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  bool printed = false;

  if (nm == anonymous)
    {
      tree_print_code tpc (os);

      // FCN is const because this member function is, so we can't
      // use it to call user_function_value, so we make a copy first.

      octave_value ftmp = fcn;

      octave_user_function *f = ftmp.user_function_value ();

      if (f)
        {
          tree_parameter_list *p = f->parameter_list ();

          os << "@(";

          if (p)
            p->accept (tpc);

          os << ") ";

          tpc.print_fcn_handle_body (f->body ());

          printed = true;
        }
    }

  if (! printed)
    octave_print_internal (os, "@" + nm, pr_as_read_syntax,
                           current_print_indent_level ());
}

octave_value
make_fcn_handle (const std::string& nm, bool local_funcs)
{
  octave_value retval;

  // Bow to the god of compatibility.

  // FIXME: it seems ugly to put this here, but there is no single
  // function in the parser that converts from the operator name to
  // the corresponding function name.  At least try to do it without N
  // string compares.

  std::string tnm = nm;

  size_t len = nm.length ();

  if (len == 3 && nm == ".**")
    tnm = "power";
  else if (len == 2)
    {
      if (nm[0] == '.')
        {
          switch (nm[1])
            {
            case '\'':
              tnm = "transpose";
              break;

            case '+':
              tnm = "plus";
              break;

            case '-':
              tnm = "minus";
              break;

            case '*':
              tnm = "times";
              break;

            case '/':
              tnm = "rdivide";
              break;

            case '^':
              tnm = "power";
              break;

            case '\\':
              tnm = "ldivide";
              break;
            }
        }
      else if (nm[1] == '=')
        {
          switch (nm[0])
            {
            case '<':
              tnm = "le";
              break;

            case '=':
              tnm = "eq";
              break;

            case '>':
              tnm = "ge";
              break;

            case '~':
            case '!':
              tnm = "ne";
              break;
            }
        }
      else if (nm == "**")
        tnm = "mpower";
    }
  else if (len == 1)
    {
      switch (nm[0])
        {
        case '~':
        case '!':
          tnm = "not";
          break;

        case '\'':
          tnm = "ctranspose";
          break;

        case '+':
          tnm = "plus";
          break;

        case '-':
          tnm = "minus";
          break;

        case '*':
          tnm = "mtimes";
          break;

        case '/':
          tnm = "mrdivide";
          break;

        case '^':
          tnm = "mpower";
          break;

        case '\\':
          tnm = "mldivide";
          break;

        case '<':
          tnm = "lt";
          break;

        case '>':
          tnm = "gt";
          break;

        case '&':
          tnm = "and";
          break;

        case '|':
          tnm = "or";
          break;
        }
    }

  octave_value f = symbol_table::find_function (tnm, octave_value_list (),
                                                local_funcs);

  octave_function *fptr = f.function_value (true);

  // Here we are just looking to see if FCN is a method or constructor
  // for any class.
  if (local_funcs && fptr
      && (fptr->is_subfunction () || fptr->is_private_function ()
          || fptr->is_class_constructor ()
          || fptr->is_classdef_constructor ()))
    {
      // Locally visible function.
      retval = octave_value (new octave_fcn_handle (f, tnm));
    }
  else
    {
      // Globally visible (or no match yet). Query overloads.
      std::list<std::string> classes = load_path::overloads (tnm);
      bool any_match = fptr != 0 || classes.size () > 0;
      if (! any_match)
        {
          // No match found, try updating load_path and query classes again.
          load_path::update ();
          classes = load_path::overloads (tnm);
          any_match = classes.size () > 0;
        }

      if (any_match)
        {
          octave_fcn_handle *fh = new octave_fcn_handle (f, tnm);
          retval = fh;

          for (std::list<std::string>::iterator iter = classes.begin ();
               iter != classes.end (); iter++)
            {
              std::string class_name = *iter;
              octave_value fmeth = symbol_table::find_method (tnm, class_name);

              bool is_builtin = false;
              for (int i = 0; i < btyp_num_types; i++)
                {
                  // FIXME: Too slow? Maybe binary lookup?
                  if (class_name == btyp_class_name[i])
                    {
                      is_builtin = true;
                      fh->set_overload (static_cast<builtin_type_t> (i), fmeth);
                    }
                }

              if (! is_builtin)
                fh->set_overload (class_name, fmeth);
            }
        }
      else
        error ("@%s: no function and no method found", tnm.c_str ());
    }

  return retval;
}

/*
%!test
%! x = {".**", "power";
%!      ".'", "transpose";
%!      ".+", "plus";
%!      ".-", "minus";
%!      ".*", "times";
%!      "./", "rdivide";
%!      ".^", "power";
%!      ".\\", "ldivide";
%!      "<=", "le";
%!      "==", "eq";
%!      ">=", "ge";
%!      "~=", "ne";
%!      "!=", "ne";
%!      "**", "mpower";
%!      "~", "not";
%!      "!", "not";
%!      "\'", "ctranspose";
%!      "+", "plus";
%!      "-", "minus";
%!      "*", "mtimes";
%!      "/", "mrdivide";
%!      "^", "mpower";
%!      "\\", "mldivide";
%!      "<", "lt";
%!      ">", "gt";
%!      "&", "and";
%!      "|", "or"};
%! for i = 1:rows (x)
%!   assert (functions (str2func (x{i,1})).function, x{i,2});
%! endfor
*/

DEFUN (functions, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{s} =} functions (@var{fcn_handle})\n\
Return a structure containing information about the function handle\n\
@var{fcn_handle}.\n\
\n\
The structure @var{s} always contains these three fields:\n\
\n\
@table @asis\n\
@item function\n\
The function name.  For an anonymous function (no name) this will be the\n\
actual function definition.\n\
\n\
@item type\n\
Type of the function.\n\
\n\
@table @asis\n\
@item anonymous\n\
The function is anonymous.\n\
\n\
@item private\n\
The function is private.\n\
\n\
@item overloaded\n\
The function overloads an existing function.\n\
\n\
@item simple\n\
The function is a built-in or m-file function.\n\
\n\
@item subfunction\n\
The function is a subfunction within an m-file.\n\
@end table\n\
\n\
@item file\n\
The m-file that will be called to perform the function.  This field is empty\n\
for anonymous and built-in functions.\n\
@end table\n\
\n\
In addition, some function types may return more information in additional\n\
fields.\n\
\n\
@strong{Warning:} @code{functions} is provided for debugging purposes only.\n\
It's behavior may change in the future and programs should not depend on a\n\
particular output.\n\
\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_fcn_handle *fh = args(0).fcn_handle_value ();

      if (! error_state)
        {
          octave_function *fcn = fh ? fh->function_value () : 0;

          if (fcn)
            {
              octave_scalar_map m;

              std::string fh_nm = fh->fcn_name ();

              if (fh_nm == octave_fcn_handle::anonymous)
                {
                  std::ostringstream buf;
                  fh->print_raw (buf);
                  m.setfield ("function", buf.str ());

                  m.setfield ("type", "anonymous");
                }
              else
                {
                  m.setfield ("function", fh_nm);

                  if (fcn->is_subfunction ())
                    {
                      m.setfield ("type", "subfunction");
                      Cell parentage (dim_vector (1, 2));
                      parentage.elem (0) = fh_nm;
                      parentage.elem (1) = fcn->parent_fcn_name ();
                      m.setfield ("parentage", octave_value (parentage));
                    }
                  else if (fcn->is_private_function ())
                    m.setfield ("type", "private");
                  else if (fh->is_overloaded ())
                    m.setfield ("type", "overloaded");
                  else
                    m.setfield ("type", "simple");
                }

              std::string nm = fcn->fcn_file_name ();

              if (fh_nm == octave_fcn_handle::anonymous)
                {
                  m.setfield ("file", nm);

                  octave_user_function *fu = fh->user_function_value ();

                  std::list<symbol_table::symbol_record> vars
                    = symbol_table::all_variables (fu->scope (), 0);

                  size_t varlen = vars.size ();

                  if (varlen > 0)
                    {
                      octave_scalar_map ws;
                      for (std::list<symbol_table::symbol_record>::const_iterator
                           p = vars.begin (); p != vars.end (); p++)
                        {
                          ws.assign (p->name (), p->varval (0));
                        }

                      m.setfield ("workspace", ws);
                    }
                }
              else if (fcn->is_user_function () || fcn->is_user_script ())
                {
                  octave_function *fu = fh->function_value ();
                  m.setfield ("file", fu->fcn_file_name ());
                }
              else
                m.setfield ("file", "");

              retval = m;
            }
          else
            error ("functions: FCN_HANDLE is not a valid function handle object");
        }
      else
        error ("functions: FCN_HANDLE argument must be a function handle object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (func2str, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} func2str (@var{fcn_handle})\n\
Return a string containing the name of the function referenced by the\n\
function handle @var{fcn_handle}.\n\
@seealso{str2func, functions}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_fcn_handle *fh = args(0).fcn_handle_value ();

      if (! error_state && fh)
        {
          std::string fh_nm = fh->fcn_name ();

          if (fh_nm == octave_fcn_handle::anonymous)
            {
              std::ostringstream buf;

              fh->print_raw (buf);

              retval = buf.str ();
            }
          else
            retval = fh_nm;
        }
      else
        error ("func2str: FCN_HANDLE must be a valid function handle");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (str2func, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} str2func (@var{fcn_name})\n\
@deftypefnx {Built-in Function} {} str2func (@var{fcn_name}, \"global\")\n\
Return a function handle constructed from the string @var{fcn_name}.\n\
\n\
If the optional @qcode{\"global\"} argument is passed, locally visible\n\
functions are ignored in the lookup.\n\
\n\
Note: @code{str2func} does not currently accept strings which define\n\
anonymous functions (those which begin with @samp{@@}).\n\
Use @w{@code{eval (@var{str})}} as a replacement.\n\
@seealso{func2str, inline}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      if (args(0).is_string ())
        {
          std::string nm = args(0).string_value ();
          retval = make_fcn_handle (nm, nargin != 2);
        }
      else
        error ("str2func: FCN_NAME must be a string");
    }
  else
    print_usage ();

  return retval;
}

/*
%!function y = __testrecursionfunc (f, x, n)
%!  if (nargin < 3)
%!    n = 0;
%!  endif
%!  if (n > 2)
%!    y = f (x);
%!  else
%!    n++;
%!    y = __testrecursionfunc (@(x) f (2*x), x, n);
%!  endif
%!endfunction
%!
%!assert (__testrecursionfunc (@(x) x, 1), 8)
*/

DEFUN (is_function_handle, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} is_function_handle (@var{x})\n\
Return true if @var{x} is a function handle.\n\
@seealso{isa, typeinfo, class, functions}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    retval = args(0).is_function_handle ();
  else
    print_usage ();

  return retval;
}

/*
%!shared fh
%! fh = @(x) x;

%!assert (is_function_handle (fh))
%!assert (! is_function_handle ({fh}))
%!assert (! is_function_handle (1))

%!error is_function_handle ()
%!error is_function_handle (1, 2)
*/

octave_fcn_binder::octave_fcn_binder (const octave_value& f,
                                      const octave_value& root,
                                      const octave_value_list& templ,
                                      const std::vector<int>& mask,
                                      int exp_nargin)
  : octave_fcn_handle (f), root_handle (root), arg_template (templ),
    arg_mask (mask), expected_nargin (exp_nargin)
{
}

octave_fcn_handle *
octave_fcn_binder::maybe_binder (const octave_value& f)
{
  octave_fcn_handle *retval = 0;

  octave_user_function *usr_fcn = f.user_function_value (false);
  tree_parameter_list *param_list = usr_fcn ? usr_fcn->parameter_list () : 0;

  tree_statement_list *cmd_list = 0;
  tree_expression *body_expr = 0;

  if (usr_fcn)
    {
      cmd_list = usr_fcn->body ();
      if (cmd_list)
        {
          // Verify that body is a single expression (always true in theory).
          body_expr = (cmd_list->length () == 1
                       ? cmd_list->front ()->expression () : 0);
        }
    }

  if (body_expr && body_expr->is_index_expression ()
      && ! (param_list && param_list->takes_varargs ()))
    {
      // It's an index expression.
      tree_index_expression *idx_expr = dynamic_cast<tree_index_expression *>
                                        (body_expr);
      tree_expression *head_expr = idx_expr->expression ();
      std::list<tree_argument_list *> arg_lists = idx_expr->arg_lists ();
      std::string type_tags = idx_expr->type_tags ();

      if (type_tags.length () == 1 && type_tags[0] == '('
          && head_expr->is_identifier ())
        {
          assert (arg_lists.size () == 1);

          // It's a single index expression: a(x,y,....)
          tree_identifier *head_id =
            dynamic_cast<tree_identifier *> (head_expr);
          tree_argument_list *arg_list = arg_lists.front ();

          // Build a map of input params to their position.
          std::map<std::string, int> arginmap;
          int npar = 0;

          if (param_list)
            {
              for (tree_parameter_list::iterator it = param_list->begin ();
                   it != param_list->end (); ++it, ++npar)
                {
                  tree_decl_elt *elt = *it;
                  tree_identifier *id = elt ? elt->ident () : 0;
                  if (id && ! id->is_black_hole ())
                    arginmap[id->name ()] = npar;
                }
            }

          if (arg_list && arg_list->length () > 0)
            {
              bool bad = false;
              int nargs = arg_list->length ();
              octave_value_list arg_template (nargs);
              std::vector<int> arg_mask (nargs);

              // Verify that each argument is either a named param, a constant,
              // or a defined identifier.
              int iarg = 0;
              for (tree_argument_list::iterator it = arg_list->begin ();
                   it != arg_list->end (); ++it, ++iarg)
                {
                  tree_expression *elt = *it;
                  if (elt && elt->is_constant ())
                    {
                      arg_template(iarg) = elt->rvalue1 ();
                      arg_mask[iarg] = -1;
                    }
                  else if (elt && elt->is_identifier ())
                    {
                      tree_identifier *elt_id =
                        dynamic_cast<tree_identifier *> (elt);
                      if (arginmap.find (elt_id->name ()) != arginmap.end ())
                        {
                          arg_mask[iarg] = arginmap[elt_id->name ()];
                        }
                      else if (elt_id->is_defined ())
                        {
                          arg_template(iarg) = elt_id->rvalue1 ();
                          arg_mask[iarg] = -1;
                        }
                      else
                        {
                          bad = true;
                          break;
                        }
                    }
                  else
                    {
                      bad = true;
                      break;
                    }
                }

              octave_value root_val;

              if (! bad)
                {
                  // If the head is a value, use it as root.
                  if (head_id->is_defined ())
                    root_val = head_id->rvalue1 ();
                  else
                    {
                      // It's a name.
                      std::string head_name = head_id->name ();
                      // Function handles can't handle legacy dispatch, so
                      // we make sure it's not defined.
                      if (symbol_table::get_dispatch (head_name).size () > 0)
                        bad = true;
                      else
                        {
                          // Simulate try/catch.
                          unwind_protect frame;
                          interpreter_try (frame);

                          root_val = make_fcn_handle (head_name);
                          if (error_state)
                            bad = true;
                        }
                    }
                }

              if (! bad)
                {
                  // Stash proper name tags.
                  std::list<string_vector> arg_names = idx_expr->arg_names ();
                  assert (arg_names.size () == 1);
                  arg_template.stash_name_tags (arg_names.front ());

                  retval = new octave_fcn_binder (f, root_val, arg_template,
                                                  arg_mask, npar);
                }
            }
        }
    }

  if (! retval)
    retval = new octave_fcn_handle (f, octave_fcn_handle::anonymous);

  return retval;
}

octave_value_list
octave_fcn_binder::do_multi_index_op (int nargout,
                                      const octave_value_list& args)
{
  return do_multi_index_op (nargout, args, 0);
}

octave_value_list
octave_fcn_binder::do_multi_index_op (int nargout,
                                      const octave_value_list& args,
                                      const std::list<octave_lvalue>* lvalue_list)
{
  octave_value_list retval;

  if (args.length () == expected_nargin)
    {
      for (int i = 0; i < arg_template.length (); i++)
        {
          int j = arg_mask[i];
          if (j >= 0)
            arg_template(i) = args(j); // May force a copy...
        }

      // Make a shallow copy of arg_template, to ensure consistency throughout
      // the following call even if we happen to get back here.
      octave_value_list tmp (arg_template);
      retval = root_handle.do_multi_index_op (nargout, tmp, lvalue_list);
    }
  else
    retval = octave_fcn_handle::do_multi_index_op (nargout, args, lvalue_list);

  return retval;
}

/*
%!function r = __f (g, i)
%!  r = g(i);
%!endfunction
%!test
%! x = [1,2;3,4];
%! assert (__f (@(i) x(:,i), 1), [1;3]);
*/
