/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#include <sstream>

#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"
#include "singleton-cleanup.h"

#include "debug.h"
#include "defun.h"
#include "dirfns.h"
#include "input.h"
#include "load-path.h"
#include "ov-classdef.h"
#include "ov-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-arg-list.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"

octave_value symbol_table::dummy_octave_value;

symbol_table *symbol_table::instance = 0;

symbol_table::scope_id_cache *symbol_table::scope_id_cache::instance = 0;

std::map<symbol_table::scope_id, symbol_table*> symbol_table::all_instances;

std::map<std::string, octave_value> symbol_table::global_table;

std::map<std::string, symbol_table::fcn_info> symbol_table::fcn_table;

std::map<std::string, std::set<std::string> >
  symbol_table::class_precedence_table;

std::map<std::string, std::list<std::string> > symbol_table::parent_map;

const symbol_table::scope_id symbol_table::xglobal_scope = 0;
const symbol_table::scope_id symbol_table::xtop_scope = 1;

symbol_table::scope_id symbol_table::xcurrent_scope = 1;

symbol_table::context_id symbol_table::xcurrent_context = 0;

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp = 1;

void
symbol_table::scope_id_cache::create_instance (void)
{
  instance = new scope_id_cache ();

  singleton_cleanup_list::add (cleanup_instance);
}

symbol_table::context_id
symbol_table::symbol_record::symbol_record_rep::active_context (void) const
{
  octave_user_function *fcn = curr_fcn;

  // FIXME: If active_context () == -1, then it does not make much
  // sense to use this symbol_record. This means an attempt at accessing
  // a variable from a function that has not been called yet is
  // happening. This should be cleared up when an implementing closures.

  return fcn && fcn->active_context () != static_cast<context_id> (-1)
         ? fcn->active_context () : xcurrent_context;
}

void
symbol_table::symbol_record::symbol_record_rep::dump
  (std::ostream& os, const std::string& prefix) const
{
  octave_value val = varval ();

  os << prefix << name;

  if (val.is_defined ())
    {
      os << " ["
         << (is_local () ? "l" : "")
         << (is_automatic () ? "a" : "")
         << (is_formal () ? "f" : "")
         << (is_hidden () ? "h" : "")
         << (is_inherited () ? "i" : "")
         << (is_global () ? "g" : "")
         << (is_persistent () ? "p" : "")
         << "] ";
      val.dump (os);
    }

  os << "\n";
}

octave_value
symbol_table::symbol_record::find (const octave_value_list& args) const
{
  octave_value retval;

  if (is_global ())
    retval = symbol_table::global_varval (name ());
  else
    {
      retval = varval ();

      if (retval.is_undefined ())
        {
          // Use cached fcn_info pointer if possible.
          if (rep->finfo)
            retval = rep->finfo->find (args);
          else
            {
              retval = symbol_table::find_function (name (), args);

              if (retval.is_defined ())
                rep->finfo = get_fcn_info (name ());
            }
        }
    }

  return retval;
}

symbol_table::symbol_record symbol_table::dummy_symbol_record;

static void
split_name_with_package (const std::string& name, std::string& fname,
                         std::string& pname)
{
  size_t pos = name.rfind ('.');

  fname.clear ();
  pname.clear ();

  if (pos != std::string::npos)
    {
      fname = name.substr (pos + 1);
      pname = name.substr (0, pos);
    }
  else
    fname = name;
}

// Check the load path to see if file that defined this is still
// visible.  If the file is no longer visible, then erase the
// definition and move on.  If the file is visible, then we also
// need to check to see whether the file has changed since the the
// function was loaded/parsed.  However, this check should only
// happen once per prompt (for files found from relative path
// elements, we also check if the working directory has changed
// since the last time the function was loaded/parsed).
//
// FIXME: perhaps this should be done for all loaded functions when
// the prompt is printed or the directory has changed, and then we
// would not check for it when finding symbol definitions.

static inline bool
load_out_of_date_fcn (const std::string& ff, const std::string& dir_name,
                      octave_value& function,
                      const std::string& dispatch_type = std::string (),
                      const std::string& package_name = std::string ())
{
  bool retval = false;

  octave_function *fcn = load_fcn_from_file (ff, dir_name, dispatch_type,
                                             package_name);

  if (fcn)
    {
      retval = true;

      function = octave_value (fcn);
    }
  else
    function = octave_value ();

  return retval;
}

bool
out_of_date_check (octave_value& function,
                   const std::string& dispatch_type,
                   bool check_relative)
{
  bool retval = false;

  octave_function *fcn = function.function_value (true);

  if (fcn)
    {
      // FIXME: we need to handle subfunctions properly here.

      if (! fcn->is_subfunction ())
        {
          std::string ff = fcn->fcn_file_name ();

          if (! ff.empty ())
            {
              octave_time tc = fcn->time_checked ();

              bool relative = check_relative && fcn->is_relative ();

              if (tc <= Vlast_prompt_time
                  || (relative && tc < Vlast_chdir_time))
                {
                  bool clear_breakpoints = false;
                  std::string nm = fcn->name ();
                  std::string pack = fcn->package_name ();
                  std::string canonical_nm = fcn->canonical_name ();

                  bool is_same_file = false;

                  std::string file;
                  std::string dir_name;

                  if (check_relative)
                    {
                      int nm_len = nm.length ();

                      if (octave_env::absolute_pathname (nm)
                          && ((nm_len > 4
                               && (nm.substr (nm_len-4) == ".oct"
                                   || nm.substr (nm_len-4) == ".mex"))
                              || (nm_len > 2
                                  && nm.substr (nm_len-2) == ".m")))
                        file = nm;
                      else
                        {
                          // We don't want to make this an absolute name,
                          // because load_fcn_file looks at the name to
                          // decide whether it came from a relative lookup.

                          if (! dispatch_type.empty ())
                            {
                              file = load_path::find_method (dispatch_type, nm,
                                                             dir_name, pack);

                              if (file.empty ())
                                {
                                  std::string s_name;
                                  std::string s_pack;

                                  const std::list<std::string>& plist
                                    = symbol_table::parent_classes (dispatch_type);
                                  std::list<std::string>::const_iterator it
                                    = plist.begin ();

                                  while (it != plist.end ())
                                    {
                                      split_name_with_package (*it, s_name,
                                                               s_pack);

                                      file = load_path::find_method (*it, nm,
                                                                     dir_name,
                                                                     s_pack);
                                      if (! file.empty ())
                                        {
                                          pack = s_pack;
                                          break;
                                        }

                                      it++;
                                    }
                                }
                            }

                          // Maybe it's an autoload?
                          if (file.empty ())
                            file = lookup_autoload (nm);

                          if (file.empty ())
                            file = load_path::find_fcn (nm, dir_name, pack);
                        }

                      if (! file.empty ())
                        is_same_file = same_file (file, ff);
                    }
                  else
                    {
                      is_same_file = true;
                      file = ff;
                    }

                  if (file.empty ())
                    {
                      // Can't see this function from current
                      // directory, so we should clear it.

                      function = octave_value ();

                      clear_breakpoints = true;
                    }
                  else if (is_same_file)
                    {
                      // Same file.  If it is out of date, then reload it.

                      octave_time ottp = fcn->time_parsed ();
                      time_t tp = ottp.unix_time ();

                      fcn->mark_fcn_file_up_to_date (octave_time ());

                      if (! (Vignore_function_time_stamp == 2
                             || (Vignore_function_time_stamp
                                 && fcn->is_system_fcn_file ())))
                        {
                          file_stat fs (ff);

                          if (fs)
                            {
                              if (fs.is_newer (tp))
                                {
                                  retval = load_out_of_date_fcn (ff, dir_name,
                                                                 function,
                                                                 dispatch_type,
                                                                 pack);

                                  clear_breakpoints = true;
                                }
                            }
                          else
                            {
                              function = octave_value ();

                              clear_breakpoints = true;
                            }
                        }
                    }
                  else
                    {
                      // Not the same file, so load the new file in
                      // place of the old.

                      retval = load_out_of_date_fcn (file, dir_name, function,
                                                     dispatch_type, pack);

                      clear_breakpoints = true;
                    }

                  // If the function has been replaced then clear any
                  // breakpoints associated with it
                  if (clear_breakpoints)
                    bp_table::remove_all_breakpoints_in_file (canonical_nm,
                                                              true);
                }
            }
        }
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::load_private_function
  (const std::string& dir_name)
{
  octave_value retval;

  std::string file_name = load_path::find_private_fcn (dir_name, name);

  if (! file_name.empty ())
    {
      octave_function *fcn = load_fcn_from_file (file_name, dir_name);

      if (fcn)
        {
          std::string class_name;

          size_t pos = dir_name.find_last_of (file_ops::dir_sep_chars ());

          if (pos != std::string::npos)
            {
              std::string tmp = dir_name.substr (pos+1);

              if (tmp[0] == '@')
                class_name = tmp.substr (1);
            }

          fcn->mark_as_private_function (class_name);

          retval = octave_value (fcn);

          private_functions[dir_name] = retval;
        }
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::load_class_constructor (void)
{
  octave_value retval;

  std::string dir_name;

  std::string file_name = load_path::find_method (name, name, dir_name,
                                                  package_name);

  if (! file_name.empty ())
    {
      octave_function *fcn = load_fcn_from_file (file_name, dir_name, name,
                                                 package_name);

      if (fcn)
        {
          retval = octave_value (fcn);

          class_constructors[name] = retval;
        }
    }
  else
    {
      // Classdef constructors can be defined anywhere in the path, not
      // necessarily in @-folders. Look for a normal function and load it.
      // If the loaded function is a classdef constructor, store it as such
      // and restore function_on_path to its previous value.

      octave_value old_function_on_path = function_on_path;

      octave_value maybe_cdef_ctor = find_user_function ();

      if (maybe_cdef_ctor.is_defined ())
        {
          octave_function *fcn = maybe_cdef_ctor.function_value (true);

          if (fcn && fcn->is_classdef_constructor ())
            {
              retval = maybe_cdef_ctor;

              class_constructors[name] = retval;

              function_on_path = old_function_on_path;
            }
        }
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::load_class_method
  (const std::string& dispatch_type)
{
  octave_value retval;

  if (full_name () == dispatch_type)
    retval = load_class_constructor ();
  else
    {
      octave_function *cm = cdef_manager::find_method_symbol (name,
                                                              dispatch_type);

      if (cm)
        retval = octave_value (cm);

      if (! retval.is_defined ())
        {
          std::string dir_name;

          std::string file_name = load_path::find_method (dispatch_type, name,
                                                          dir_name);

          if (! file_name.empty ())
            {
              octave_function *fcn = load_fcn_from_file (file_name, dir_name,
                                                         dispatch_type);

              if (fcn)
                {
                  retval = octave_value (fcn);

                  class_methods[dispatch_type] = retval;
                }
            }

          if (retval.is_undefined ())
            {
              // Search parent classes

              const std::list<std::string>& plist =
                parent_classes (dispatch_type);

              std::list<std::string>::const_iterator it = plist.begin ();

              while (it != plist.end ())
                {
                  retval = find_method (*it);

                  if (retval.is_defined ())
                    {
                      class_methods[dispatch_type] = retval;
                      break;
                    }

                  it++;
                }
            }
        }
    }

  return retval;
}

void
symbol_table::fcn_info::fcn_info_rep::mark_subfunction_in_scope_as_private
  (scope_id scope, const std::string& class_name)
{
  scope_val_iterator p = subfunctions.find (scope);

  if (p != subfunctions.end ())
    {
      octave_function *fcn = p->second.function_value ();

      if (fcn)
        fcn->mark_as_private_function (class_name);
    }
}

void
symbol_table::fcn_info::fcn_info_rep::print_dispatch (std::ostream& os) const
{
  if (dispatch_map.empty ())
    os << "dispatch: " << name << " is not overloaded" << std::endl;
  else
    {
      os << "Overloaded function " << name << ":\n\n";

      for (dispatch_map_const_iterator p = dispatch_map.begin ();
           p != dispatch_map.end (); p++)
        os << "  " << name << " (" << p->first << ", ...) -> "
           << p->second << " (" << p->first << ", ...)\n";

      os << std::endl;
    }
}

std::string
symbol_table::fcn_info::fcn_info_rep::help_for_dispatch (void) const
{
  std::string retval;

  if (! dispatch_map.empty ())
    {
      retval = "Overloaded function:\n\n";

      for (dispatch_map_const_iterator p = dispatch_map.begin ();
           p != dispatch_map.end (); p++)
        retval += "  " + p->second + " (" + p->first + ", ...)\n\n";
    }

  return retval;
}

// :-) JWE, can you parse this? Returns a 2D array with second dimension equal
// to btyp_num_types (static constant). Only the leftmost dimension can be
// variable in C/C++. Typedefs are boring.

static builtin_type_t (*build_sup_table (void))[btyp_num_types]
{
  static builtin_type_t sup_table[btyp_num_types][btyp_num_types];
  for (int i = 0; i < btyp_num_types; i++)
    for (int j = 0; j < btyp_num_types; j++)
      {
        builtin_type_t ityp = static_cast<builtin_type_t> (i);
        builtin_type_t jtyp = static_cast<builtin_type_t> (j);
        // FIXME: Is this really right?
        bool use_j =
          (jtyp == btyp_func_handle || ityp == btyp_bool
           || (btyp_isarray (ityp)
               && (! btyp_isarray (jtyp)
                   || (btyp_isinteger (jtyp) && ! btyp_isinteger (ityp))
                   || ((ityp == btyp_double || ityp == btyp_complex
                        || ityp == btyp_char)
                       && (jtyp == btyp_float
                           || jtyp == btyp_float_complex)))));

        sup_table[i][j] = use_j ? jtyp : ityp;
      }

  return sup_table;
}

std::string
get_dispatch_type (const octave_value_list& args,
                   builtin_type_t& builtin_type)
{
  static builtin_type_t (*sup_table)[btyp_num_types] = build_sup_table ();
  std::string dispatch_type;

  int n = args.length ();

  if (n > 0)
    {
      int i = 0;
      builtin_type = args(0).builtin_type ();
      if (builtin_type != btyp_unknown)
        {
          for (i = 1; i < n; i++)
            {
              builtin_type_t bti = args(i).builtin_type ();
              if (bti != btyp_unknown)
                builtin_type = sup_table[builtin_type][bti];
              else
                {
                  builtin_type = btyp_unknown;
                  break;
                }
            }
        }

      if (builtin_type == btyp_unknown)
        {
          // There's a non-builtin class in the argument list.
          dispatch_type = args(i).class_name ();

          for (int j = i+1; j < n; j++)
            {
              octave_value arg = args(j);

              if (arg.builtin_type () == btyp_unknown)
                {
                  std::string cname = arg.class_name ();

                  // Only switch to type of ARG if it is marked superior
                  // to the current DISPATCH_TYPE.
                  if (! symbol_table::is_superiorto (dispatch_type, cname)
                      && symbol_table::is_superiorto (cname, dispatch_type))
                    dispatch_type = cname;
                }
            }
        }
      else
        dispatch_type = btyp_class_name[builtin_type];
    }
  else
    builtin_type = btyp_unknown;

  return dispatch_type;
}

std::string
get_dispatch_type (const octave_value_list& args)
{
  builtin_type_t builtin_type;
  return get_dispatch_type (args, builtin_type);
}

// Find the definition of NAME according to the following precedence
// list:
//
//   variable
//   subfunction
//   private function
//   class method
//   class constructor
//   legacy dispatch
//   command-line function
//   autoload function
//   function on the path
//   built-in function
//
// Matlab documentation states that constructors have higher precedence
// than methods, but that does not seem to be the case.

octave_value
symbol_table::fcn_info::fcn_info_rep::find (const octave_value_list& args,
                                            bool local_funcs)
{
  octave_value retval = xfind (args, local_funcs);

  if (! (error_state || retval.is_defined ()))
    {
      // It is possible that the user created a file on the fly since
      // the last prompt or chdir, so try updating the load path and
      // searching again.

      load_path::update ();

      retval = xfind (args, local_funcs);
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::xfind (const octave_value_list& args,
                                             bool local_funcs)
{
  if (local_funcs)
    {
      // Subfunction.  I think it only makes sense to check for
      // subfunctions if we are currently executing a function defined
      // from a .m file.

      octave_user_function *curr_fcn = symbol_table::get_curr_fcn ();

      for (scope_id scope = xcurrent_scope; scope >= 0;)
        {
          scope_val_iterator r = subfunctions.find (scope);
          if (r != subfunctions.end ())
            {
              // FIXME: out-of-date check here.

              return r->second;
            }

          octave_user_function *scope_curr_fcn = get_curr_fcn (scope);
          if (scope_curr_fcn)
            scope = scope_curr_fcn->parent_fcn_scope ();
          else
            scope = -1;
        }

      // Private function.

      if (curr_fcn)
        {
          std::string dir_name = curr_fcn->dir_name ();

          if (! dir_name.empty ())
            {
              str_val_iterator q = private_functions.find (dir_name);

              if (q == private_functions.end ())
                {
                  octave_value val = load_private_function (dir_name);

                  if (val.is_defined ())
                    return val;
                }
              else
                {
                  octave_value& fval = q->second;

                  if (fval.is_defined ())
                    out_of_date_check (fval, "", false);

                  if (fval.is_defined ())
                    return fval;
                  else
                    {
                      octave_value val = load_private_function (dir_name);

                      if (val.is_defined ())
                        return val;
                    }
                }
            }
        }
    }

  // Class methods.

  if (! args.empty ())
    {
      std::string dispatch_type = get_dispatch_type (args);

      octave_value fcn = find_method (dispatch_type);

      if (fcn.is_defined ())
        return fcn;
    }

  // Class constructors.  The class name and function name are the same.

  str_val_iterator q = class_constructors.find (name);

  if (q == class_constructors.end ())
    {
      octave_value val = load_class_constructor ();

      if (val.is_defined ())
        return val;
    }
  else
    {
      octave_value& fval = q->second;

      if (fval.is_defined ())
        out_of_date_check (fval, name);

      if (fval.is_defined ())
        return fval;
      else
        {
          octave_value val = load_class_constructor ();

          if (val.is_defined ())
            return val;
        }
    }

  // Legacy dispatch.

  if (! args.empty () && ! dispatch_map.empty ())
    {
      std::string dispatch_type = args(0).type_name ();

      std::string fname;

      dispatch_map_iterator p = dispatch_map.find (dispatch_type);

      if (p == dispatch_map.end ())
        p = dispatch_map.find ("any");

      if (p != dispatch_map.end ())
        {
          fname = p->second;

          octave_value fcn
            = symbol_table::find_function (fname, args);

          if (fcn.is_defined ())
            return fcn;
        }
    }

  // Command-line function.

  if (cmdline_function.is_defined ())
    return cmdline_function;

  // Autoload?

  octave_value fcn = find_autoload ();

  if (fcn.is_defined ())
    return fcn;

  // Function on the path.

  fcn = find_user_function ();

  if (fcn.is_defined ())
    return fcn;

  // Package

  fcn = find_package ();

  if (fcn.is_defined ())
    return fcn;

  // Built-in function (might be undefined).

  return built_in_function;
}

// Find the definition of NAME according to the following precedence
// list:
//
//   built-in function
//   function on the path
//   autoload function
//   command-line function
//   private function
//   subfunction

// This function is used to implement the "builtin" function, which
// searches for "built-in" functions.  In Matlab, "builtin" only
// returns functions that are actually built-in to the interpreter.
// But since the list of built-in functions is different in Octave and
// Matlab, we also search up the precedence list until we find
// something that matches.  Note that we are only searching by name,
// so class methods, constructors, and legacy dispatch functions are
// skipped.

octave_value
symbol_table::fcn_info::fcn_info_rep::builtin_find (void)
{
  octave_value retval = x_builtin_find ();

  if (! retval.is_defined ())
    {
      // It is possible that the user created a file on the fly since
      // the last prompt or chdir, so try updating the load path and
      // searching again.

      load_path::update ();

      retval = x_builtin_find ();
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::x_builtin_find (void)
{
  // Built-in function.
  if (built_in_function.is_defined ())
    return built_in_function;

  // Function on the path.

  octave_value fcn = find_user_function ();

  if (fcn.is_defined ())
    return fcn;

  // Autoload?

  fcn = find_autoload ();

  if (fcn.is_defined ())
    return fcn;

  // Command-line function.

  if (cmdline_function.is_defined ())
    return cmdline_function;

  // Private function.

  octave_user_function *curr_fcn = symbol_table::get_curr_fcn ();

  if (curr_fcn)
    {
      std::string dir_name = curr_fcn->dir_name ();

      if (! dir_name.empty ())
        {
          str_val_iterator q = private_functions.find (dir_name);

          if (q == private_functions.end ())
            {
              octave_value val = load_private_function (dir_name);

              if (val.is_defined ())
                return val;
            }
          else
            {
              octave_value& fval = q->second;

              if (fval.is_defined ())
                out_of_date_check (fval);

              if (fval.is_defined ())
                return fval;
              else
                {
                  octave_value val = load_private_function (dir_name);

                  if (val.is_defined ())
                    return val;
                }
            }
        }
    }

  // Subfunction.  I think it only makes sense to check for
  // subfunctions if we are currently executing a function defined
  // from a .m file.

  for (scope_id scope = xcurrent_scope; scope >= 0;)
    {
      scope_val_iterator r = subfunctions.find (scope);
      if (r != subfunctions.end ())
        {
          // FIXME: out-of-date check here.

          return r->second;
        }

      octave_user_function *scope_curr_fcn = get_curr_fcn (scope);
      if (scope_curr_fcn)
        scope = scope_curr_fcn->parent_fcn_scope ();
      else
        scope = -1;
    }

  return octave_value ();
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_method
  (const std::string& dispatch_type)
{
  octave_value retval;

  str_val_iterator q = class_methods.find (dispatch_type);

  if (q == class_methods.end ())
    {
      octave_value val = load_class_method (dispatch_type);

      if (val.is_defined ())
        return val;
    }
  else
    {
      octave_value& fval = q->second;

      if (fval.is_defined ())
        out_of_date_check (fval, dispatch_type);

      if (fval.is_defined ())
        return fval;
      else
        {
          octave_value val = load_class_method (dispatch_type);

          if (val.is_defined ())
            return val;
        }
    }

  return retval;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_autoload (void)
{
  octave_value retval;

  // Autoloaded function.

  if (autoload_function.is_defined ())
    out_of_date_check (autoload_function);

  if (! autoload_function.is_defined ())
    {
      std::string file_name = lookup_autoload (name);

      if (! file_name.empty ())
        {
          size_t pos = file_name.find_last_of (file_ops::dir_sep_chars ());

          std::string dir_name = file_name.substr (0, pos);

          octave_function *fcn = load_fcn_from_file (file_name, dir_name, "",
                                                     "", name, true);

          if (fcn)
            autoload_function = octave_value (fcn);
        }
    }

  return autoload_function;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_user_function (void)
{
  // Function on the path.

  if (function_on_path.is_defined ())
    out_of_date_check (function_on_path);

  if (! (error_state || function_on_path.is_defined ()))
    {
      std::string dir_name;

      std::string file_name = load_path::find_fcn (name, dir_name,
                                                   package_name);

      if (! file_name.empty ())
        {
          octave_function *fcn = load_fcn_from_file (file_name, dir_name, "",
                                                     package_name);

          if (fcn)
            function_on_path = octave_value (fcn);
        }
    }

  return function_on_path;
}

octave_value
symbol_table::fcn_info::fcn_info_rep::find_package (void)
{
  // FIXME: implement correct way to check out of date package
  //if (package.is_defined ())
  //  out_of_date_check (package);

  if (! (error_state || package.is_defined ()))
    {
      octave_function * fcn =
        cdef_manager::find_package_symbol (full_name ());

      if (fcn)
        package = octave_value (fcn);
    }

  return package;
}

// Insert INF_CLASS in the set of class names that are considered
// inferior to SUP_CLASS.  Return FALSE if INF_CLASS is currently
// marked as superior to  SUP_CLASS.

bool
symbol_table::set_class_relationship (const std::string& sup_class,
                                      const std::string& inf_class)
{
  if (is_superiorto (inf_class, sup_class))
    return false;

  // If sup_class doesn't have an entry in the precedence table,
  // this will automatically create it, and associate to it a
  // singleton set {inf_class} of inferior classes.
  class_precedence_table[sup_class].insert (inf_class);

  return true;
}

// Has class A been marked as superior to class B?  Also returns
// TRUE if B has been marked as inferior to A, since we only keep
// one table, and convert inferiorto information to a superiorto
// relationship.  Two calls are required to determine whether there
// is no relationship between two classes:
//
//  if (symbol_table::is_superiorto (a, b))
//    // A is superior to B, or B has been marked inferior to A.
//  else if (symbol_table::is_superiorto (b, a))
//    // B is superior to A, or A has been marked inferior to B.
//  else
//    // No relation.

bool
symbol_table::is_superiorto (const std::string& a, const std::string& b)
{
  class_precedence_table_const_iterator p = class_precedence_table.find (a);
  // If a has no entry in the precedence table, return false
  if (p == class_precedence_table.end ())
    return false;

  const std::set<std::string>& inferior_classes = p->second;
  std::set<std::string>::const_iterator q = inferior_classes.find (b);
  return (q != inferior_classes.end ());
}

static std::string
fcn_file_name (const octave_value& fcn)
{
  const octave_function *f = fcn.function_value ();

  return f ? f->fcn_file_name () : std::string ();
}

void
symbol_table::fcn_info::fcn_info_rep::dump (std::ostream& os,
                                            const std::string& prefix) const
{
  os << prefix << full_name ()
     << " ["
     << (cmdline_function.is_defined () ? "c" : "")
     << (built_in_function.is_defined () ? "b" : "")
     << (package.is_defined () ? "p" : "")
     << "]\n";

  std::string tprefix = prefix + "  ";

  if (autoload_function.is_defined ())
    os << tprefix << "autoload: "
       << fcn_file_name (autoload_function) << "\n";

  if (function_on_path.is_defined ())
    os << tprefix << "function from path: "
       << fcn_file_name (function_on_path) << "\n";

  if (! subfunctions.empty ())
    {
      for (scope_val_const_iterator p = subfunctions.begin ();
           p != subfunctions.end (); p++)
        os << tprefix << "subfunction: " << fcn_file_name (p->second)
           << " [" << p->first << "]\n";
    }

  if (! private_functions.empty ())
    {
      for (str_val_const_iterator p = private_functions.begin ();
           p != private_functions.end (); p++)
        os << tprefix << "private: " << fcn_file_name (p->second)
           << " [" << p->first << "]\n";
    }

  if (! class_constructors.empty ())
    {
      for (str_val_const_iterator p = class_constructors.begin ();
           p != class_constructors.end (); p++)
        os << tprefix << "constructor: " << fcn_file_name (p->second)
           << " [" << p->first << "]\n";
    }

  if (! class_methods.empty ())
    {
      for (str_val_const_iterator p = class_methods.begin ();
           p != class_methods.end (); p++)
        os << tprefix << "method: " << fcn_file_name (p->second)
           << " [" << p->first << "]\n";
    }

  if (! dispatch_map.empty ())
    {
      for (dispatch_map_const_iterator p = dispatch_map.begin ();
           p != dispatch_map.end (); p++)
        os << tprefix << "dispatch: " << fcn_file_name (p->second)
           << " [" << p->first << "]\n";
    }
}

void
symbol_table::install_nestfunction (const std::string& name,
                                    const octave_value& fcn,
                                    scope_id parent_scope)
{
  install_subfunction (name, fcn, parent_scope);

  // Stash the nest_parent for resolving variables after parsing is done.
  octave_function *fv = fcn.function_value ();

  symbol_table *fcn_table_loc = get_instance (fv->scope ());

  symbol_table *parent_table = get_instance (parent_scope);

  parent_table->add_nest_child (*fcn_table_loc);
}

octave_value
symbol_table::find (const std::string& name,
                    const octave_value_list& args,
                    bool skip_variables,
                    bool local_funcs)
{
  symbol_table *inst = get_instance (xcurrent_scope);

  return inst
         ? inst->do_find (name, args, skip_variables, local_funcs)
         : octave_value ();
}

octave_value
symbol_table::builtin_find (const std::string& name)
{
  symbol_table *inst = get_instance (xcurrent_scope);

  return inst ? inst->do_builtin_find (name) : octave_value ();
}

octave_value
symbol_table::find_function (const std::string& name,
                             const octave_value_list& args,
                             bool local_funcs)
{
  octave_value retval;

  if (! name.empty () && name[0] == '@')
    {
      // Look for a class specific function.
      std::string dispatch_type =
        name.substr (1, name.find_first_of (file_ops::dir_sep_str ()) - 1);

      std::string method;
      size_t pos = name.find_last_of (file_ops::dir_sep_str ());
      if (pos != std::string::npos)
        method = name.substr (pos + 1);

      retval = find_method (method, dispatch_type);
    }
  else
    {
      size_t pos = name.find_first_of (Vfilemarker);

      if (pos == std::string::npos)
        retval = find (name, args, true, local_funcs);
      else
        {
          std::string fcn_scope = name.substr (0, pos);
          scope_id stored_scope = xcurrent_scope;
          xcurrent_scope = xtop_scope;
          octave_value parent = find_function (name.substr (0, pos),
                                               octave_value_list (), false);

          if (parent.is_defined ())
            {
              octave_function *parent_fcn = parent.function_value ();

              if (parent_fcn)
                {
                  xcurrent_scope = parent_fcn->scope ();

                  if (xcurrent_scope > 1)
                    retval = find_function (name.substr (pos + 1), args);
                }
            }

          xcurrent_scope = stored_scope;
        }
    }

  return retval;
}

void
symbol_table::dump (std::ostream& os, scope_id scope)
{
  if (scope == xglobal_scope)
    dump_global (os);
  else
    {
      symbol_table *inst = get_instance (scope, false);

      if (inst)
        {
          os << "*** dumping symbol table scope " << scope
             << " (" << inst->table_name << ")\n\n";

          std::map<std::string, octave_value> sfuns
            = symbol_table::subfunctions_defined_in_scope (scope);

          if (! sfuns.empty ())
            {
              os << "  subfunctions defined in this scope:\n";

              for (std::map<std::string,
                   octave_value>::const_iterator p = sfuns.begin ();
                   p != sfuns.end (); p++)
                os << "    " << p->first << "\n";

              os << "\n";
            }

          inst->do_dump (os);
        }
    }
}

void
symbol_table::dump_global (std::ostream& os)
{
  if (! global_table.empty ())
    {
      os << "*** dumping global symbol table\n\n";

      for (global_table_const_iterator p = global_table.begin ();
           p != global_table.end (); p++)
        {
          std::string nm = p->first;
          octave_value val = p->second;

          os << "  " << nm << " ";
          val.dump (os);
          os << "\n";
        }
    }
}

void
symbol_table::dump_functions (std::ostream& os)
{
  if (! fcn_table.empty ())
    {
      os << "*** dumping globally visible functions from symbol table\n"
         << "    (c=commandline, b=built-in)\n\n";

      for (fcn_table_const_iterator p = fcn_table.begin ();
           p != fcn_table.end (); p++)
        p->second.dump (os, "  ");

      os << "\n";
    }
}

void
symbol_table::stash_dir_name_for_subfunctions (scope_id scope,
                                               const std::string& dir_name)
{
  // FIXME: is this the best way to do this?  Maybe it would be
  // better if we had a map from scope to list of subfunctions
  // stored with the function.  Do we?

  for (fcn_table_const_iterator p = fcn_table.begin ();
       p != fcn_table.end (); p++)
    {
      std::pair<std::string, octave_value> tmp
        = p->second.subfunction_defined_in_scope (scope);

      std::string nm = tmp.first;

      if (! nm.empty ())
        {
          octave_value& fcn = tmp.second;

          octave_user_function *f = fcn.user_function_value ();

          if (f)
            f->stash_dir_name (dir_name);
        }
    }
}

octave_value
symbol_table::do_find (const std::string& name,
                       const octave_value_list& args,
                       bool skip_variables,
                       bool local_funcs)
{
  octave_value retval;

  // Variable.

  if (! skip_variables)
    {
      table_iterator p = table.find (name);

      if (p != table.end ())
        {
          symbol_record sr = p->second;

          if (sr.is_global ())
            return symbol_table::global_varval (name);
          else
            {
              octave_value val = sr.varval ();

              if (val.is_defined ())
                return val;
            }
        }
    }

  fcn_table_iterator p = fcn_table.find (name);

  if (p != fcn_table.end ())
    return p->second.find (args, local_funcs);
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.find (args, local_funcs);

      if (fcn.is_defined ())
        fcn_table[name] = finfo;

      return fcn;
    }

  return retval;
}

octave_value
symbol_table::do_builtin_find (const std::string& name)
{
  octave_value retval;

  fcn_table_iterator p = fcn_table.find (name);

  if (p != fcn_table.end ())
    return p->second.builtin_find ();
  else
    {
      fcn_info finfo (name);

      octave_value fcn = finfo.builtin_find ();

      if (fcn.is_defined ())
        fcn_table[name] = finfo;

      return fcn;
    }

  return retval;
}

std::list<workspace_element>
symbol_table::do_workspace_info (void) const
{
  std::list<workspace_element> retval;

  for (table_const_iterator p = table.begin (); p != table.end (); p++)
    {
      std::string nm = p->first;
      symbol_record sr = p->second;

      if (! sr.is_hidden ())
        {
          octave_value val = sr.varval ();

          if (val.is_defined ())
            {
              // FIXME: fix size for objects, see kluge in variables.cc
              //dim_vector dv = val.dims ();
              octave_value tmp = val;
              Matrix sz = tmp.size ();
              dim_vector dv = dim_vector::alloc (sz.numel ());
              for (octave_idx_type i = 0; i < dv.length (); i++)
                dv(i) = sz(i);

              char storage = ' ';
              if (sr.is_global ())
                storage = 'g';
              else if (sr.is_persistent ())
                storage = 'p';
              else if (sr.is_automatic ())
                storage = 'a';
              else if (sr.is_formal ())
                storage = 'f';
              else if (sr.is_hidden ())
                storage = 'h';
              else if (sr.is_inherited ())
                storage = 'i';

              std::ostringstream buf;
              val.short_disp (buf);
              std::string short_disp_str = buf.str ();

              workspace_element elt (storage, nm, val.class_name (),
                                     short_disp_str, dv.str (),
                                     val.is_complex_type ());

              retval.push_back (elt);
            }
        }
    }

  return retval;
}

void
symbol_table::do_dump (std::ostream& os)
{
  if (! persistent_table.empty ())
    {
      os << "  persistent variables in this scope:\n\n";

      for (persistent_table_const_iterator p = persistent_table.begin ();
           p != persistent_table.end (); p++)
        {
          std::string nm = p->first;
          octave_value val = p->second;

          os << "    " << nm << " ";
          val.dump (os);
          os << "\n";
        }

      os << "\n";
    }

  if (! table.empty ())
    {
      os << "  other symbols in this scope (l=local; a=auto; f=formal\n"
         << "    h=hidden; i=inherited; g=global; p=persistent)\n\n";

      for (table_const_iterator p = table.begin (); p != table.end (); p++)
        p->second.dump (os, "    ");

      os << "\n";
    }
}

void symbol_table::cleanup (void)
{
  clear_all (true);

  // Delete all possibly remaining scopes.
  for (all_instances_iterator iter = all_instances.begin ();
       iter != all_instances.end (); iter++)
    {
      // First zero the table entry to avoid possible duplicate delete.
      symbol_table *inst = iter->second;
      iter->second = 0;

      // Now delete the scope. Note that there may be side effects, such as
      // deleting other scopes.
      delete inst;
    }

  global_table.clear ();
  fcn_table.clear ();
  class_precedence_table.clear ();
  parent_map.clear ();
  all_instances.clear ();
}

void
symbol_table::do_update_nest (void)
{
  if (nest_parent || nest_children.size ())
    curr_fcn->mark_as_nested_function ();

  if (nest_parent)
    {
      // fix bad symbol_records
      for (table_iterator ti = table.begin (); ti != table.end (); ++ti)
        {
          symbol_record &ours = ti->second;
          symbol_record parents;
          if (! ours.is_formal ()
              && nest_parent->look_nonlocal (ti->first, parents))
            {
              if (ours.is_global () || ours.is_persistent ())
                ::error ("global and persistent may only be used in the topmost level in which a nested variable is used");

              if (! ours.is_formal ())
                {
                  ours.invalidate ();
                  ti->second = parents;
                }
            }
          else
            ours.set_curr_fcn (curr_fcn);
        }
    }
  else if (nest_children.size ())
    {
      static_workspace = true;
      for (table_iterator ti = table.begin (); ti != table.end (); ++ti)
        ti->second.set_curr_fcn (curr_fcn);
    }

  for (std::vector<symbol_table*>::iterator iter = nest_children.begin ();
       iter != nest_children.end (); ++iter)
    (*iter)->do_update_nest ();
}

DEFUN (ignore_function_time_stamp, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} ignore_function_time_stamp ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} ignore_function_time_stamp (@var{new_val})\n\
Query or set the internal variable that controls whether Octave checks\n\
the time stamp on files each time it looks up functions defined in\n\
function files.\n\
\n\
If the internal variable is set to @qcode{\"system\"}, Octave will not\n\
automatically recompile function files in subdirectories of\n\
@file{@var{octave-home}/lib/@var{version}} if they have changed since they were last compiled, but will recompile other function files in the search path if they change.\n\
\n\
If set to @qcode{\"all\"}, Octave will not recompile any function files\n\
unless their definitions are removed with @code{clear}.\n\
\n\
If set to @qcode{\"none\"}, Octave will always check time stamps on files to\n\
determine whether functions defined in function files need to recompiled.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    {
      switch (Vignore_function_time_stamp)
        {
        case 1:
          retval = "system";
          break;

        case 2:
          retval = "all";
          break;

        default:
          retval = "none";
          break;
        }
    }

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string sval = args(0).string_value ();
          if (sval == "all")
            Vignore_function_time_stamp = 2;
          else if (sval == "system")
            Vignore_function_time_stamp = 1;
          else if (sval == "none")
            Vignore_function_time_stamp = 0;
          else
            error ("ignore_function_time_stamp: argument must be \"all\", \"system\", or \"none\"");
        }
      else
        error ("ignore_function_time_stamp: expecting argument to be character string");
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

/*
%!shared old_state
%! old_state = ignore_function_time_stamp ();
%!test
%! state = ignore_function_time_stamp ("all");
%! assert (state, old_state);
%! assert (ignore_function_time_stamp (), "all");
%! state = ignore_function_time_stamp ("system");
%! assert (state, "all");
%! assert (ignore_function_time_stamp (), "system");
%! ignore_function_time_stamp (old_state);

## Test input validation
%!error (ignore_function_time_stamp ("all", "all"))
%!error (ignore_function_time_stamp ("UNKNOWN_VALUE"))
%!error (ignore_function_time_stamp (42))
*/

DEFUN (__current_scope__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{scope}, @var{context}]} __dump_symtab_info__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = symbol_table::current_context ();
  retval(0) = symbol_table::current_scope ();

  return retval;
}

DEFUN (__dump_symtab_info__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} __dump_symtab_info__ ()\n\
@deftypefnx {Built-in Function} {} __dump_symtab_info__ (@var{scope})\n\
@deftypefnx {Built-in Function} {} __dump_symtab_info__ (\"scopes\")\n\
@deftypefnx {Built-in Function} {} __dump_symtab_info__ (\"functions\")\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      symbol_table::dump_functions (octave_stdout);

      symbol_table::dump_global (octave_stdout);

      std::list<symbol_table::scope_id> lst = symbol_table::scopes ();

      for (std::list<symbol_table::scope_id>::const_iterator p = lst.begin ();
           p != lst.end (); p++)
        symbol_table::dump (octave_stdout, *p);
    }
  else if (nargin == 1)
    {
      octave_value arg = args(0);

      if (arg.is_string ())
        {
          std::string s_arg = arg.string_value ();

          if (s_arg == "scopes")
            {
              std::list<symbol_table::scope_id> lst = symbol_table::scopes ();

              RowVector v (lst.size ());

              octave_idx_type k = 0;

              for (std::list<symbol_table::scope_id>::const_iterator
                   p = lst.begin (); p != lst.end (); p++)
                v.xelem (k++) = *p;

              retval = v;
            }
          else if (s_arg == "functions")
            {
              symbol_table::dump_functions (octave_stdout);
            }
          else
            error ("__dump_symtab_info__: expecting \"functions\" or \"scopes\"");
        }
      else
        {
          int s = arg.int_value ();

          if (! error_state)
            symbol_table::dump (octave_stdout, s);
          else
            error ("__dump_symtab_info__: expecting string or scope id");
        }
    }
  else
    print_usage ();

  return retval;
}

#if 0

// FIXME: should we have functions like this in Octave?

DEFUN (set_variable, args, , "set_variable (NAME, VALUE)")
{
  octave_value retval;

  if (args.length () == 2)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        symbol_table::assign (name, args(1));
      else
        error ("set_variable: expecting variable name as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (variable_value, args, , "VALUE = variable_value (NAME)")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        {
          retval = symbol_table::varval (name);

          if (retval.is_undefined ())
            error ("variable_value: '%s' is not a variable in the current scope",
                   name.c_str ());
        }
      else
        error ("variable_value: expecting variable name as first argument");
    }
  else
    print_usage ();

  return retval;
}
#endif


/*
bug #34497: 'clear -f' does not work for command line functions

This test relies on bar being a core function that is implemented in an m-file.
If the first assert fails, this is no longer the case and the tests need to be
updated to use some other function.

%!assert (! strcmp (which ("bar"), ""));

%!function x = bar ()
%!  x = 5;
%!endfunction
%!test
%! assert (bar == 5);
%! assert (strcmp (which ("bar"), ""));
%! clear -f bar;
%! assert (! strcmp (which ("bar"), ""));

%!function x = bar ()
%!  x = 5;
%!endfunction
%!test
%! assert (bar == 5);
%! assert (strcmp (which ("bar"), ""));
%! clear bar;
%! assert (! strcmp (which ("bar"), ""));
 */
