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

#include <sstream>
#include <iostream>
#include <string>

#include "defun.h"
#include "dynamic-ld.h"
#include "error.h"
#include "help.h"
#include "ov.h"
#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "ov-fcn.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "pager.h"
#include "symtab.h"
#include "toplev.h"
#include "variables.h"
#include "parse.h"

// Print the usage part of the doc string of FCN (user-defined or DEFUN).
void
print_usage (void)
{
  const octave_function *cur = octave_call_stack::current ();
  if (cur)
    print_usage (cur->name ());
  else
    error ("print_usage: invalid function");
}

void
print_usage (const std::string& name)
{
  feval ("print_usage", octave_value (name), 0);
}

void
check_version (const std::string& version, const std::string& fcn)
{
  if (version != OCTAVE_API_VERSION)
    {
      error ("API version %s found in .oct file function '%s'\n"
             "       does not match the running Octave (API version %s)\n"
             "       this can lead to incorrect results or other failures\n"
             "       you can fix this problem by recompiling this .oct file",
             version.c_str (), fcn.c_str (), OCTAVE_API_VERSION);
    }
}

// Install variables and functions in the symbol tables.

void
install_builtin_function (octave_builtin::fcn f, const std::string& name,
                          const std::string& file, const std::string& doc,
                          bool /* can_hide_function -- not yet implemented */)
{
  octave_value fcn (new octave_builtin (f, name, file, doc));

  symbol_table::install_built_in_function (name, fcn);
}

void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
                      const octave_shlib& shl, const std::string& doc,
                      bool relative)
{
  octave_dld_function *fcn = new octave_dld_function (f, shl, name, doc);

  if (relative)
    fcn->mark_relative ();

  octave_value fval (fcn);

  symbol_table::install_built_in_function (name, fval);
}

void
install_mex_function (void *fptr, bool fmex, const std::string& name,
                      const octave_shlib& shl, bool relative)
{
  octave_mex_function *fcn = new octave_mex_function (fptr, fmex, shl, name);

  if (relative)
    fcn->mark_relative ();

  octave_value fval (fcn);

  symbol_table::install_built_in_function (name, fval);
}

void
alias_builtin (const std::string& alias, const std::string& name)
{
  symbol_table::alias_built_in_function (alias, name);
}

octave_shlib
get_current_shlib (void)
{
  octave_shlib retval;

  octave_function *curr_fcn = octave_call_stack::current ();
  if (curr_fcn)
    {
      if (curr_fcn->is_dld_function ())
        {
          octave_dld_function *dld
            = dynamic_cast<octave_dld_function *> (curr_fcn);
          retval = dld->get_shlib ();
        }
      else if (curr_fcn->is_mex_function ())
        {
          octave_mex_function *mex
            = dynamic_cast<octave_mex_function *> (curr_fcn);
          retval = mex->get_shlib ();
        }
    }

  return retval;
}

bool defun_isargout (int nargout, int iout)
{
  const std::list<octave_lvalue> *lvalue_list
    = octave_builtin::curr_lvalue_list;
  if (iout >= std::max (nargout, 1))
    return false;
  else if (lvalue_list)
    {
      int k = 0;
      for (std::list<octave_lvalue>::const_iterator p = lvalue_list->begin ();
           p != lvalue_list->end (); p++)
        {
          if (k == iout)
            return ! p->is_black_hole ();
          k += p->numel ();
          if (k > iout)
            break;
        }

      return true;
    }
  else
    return true;
}

void defun_isargout (int nargout, int nout, bool *isargout)
{
  const std::list<octave_lvalue> *lvalue_list
    = octave_builtin::curr_lvalue_list;

  if (lvalue_list)
    {
      int k = 0;
      for (std::list<octave_lvalue>::const_iterator p = lvalue_list->begin ();
           p != lvalue_list->end () && k < nout; p++)
        {
          if (p->is_black_hole ())
            isargout[k++] = false;
          else
            {
              int l = std::min (k + p->numel (),
                                static_cast<octave_idx_type> (nout));
              while (k < l)
                isargout[k++] = true;
            }
        }
    }
  else
    for (int i = 0; i < nout; i++)
      isargout[i] = true;

  for (int i = std::max (nargout, 1); i < nout; i++)
    isargout[i] = false;
}

