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

#include "oct-shlib.h"

#include <defaults.h>
#include "dynamic-ld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-mex-fcn.h"
#include "ov.h"
#include "profiler.h"
#include "toplev.h"
#include "unwind-prot.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_mex_function,
                                     "mex function", "mex function");

octave_mex_function::octave_mex_function
  (void *fptr, bool fmex, const octave_shlib& shl,
   const std::string& nm)
  : octave_function (nm), mex_fcn_ptr (fptr), exit_fcn_ptr (0),
    have_fmex (fmex), sh_lib (shl)
{
  mark_fcn_file_up_to_date (time_parsed ());

  std::string file_name = fcn_file_name ();

  system_fcn_file
    = (! file_name.empty ()
       && Voct_file_dir == file_name.substr (0, Voct_file_dir.length ()));
}

octave_mex_function::~octave_mex_function (void)
{
  if (exit_fcn_ptr)
    (*exit_fcn_ptr) ();

  octave_dynamic_loader::remove_mex (my_name, sh_lib);
}

std::string
octave_mex_function::fcn_file_name (void) const
{
  return sh_lib.file_name ();
}

octave_time
octave_mex_function::time_parsed (void) const
{
  return sh_lib.time_loaded ();
}

octave_value_list
octave_mex_function::subsref (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
        int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

        retval = do_multi_index_op (tmp_nargout, idx.front ());
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

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.
  //
  // FIXME: Note that if a function call returns multiple
  // values, and there is further indexing to perform, then we are
  // ignoring all but the first value.  Is this really what we want to
  // do?  If it is not, then what should happen for stat("file").size,
  // for exmaple?

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

// FIXME: shouldn't this declaration be a header file somewhere?
extern octave_value_list
call_mex (bool have_fmex, void *f, const octave_value_list& args,
          int nargout, octave_mex_function *curr_mex_fcn);

octave_value_list
octave_mex_function::do_multi_index_op (int nargout,
                                        const octave_value_list& args)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (args.has_magic_colon ())
    ::error ("invalid use of colon in function argument list");
  else
    {
      unwind_protect frame;

      octave_call_stack::push (this);

      frame.add_fcn (octave_call_stack::pop);

      try
        {
          BEGIN_PROFILER_BLOCK (octave_mex_function)

          retval = call_mex (have_fmex, mex_fcn_ptr, args, nargout, this);

          END_PROFILER_BLOCK
        }
      catch (octave_execution_exception)
        {
          gripe_library_execution_error ();
        }
    }

  return retval;
}
