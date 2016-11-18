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

#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-builtin.h"
#include "ov.h"
#include "profiler.h"
#include "toplev.h"
#include "unwind-prot.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_builtin,
                                     "built-in function",
                                     "built-in function");

octave_value_list
octave_builtin::subsref (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         int nargout)
{
  return octave_builtin::subsref (type, idx, nargout, 0);
}

octave_value_list
octave_builtin::subsref (const std::string& type,
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

octave_value_list
octave_builtin::do_multi_index_op (int nargout, const octave_value_list& args)
{
  return octave_builtin::do_multi_index_op (nargout, args, 0);
}

octave_value_list
octave_builtin::do_multi_index_op (int nargout, const octave_value_list& args,
                                   const std::list<octave_lvalue> *lvalue_list)
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

      if (lvalue_list || curr_lvalue_list)
        {
          frame.protect_var (curr_lvalue_list);
          curr_lvalue_list = lvalue_list;
        }

      try
        {
          BEGIN_PROFILER_BLOCK (octave_builtin)

          retval = (*f) (args, nargout);
          // Do not allow null values to be returned from functions.
          // FIXME: perhaps true builtins should be allowed?
          retval.make_storable_values ();
          // Fix the case of a single undefined value.
          // This happens when a compiled function uses
          //   octave_value retval;
          // instead of
          //   octave_value_list retval;
          // the idiom is very common, so we solve that here.
          if (retval.length () == 1 && retval.xelem (0).is_undefined ())
            retval.clear ();

          END_PROFILER_BLOCK
        }
      catch (octave_execution_exception)
        {
          gripe_library_execution_error ();
        }
    }

  return retval;
}

jit_type *
octave_builtin::to_jit (void) const
{
  return jtype;
}

void
octave_builtin::stash_jit (jit_type &type)
{
  jtype = &type;
}

octave_builtin::fcn
octave_builtin::function (void) const
{
  return f;
}

const std::list<octave_lvalue> *octave_builtin::curr_lvalue_list = 0;
