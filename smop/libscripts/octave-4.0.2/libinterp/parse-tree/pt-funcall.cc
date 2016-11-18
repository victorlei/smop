/*

Copyright (C) 2012-2015 John W. Eaton

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

#include "ov-fcn.h"
#include "pt-funcall.h"
#include "pt-walk.h"

// Function call objects.

void
tree_funcall::print (std::ostream& os, bool pr_as_read_syntax,
                     bool pr_orig_text)
{
  print_raw (os, pr_as_read_syntax, pr_orig_text);
}

void
tree_funcall::print_raw (std::ostream& os, bool pr_as_read_syntax,
                         bool pr_orig_text)
{
  if (pr_orig_text)
    {
      os << original_text ();
    }
  else
    {
      octave_function *fp = fcn.function_value ();
      std::string nm = fp ? fp->name () : std::string ("<invalid-function>");

      os << nm << " (";

      octave_idx_type len = args.length ();
      for (octave_idx_type i = 0; i < len; i++)
        {
          args(i).print_raw (os, pr_as_read_syntax);

          if (i < len - 1)
            os << ", ";
        }

      os << ")";
    }
}

tree_funcall *
tree_funcall::dup (symbol_table::scope_id, symbol_table::context_id) const
{
  tree_funcall *new_fc = new tree_funcall (fcn, args, line (), column ());

  new_fc->copy_base (*new_fc);

  return new_fc;
}

void
tree_funcall::accept (tree_walker& tw)
{
  tw.visit_funcall (*this);
}

octave_value_list
tree_funcall::rvalue (int nargout)
{
  octave_value_list retval;

  retval = feval (fcn.function_value (), args, nargout);

  if (retval.length () == 1 && retval(0).is_function ())
    {
      // The return object is a function. We may need to re-index it using the
      // same logic as for identifier. This is primarily used for superclass
      // references in classdef.

      octave_value val = retval(0);
      octave_function *f = val.function_value (true);

      if (f && ! (is_postfix_indexed ()
                  && f->is_postfix_index_handled (postfix_index ())))
        {
          octave_value_list tmp_args;

          retval = val.do_multi_index_op (nargout, tmp_args);
        }
    }

  return retval;
}
