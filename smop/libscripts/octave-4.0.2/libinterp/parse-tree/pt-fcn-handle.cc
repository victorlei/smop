/*

Copyright (C) 2003-2015 John W. Eaton

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

#include "error.h"
#include "oct-obj.h"
#include "ov-fcn-handle.h"
#include "pt-fcn-handle.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-walk.h"
#include "variables.h"

void
tree_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax,
                        bool pr_orig_text)
{
  print_raw (os, pr_as_read_syntax, pr_orig_text);
}

void
tree_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax,
                            bool pr_orig_text)
{
  os << ((pr_as_read_syntax || pr_orig_text) ? "@" : "") << nm;
}

octave_value
tree_fcn_handle::rvalue1 (int)
{
  return make_fcn_handle (nm);
}

octave_value_list
tree_fcn_handle::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for function handle expression");
  else
    retval = rvalue1 (nargout);

  return retval;
}

tree_expression *
tree_fcn_handle::dup (symbol_table::scope_id,
                      symbol_table::context_id) const
{
  tree_fcn_handle *new_fh = new tree_fcn_handle (nm, line (), column ());

  new_fh->copy_base (*this);

  return new_fh;
}

void
tree_fcn_handle::accept (tree_walker& tw)
{
  tw.visit_fcn_handle (*this);
}

octave_value
tree_anon_fcn_handle::rvalue1 (int)
{
  // FIXME: should CMD_LIST be limited to a single expression?
  // I think that is what Matlab does.

  tree_parameter_list *param_list = parameter_list ();
  tree_parameter_list *ret_list = return_list ();
  tree_statement_list *cmd_list = body ();
  symbol_table::scope_id this_scope = scope ();

  symbol_table::scope_id new_scope = symbol_table::dup_scope (this_scope);

  if (new_scope > 0)
    symbol_table::inherit (new_scope, symbol_table::current_scope (),
                           symbol_table::current_context ());

  octave_user_function *uf
    = new octave_user_function (new_scope,
                                param_list ? param_list->dup (new_scope, 0) : 0,
                                ret_list ? ret_list->dup (new_scope, 0) : 0,
                                cmd_list ? cmd_list->dup (new_scope, 0) : 0);

  octave_function *curr_fcn = octave_call_stack::current ();

  if (curr_fcn)
    {
      // FIXME: maybe it would be better to just stash curr_fcn
      // instead of individual bits of info about it?

      uf->stash_parent_fcn_name (curr_fcn->name ());
      uf->stash_dir_name (curr_fcn->dir_name ());

      symbol_table::scope_id parent_scope = curr_fcn->parent_fcn_scope ();

      if (parent_scope < 0)
        parent_scope = curr_fcn->scope ();

      uf->stash_parent_fcn_scope (parent_scope);

      if (curr_fcn->is_class_method () || curr_fcn->is_class_constructor ())
        uf->stash_dispatch_class (curr_fcn->dispatch_class ());
    }

  uf->mark_as_anonymous_function ();
  uf->stash_fcn_file_name (file_name);
  uf->stash_fcn_location (line (), column ());

  octave_value ov_fcn (uf);

  octave_value fh (octave_fcn_binder::maybe_binder (ov_fcn));

  return fh;
}

/*
%!function r = __f2 (f, x)
%!  r = f (x);
%!endfunction
%!function f = __f1 (k)
%!  f = @(x) __f2 (@(y) y-k, x);
%!endfunction

%!assert ((__f1 (3)) (10) == 7)

%!test
%! g = @(t) feval (@(x) t*x, 2);
%! assert (g(0.5) == 1);

%!test
%! h = @(x) sin (x);
%! g = @(f, x) h (x);
%! f = @() g (@(x) h, pi);
%! assert (f () == sin (pi));

The next two tests are intended to test parsing of a character string
vs. hermitian operator at the beginning of an anonymous function
expression.  The use of ' for the character string and the spacing is
intentional, so don't change it.

%!test
%! f = @() 'foo';
%! assert (f (), 'foo');

%!test
%! f = @()'foo';
%! assert (f (), 'foo');
*/

octave_value_list
tree_anon_fcn_handle::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for anonymous function handle expression");
  else
    retval = rvalue1 (nargout);

  return retval;
}

tree_expression *
tree_anon_fcn_handle::dup (symbol_table::scope_id,
                           symbol_table::context_id) const
{
  tree_parameter_list *param_list = parameter_list ();
  tree_parameter_list *ret_list = return_list ();
  tree_statement_list *cmd_list = body ();
  symbol_table::scope_id this_scope = scope ();

  symbol_table::scope_id new_scope = symbol_table::dup_scope (this_scope);

  if (new_scope > 0)
    symbol_table::inherit (new_scope, symbol_table::current_scope (),
                           symbol_table::current_context ());

  tree_anon_fcn_handle *new_afh = new
    tree_anon_fcn_handle (param_list ? param_list->dup (new_scope, 0) : 0,
                          ret_list ? ret_list->dup (new_scope, 0) : 0,
                          cmd_list ? cmd_list->dup (new_scope, 0) : 0,
                          new_scope, line (), column ());

  new_afh->copy_base (*this);

  return new_afh;
}

void
tree_anon_fcn_handle::accept (tree_walker& tw)
{
  tw.visit_anon_fcn_handle (*this);
}
