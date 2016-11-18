/*

Copyright (C) 1994-2015 John W. Eaton

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

#include "Cell.h"

#include "defun.h"
#include "error.h"
#include "ov.h"
#include "oct-lvalue.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-walk.h"
#include "utils.h"

// Parameter lists.

tree_parameter_list::~tree_parameter_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_parameter_list::mark_as_formal_parameters (void)
{
  for (iterator p = begin (); p != end (); p++)
    {
      tree_decl_elt *elt = *p;
      elt->mark_as_formal_parameter ();
    }
}

bool
tree_parameter_list::validate (in_or_out type)
{
  bool retval = true;

  std::set<std::string> dict;

  for (iterator p = begin (); p != end (); p++)
    {
      tree_decl_elt *elt = *p;

      tree_identifier *id = elt->ident ();

      if (id)
        {
          std::string name = id->name ();

          if (id->is_black_hole ())
            {
              if (type != in)
                error ("invalid use of ~ in output list");
            }
          else if (dict.find (name) != dict.end ())
            {
              retval = false;
              error ("'%s' appears more than once in parameter list",
                     name.c_str ());
              break;
            }
          else
            dict.insert (name);
        }
    }

  if (! error_state)
    {
      std::string va_type = (type == in ? "varargin" : "varargout");

      size_t len = length ();

      if (len > 0)
        {
          tree_decl_elt *elt = back ();

          tree_identifier *id = elt->ident ();

          if (id && id->name () == va_type)
            {
              if (len == 1)
                mark_varargs_only ();
              else
                mark_varargs ();

              iterator p = end ();
              --p;
              delete *p;
              erase (p);
            }
        }
    }

  return retval;
}

void
tree_parameter_list::initialize_undefined_elements (const std::string& warnfor,
                                                    int nargout,
                                                    const octave_value& val)
{
  bool warned = false;

  int count = 0;

  octave_value tmp = symbol_table::varval (".ignored.");
  const Matrix ignored = tmp.is_defined () ? tmp.matrix_value () : Matrix ();

  octave_idx_type k = 0;

  for (iterator p = begin (); p != end (); p++)
    {
      if (++count > nargout)
        break;

      tree_decl_elt *elt = *p;

      if (! elt->is_variable ())
        {
          if (! warned)
            {
              warned = true;

              while (k < ignored.numel ())
                {
                  octave_idx_type l = ignored (k);
                  if (l == count)
                    {
                      warned = false;
                      break;
                    }
                  else if (l > count)
                    break;
                  else
                    k++;
                }

              if (warned)
                {
                  warning_with_id
                    ("Octave:undefined-return-values",
                     "%s: some elements in list of return values are undefined",
                     warnfor.c_str ());
                }
            }

          octave_lvalue lval = elt->lvalue ();

          lval.assign (octave_value::op_asn_eq, val);
        }
    }
}

void
tree_parameter_list::define_from_arg_vector (const octave_value_list& args)
{
  int nargin = args.length ();

  int expected_nargin = length ();

  iterator p = begin ();

  for (int i = 0; i < expected_nargin; i++)
    {
      tree_decl_elt *elt = *p++;

      octave_lvalue ref = elt->lvalue ();

      if (i < nargin)
        {
          if (args(i).is_defined () && args(i).is_magic_colon ())
            {
              if (! elt->eval ())
                {
                  ::error ("no default value for argument %d\n", i+1);
                  return;
                }
            }
          else
            ref.define (args(i));
        }
      else
        elt->eval ();
    }
}

void
tree_parameter_list::undefine (void)
{
  int len = length ();

  iterator p = begin ();

  for (int i = 0; i < len; i++)
    {
      tree_decl_elt *elt = *p++;

      octave_lvalue ref = elt->lvalue ();

      ref.assign (octave_value::op_asn_eq, octave_value ());
    }
}

std::list<std::string>
tree_parameter_list::variable_names (void) const
{
  std::list<std::string> retval;

  for (const_iterator p = begin (); p != end (); p++)
    {
      tree_decl_elt *elt = *p;

      retval.push_back (elt->name ());
    }

  return retval;
}

octave_value_list
tree_parameter_list::convert_to_const_vector (int nargout,
                                              const Cell& varargout)
{
  octave_idx_type vlen = varargout.numel ();
  int len = length ();

  // Special case. Will do a shallow copy.
  if (len == 0)
    return varargout;
  else if (nargout <= len)
    {
      octave_value_list retval (nargout);

      int i = 0;

      for (iterator p = begin (); p != end (); p++)
        {
          tree_decl_elt *elt = *p;
          if (elt->is_defined ())
            retval(i++) = elt->rvalue1 ();
          else
            break;
        }

      return retval;
    }
  else
    {
      octave_value_list retval (len + vlen);

      int i = 0;

      for (iterator p = begin (); p != end (); p++)
        {
          tree_decl_elt *elt = *p;
          retval(i++) = elt->rvalue1 ();
        }

      for (octave_idx_type j = 0; j < vlen; j++)
        retval(i++) = varargout(j);

      return retval;
    }
}

bool
tree_parameter_list::is_defined (void)
{
  bool status = true;

  for (iterator p = begin (); p != end (); p++)
    {
      tree_decl_elt *elt = *p;

      if (! elt->is_variable ())
        {
          status = false;
          break;
        }
    }

  return status;
}

tree_parameter_list *
tree_parameter_list::dup (symbol_table::scope_id scope,
                          symbol_table::context_id context) const
{
  tree_parameter_list *new_list = new tree_parameter_list ();

  if (takes_varargs ())
    new_list->mark_varargs ();

  for (const_iterator p = begin (); p != end (); p++)
    {
      const tree_decl_elt *elt = *p;

      new_list->append (elt->dup (scope, context));
    }

  return new_list;
}

void
tree_parameter_list::accept (tree_walker& tw)
{
  tw.visit_parameter_list (*this);
}

// Return lists.

tree_return_list::~tree_return_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

tree_return_list *
tree_return_list::dup (symbol_table::scope_id scope,
                       symbol_table::context_id context) const
{
  tree_return_list *new_list = new tree_return_list ();

  for (const_iterator p = begin (); p != end (); p++)
    {
      const tree_index_expression *elt = *p;

      new_list->append (elt->dup (scope, context));
    }

  return new_list;
}

void
tree_return_list::accept (tree_walker& tw)
{
  tw.visit_return_list (*this);
}
