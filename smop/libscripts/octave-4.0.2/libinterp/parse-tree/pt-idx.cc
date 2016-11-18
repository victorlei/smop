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

#include "Cell.h"
#include "error.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pager.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-walk.h"
#include "utils.h"
#include "variables.h"
#include "gripes.h"

// Index expressions.

tree_index_expression::tree_index_expression (int l, int c)
  : tree_expression (l, c), expr (0), args (0), type (),
    arg_nm (), dyn_field () { }

tree_index_expression::tree_index_expression (tree_expression *e,
                                              tree_argument_list *lst,
                                              int l, int c, char t)
  : tree_expression (l, c), expr (e), args (0), type (),
    arg_nm (), dyn_field ()
{
  append (lst, t);
}

tree_index_expression::tree_index_expression (tree_expression *e,
                                              const std::string& n,
                                              int l, int c)
  : tree_expression (l, c), expr (e), args (0), type (),
    arg_nm (), dyn_field ()
{
  append (n);
}

tree_index_expression::tree_index_expression (tree_expression *e,
                                              tree_expression *df,
                                              int l, int c)
  : tree_expression (l, c), expr (e), args (0), type (),
    arg_nm (), dyn_field ()
{
  append (df);
}

void
tree_index_expression::append (tree_argument_list *lst, char t)
{
  args.push_back (lst);
  type.append (1, t);
  arg_nm.push_back (lst ? lst->get_arg_names () : string_vector ());
  dyn_field.push_back (static_cast<tree_expression *> (0));

  if (lst && lst->has_magic_tilde ())
    error ("invalid use of empty argument (~) in index expression");
}

void
tree_index_expression::append (const std::string& n)
{
  args.push_back (static_cast<tree_argument_list *> (0));
  type.append (".");
  arg_nm.push_back (n);
  dyn_field.push_back (static_cast<tree_expression *> (0));
}

void
tree_index_expression::append (tree_expression *df)
{
  args.push_back (static_cast<tree_argument_list *> (0));
  type.append (".");
  arg_nm.push_back ("");
  dyn_field.push_back (df);
}

tree_index_expression::~tree_index_expression (void)
{
  delete expr;

  while (! args.empty ())
    {
      std::list<tree_argument_list *>::iterator p = args.begin ();
      delete *p;
      args.erase (p);
    }

  while (! dyn_field.empty ())
    {
      std::list<tree_expression *>::iterator p = dyn_field.begin ();
      delete *p;
      dyn_field.erase (p);
    }
}

bool
tree_index_expression::has_magic_end (void) const
{
  for (std::list<tree_argument_list *>::const_iterator p = args.begin ();
       p != args.end ();
       p++)
    {
      tree_argument_list *elt = *p;

      if (elt && elt->has_magic_end ())
        return true;
    }

  return false;
}

// This is useful for printing the name of the variable in an indexed
// assignment.

std::string
tree_index_expression::name (void) const
{
  return expr->name ();
}

static Cell
make_subs_cell (tree_argument_list *args, const string_vector& arg_nm)
{
  Cell retval;

  octave_value_list arg_values;

  if (args)
    arg_values = args->convert_to_const_vector ();

  if (! error_state)
    {
      int n = arg_values.length ();

      if (n > 0)
        {
          arg_values.stash_name_tags (arg_nm);

          retval.resize (dim_vector (1, n));

          for (int i = 0; i < n; i++)
            retval(0,i) = arg_values(i);
        }
    }

  return retval;
}

static inline octave_value_list
make_value_list (tree_argument_list *args, const string_vector& arg_nm,
                 const octave_value *object, bool rvalue = true)
{
  octave_value_list retval;

  if (args)
    {
      if (rvalue && object && args->has_magic_end () && object->is_undefined ())
        gripe_invalid_inquiry_subscript ();
      else
        retval = args->convert_to_const_vector (object);
    }

  if (! error_state)
    {
      octave_idx_type n = retval.length ();

      if (n > 0)
        retval.stash_name_tags (arg_nm);
    }

  return retval;
}

std::string
tree_index_expression::get_struct_index
  (std::list<string_vector>::const_iterator p_arg_nm,
   std::list<tree_expression *>::const_iterator p_dyn_field) const
{
  std::string fn = (*p_arg_nm)(0);

  if (fn.empty ())
    {
      tree_expression *df = *p_dyn_field;

      if (df)
        {
          octave_value t = df->rvalue1 ();

          if (! error_state)
            {
              if (t.is_string () && t.rows () == 1)
                fn = t.string_value ();
              else
                error ("dynamic structure field names must be strings");
            }
        }
      else
        panic_impossible ();
    }

  return fn;
}

octave_map
tree_index_expression::make_arg_struct (void) const
{
  int n = args.size ();

  Cell type_field (n, 1);
  Cell subs_field (n, 1);

  std::list<tree_argument_list *>::const_iterator p_args = args.begin ();
  std::list<string_vector>::const_iterator p_arg_nm = arg_nm.begin ();
  std::list<tree_expression *>::const_iterator p_dyn_field = dyn_field.begin ();

  octave_map m;

  for (int i = 0; i < n; i++)
    {
      switch (type[i])
        {
        case '(':
          subs_field(i) = make_subs_cell (*p_args, *p_arg_nm);
          break;

        case '{':
          subs_field(i) = make_subs_cell (*p_args, *p_arg_nm);
          break;

        case '.':
          subs_field(i) = get_struct_index (p_arg_nm, p_dyn_field);
          break;

        default:
          panic_impossible ();
        }

      if (error_state)
        return m;

      p_args++;
      p_arg_nm++;
      p_dyn_field++;
    }

  m.assign ("type", type_field);
  m.assign ("subs", subs_field);

  return m;
}

octave_value_list
tree_index_expression::rvalue (int nargout)
{
  return tree_index_expression::rvalue (nargout, 0);
}

octave_value_list
tree_index_expression::rvalue (int nargout,
                               const std::list<octave_lvalue> *lvalue_list)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  octave_value first_expr_val;

  octave_value_list first_args;

  bool have_args = false;

  if (expr->is_identifier () && type[0] == '(')
    {
      tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

      if (! (id->is_variable () || args.empty ()))
        {
          tree_argument_list *al = *(args.begin ());

          size_t n = al ? al->length () : 0;

          if (n > 0)
            {
              string_vector anm = *(arg_nm.begin ());
              have_args = true;
              first_args = al -> convert_to_const_vector ();
              first_args.stash_name_tags (anm);

              if (! error_state)
                first_expr_val = id->do_lookup  (first_args);
            }
        }
    }

  if (! error_state)
    {
      if (first_expr_val.is_undefined ())
        first_expr_val = expr->rvalue1 ();

      octave_value tmp = first_expr_val;
      octave_idx_type tmpi = 0;

      std::list<octave_value_list> idx;

      int n = args.size ();

      std::list<tree_argument_list *>::iterator p_args = args.begin ();
      std::list<string_vector>::iterator p_arg_nm = arg_nm.begin ();
      std::list<tree_expression *>::iterator p_dyn_field = dyn_field.begin ();

      for (int i = 0; i < n; i++)
        {
          if (i > 0)
            {
              tree_argument_list *al = *p_args;

              // In Matlab, () can only be followed by . In Octave, we do not
              // enforce this for rvalue expressions, but we'll split the
              // evaluation at this point. This will, hopefully, allow Octave's
              // looser rules apply smoothly for Matlab overloaded subsref
              // codes.
              bool force_split = type[i-1] == '(' && type[i] != '.';

              if (force_split || (al && al->has_magic_end ()))
                {
                  // We have an expression like
                  //
                  //   x{end}.a(end)
                  //
                  // and we are looking at the argument list that
                  // contains the second (or third, etc.) "end" token,
                  // so we must evaluate everything up to the point of
                  // that argument list so we can pass the appropriate
                  // value to the built-in end function.

                  octave_value_list tmp_list
                    = tmp.subsref (type.substr (tmpi, i - tmpi), idx, nargout);

                  tmp = tmp_list.length () ? tmp_list(0) : octave_value ();
                  tmpi = i;
                  idx.clear ();

                  if (tmp.is_cs_list ())
                    gripe_indexed_cs_list ();

                  if (error_state)
                    break;

                  if (tmp.is_function ())
                    {
                      octave_function *fcn = tmp.function_value (true);

                      if (fcn && ! fcn->is_postfix_index_handled (type[i]))
                        {
                          octave_value_list empty_args;

                          tmp_list = tmp.do_multi_index_op (1, empty_args);
                          tmp = (tmp_list.length ()
                                 ? tmp_list(0) : octave_value ());

                          if (tmp.is_cs_list ())
                            gripe_indexed_cs_list ();

                          if (error_state)
                            break;
                        }
                    }
                }
            }

          switch (type[i])
            {
            case '(':
              if (have_args)
                {
                  idx.push_back (first_args);
                  have_args = false;
                }
              else
                idx.push_back (make_value_list (*p_args, *p_arg_nm, &tmp));
              break;

            case '{':
              idx.push_back (make_value_list (*p_args, *p_arg_nm, &tmp));
              break;

            case '.':
              idx.push_back (octave_value (get_struct_index (p_arg_nm,
                                                             p_dyn_field)));
              break;

            default:
              panic_impossible ();
            }

          if (error_state)
            break;

          p_args++;
          p_arg_nm++;
          p_dyn_field++;
        }

      if (! error_state)
        {
          retval = tmp.subsref (type.substr (tmpi, n - tmpi), idx, nargout,
                                lvalue_list);

          octave_value val = retval.length () ? retval(0) : octave_value ();

          if (! error_state && val.is_function ())
            {
              octave_function *fcn = val.function_value (true);

              if (fcn)
                {
                  octave_value_list empty_args;

                  retval = (lvalue_list
                            ? val.do_multi_index_op (nargout, empty_args,
                                                     lvalue_list)
                            : val.do_multi_index_op (nargout, empty_args));
                }
            }
        }
    }

  return retval;
}

octave_value
tree_index_expression::rvalue1 (int nargout)
{
  octave_value retval;

  const octave_value_list tmp = rvalue (nargout);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

octave_lvalue
tree_index_expression::lvalue (void)
{
  octave_lvalue retval;

  std::list<octave_value_list> idx;
  std::string tmp_type;

  int n = args.size ();

  std::list<tree_argument_list *>::iterator p_args = args.begin ();
  std::list<string_vector>::iterator p_arg_nm = arg_nm.begin ();
  std::list<tree_expression *>::iterator p_dyn_field = dyn_field.begin ();

  retval = expr->lvalue ();

  if (! error_state)
    {
      octave_value tmp = retval.value ();

      octave_idx_type tmpi = 0;
      std::list<octave_value_list> tmpidx;

      for (int i = 0; i < n; i++)
        {
          if (retval.numel () != 1)
            gripe_indexed_cs_list ();
          else if (tmpi < i)
            {
              tmp = tmp.subsref (type.substr (tmpi, i - tmpi), tmpidx, true);
              tmpidx.clear ();
            }

          if (error_state)
            break;

          switch (type[i])
            {
            case '(':
              {
                octave_value_list tidx
                  = make_value_list (*p_args, *p_arg_nm, &tmp, false);

                idx.push_back (tidx);

                if (i < n - 1)
                  {
                    if (type[i+1] == '.')
                      {
                        tmpidx.push_back (tidx);
                        tmpi = i+1;
                      }
                    else
                      error ("() must be followed by . or close the index chain");
                  }
              }
              break;

            case '{':
              {
                octave_value_list tidx
                  = make_value_list (*p_args, *p_arg_nm, &tmp, false);

                if (tmp.is_undefined ())
                  {
                    if (tidx.has_magic_colon ())
                      gripe_invalid_inquiry_subscript ();
                    else
                      tmp = Cell ();
                  }
                else if (tmp.is_zero_by_zero ()
                         && (tmp.is_matrix_type () || tmp.is_string ()))
                  {
                    tmp = Cell ();
                  }

                retval.numel (tmp.numel (tidx));

                if (error_state)
                  break;

                idx.push_back (tidx);
                tmpidx.push_back (tidx);
                tmpi = i;
              }
              break;

            case '.':
              {
                octave_value tidx = get_struct_index (p_arg_nm, p_dyn_field);
                if (error_state)
                  break;

                bool autoconv = (tmp.is_zero_by_zero ()
                                 && (tmp.is_matrix_type () || tmp.is_string ()
                                     || tmp.is_cell ()));

                if (i > 0 && type[i-1] == '(')
                  {
                    octave_value_list pidx = idx.back ();

                    // Use octave_map, not octave_scalar_map so that the
                    // dimensions are 0x0, not 1x1.
                    if (tmp.is_undefined ())
                      {
                        if (pidx.has_magic_colon ())
                          gripe_invalid_inquiry_subscript ();
                        else
                          tmp = octave_map ();
                      }
                    else if (autoconv)
                      tmp = octave_map ();

                    retval.numel (tmp.numel (pidx));

                    tmpi = i-1;
                    tmpidx.push_back (tidx);
                  }
                else
                  {
                    if (tmp.is_undefined () || autoconv)
                      {
                        tmpi = i+1;
                        tmp = octave_value ();
                      }
                    else
                      {
                        retval.numel (tmp.numel (octave_value_list ()));

                        tmpi = i;
                        tmpidx.push_back (tidx);
                      }
                  }

                if (error_state)
                  break;

                idx.push_back (tidx);
              }
              break;

            default:
              panic_impossible ();
            }

          if (idx.back ().empty ())
            error ("invalid empty index list");

          if (error_state)
            break;

          p_args++;
          p_arg_nm++;
          p_dyn_field++;
        }

      if (! error_state)
        retval.set_index (type, idx);

    }

  return retval;
}

/*
%!test
%! clear x;
%! clear y;
%! y = 3;
%! x(y(end)) = 1;
%! assert (x, [0, 0, 1]);
%! clear x;
%! clear y;
%! y = {3};
%! x(y{end}) = 1;
%! assert (x, [0, 0, 1]);

%!test
%! x = {1, 2, 3};
%! [x{:}] = deal (4, 5, 6);
%! assert (x, {4, 5, 6});

%!test
%! [x.a, x.b.c] = deal (1, 2);
%! assert (x.a == 1 && x.b.c == 2);

%!test
%! [x.a, x(2).b] = deal (1, 2);
%! assert (x(1).a == 1 && isempty (x(2).a) && isempty (x(1).b) && x(2).b == 2);

%!test
%! x = struct (zeros (0, 1), {"a", "b"});
%! x(2).b = 1;
%! assert (x(2).b == 1);

%!test
%! x = struct (zeros (0, 1), {"a", "b"});
%! x(2).b = 1;
%! assert (x(2).b == 1);
*/

tree_index_expression *
tree_index_expression::dup (symbol_table::scope_id scope,
                            symbol_table::context_id context) const
{
  tree_index_expression *new_idx_expr
    = new tree_index_expression (line (), column ());

  new_idx_expr->expr = expr ? expr->dup (scope, context) : 0;

  std::list<tree_argument_list *> new_args;

  for (std::list<tree_argument_list *>::const_iterator p = args.begin ();
       p != args.end ();
       p++)
    {
      const tree_argument_list *elt = *p;

      new_args.push_back (elt ? elt->dup (scope, context) : 0);
    }

  new_idx_expr->args = new_args;

  new_idx_expr->type = type;

  new_idx_expr->arg_nm = arg_nm;

  std::list<tree_expression *> new_dyn_field;

  for (std::list<tree_expression *>::const_iterator p = dyn_field.begin ();
       p != dyn_field.end ();
       p++)
    {
      const tree_expression *elt = *p;

      new_dyn_field.push_back (elt ? elt->dup (scope, context) : 0);
    }

  new_idx_expr->dyn_field = new_dyn_field;

  new_idx_expr->copy_base (*this);

  return new_idx_expr;
}

void
tree_index_expression::accept (tree_walker& tw)
{
  tw.visit_index_expression (*this);
}
