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

#include "oct-locbuf.h"
#include "quit.h"

#include "data.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-mat.h"
#include "pt-walk.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// The character to fill with when creating string arrays.
char Vstring_fill_char = ' ';

// General matrices.  This list type is much more work to handle than
// constant matrices, but it allows us to construct matrices from
// other matrices, variables, and functions.

// But first, some internal classes that make our job much easier.

class
tm_row_const
{
private:

  class
  tm_row_const_rep : public octave_base_list<octave_value>
  {
  public:

    tm_row_const_rep (void)
      : count (1), dv (0, 0), all_str (false),
        all_sq_str (false), all_dq_str (false),
        some_str (false), all_real (false), all_cmplx (false),
        all_mt (true), any_cell (false), any_sparse (false),
        any_class (false), all_1x1 (false),
        first_elem_is_struct (false), class_nm (), ok (false)
    { }

    tm_row_const_rep (const tree_argument_list& row)
      : count (1), dv (0, 0), all_str (false), all_sq_str (false),
        some_str (false), all_real (false), all_cmplx (false),
        all_mt (true), any_cell (false), any_sparse (false),
        any_class (false), all_1x1 (! row.empty ()),
        first_elem_is_struct (false), class_nm (), ok (false)
    { init (row); }

    ~tm_row_const_rep (void) { }

    octave_refcount<int> count;

    dim_vector dv;

    bool all_str;
    bool all_sq_str;
    bool all_dq_str;
    bool some_str;
    bool all_real;
    bool all_cmplx;
    bool all_mt;
    bool any_cell;
    bool any_sparse;
    bool any_class;
    bool all_1x1;
    bool first_elem_is_struct;

    std::string class_nm;

    bool ok;

    void do_init_element (const octave_value&, bool&);

    void init (const tree_argument_list&);

    void cellify (void);

  private:

    tm_row_const_rep (const tm_row_const_rep&);

    tm_row_const_rep& operator = (const tm_row_const_rep&);

  };

public:

  typedef tm_row_const_rep::iterator iterator;
  typedef tm_row_const_rep::const_iterator const_iterator;

  tm_row_const (void)
    : rep (0) { }

  tm_row_const (const tree_argument_list& row)
    : rep (new tm_row_const_rep (row)) { }

  tm_row_const (const tm_row_const& x)
    : rep (x.rep)
  {
    if (rep)
      rep->count++;
  }

  tm_row_const& operator = (const tm_row_const& x)
  {
    if (this != &x && rep != x.rep)
      {
        if (rep && --rep->count == 0)
          delete rep;

        rep = x.rep;

        if (rep)
          rep->count++;
      }

    return *this;
  }

  ~tm_row_const (void)
  {
    if (rep && --rep->count == 0)
      delete rep;
  }

  octave_idx_type rows (void) { return rep->dv(0); }
  octave_idx_type cols (void) { return rep->dv(1); }

  bool empty (void) const { return rep->empty (); }

  size_t length (void) const { return rep->length (); }

  dim_vector dims (void) { return rep->dv; }

  bool all_strings_p (void) const { return rep->all_str; }
  bool all_sq_strings_p (void) const { return rep->all_sq_str; }
  bool all_dq_strings_p (void) const { return rep->all_dq_str; }
  bool some_strings_p (void) const { return rep->some_str; }
  bool all_real_p (void) const { return rep->all_real; }
  bool all_complex_p (void) const { return rep->all_cmplx; }
  bool all_empty_p (void) const { return rep->all_mt; }
  bool any_cell_p (void) const { return rep->any_cell; }
  bool any_sparse_p (void) const { return rep->any_sparse; }
  bool any_class_p (void) const { return rep->any_class; }
  bool all_1x1_p (void) const { return rep->all_1x1; }
  bool first_elem_struct_p (void) const { return rep->first_elem_is_struct; }

  std::string class_name (void) const { return rep->class_nm; }

  void cellify (void) { rep->cellify (); }

  operator bool () const { return (rep && rep->ok); }

  iterator begin (void) { return rep->begin (); }
  const_iterator begin (void) const { return rep->begin (); }

  iterator end (void) { return rep->end (); }
  const_iterator end (void) const { return rep->end (); }

private:

  tm_row_const_rep *rep;
};

std::string
get_concat_class (const std::string& c1, const std::string& c2)
{
  std::string retval = octave_base_value::static_class_name ();

  if (c1 == c2)
    retval = c1;
  else if (c1.empty ())
    retval = c2;
  else if (c2.empty ())
    retval = c1;
  else if (c1 == "class" || c2 == "class")
    retval = "class";
  else
    {
      bool c1_is_int = (c1 == "int8" || c1 == "uint8"
                        || c1 == "int16" || c1 == "uint16"
                        || c1 == "int32" || c1 == "uint32"
                        || c1 == "int64" || c1 == "uint64");
      bool c2_is_int = (c2 == "int8" || c2 == "uint8"
                        || c2 == "int16" || c2 == "uint16"
                        || c2 == "int32" || c2 == "uint32"
                        || c2 == "int64" || c2 == "uint64");

      bool c1_is_char = (c1 == "char");
      bool c2_is_char = (c2 == "char");

      bool c1_is_double = (c1 == "double");
      bool c2_is_double = (c2 == "double");

      bool c1_is_single = (c1 == "single");
      bool c2_is_single = (c2 == "single");

      bool c1_is_logical = (c1 == "logical");
      bool c2_is_logical = (c2 == "logical");

      bool c1_is_built_in_type
        = (c1_is_int || c1_is_char || c1_is_double || c1_is_single
           || c1_is_logical);

      bool c2_is_built_in_type
        = (c2_is_int || c2_is_char ||  c2_is_double || c2_is_single
           || c2_is_logical);

      // Order is important here...

      if (c1 == "struct" && c2 == c1)
        retval = c1;
      else if (c1 == "cell" || c2 == "cell")
        retval = "cell";
      else if (c1_is_char && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_char && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_int && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_int && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_single && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_single && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_double && c2_is_built_in_type)
        retval = c1;
      else if (c2_is_double && c1_is_built_in_type)
        retval = c2;
      else if (c1_is_logical && c2_is_logical)
        retval = c1;
    }

  return retval;
}

static void
eval_error (const char *msg, const dim_vector& x, const dim_vector& y)
{
  ::error ("%s (%s vs %s)", msg, x.str ().c_str (), y.str ().c_str ());
}

void
tm_row_const::tm_row_const_rep::do_init_element (const octave_value& val,
                                                 bool& first_elem)
{
  std::string this_elt_class_nm
    = val.is_object () ? std::string ("class") : val.class_name ();

  class_nm = get_concat_class (class_nm, this_elt_class_nm);

  dim_vector this_elt_dv = val.dims ();

  if (! this_elt_dv.zero_by_zero ())
    {
      all_mt = false;

      if (first_elem)
        {
          if (val.is_map ())
            first_elem_is_struct = true;

          first_elem = false;
        }
    }

  append (val);

  if (all_str && ! val.is_string ())
    all_str = false;

  if (all_sq_str && ! val.is_sq_string ())
    all_sq_str = false;

  if (all_dq_str && ! val.is_dq_string ())
    all_dq_str = false;

  if (! some_str && val.is_string ())
    some_str = true;

  if (all_real && ! val.is_real_type ())
    all_real = false;

  if (all_cmplx && ! (val.is_complex_type () || val.is_real_type ()))
    all_cmplx = false;

  if (!any_cell && val.is_cell ())
    any_cell = true;

  if (!any_sparse && val.is_sparse_type ())
    any_sparse = true;

  if (!any_class && val.is_object ())
    any_class = true;

  // Special treatment of sparse matrices to avoid out-of-memory error
  all_1x1 = all_1x1 && ! val.is_sparse_type () && val.numel () == 1;
}

void
tm_row_const::tm_row_const_rep::init (const tree_argument_list& row)
{
  all_str = true;
  all_sq_str = true;
  all_dq_str = true;
  all_real = true;
  all_cmplx = true;
  any_cell = false;
  any_sparse = false;
  any_class = false;

  bool first_elem = true;

  for (tree_argument_list::const_iterator p = row.begin ();
       p != row.end ();
       p++)
    {
      octave_quit ();

      tree_expression *elt = *p;

      octave_value tmp = elt->rvalue1 ();

      if (error_state || tmp.is_undefined ())
        {
          ok = ! error_state;
          return;
        }
      else
        {
          if (tmp.is_cs_list ())
            {
              octave_value_list tlst = tmp.list_value ();

              for (octave_idx_type i = 0; i < tlst.length (); i++)
                {
                  octave_quit ();

                  do_init_element (tlst(i), first_elem);
                }
            }
          else
            do_init_element (tmp, first_elem);
        }
    }

  if (any_cell && ! any_class && ! first_elem_is_struct)
    cellify ();

  first_elem = true;

  if (! error_state)
    {
      for (iterator p = begin (); p != end (); p++)
        {
          octave_quit ();

          octave_value val = *p;

          dim_vector this_elt_dv = val.dims ();

          if (! this_elt_dv.zero_by_zero ())
            {
              all_mt = false;

              if (first_elem)
                {
                  first_elem = false;
                  dv = this_elt_dv;
                }
              else if ((! any_class) && (! dv.hvcat (this_elt_dv, 1)))
                {
                  eval_error ("horizontal dimensions mismatch", dv, this_elt_dv);
                  break;
                }
            }
        }
    }

  ok = ! error_state;
}

void
tm_row_const::tm_row_const_rep::cellify (void)
{
  bool elt_changed = false;

  for (iterator p = begin (); p != end (); p++)
    {
      octave_quit ();

      if (! p->is_cell ())
        {
          elt_changed = true;

          *p = Cell (*p);
        }
    }

  if (elt_changed)
    {
      bool first_elem = true;

      for (iterator p = begin (); p != end (); p++)
        {
          octave_quit ();

          octave_value val = *p;

          dim_vector this_elt_dv = val.dims ();

          if (! this_elt_dv.zero_by_zero ())
            {
              if (first_elem)
                {
                  first_elem = false;
                  dv = this_elt_dv;
                }
              else if (! dv.hvcat (this_elt_dv, 1))
                {
                  eval_error ("horizontal dimensions mismatch",
                              dv, this_elt_dv);
                  break;
                }
            }
        }
    }
}

class
tm_const : public octave_base_list<tm_row_const>
{
public:

  tm_const (const tree_matrix& tm)
    : dv (0, 0), all_str (false), all_sq_str (false), all_dq_str (false),
      some_str (false), all_real (false), all_cmplx (false),
      all_mt (true), any_cell (false), any_sparse (false),
      any_class (false), class_nm (), ok (false)
  { init (tm); }

  ~tm_const (void) { }

  octave_idx_type rows (void) const { return dv.elem (0); }
  octave_idx_type cols (void) const { return dv.elem (1); }

  dim_vector dims (void) const { return dv; }

  bool all_strings_p (void) const { return all_str; }
  bool all_sq_strings_p (void) const { return all_sq_str; }
  bool all_dq_strings_p (void) const { return all_dq_str; }
  bool some_strings_p (void) const { return some_str; }
  bool all_real_p (void) const { return all_real; }
  bool all_complex_p (void) const { return all_cmplx; }
  bool all_empty_p (void) const { return all_mt; }
  bool any_cell_p (void) const { return any_cell; }
  bool any_sparse_p (void) const { return any_sparse; }
  bool any_class_p (void) const { return any_class; }
  bool all_1x1_p (void) const { return all_1x1; }

  std::string class_name (void) const { return class_nm; }

  operator bool () const { return ok; }

private:

  dim_vector dv;

  bool all_str;
  bool all_sq_str;
  bool all_dq_str;
  bool some_str;
  bool all_real;
  bool all_cmplx;
  bool all_mt;
  bool any_cell;
  bool any_sparse;
  bool any_class;
  bool all_1x1;

  std::string class_nm;

  bool ok;

  tm_const (void);

  tm_const (const tm_const&);

  tm_const& operator = (const tm_const&);

  void init (const tree_matrix& tm);
};

void
tm_const::init (const tree_matrix& tm)
{
  all_str = true;
  all_sq_str = true;
  all_dq_str = true;
  all_real = true;
  all_cmplx = true;
  any_cell = false;
  any_sparse = false;
  any_class = false;
  all_1x1 = ! tm.empty ();

  bool first_elem = true;
  bool first_elem_is_struct = false;

  // Just eval and figure out if what we have is complex or all
  // strings.  We can't check columns until we know that this is a
  // numeric matrix -- collections of strings can have elements of
  // different lengths.

  for (tree_matrix::const_iterator p = tm.begin (); p != tm.end (); p++)
    {
      octave_quit ();

      tree_argument_list *elt = *p;

      tm_row_const tmp (*elt);

      if (first_elem)
        {
          first_elem_is_struct = tmp.first_elem_struct_p ();

          first_elem = false;
        }

      if (tmp && ! tmp.empty ())
        {
          if (all_str && ! tmp.all_strings_p ())
            all_str = false;

          if (all_sq_str && ! tmp.all_sq_strings_p ())
            all_sq_str = false;

          if (all_dq_str && ! tmp.all_dq_strings_p ())
            all_dq_str = false;

          if (! some_str && tmp.some_strings_p ())
            some_str = true;

          if (all_real && ! tmp.all_real_p ())
            all_real = false;

          if (all_cmplx && ! tmp.all_complex_p ())
            all_cmplx = false;

          if (all_mt && ! tmp.all_empty_p ())
            all_mt = false;

          if (!any_cell && tmp.any_cell_p ())
            any_cell = true;

          if (!any_sparse && tmp.any_sparse_p ())
            any_sparse = true;

          if (!any_class && tmp.any_class_p ())
            any_class = true;

          all_1x1 = all_1x1 && tmp.all_1x1_p ();

          append (tmp);
        }
      else
        break;
    }

  if (! error_state)
    {
      if (any_cell && ! any_class && ! first_elem_is_struct)
        {
          for (iterator q = begin (); q != end (); q++)
            {
              octave_quit ();

              q->cellify ();
            }
        }

      first_elem = true;

      for (iterator q = begin (); q != end (); q++)
        {
          octave_quit ();

          tm_row_const elt = *q;

          octave_idx_type this_elt_nr = elt.rows ();
          octave_idx_type this_elt_nc = elt.cols ();

          std::string this_elt_class_nm = elt.class_name ();
          class_nm = get_concat_class (class_nm, this_elt_class_nm);

          dim_vector this_elt_dv = elt.dims ();

          all_mt = false;

          if (first_elem)
            {
              first_elem = false;

              dv = this_elt_dv;
            }
          else if (all_str && dv.length () == 2
                   && this_elt_dv.length () == 2)
            {
              // FIXME: this is Octave's specialty. Character matrices allow
              // rows of unequal length.
              if (this_elt_nc > cols ())
                dv(1) = this_elt_nc;
              dv(0) += this_elt_nr;
            }
          else if ((!any_class) && (!dv.hvcat (this_elt_dv, 0)))
            {
              eval_error ("vertical dimensions mismatch", dv, this_elt_dv);
              return;
            }
        }
    }

  ok = ! error_state;
}

octave_value_list
tree_matrix::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for matrix list");
  else
    retval = rvalue1 (nargout);

  return retval;
}

void
maybe_warn_string_concat (bool all_dq_strings_p, bool all_sq_strings_p)
{
  if (! (all_dq_strings_p || all_sq_strings_p))
    warning_with_id ("Octave:mixed-string-concat",
                     "concatenation of different character string types may have unintended consequences");
}

template<class TYPE, class T>
static void
single_type_concat (Array<T>& result,
                    tm_const& tmp)
{
  octave_idx_type r = 0;
  octave_idx_type c = 0;

  for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
    {
      tm_row_const row = *p;
      // Skip empty arrays to allow looser rules.
      if (row.dims ().any_zero ())
        continue;

      for (tm_row_const::iterator q = row.begin ();
           q != row.end ();
           q++)
        {
          octave_quit ();

          TYPE ra = octave_value_extract<TYPE> (*q);

          // Skip empty arrays to allow looser rules.
          if (! error_state)
            {
              if (! ra.is_empty ())
                {
                  result.insert (ra, r, c);

                  if (! error_state)
                    c += ra.columns ();
                  else
                    return;
                }
            }
          else
            return;
        }

      r += row.rows ();
      c = 0;
    }
}

template<class TYPE, class T>
static void
single_type_concat (Array<T>& result,
                    const dim_vector& dv,
                    tm_const& tmp)
{
  if (dv.any_zero ())
    {
      result = Array<T> (dv);
      return;
    }

  if (tmp.length () == 1)
    {
      // If possible, forward the operation to liboctave.
      // Single row.
      tm_row_const& row = tmp.front ();
      if (! (equal_types<T, char>::value || equal_types<T, octave_value>::value)
          && row.all_1x1_p ())
        {
          // Optimize all scalars case.
          result.clear (dv);
          assert (static_cast<size_t> (result.numel ()) == row.length ());
          octave_idx_type i = 0;
          for (tm_row_const::iterator q = row.begin ();
               q != row.end () && ! error_state; q++)
            result(i++) = octave_value_extract<T> (*q);

          return;
        }

      octave_idx_type ncols = row.length ();
      octave_idx_type i = 0;
      OCTAVE_LOCAL_BUFFER (Array<T>, array_list, ncols);

      for (tm_row_const::iterator q = row.begin ();
           q != row.end () && ! error_state;
           q++)
        {
          octave_quit ();

          array_list[i] = octave_value_extract<TYPE> (*q);
          i++;
        }

      if (! error_state)
        result = Array<T>::cat (-2, ncols, array_list);
    }
  else
    {
      result = Array<T> (dv);
      single_type_concat<TYPE> (result, tmp);
    }
}

template<class TYPE, class T>
static void
single_type_concat (Sparse<T>& result,
                    const dim_vector& dv,
                    tm_const& tmp)
{
  if (dv.any_zero ())
    {
      result = Sparse<T> (dv);
      return;
    }

  // Sparse matrices require preallocation for efficient indexing; besides,
  // only horizontal concatenation can be efficiently handled by indexing.
  // So we just cat all rows through liboctave, then cat the final column.
  octave_idx_type nrows = tmp.length ();
  octave_idx_type j = 0;
  OCTAVE_LOCAL_BUFFER (Sparse<T>, sparse_row_list, nrows);
  for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
    {
      tm_row_const row = *p;
      octave_idx_type ncols = row.length ();
      octave_idx_type i = 0;
      OCTAVE_LOCAL_BUFFER (Sparse<T>, sparse_list, ncols);

      for (tm_row_const::iterator q = row.begin ();
           q != row.end () && ! error_state;
           q++)
        {
          octave_quit ();

          sparse_list[i] = octave_value_extract<TYPE> (*q);
          i++;
        }

      Sparse<T> stmp = Sparse<T>::cat (-2, ncols, sparse_list);
      sparse_row_list[j] = stmp;
      j++;
    }

  result = Sparse<T>::cat (-1, nrows, sparse_row_list);
}

template<class MAP>
static void
single_type_concat (octave_map& result,
                    const dim_vector& dv,
                    tm_const& tmp)
{
  if (dv.any_zero ())
    {
      result = octave_map (dv);
      return;
    }

  octave_idx_type nrows = tmp.length ();
  octave_idx_type j = 0;
  OCTAVE_LOCAL_BUFFER (octave_map, map_row_list, nrows);
  for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
    {
      tm_row_const row = *p;
      octave_idx_type ncols = row.length ();
      octave_idx_type i = 0;
      OCTAVE_LOCAL_BUFFER (MAP, map_list, ncols);

      for (tm_row_const::iterator q = row.begin ();
           q != row.end () && ! error_state;
           q++)
        {
          octave_quit ();

          map_list[i] = octave_value_extract<MAP> (*q);
          i++;
        }

      octave_map mtmp = octave_map::cat (-2, ncols, map_list);
      map_row_list[j] = mtmp;
      j++;
    }

  result = octave_map::cat (-1, nrows, map_row_list);
}

template<class TYPE>
static octave_value
do_single_type_concat (const dim_vector& dv,
                       tm_const& tmp)
{
  TYPE result;

  single_type_concat<TYPE> (result, dv, tmp);

  return result;
}

template<>
octave_value
do_single_type_concat<octave_map> (const dim_vector& dv,
                                   tm_const& tmp)
{
  octave_map result;

  if (tmp.all_1x1_p ())
    single_type_concat<octave_scalar_map> (result, dv, tmp);
  else
    single_type_concat<octave_map> (result, dv, tmp);

  return result;
}

static octave_value
do_class_concat (tm_const& tmc)
{
  octave_value retval;

  octave_value_list rows (tmc.length (), octave_value ());

  octave_idx_type j = 0;
  for (tm_const::iterator p = tmc.begin (); p != tmc.end (); p++)
    {
      octave_quit ();

      tm_row_const tmrc = *p;

      if (tmrc.length () == 1)
        rows(j++) = *(tmrc.begin ());
      else
        {
          octave_value_list row (tmrc.length (), octave_value ());

          octave_idx_type i = 0;
          for (tm_row_const::iterator q = tmrc.begin (); q != tmrc.end (); q++)
            row(i++) = *q;

          rows(j++) = do_class_concat (row, "horzcat", 1);
        }
    }

  if (! error_state)
    {
      if (rows.length () == 1)
        retval = rows(0);
      else
        retval = do_class_concat (rows, "vertcat", 0);
    }

  return retval;
}

octave_value
tree_matrix::rvalue1 (int)
{
  octave_value retval = Matrix ();

  bool all_strings_p = false;
  bool all_sq_strings_p = false;
  bool all_dq_strings_p = false;
  bool all_empty_p = false;
  bool all_real_p = false;
  bool any_sparse_p = false;
  bool any_class_p = false;
  bool frc_str_conv = false;

  tm_const tmp (*this);

  if (tmp && ! tmp.empty ())
    {
      dim_vector dv = tmp.dims ();
      all_strings_p = tmp.all_strings_p ();
      all_sq_strings_p = tmp.all_sq_strings_p ();
      all_dq_strings_p = tmp.all_dq_strings_p ();
      all_empty_p = tmp.all_empty_p ();
      all_real_p = tmp.all_real_p ();
      any_sparse_p = tmp.any_sparse_p ();
      any_class_p = tmp.any_class_p ();
      frc_str_conv = tmp.some_strings_p ();

      // Try to speed up the common cases.

      std::string result_type = tmp.class_name ();

      if (any_class_p)
        {
          retval = do_class_concat (tmp);
        }
      else if (result_type == "double")
        {
          if (any_sparse_p)
            {
              if (all_real_p)
                retval = do_single_type_concat<SparseMatrix> (dv, tmp);
              else
                retval = do_single_type_concat<SparseComplexMatrix> (dv, tmp);
            }
          else
            {
              if (all_real_p)
                retval = do_single_type_concat<NDArray> (dv, tmp);
              else
                retval = do_single_type_concat<ComplexNDArray> (dv, tmp);
            }
        }
      else if (result_type == "single")
        {
          if (all_real_p)
            retval = do_single_type_concat<FloatNDArray> (dv, tmp);
          else
            retval = do_single_type_concat<FloatComplexNDArray> (dv, tmp);
        }
      else if (result_type == "char")
        {
          char type = all_dq_strings_p ? '"' : '\'';

          if (! all_strings_p)
            gripe_implicit_conversion ("Octave:num-to-str",
                                       "numeric", result_type);
          else
            maybe_warn_string_concat (all_dq_strings_p, all_sq_strings_p);

          charNDArray result (dv, Vstring_fill_char);

          single_type_concat<charNDArray> (result, tmp);

          retval = octave_value (result, type);
        }
      else if (result_type == "logical")
        {
          if (any_sparse_p)
            retval = do_single_type_concat<SparseBoolMatrix> (dv, tmp);
          else
            retval = do_single_type_concat<boolNDArray> (dv, tmp);
        }
      else if (result_type == "int8")
        retval = do_single_type_concat<int8NDArray> (dv, tmp);
      else if (result_type == "int16")
        retval = do_single_type_concat<int16NDArray> (dv, tmp);
      else if (result_type == "int32")
        retval = do_single_type_concat<int32NDArray> (dv, tmp);
      else if (result_type == "int64")
        retval = do_single_type_concat<int64NDArray> (dv, tmp);
      else if (result_type == "uint8")
        retval = do_single_type_concat<uint8NDArray> (dv, tmp);
      else if (result_type == "uint16")
        retval = do_single_type_concat<uint16NDArray> (dv, tmp);
      else if (result_type == "uint32")
        retval = do_single_type_concat<uint32NDArray> (dv, tmp);
      else if (result_type == "uint64")
        retval = do_single_type_concat<uint64NDArray> (dv, tmp);
      else if (result_type == "cell")
        retval = do_single_type_concat<Cell> (dv, tmp);
      else if (result_type == "struct")
        retval = do_single_type_concat<octave_map> (dv, tmp);
      else
        {
          // The line below might seem crazy, since we take a copy of
          // the first argument, resize it to be empty and then resize
          // it to be full. This is done since it means that there is
          // no recopying of data, as would happen if we used a single
          // resize.  It should be noted that resize operation is also
          // significantly slower than the do_cat_op function, so it
          // makes sense to have an empty matrix and copy all data.
          //
          // We might also start with a empty octave_value using
          //
          //    ctmp = octave_value_typeinfo::lookup_type
          //          (tmp.begin() -> begin() -> type_name());
          //
          // and then directly resize. However, for some types there
          // might be some additional setup needed, and so this should
          // be avoided.

          octave_value ctmp;

          // Find the first non-empty object

          if (any_sparse_p)
            {
              // Start with sparse matrix to avoid issues memory issues
              // with things like [ones(1,4),sprandn(1e8,4,1e-4)]
              if (all_real_p)
                ctmp = octave_sparse_matrix ().resize (dv);
              else
                ctmp = octave_sparse_complex_matrix ().resize (dv);
            }
          else
            {
              for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
                {
                  octave_quit ();

                  tm_row_const row = *p;

                  for (tm_row_const::iterator q = row.begin ();
                       q != row.end (); q++)
                    {
                      octave_quit ();

                      ctmp = *q;

                      if (! ctmp.all_zero_dims ())
                        goto found_non_empty;
                    }
                }

              ctmp = (*(tmp.begin () -> begin ()));

            found_non_empty:

              if (! all_empty_p)
                ctmp = ctmp.resize (dim_vector (0,0)).resize (dv);
            }

          if (! error_state)
            {
              // Now, extract the values from the individual elements and
              // insert them in the result matrix.

              int dv_len = dv.length ();
              octave_idx_type ntmp = dv_len > 1 ? dv_len : 2;
              Array<octave_idx_type> ra_idx (dim_vector (ntmp, 1), 0);

              for (tm_const::iterator p = tmp.begin (); p != tmp.end (); p++)
                {
                  octave_quit ();

                  tm_row_const row = *p;

                  for (tm_row_const::iterator q = row.begin ();
                       q != row.end ();
                       q++)
                    {
                      octave_quit ();

                      octave_value elt = *q;

                      if (elt.is_empty ())
                        continue;

                      ctmp = do_cat_op (ctmp, elt, ra_idx);

                      if (error_state)
                        goto done;

                      ra_idx (1) += elt.columns ();
                    }

                  ra_idx (0) += row.rows ();
                  ra_idx (1) = 0;
                }

              retval = ctmp;

              if (frc_str_conv && ! retval.is_string ())
                retval = retval.convert_to_str ();
            }
        }
    }

done:
  return retval;
}

tree_expression *
tree_matrix::dup (symbol_table::scope_id scope,
                  symbol_table::context_id context) const
{
  tree_matrix *new_matrix = new tree_matrix (0, line (), column ());

  new_matrix->copy_base (*this, scope, context);

  return new_matrix;
}

void
tree_matrix::accept (tree_walker& tw)
{
  tw.visit_matrix (*this);
}

/*
## test concatenation with all zero matrices
%!assert ([ "" 65*ones(1,10) ], "AAAAAAAAAA");
%!assert ([ 65*ones(1,10) "" ], "AAAAAAAAAA");

%!test
%! c = {"foo"; "bar"; "bazoloa"};
%! assert ([c; "a"; "bc"; "def"], {"foo"; "bar"; "bazoloa"; "a"; "bc"; "def"});

%!assert (class ([int64(1), int64(1)]), "int64")
%!assert (class ([int64(1), int32(1)]), "int64")
%!assert (class ([int64(1), int16(1)]), "int64")
%!assert (class ([int64(1), int8(1)]), "int64")
%!assert (class ([int64(1), uint64(1)]), "int64")
%!assert (class ([int64(1), uint32(1)]), "int64")
%!assert (class ([int64(1), uint16(1)]), "int64")
%!assert (class ([int64(1), uint8(1)]), "int64")
%!assert (class ([int64(1), single(1)]), "int64")
%!assert (class ([int64(1), double(1)]), "int64")
%!assert (class ([int64(1), cell(1)]), "cell")
%!assert (class ([int64(1), true]), "int64")
%!assert (class ([int64(1), "a"]), "char")

%!assert (class ([int32(1), int64(1)]), "int32")
%!assert (class ([int32(1), int32(1)]), "int32")
%!assert (class ([int32(1), int16(1)]), "int32")
%!assert (class ([int32(1), int8(1)]), "int32")
%!assert (class ([int32(1), uint64(1)]), "int32")
%!assert (class ([int32(1), uint32(1)]), "int32")
%!assert (class ([int32(1), uint16(1)]), "int32")
%!assert (class ([int32(1), uint8(1)]), "int32")
%!assert (class ([int32(1), single(1)]), "int32")
%!assert (class ([int32(1), double(1)]), "int32")
%!assert (class ([int32(1), cell(1)]), "cell")
%!assert (class ([int32(1), true]), "int32")
%!assert (class ([int32(1), "a"]), "char")

%!assert (class ([int16(1), int64(1)]), "int16")
%!assert (class ([int16(1), int32(1)]), "int16")
%!assert (class ([int16(1), int16(1)]), "int16")
%!assert (class ([int16(1), int8(1)]), "int16")
%!assert (class ([int16(1), uint64(1)]), "int16")
%!assert (class ([int16(1), uint32(1)]), "int16")
%!assert (class ([int16(1), uint16(1)]), "int16")
%!assert (class ([int16(1), uint8(1)]), "int16")
%!assert (class ([int16(1), single(1)]), "int16")
%!assert (class ([int16(1), double(1)]), "int16")
%!assert (class ([int16(1), cell(1)]), "cell")
%!assert (class ([int16(1), true]), "int16")
%!assert (class ([int16(1), "a"]), "char")

%!assert (class ([int8(1), int64(1)]), "int8")
%!assert (class ([int8(1), int32(1)]), "int8")
%!assert (class ([int8(1), int16(1)]), "int8")
%!assert (class ([int8(1), int8(1)]), "int8")
%!assert (class ([int8(1), uint64(1)]), "int8")
%!assert (class ([int8(1), uint32(1)]), "int8")
%!assert (class ([int8(1), uint16(1)]), "int8")
%!assert (class ([int8(1), uint8(1)]), "int8")
%!assert (class ([int8(1), single(1)]), "int8")
%!assert (class ([int8(1), double(1)]), "int8")
%!assert (class ([int8(1), cell(1)]), "cell")
%!assert (class ([int8(1), true]), "int8")
%!assert (class ([int8(1), "a"]), "char")

%!assert (class ([uint64(1), int64(1)]), "uint64")
%!assert (class ([uint64(1), int32(1)]), "uint64")
%!assert (class ([uint64(1), int16(1)]), "uint64")
%!assert (class ([uint64(1), int8(1)]), "uint64")
%!assert (class ([uint64(1), uint64(1)]), "uint64")
%!assert (class ([uint64(1), uint32(1)]), "uint64")
%!assert (class ([uint64(1), uint16(1)]), "uint64")
%!assert (class ([uint64(1), uint8(1)]), "uint64")
%!assert (class ([uint64(1), single(1)]), "uint64")
%!assert (class ([uint64(1), double(1)]), "uint64")
%!assert (class ([uint64(1), cell(1)]), "cell")
%!assert (class ([uint64(1), true]), "uint64")
%!assert (class ([uint64(1), "a"]), "char")

%!assert (class ([uint32(1), int64(1)]), "uint32")
%!assert (class ([uint32(1), int32(1)]), "uint32")
%!assert (class ([uint32(1), int16(1)]), "uint32")
%!assert (class ([uint32(1), int8(1)]), "uint32")
%!assert (class ([uint32(1), uint64(1)]), "uint32")
%!assert (class ([uint32(1), uint32(1)]), "uint32")
%!assert (class ([uint32(1), uint16(1)]), "uint32")
%!assert (class ([uint32(1), uint8(1)]), "uint32")
%!assert (class ([uint32(1), single(1)]), "uint32")
%!assert (class ([uint32(1), double(1)]), "uint32")
%!assert (class ([uint32(1), cell(1)]), "cell")
%!assert (class ([uint32(1), true]), "uint32")
%!assert (class ([uint32(1), "a"]), "char")

%!assert (class ([uint16(1), int64(1)]), "uint16")
%!assert (class ([uint16(1), int32(1)]), "uint16")
%!assert (class ([uint16(1), int16(1)]), "uint16")
%!assert (class ([uint16(1), int8(1)]), "uint16")
%!assert (class ([uint16(1), uint64(1)]), "uint16")
%!assert (class ([uint16(1), uint32(1)]), "uint16")
%!assert (class ([uint16(1), uint16(1)]), "uint16")
%!assert (class ([uint16(1), uint8(1)]), "uint16")
%!assert (class ([uint16(1), single(1)]), "uint16")
%!assert (class ([uint16(1), double(1)]), "uint16")
%!assert (class ([uint16(1), cell(1)]), "cell")
%!assert (class ([uint16(1), true]), "uint16")
%!assert (class ([uint16(1), "a"]), "char")

%!assert (class ([uint8(1), int64(1)]), "uint8")
%!assert (class ([uint8(1), int32(1)]), "uint8")
%!assert (class ([uint8(1), int16(1)]), "uint8")
%!assert (class ([uint8(1), int8(1)]), "uint8")
%!assert (class ([uint8(1), uint64(1)]), "uint8")
%!assert (class ([uint8(1), uint32(1)]), "uint8")
%!assert (class ([uint8(1), uint16(1)]), "uint8")
%!assert (class ([uint8(1), uint8(1)]), "uint8")
%!assert (class ([uint8(1), single(1)]), "uint8")
%!assert (class ([uint8(1), double(1)]), "uint8")
%!assert (class ([uint8(1), cell(1)]), "cell")
%!assert (class ([uint8(1), true]), "uint8")
%!assert (class ([uint8(1), "a"]), "char")

%!assert (class ([single(1), int64(1)]), "int64")
%!assert (class ([single(1), int32(1)]), "int32")
%!assert (class ([single(1), int16(1)]), "int16")
%!assert (class ([single(1), int8(1)]), "int8")
%!assert (class ([single(1), uint64(1)]), "uint64")
%!assert (class ([single(1), uint32(1)]), "uint32")
%!assert (class ([single(1), uint16(1)]), "uint16")
%!assert (class ([single(1), uint8(1)]), "uint8")
%!assert (class ([single(1), single(1)]), "single")
%!assert (class ([single(1), double(1)]), "single")
%!assert (class ([single(1), cell(1)]), "cell")
%!assert (class ([single(1), true]), "single")
%!assert (class ([single(1), "a"]), "char")

%!assert (class ([double(1), int64(1)]), "int64")
%!assert (class ([double(1), int32(1)]), "int32")
%!assert (class ([double(1), int16(1)]), "int16")
%!assert (class ([double(1), int8(1)]), "int8")
%!assert (class ([double(1), uint64(1)]), "uint64")
%!assert (class ([double(1), uint32(1)]), "uint32")
%!assert (class ([double(1), uint16(1)]), "uint16")
%!assert (class ([double(1), uint8(1)]), "uint8")
%!assert (class ([double(1), single(1)]), "single")
%!assert (class ([double(1), double(1)]), "double")
%!assert (class ([double(1), cell(1)]), "cell")
%!assert (class ([double(1), true]), "double")
%!assert (class ([double(1), "a"]), "char")

%!assert (class ([cell(1), int64(1)]), "cell")
%!assert (class ([cell(1), int32(1)]), "cell")
%!assert (class ([cell(1), int16(1)]), "cell")
%!assert (class ([cell(1), int8(1)]), "cell")
%!assert (class ([cell(1), uint64(1)]), "cell")
%!assert (class ([cell(1), uint32(1)]), "cell")
%!assert (class ([cell(1), uint16(1)]), "cell")
%!assert (class ([cell(1), uint8(1)]), "cell")
%!assert (class ([cell(1), single(1)]), "cell")
%!assert (class ([cell(1), double(1)]), "cell")
%!assert (class ([cell(1), cell(1)]), "cell")
%!assert (class ([cell(1), true]), "cell")
%!assert (class ([cell(1), "a"]), "cell")

%!assert (class ([true, int64(1)]), "int64")
%!assert (class ([true, int32(1)]), "int32")
%!assert (class ([true, int16(1)]), "int16")
%!assert (class ([true, int8(1)]), "int8")
%!assert (class ([true, uint64(1)]), "uint64")
%!assert (class ([true, uint32(1)]), "uint32")
%!assert (class ([true, uint16(1)]), "uint16")
%!assert (class ([true, uint8(1)]), "uint8")
%!assert (class ([true, single(1)]), "single")
%!assert (class ([true, double(1)]), "double")
%!assert (class ([true, cell(1)]), "cell")
%!assert (class ([true, true]), "logical")
%!assert (class ([true, "a"]), "char")

%!assert (class (["a", int64(1)]), "char")
%!assert (class (["a", int32(1)]), "char")
%!assert (class (["a", int16(1)]), "char")
%!assert (class (["a", int8(1)]), "char")
%!assert (class (["a", int64(1)]), "char")
%!assert (class (["a", int32(1)]), "char")
%!assert (class (["a", int16(1)]), "char")
%!assert (class (["a", int8(1)]), "char")
%!assert (class (["a", single(1)]), "char")
%!assert (class (["a", double(1)]), "char")
%!assert (class (["a", cell(1)]), "cell")
%!assert (class (["a", true]), "char")
%!assert (class (["a", "a"]), "char")

%!assert (class ([cell(1), struct("foo", "bar")]), "cell")
%!error [struct("foo", "bar"), cell(1)]

%!assert ([,1], 1)
%!assert ([1,], 1)
%!assert ([,1,], 1)
%!assert ([,1,;;], 1)
%!assert ([,1,;,;], 1)

%!assert ([1,1], ones (1, 2))
%!assert ([,1,1], ones (1, 2))
%!assert ([1,1,], ones (1, 2))
%!assert ([,1,1,], ones (1, 2))
%!assert ([,1,1,;;], ones (1, 2))
%!assert ([,1,1,;,;], ones (1, 2))
%!assert ([,;,1,1], ones (1, 2))

%!assert ([1;1], ones (2, 1))
%!assert ([1,;1], ones (2, 1))
%!assert ([1,;,;1], ones (2, 1))

%!error eval ("[,,]")
%!error eval ("[,,;,]")
%!error eval ("[,;,,;,]")

%!assert (isnull ([,]))
%!assert (isnull ([;]))
%!assert (isnull ([;;]))
%!assert (isnull ([;,;]))
%!assert (isnull ([,;,;,]))
*/

DEFUN (string_fill_char, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} string_fill_char ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} string_fill_char (@var{new_val})\n\
@deftypefnx {Built-in Function} {} string_fill_char (@var{new_val}, \"local\")\n\
Query or set the internal variable used to pad all rows of a character\n\
matrix to the same length.\n\
\n\
The value must be a single character and the default is @qcode{\" \"} (a\n\
single space).  For example:\n\
\n\
@example\n\
@group\n\
string_fill_char (\"X\");\n\
[ \"these\"; \"are\"; \"strings\" ]\n\
      @result{}  \"theseXX\"\n\
          \"areXXXX\"\n\
          \"strings\"\n\
@end group\n\
@end example\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (string_fill_char);
}

/*
## string_fill_char() function call must be outside of %!test block
## due to the way a %!test block is wrapped inside a function
%!shared orig_val, old_val
%! orig_val = string_fill_char ();
%! old_val  = string_fill_char ("X");
%!test
%! assert (orig_val, old_val);
%! assert (string_fill_char (), "X");
%! assert (["these"; "are"; "strings"], ["theseXX"; "areXXXX"; "strings"]);
%! string_fill_char (orig_val);
%! assert (string_fill_char (), orig_val);

%!error (string_fill_char (1, 2))
*/
