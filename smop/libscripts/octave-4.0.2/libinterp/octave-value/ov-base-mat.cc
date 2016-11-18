/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#include "Array-util.h"

#include "Cell.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-scalar.h"
#include "pr-output.h"

template <class MT>
octave_value
octave_base_matrix<MT>::subsref (const std::string& type,
                                 const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
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

  return retval.next_subsref (type, idx);
}

template <class MT>
octave_value
octave_base_matrix<MT>::subsasgn (const std::string& type,
                                  const std::list<octave_value_list>& idx,
                                  const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
        if (type.length () == 1)
          retval = numeric_assign (type, idx, rhs);
        else if (is_empty ())
          {
            // Allow conversion of empty matrix to some other type in
            // cases like
            //
            //  x = []; x(i).f = rhs

            if (type[1] == '.')
              {
                octave_value tmp = octave_value::empty_conv (type, rhs);

                retval = tmp.subsasgn (type, idx, rhs);
              }
            else
              error ("invalid assignment expression");
          }
        else
          {
            std::string nm = type_name ();
            error ("in indexed assignment of %s, last lhs index must be ()",
                   nm.c_str ());
          }
      }
      break;

    case '{':
    case '.':
      {
        if (is_empty ())
          {
            octave_value tmp = octave_value::empty_conv (type, rhs);

            retval = tmp.subsasgn (type, idx, rhs);
          }
        else
          {
            std::string nm = type_name ();
            error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
          }
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <class MT>
octave_value
octave_base_matrix<MT>::do_index_op (const octave_value_list& idx,
                                     bool resize_ok)
{
  octave_value retval;

  octave_idx_type n_idx = idx.length ();

  int nd = matrix.ndims ();
  const MT& cmatrix = matrix;

  switch (n_idx)
    {
    case 0:
      retval = matrix;
      break;

    case 1:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            // optimize single scalar index.
            if (! resize_ok && i.is_scalar ())
              retval = cmatrix.checkelem (i(0));
            else
              retval = MT (matrix.index (i, resize_ok));
          }
      }
      break;

    case 2:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx (1).index_vector ();

            if (! error_state)
              {
                // optimize two scalar indices.
                if (! resize_ok && i.is_scalar () && j.is_scalar ())
                  retval = cmatrix.checkelem (i(0), j(0));
                else
                  retval = MT (matrix.index (i, j, resize_ok));
              }
          }
      }
      break;

    default:
      {
        Array<idx_vector> idx_vec (dim_vector (n_idx, 1));
        bool scalar_opt = n_idx == nd && ! resize_ok;
        const dim_vector dv = matrix.dims ();

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            idx_vec(i) = idx(i).index_vector ();

            if (error_state)
              break;

            scalar_opt = (scalar_opt && idx_vec(i).is_scalar ());
          }

        if (! error_state)
          {
            if (scalar_opt)
              retval = cmatrix.checkelem (conv_to_int_array (idx_vec));
            else
              retval = MT (matrix.index (idx_vec, resize_ok));
          }
      }
      break;
    }

  return retval;
}

template <class MT>
void
octave_base_matrix<MT>::assign (const octave_value_list& idx, const MT& rhs)
{
  octave_idx_type n_idx = idx.length ();

  switch (n_idx)
    {
    case 0:
      panic_impossible ();
      break;

    case 1:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          matrix.assign (i, rhs);
      }
      break;

    case 2:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx (1).index_vector ();

            if (! error_state)
              matrix.assign (i, j, rhs);
          }
      }
      break;

    default:
      {
        Array<idx_vector> idx_vec (dim_vector (n_idx, 1));

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            idx_vec(i) = idx(i).index_vector ();

            if (error_state)
              break;
          }

        if (! error_state)
          matrix.assign (idx_vec, rhs);
      }
      break;
    }

  // Clear cache.
  clear_cached_info ();
}

template <class MT>
MatrixType
octave_base_matrix<MT>::matrix_type (const MatrixType& _typ) const
{
  delete typ;
  typ = new MatrixType (_typ);
  return *typ;
}

template <class MT>
void
octave_base_matrix<MT>::assign (const octave_value_list& idx,
                                typename MT::element_type rhs)
{
  octave_idx_type n_idx = idx.length ();

  int nd = matrix.ndims ();

  MT mrhs (dim_vector (1, 1), rhs);

  switch (n_idx)
    {
    case 0:
      panic_impossible ();
      break;

    case 1:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            // optimize single scalar index.
            if (i.is_scalar () && i(0) < matrix.numel ())
              matrix(i(0)) = rhs;
            else
              matrix.assign (i, mrhs);
          }
      }
      break;

    case 2:
      {
        idx_vector i = idx (0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx (1).index_vector ();

            if (! error_state)
              {
                // optimize two scalar indices.
                if (i.is_scalar () && j.is_scalar () && nd == 2
                    && i(0) < matrix.rows () && j(0) < matrix.columns ())
                  matrix(i(0), j(0)) = rhs;
                else
                  matrix.assign (i, j, mrhs);
              }
          }
      }
      break;

    default:
      {
        Array<idx_vector> idx_vec (dim_vector (n_idx, 1));
        bool scalar_opt = n_idx == nd;
        const dim_vector dv = matrix.dims ().redim (n_idx);

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            idx_vec(i) = idx(i).index_vector ();

            if (error_state)
              break;

            scalar_opt = (scalar_opt && idx_vec(i).is_scalar ()
                          && idx_vec(i)(0) < dv(i));
          }

        if (! error_state)
          {
            if (scalar_opt)
              {
                // optimize all scalar indices. Don't construct an index array,
                // but rather calc a scalar index directly.
                octave_idx_type k = 1;
                octave_idx_type j = 0;
                for (octave_idx_type i = 0; i < n_idx; i++)
                  {
                    j += idx_vec(i)(0) * k;
                    k *= dv (i);
                  }
                matrix(j) = rhs;
              }
            else
              matrix.assign (idx_vec, mrhs);
          }
      }
      break;
    }

  // Clear cache.
  clear_cached_info ();
}

template <class MT>
void
octave_base_matrix<MT>::delete_elements (const octave_value_list& idx)
{
  octave_idx_type len = idx.length ();

  Array<idx_vector> ra_idx (dim_vector (len, 1));

  for (octave_idx_type i = 0; i < len; i++)
    ra_idx(i) = idx(i).index_vector ();

  matrix.delete_elements (ra_idx);

  // Clear cache.
  clear_cached_info ();
}

template <class MT>
octave_value
octave_base_matrix<MT>::resize (const dim_vector& dv, bool fill) const
{
  MT retval (matrix);
  if (fill)
    retval.resize (dv, 0);
  else
    retval.resize (dv);
  return retval;
}

template <class MT>
bool
octave_base_matrix<MT>::is_true (void) const
{
  bool retval = false;
  dim_vector dv = matrix.dims ();
  int nel = dv.numel ();

  if (nel > 0)
    {
      MT t1 (matrix.reshape (dim_vector (nel, 1)));

      if (t1.any_element_is_nan ())
        gripe_nan_to_logical_conversion ();
      else
        {
          boolNDArray t2 = t1.all ();

          retval = t2(0);
        }
    }

  return retval;
}

template <class MT>
bool
octave_base_matrix<MT>::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

template <class MT>
void
octave_base_matrix<MT>::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class MT>
void
octave_base_matrix<MT>::print_info (std::ostream& os,
                                    const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}

template <class MT>
void
octave_base_matrix<MT>::short_disp (std::ostream& os) const
{
  if (matrix.is_empty ())
    os << "[]";
  else if (matrix.ndims () == 2)
    {
      // FIXME: should this be configurable?
      octave_idx_type max_elts = 10;
      octave_idx_type elts = 0;

      octave_idx_type nel = matrix.numel ();

      octave_idx_type nr = matrix.rows ();
      octave_idx_type nc = matrix.columns ();

      os << "[";

      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              std::ostringstream buf;
              octave_print_internal (buf, matrix(j*nr+i));
              std::string tmp = buf.str ();
              size_t pos = tmp.find_first_not_of (" ");
              if (pos != std::string::npos)
                os << tmp.substr (pos);
              else if (! tmp.empty ())
                os << tmp[0];

              if (++elts >= max_elts)
                goto done;

              if (j < nc - 1)
                os << ", ";
            }

          if (i < nr - 1 && elts < max_elts)
            os << "; ";
        }

    done:

      if (nel <= max_elts)
        os << "]";
    }
  else
    os << "...";
}

template <class MT>
octave_value
octave_base_matrix<MT>::fast_elem_extract (octave_idx_type n) const
{
  if (n < matrix.numel ())
    return matrix(n);
  else
    return octave_value ();
}

template <class MT>
bool
octave_base_matrix<MT>::fast_elem_insert (octave_idx_type n,
                                          const octave_value& x)
{
  if (n < matrix.numel ())
    {
      // Don't use builtin_type () here to avoid an extra VM call.
      typedef typename MT::element_type ET;
      const builtin_type_t btyp = class_to_btyp<ET>::btyp;
      if (btyp == btyp_unknown) // Dead branch?
        return false;

      // Set up the pointer to the proper place.
      void *here = reinterpret_cast<void *> (&matrix(n));
      // Ask x to store there if it can.
      return x.get_rep ().fast_elem_insert_self (here, btyp);
    }
  else
    return false;
}
