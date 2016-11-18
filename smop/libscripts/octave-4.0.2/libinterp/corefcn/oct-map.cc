/*

Copyright (C) 1995-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#include "Array-util.h"
#include "error.h"
#include "oct-locbuf.h"
#include "str-vec.h"

#include "oct-map.h"
#include "utils.h"

octave_fields::fields_rep *
octave_fields::nil_rep (void)
{
  static fields_rep nr;
  return &nr;
}

octave_fields::octave_fields (const string_vector& fields)
  : rep (new fields_rep)
{
  octave_idx_type n = fields.numel ();
  for (octave_idx_type i = 0; i < n; i++)
    (*rep)[fields(i)] = i;
}

octave_fields::octave_fields (const char * const *fields)
  : rep (new fields_rep)
{
  octave_idx_type n = 0;
  while (*fields)
    (*rep)[std::string (*fields++)] = n++;
}

bool
octave_fields::isfield (const std::string& field) const
{
  return rep->find (field) != rep->end ();
}

octave_idx_type
octave_fields::getfield (const std::string& field) const
{
  fields_rep::iterator p = rep->find (field);
  return (p != rep->end ()) ? p->second : -1;
}

octave_idx_type
octave_fields::getfield (const std::string& field)
{
  fields_rep::iterator p = rep->find (field);
  if (p != rep->end ())
    return p->second;
  else
    {
      make_unique ();
      octave_idx_type n = rep->size ();
      return (*rep)[field] = n;
    }
}

octave_idx_type
octave_fields::rmfield (const std::string& field)
{
  fields_rep::iterator p = rep->find (field);
  if (p == rep->end ())
    return -1;
  else
    {
      octave_idx_type n = p->second;
      make_unique ();
      rep->erase (field);
      for (fields_rep::iterator q = rep->begin (); q != rep->end (); q++)
        {
          if (q->second >= n)
            q->second--;
        }

      return n;
    }
}

void
octave_fields::orderfields (Array<octave_idx_type>& perm)
{
  octave_idx_type n = rep->size ();
  perm.clear (n, 1);

  make_unique ();
  octave_idx_type i = 0;
  for (fields_rep::iterator q = rep->begin (); q != rep->end (); q++)
    {
      octave_idx_type j = q->second;
      q->second = i;
      perm(i++) = j;
    }
}

bool
octave_fields::equal_up_to_order (const octave_fields& other,
                                  octave_idx_type* perm) const
{
  bool retval = true;

  iterator p = begin ();
  iterator q = other.begin ();
  for (; p != end () && q != other.end (); p++, q++)
    {
      if (p->first == q->first)
        perm[p->second] = q->second;
      else
        {
          retval = false;
          break;
        }
    }

  retval = (p == end () && q == other.end ());

  return retval;
}

bool
octave_fields::equal_up_to_order (const octave_fields& other,
                                  Array<octave_idx_type>& perm) const
{
  octave_idx_type n = nfields ();
  if (perm.length () != n)
    perm.clear (1, n);

  return equal_up_to_order (other, perm.fortran_vec ());
}

string_vector
octave_fields::fieldnames (void) const
{
  octave_idx_type n = nfields ();
  string_vector retval(n);

  for (iterator p = begin (); p != end (); p++)
    retval.xelem (p->second) = p->first;

  return retval;
}

octave_value
octave_scalar_map::getfield (const std::string& k) const
{
  octave_idx_type idx = xkeys.getfield (k);
  return (idx >= 0) ? xvals[idx] : octave_value ();
}

void
octave_scalar_map::setfield (const std::string& k, const octave_value& val)
{
  octave_idx_type idx = xkeys.getfield (k);
  if (idx < static_cast<octave_idx_type> (xvals.size ()))
    xvals[idx] = val;
  else
    xvals.push_back (val);
}

void
octave_scalar_map::rmfield (const std::string& k)
{
  octave_idx_type idx = xkeys.rmfield (k);
  if (idx >= 0)
    xvals.erase (xvals.begin () + idx);
}

octave_scalar_map
octave_scalar_map::orderfields (void) const
{
  Array<octave_idx_type> perm;
  return orderfields (perm);
}

octave_scalar_map
octave_scalar_map::orderfields (Array<octave_idx_type>& perm) const
{
  octave_scalar_map retval (xkeys);
  retval.xkeys.orderfields (perm);

  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    retval.xvals[i] = xvals[perm.xelem (i)];

  return retval;
}

octave_scalar_map
octave_scalar_map::orderfields (const octave_scalar_map& other,
                                Array<octave_idx_type>& perm) const
{
  if (xkeys.is_same (other.xkeys))
    return *this;
  else
    {
      octave_scalar_map retval (other.xkeys);
      if (other.xkeys.equal_up_to_order (xkeys, perm))
        {
          octave_idx_type nf = nfields ();
          for (octave_idx_type i = 0; i < nf; i++)
            retval.xvals[i] = xvals[perm.xelem (i)];
        }
      else
        error ("orderfields: structs must have same fields up to order");

      return retval;
    }
}

octave_value
octave_scalar_map::contents (const std::string& k) const
{
  return getfield (k);
}

octave_value&
octave_scalar_map::contents (const std::string& k)
{
  octave_idx_type idx = xkeys.getfield (k);
  if (idx >= static_cast<octave_idx_type> (xvals.size ()))
    xvals.resize (idx+1);
  return xvals[idx];
}

octave_map::octave_map (const octave_scalar_map& m)
  : xkeys (m.xkeys), xvals (), dimensions (1, 1)
{
  octave_idx_type nf = m.nfields ();
  xvals.reserve (nf);
  for (octave_idx_type i = 0; i < nf; i++)
    {
      xvals.push_back (Cell (dimensions));
      xvals[i].xelem (0) = m.xvals[i];
    }
}

Cell
octave_map::getfield (const std::string& k) const
{
  octave_idx_type idx = xkeys.getfield (k);
  return (idx >= 0) ? xvals[idx] : Cell ();
}

void
octave_map::setfield (const std::string& k, const Cell& val)
{
  if (nfields () == 0)
    dimensions = val.dims ();

  if (val.dims () == dimensions)
    {
      octave_idx_type idx = xkeys.getfield (k);
      if (idx < static_cast<octave_idx_type> (xvals.size ()))
        xvals[idx] = val;
      else
        xvals.push_back (val);
    }
  else
    error ("octave_map::setfield: internal error");
}

void
octave_map::rmfield (const std::string& k)
{
  octave_idx_type idx = xkeys.rmfield (k);
  if (idx >= 0)
    xvals.erase (xvals.begin () + idx);
}

octave_map
octave_map::orderfields (void) const
{
  Array<octave_idx_type> perm;
  return orderfields (perm);
}

octave_map
octave_map::orderfields (Array<octave_idx_type>& perm) const
{
  octave_map retval (xkeys);
  retval.xkeys.orderfields (perm);

  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    retval.xvals[i] = xvals[perm.xelem (i)];

  return retval;
}

octave_map
octave_map::orderfields (const octave_map& other,
                         Array<octave_idx_type>& perm) const
{
  if (xkeys.is_same (other.xkeys))
    return *this;
  else
    {
      octave_map retval (other.xkeys);
      if (other.xkeys.equal_up_to_order (xkeys, perm))
        {
          octave_idx_type nf = nfields ();
          for (octave_idx_type i = 0; i < nf; i++)
            retval.xvals[i] = xvals[perm.xelem (i)];
        }
      else
        error ("orderfields: structs must have same fields up to order");

      return retval;
    }
}

Cell
octave_map::contents (const std::string& k) const
{
  return getfield (k);
}

Cell&
octave_map::contents (const std::string& k)
{
  octave_idx_type idx = xkeys.getfield (k);
  if (idx >= static_cast<octave_idx_type> (xvals.size ()))
    xvals.push_back (Cell (dimensions)); // auto-set correct dims.
  return xvals[idx];
}

void
octave_map::extract_scalar (octave_scalar_map& dest,
                            octave_idx_type idx) const
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    dest.xvals[i] = xvals[i](idx);
}

octave_scalar_map
octave_map::checkelem (octave_idx_type n) const
{
  octave_scalar_map retval (xkeys);

  // Optimize this so that there is just one check.
  extract_scalar (retval, compute_index (n, dimensions));

  return retval;
}

octave_scalar_map
octave_map::checkelem (octave_idx_type i, octave_idx_type j) const
{
  octave_scalar_map retval (xkeys);

  // Optimize this so that there is just one check.
  extract_scalar (retval, compute_index (i, j, dimensions));

  return retval;
}

octave_scalar_map
octave_map::checkelem (const Array<octave_idx_type>& ra_idx) const
{
  octave_scalar_map retval (xkeys);

  // Optimize this so that there is just one check.
  extract_scalar (retval, compute_index (ra_idx, dimensions));

  return retval;
}

octave_scalar_map
octave_map::fast_elem_extract (octave_idx_type n) const
{
  octave_scalar_map retval (xkeys);

  extract_scalar (retval, n);

  return retval;
}

bool
octave_map::fast_elem_insert (octave_idx_type n,
                              const octave_scalar_map& rhs)
{
  bool retval = false;

  octave_idx_type nf = nfields ();
  if (rhs.xkeys.is_same (xkeys))
    {
      for (octave_idx_type i = 0; i < nf; i++)
        xvals[i](n) = rhs.xvals[i];

      retval = true;
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (octave_idx_type, perm, nf);
      if (xkeys.equal_up_to_order (rhs.xkeys, perm))
        {
          for (octave_idx_type i = 0; i < nf; i++)
            xvals[i](n) = rhs.xvals[perm[i]];

          retval = true;
        }
    }

  return retval;
}

octave_map
octave_map::squeeze (void) const
{
  octave_map retval (*this);
  octave_idx_type nf = nfields ();

  retval.dimensions = dimensions.squeeze ();

  for (octave_idx_type i = 0; i < nf; i++)
    retval.xvals[i] = xvals[i].squeeze ();

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of xkeys by squeeze
%!test
%! x(1,1,1,1).d = 10;  x(3,5,1,7).a = "b";  x(2,4,1,7).f = 27;
%! assert (fieldnames (squeeze (x)), {"d"; "a"; "f"});
*/

octave_map
octave_map::permute (const Array<int>& vec, bool inv) const
{
  octave_map retval (xkeys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type i = 0; i < nf; i++)
    retval.xvals[i] = xvals[i].permute (vec, inv);

  // FIXME:
  // There is no dim_vector::permute for technical reasons.
  // We pick the dim vector from results if possible, otherwise use a dummy
  // array to get it. Need (?) a better solution to this problem.
  if (nf > 0)
    retval.dimensions = retval.xvals[0].dims ();
  else
    {
      Array<char> dummy (dimensions);
      dummy = dummy.permute (vec, inv);
      retval.dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of key order by permute
%!test
%! x(1,1,1,1).d = 10;  x(3,5,1,7).a = "b";  x(2,4,1,7).f = 27;
%! assert (fieldnames (permute (x, [3, 4, 1, 2])), {"d"; "a"; "f"});
*/

octave_map
octave_map::transpose (void) const
{
  assert (ndims () == 2);

  octave_map retval (xkeys);

  retval.dimensions = dim_vector (dimensions (1), dimensions (0));

  octave_idx_type nf = nfields ();
  for (octave_idx_type i = 0; i < nf; i++)
    retval.xvals[i] = xvals[i].transpose ();

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of key order by transpose
%!test
%! x(1,1).d = 10;  x(3,5).a = "b";  x(2,4).f = 27;
%! assert (fieldnames (transpose (x)), {"d"; "a"; "f"});
%! assert (fieldnames (x'), {"d"; "a"; "f"});
%! assert (fieldnames (x.'), {"d"; "a"; "f"});
*/

octave_map
octave_map::reshape (const dim_vector& dv) const
{
  octave_map retval (xkeys);
  retval.dimensions = dv;

  octave_idx_type nf = nfields ();
  if (nf > 0)
    {
      retval.xvals.reserve (nf);
      for (octave_idx_type i = 0; i < nf; i++)
        retval.xvals[i] = xvals[i].reshape (dv);
    }
  else
    {
      // FIXME: Do it with a dummy array, to reuse error message.
      // Need (?) a better solution.
      Array<char> dummy (dimensions);
      dummy.reshape (dv);
    }

  retval.optimize_dimensions ();

  return retval;
}

/*
## test preservation of key order by reshape
%!test
%! x(1,1).d = 10;  x(4,6).a = "b";  x(2,4).f = 27;
%! assert (fieldnames (reshape (x, 3, 8)), {"d"; "a"; "f"});
*/

void
octave_map::resize (const dim_vector& dv, bool fill)
{
  octave_idx_type nf = nfields ();
  if (nf > 0)
    {
      for (octave_idx_type i = 0; i < nf; i++)
        {
          if (fill)
            xvals[i].resize (dv, Matrix ());
          else
            xvals[i].resize (dv);
        }
    }
  else
    {
      // FIXME: Do it with a dummy array, to reuse error message.
      // Need (?) a better solution.
      Array<char> dummy (dimensions);
      dummy.resize (dv);
    }

  dimensions = dv;
  optimize_dimensions ();
}

void
octave_map::do_cat (int dim, octave_idx_type n,
                    const octave_scalar_map *map_list,
                    octave_map& retval)
{
  octave_idx_type nf = retval.nfields ();
  retval.xvals.reserve (nf);

  dim_vector& rd = retval.dimensions;
  rd.resize (dim+1, 1);
  rd(0) = rd(1) = 1;
  rd(dim) = n;

  for (octave_idx_type j = 0; j < nf; j++)
    {
      retval.xvals.push_back (Cell (rd));
      assert (retval.xvals[j].numel () == n);
      for (octave_idx_type i = 0; i < n; i++)
        retval.xvals[j].xelem (i) = map_list[i].xvals[j];
    }
}

void
octave_map::do_cat (int dim, octave_idx_type n, const octave_map *map_list,
                    octave_map& retval)
{
  octave_idx_type nf = retval.nfields ();
  retval.xvals.reserve (nf);

  OCTAVE_LOCAL_BUFFER (Array<octave_value>, field_list, n);

  for (octave_idx_type j = 0; j < nf; j++)
    {
      for (octave_idx_type i = 0; i < n; i++)
        field_list[i] = map_list[i].xvals[j];

      retval.xvals.push_back (Array<octave_value>::cat (dim, n, field_list));
      if (j == 0)
        retval.dimensions = retval.xvals[j].dims ();
    }
}

// This is just a wrapper.
void permute_to_correct_order1 (const octave_scalar_map& ref,
                                const octave_scalar_map& src,
                                octave_scalar_map& dest,
                                Array<octave_idx_type>& perm)
{
  dest = src.orderfields (ref, perm);
}

// In non-scalar case, we also promote empty structs without fields.
void permute_to_correct_order1 (const octave_map& ref, const octave_map& src,
                                octave_map& dest, Array<octave_idx_type>& perm)
{
  if (src.nfields () == 0 && src.is_empty ())
    dest = octave_map (src.dims (), ref.keys ());
  else
    dest = src.orderfields (ref, perm);
}

template <class map>
static void
permute_to_correct_order (octave_idx_type n, octave_idx_type nf,
                          octave_idx_type idx, const map *map_list,
                          map *new_map_list)
{
  new_map_list[idx] = map_list[idx];

  Array<octave_idx_type> perm (dim_vector (1, nf));

  for (octave_idx_type i = 0; i < n; i++)
    {
      if (i == idx)
        continue;

      permute_to_correct_order1 (map_list[idx], map_list[i], new_map_list[i],
                                 perm);

      if (error_state)
        {
          // Use liboctave exception to be consistent.
          (*current_liboctave_error_handler)
            ("cat: field names mismatch in concatenating structs");
          break;
        }
    }
}


octave_map
octave_map::cat (int dim, octave_idx_type n, const octave_scalar_map *map_list)
{
  octave_map retval;

  // Allow dim = -1, -2 for compatibility, though it makes no difference here.
  if (dim == -1 || dim == -2)
    dim = -dim - 1;
  else if (dim < 0)
    (*current_liboctave_error_handler)
      ("cat: invalid dimension");

  if (n == 1)
    retval = map_list[0];
  else if (n > 1)
    {
      octave_idx_type idx, nf = 0;
      for (idx = 0; idx < n; idx++)
        {
          nf = map_list[idx].nfields ();
          if (nf > 0)
            {
              retval.xkeys = map_list[idx].xkeys;
              break;
            }
        }

      if (nf > 0)
        {
          // Try the fast case.
          bool all_same = true;
          for (octave_idx_type i = 0; i < n; i++)
            {
              all_same = map_list[idx].xkeys.is_same (map_list[i].xkeys);
              if (! all_same)
                break;
            }

          if (all_same)
            do_cat (dim, n, map_list, retval);
          else
            {
              // permute all structures to common order.
              OCTAVE_LOCAL_BUFFER (octave_scalar_map, new_map_list, n);

              permute_to_correct_order (n, nf, idx, map_list, new_map_list);

              do_cat (dim, n, new_map_list, retval);
            }

        }
      else
        {
          dim_vector& rd = retval.dimensions;
          rd.resize (dim+1, 1);
          rd(0) = rd(1) = 1;
          rd(dim) = n;
        }

      retval.optimize_dimensions ();
    }

  return retval;
}

octave_map
octave_map::cat (int dim, octave_idx_type n, const octave_map *map_list)
{
  octave_map retval;

  // Allow dim = -1, -2 for compatibility, though it makes no difference here.
  if (dim == -1 || dim == -2)
    dim = -dim - 1;
  else if (dim < 0)
    (*current_liboctave_error_handler)
      ("cat: invalid dimension");

  if (n == 1)
    retval = map_list[0];
  else if (n > 1)
    {
      octave_idx_type idx, nf = 0;

      for (idx = 0; idx < n; idx++)
        {
          nf = map_list[idx].nfields ();
          if (nf > 0)
            {
              retval.xkeys = map_list[idx].xkeys;
              break;
            }
        }

      // Try the fast case.
      bool all_same = true;

      if (nf > 0)
        {
          for (octave_idx_type i = 0; i < n; i++)
            {
              all_same = map_list[idx].xkeys.is_same (map_list[i].xkeys);

              if (! all_same)
                break;
            }
        }

      if (all_same && nf > 0)
        do_cat (dim, n, map_list, retval);
      else
        {
          if (nf > 0)
            {
              // permute all structures to correct order.
              OCTAVE_LOCAL_BUFFER (octave_map, new_map_list, n);

              permute_to_correct_order (n, nf, idx, map_list, new_map_list);

              do_cat (dim, n, new_map_list, retval);
            }
          else
            {
              dim_vector dv = map_list[0].dimensions;

              for (octave_idx_type i = 1; i < n; i++)
                {
                  if (! dv.concat (map_list[i].dimensions, dim))
                    {
                      error ("dimension mismatch in struct concatenation");
                      return retval;
                    }
                }

              retval.dimensions = dv;
            }
        }

      retval.optimize_dimensions ();
    }

  return retval;
}

/*
## test preservation of key order by concatenation
%!test
%! x(1, 1).d = 10;  x(4, 6).a = "b";  x(2, 4).f = 27;
%! y(1, 6).f = 11;  y(1, 6).a = "c";  y(1, 6).d = 33;
%! assert (fieldnames ([x; y]), {"d"; "a"; "f"});

%!test
%! s = struct ();
%! sr = [s,s];
%! sc = [s;s];
%! sm = [s,s;s,s];
%! assert (numfields (sr), 0);
%! assert (numfields (sc), 0);
%! assert (numfields (sm), 0);
%! assert (size (sr), [1, 2]);
%! assert (size (sc), [2, 1]);
%! assert (size (sm), [2, 2]);
*/

octave_map
octave_map::index (const idx_vector& i, bool resize_ok) const
{
  octave_map retval (xkeys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type k = 0; k < nf; k++)
    retval.xvals[k] = xvals[k].index (i, resize_ok);

  if (nf > 0)
    retval.dimensions = retval.xvals[0].dims ();
  else
    {
      // Use dummy array. FIXME: Need(?) a better solution.
      Array<char> dummy (dimensions);
      dummy = dummy.index (i, resize_ok);
      retval.dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

octave_map
octave_map::index (const idx_vector& i, const idx_vector& j,
                   bool resize_ok) const
{
  octave_map retval (xkeys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type k = 0; k < nf; k++)
    retval.xvals[k] = xvals[k].index (i, j, resize_ok);

  if (nf > 0)
    retval.dimensions = retval.xvals[0].dims ();
  else
    {
      // Use dummy array. FIXME: Need(?) a better solution.
      Array<char> dummy (dimensions);
      dummy = dummy.index (i, j, resize_ok);
      retval.dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

octave_map
octave_map::index (const Array<idx_vector>& ia, bool resize_ok) const
{
  octave_map retval (xkeys);
  octave_idx_type nf = nfields ();

  for (octave_idx_type k = 0; k < nf; k++)
    retval.xvals[k] = xvals[k].index (ia, resize_ok);

  if (nf > 0)
    retval.dimensions = retval.xvals[0].dims ();
  else
    {
      // Use dummy array. FIXME: Need(?) a better solution.
      Array<char> dummy (dimensions);
      dummy = dummy.index (ia, resize_ok);
      retval.dimensions = dummy.dims ();
    }

  retval.optimize_dimensions ();

  return retval;
}

octave_map
octave_map::index (const octave_value_list& idx, bool resize_ok) const
{
  octave_idx_type n_idx = idx.length ();
  octave_map retval;

  switch (n_idx)
    {
    case 1:
      {
        idx_vector i = idx(0).index_vector ();

        if (! error_state)
          retval = index (i, resize_ok);
      }
      break;

    case 2:
      {
        idx_vector i = idx(0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx(1).index_vector ();

            retval = index (i, j, resize_ok);
          }
      }
      break;

    default:
      {
        Array<idx_vector> ia (dim_vector (n_idx, 1));

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            ia(i) = idx(i).index_vector ();

            if (error_state)
              break;
          }

        if (! error_state)
          retval = index (ia, resize_ok);
      }
      break;
    }

  return retval;
}

// Perhaps one day these will be optimized. Right now, they just call index.
octave_map
octave_map::column (octave_idx_type k) const
{
  return index (idx_vector::colon, k);
}

octave_map
octave_map::page (octave_idx_type k) const
{
  static Array<idx_vector> ia (dim_vector (3, 1), idx_vector::colon);

  ia(2) = k;
  return index (ia);
}

void
octave_map::assign (const idx_vector& i, const octave_map& rhs)
{
  if (rhs.xkeys.is_same (xkeys))
    {
      octave_idx_type nf = nfields ();

      for (octave_idx_type k = 0; k < nf; k++)
        xvals[k].assign (i, rhs.xvals[k], Matrix ());

      if (nf > 0)
        dimensions = xvals[0].dims ();
      else
        {
          // Use dummy array. FIXME: Need(?) a better solution.
          Array<char> dummy (dimensions), rhs_dummy (rhs.dimensions);
          dummy.assign (i, rhs_dummy);;
          dimensions = dummy.dims ();
        }

      optimize_dimensions ();
    }
  else if (nfields () == 0)
    {
      octave_map tmp (dimensions, rhs.xkeys);
      tmp.assign (i, rhs);
      *this = tmp;
    }
  else
    {
      Array<octave_idx_type> perm;
      octave_map rhs1 = rhs.orderfields (*this, perm);
      if (! error_state)
        {
          assert (rhs1.xkeys.is_same (xkeys));
          assign (i, rhs1);
        }
      else
        error ("incompatible fields in struct assignment");
    }
}

void
octave_map::assign (const idx_vector& i, const idx_vector& j,
                    const octave_map& rhs)
{
  if (rhs.xkeys.is_same (xkeys))
    {
      octave_idx_type nf = nfields ();

      for (octave_idx_type k = 0; k < nf; k++)
        xvals[k].assign (i, j, rhs.xvals[k], Matrix ());

      if (nf > 0)
        dimensions = xvals[0].dims ();
      else
        {
          // Use dummy array. FIXME: Need(?) a better solution.
          Array<char> dummy (dimensions), rhs_dummy (rhs.dimensions);
          dummy.assign (i, j, rhs_dummy);;
          dimensions = dummy.dims ();
        }

      optimize_dimensions ();
    }
  else if (nfields () == 0)
    {
      octave_map tmp (dimensions, rhs.xkeys);
      tmp.assign (i, j, rhs);
      *this = tmp;
    }
  else
    {
      Array<octave_idx_type> perm;
      octave_map rhs1 = rhs.orderfields (*this, perm);
      if (! error_state)
        {
          assert (rhs1.xkeys.is_same (xkeys));
          assign (i, j, rhs1);
        }
      else
        error ("incompatible fields in struct assignment");
    }
}

void
octave_map::assign (const Array<idx_vector>& ia,
                    const octave_map& rhs)
{
  if (rhs.xkeys.is_same (xkeys))
    {
      octave_idx_type nf = nfields ();

      for (octave_idx_type k = 0; k < nf; k++)
        xvals[k].assign (ia, rhs.xvals[k], Matrix ());

      if (nf > 0)
        dimensions = xvals[0].dims ();
      else
        {
          // Use dummy array. FIXME: Need(?) a better solution.
          Array<char> dummy (dimensions), rhs_dummy (rhs.dimensions);
          dummy.assign (ia, rhs_dummy);;
          dimensions = dummy.dims ();
        }

      optimize_dimensions ();
    }
  else if (nfields () == 0)
    {
      octave_map tmp (dimensions, rhs.xkeys);
      tmp.assign (ia, rhs);
      *this = tmp;
    }
  else
    {
      Array<octave_idx_type> perm;
      octave_map rhs1 = rhs.orderfields (*this, perm);
      if (! error_state)
        {
          assert (rhs1.xkeys.is_same (xkeys));
          assign (ia, rhs1);
        }
      else
        error ("incompatible fields in struct assignment");
    }
}

void
octave_map::assign (const octave_value_list& idx, const octave_map& rhs)
{
  octave_idx_type n_idx = idx.length ();

  switch (n_idx)
    {
    case 1:
      {
        idx_vector i = idx(0).index_vector ();

        if (! error_state)
          assign (i, rhs);
      }
      break;

    case 2:
      {
        idx_vector i = idx(0).index_vector ();

        if (! error_state)
          {
            idx_vector j = idx(1).index_vector ();

            assign (i, j, rhs);
          }
      }
      break;

    default:
      {
        Array<idx_vector> ia (dim_vector (n_idx, 1));

        for (octave_idx_type i = 0; i < n_idx; i++)
          {
            ia(i) = idx(i).index_vector ();

            if (error_state)
              break;
          }

        if (! error_state)
          assign (ia, rhs);
      }
      break;
    }
}

void
octave_map::assign (const octave_value_list& idx, const std::string& k,
                    const Cell& rhs)
{
  Cell tmp;
  iterator p = seek (k);
  Cell& ref = p != end () ? contents (p) : tmp;

  if (&ref == &tmp)
    ref = Cell (dimensions);

  ref.assign (idx, rhs);

  if (! error_state && ref.dims () != dimensions)
    {
      dimensions = ref.dims ();

      octave_idx_type nf = nfields ();
      for (octave_idx_type i = 0; i < nf; i++)
        {
          if (&xvals[i] != &ref)
            xvals[i].resize (dimensions, Matrix ());
        }

      optimize_dimensions ();
    }

  if (! error_state && &ref == &tmp)
    setfield (k, tmp);
}

/*
%!test
%! rhs.b = 1;
%! a(3) = rhs;
%! assert ({a.b}, {[], [], 1})
*/

void
octave_map::delete_elements (const idx_vector& i)
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type k = 0; k < nf; k++)
    xvals[k].delete_elements (i);

  if (nf > 0)
    dimensions = xvals[0].dims ();
  else
    {
      // Use dummy array. FIXME: Need(?) a better solution.
      Array<char> dummy (dimensions);
      dummy.delete_elements (i);
      dimensions = dummy.dims ();
    }

  optimize_dimensions ();
}

void
octave_map::delete_elements (int dim, const idx_vector& i)
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type k = 0; k < nf; k++)
    xvals[k].delete_elements (dim, i);

  if (nf > 0)
    dimensions = xvals[0].dims ();
  else
    {
      // Use dummy array. FIXME: Need(?) a better solution.
      Array<char> dummy (dimensions);
      dummy.delete_elements (dim, i);
      dimensions = dummy.dims ();
    }

  optimize_dimensions ();
}

void
octave_map::delete_elements (const Array<idx_vector>& ia)
{
  octave_idx_type nf = nfields ();
  for (octave_idx_type k = 0; k < nf; k++)
    xvals[k].delete_elements (ia);

  if (nf > 0)
    dimensions = xvals[0].dims ();
  else
    {
      // Use dummy array. FIXME: Need(?) a better solution.
      Array<char> dummy (dimensions);
      dummy.delete_elements (ia);
      dimensions = dummy.dims ();
    }

  optimize_dimensions ();
}

void
octave_map::delete_elements (const octave_value_list& idx)
{
  octave_idx_type n_idx = idx.length ();

  Array<idx_vector> ia (dim_vector (n_idx, 1));

  for (octave_idx_type i = 0; i < n_idx; i++)
    {
      ia(i) = idx(i).index_vector ();

      if (error_state)
        break;
    }

  if (! error_state)
    delete_elements (ia);
}

/*
## test preservation of key order by indexing
%!test
%! x(1, 1).d = 10;  x(4, 6).a = "b";  x(2, 4).f = 27;
%! assert (fieldnames (x([1, 2], [2:5])), {"d"; "a"; "f"});
*/

octave_map
octave_map::concat (const octave_map& rb, const Array<octave_idx_type>& ra_idx)
{
  if (nfields () == rb.nfields ())
    {
      for (const_iterator pa = begin (); pa != end (); pa++)
        {
          const_iterator pb = rb.seek (key(pa));

          if (pb == rb.end ())
            {
              error ("field name mismatch in structure concatenation");
              break;
            }

          contents(pa).insert (rb.contents (pb), ra_idx);
        }
    }
  else
    {
      dim_vector dv = dims ();

      if (dv.all_zero ())
        *this = rb;
      else if (! rb.dims ().all_zero ())
        error ("invalid structure concatenation");
    }

  return *this;
}

void
octave_map::optimize_dimensions (void)
{
  octave_idx_type nf = nfields ();

  for (octave_idx_type i = 0; i < nf; i++)
    {
      if (! xvals[i].optimize_dimensions (dimensions))
        {
          error ("internal error: dimension mismatch across fields in struct");
          break;
        }
    }

}

