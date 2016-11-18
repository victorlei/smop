/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 2009 VZLU Prague

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

#include <algorithm>
#include "Array.h"
#include "Sparse.h"
#include "mx-base.h"

#include "ov.h"
#include "Cell.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

// The bulk of the work.
template <class T>
static Array<T>
do_tril (const Array<T>& a, octave_idx_type k, bool pack)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();
  const T *avec = a.fortran_vec ();
  octave_idx_type zero = 0;

  if (pack)
    {
      octave_idx_type j1 = std::min (std::max (zero, k), nc);
      octave_idx_type j2 = std::min (std::max (zero, nr + k), nc);
      octave_idx_type n = j1 * nr + ((j2 - j1) * (nr-(j1-k) + nr-(j2-1-k))) / 2;
      Array<T> r (dim_vector (n, 1));
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j - k), nr);
          rvec = std::copy (avec + ii, avec + nr, rvec);
          avec += nr;
        }

      return r;
    }
  else
    {
      Array<T> r (a.dims ());
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j - k), nr);
          std::fill (rvec, rvec + ii, T ());
          std::copy (avec + ii, avec + nr, rvec + ii);
          avec += nr;
          rvec += nr;
        }

      return r;
    }
}

template <class T>
static Array<T>
do_triu (const Array<T>& a, octave_idx_type k, bool pack)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();
  const T *avec = a.fortran_vec ();
  octave_idx_type zero = 0;

  if (pack)
    {
      octave_idx_type j1 = std::min (std::max (zero, k), nc);
      octave_idx_type j2 = std::min (std::max (zero, nr + k), nc);
      octave_idx_type n
        = ((j2 - j1) * ((j1+1-k) + (j2-k))) / 2 + (nc - j2) * nr;
      Array<T> r (dim_vector (n, 1));
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j + 1 - k), nr);
          rvec = std::copy (avec, avec + ii, rvec);
          avec += nr;
        }

      return r;
    }
  else
    {
      NoAlias<Array<T> > r (a.dims ());
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j + 1 - k), nr);
          std::copy (avec, avec + ii, rvec);
          std::fill (rvec + ii, rvec + nr, T ());
          avec += nr;
          rvec += nr;
        }

      return r;
    }
}

// These two are by David Bateman.
// FIXME: optimizations possible. "pack" support missing.

template <class T>
static Sparse<T>
do_tril (const Sparse<T>& a, octave_idx_type k, bool pack)
{
  if (pack) // FIXME
    {
      error ("tril: \"pack\" not implemented for sparse matrices");
      return Sparse<T> ();
    }

  Sparse<T> m = a;
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      if (m.ridx (i) < j-k)
        m.data(i) = 0.;

  m.maybe_compress (true);
  return m;
}

template <class T>
static Sparse<T>
do_triu (const Sparse<T>& a, octave_idx_type k, bool pack)
{
  if (pack) // FIXME
    {
      error ("triu: \"pack\" not implemented for sparse matrices");
      return Sparse<T> ();
    }

  Sparse<T> m = a;
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      if (m.ridx (i) > j-k)
        m.data(i) = 0.;

  m.maybe_compress (true);
  return m;
}

// Convenience dispatchers.
template <class T>
static Array<T>
do_trilu (const Array<T>& a, octave_idx_type k, bool lower, bool pack)
{
  return lower ? do_tril (a, k, pack) : do_triu (a, k, pack);
}

template <class T>
static Sparse<T>
do_trilu (const Sparse<T>& a, octave_idx_type k, bool lower, bool pack)
{
  return lower ? do_tril (a, k, pack) : do_triu (a, k, pack);
}

static octave_value
do_trilu (const std::string& name,
          const octave_value_list& args)
{
  bool lower = name == "tril";

  octave_value retval;
  int nargin = args.length ();
  octave_idx_type k = 0;
  bool pack = false;
  if (nargin >= 2 && args(nargin-1).is_string ())
    {
      pack = args(nargin-1).string_value () == "pack";
      nargin--;
    }

  if (nargin == 2)
    {
      k = args(1).int_value (true);

      if (error_state)
        return retval;
    }

  if (nargin < 1 || nargin > 2)
    print_usage ();
  else
    {
      octave_value arg = args(0);

      dim_vector dims = arg.dims ();
      if (dims.length () != 2)
        error ("%s: need a 2-D matrix", name.c_str ());
      else if (k < -dims (0) || k > dims(1))
        error ("%s: requested diagonal out of range", name.c_str ());
      else
        {
          switch (arg.builtin_type ())
            {
            case btyp_double:
              if (arg.is_sparse_type ())
                retval = do_trilu (arg.sparse_matrix_value (), k, lower, pack);
              else
                retval = do_trilu (arg.array_value (), k, lower, pack);
              break;
            case btyp_complex:
              if (arg.is_sparse_type ())
                retval = do_trilu (arg.sparse_complex_matrix_value (), k, lower,
                                   pack);
              else
                retval = do_trilu (arg.complex_array_value (), k, lower, pack);
              break;
            case btyp_bool:
              if (arg.is_sparse_type ())
                retval = do_trilu (arg.sparse_bool_matrix_value (), k, lower,
                                   pack);
              else
                retval = do_trilu (arg.bool_array_value (), k, lower, pack);
              break;
#define ARRAYCASE(TYP) \
            case btyp_ ## TYP: \
              retval = do_trilu (arg.TYP ## _array_value (), k, lower, pack); \
              break
            ARRAYCASE (float);
            ARRAYCASE (float_complex);
            ARRAYCASE (int8);
            ARRAYCASE (int16);
            ARRAYCASE (int32);
            ARRAYCASE (int64);
            ARRAYCASE (uint8);
            ARRAYCASE (uint16);
            ARRAYCASE (uint32);
            ARRAYCASE (uint64);
            ARRAYCASE (char);
#undef ARRAYCASE
            default:
              {
                // Generic code that works on octave-values, that is slow
                // but will also work on arbitrary user types

                if (pack) // FIXME
                  {
                    error ("%s: \"pack\" not implemented for class %s",
                           name.c_str (), arg.class_name ().c_str ());
                    return octave_value ();
                  }

                octave_value tmp = arg;
                if (arg.numel () == 0)
                  return arg;

                octave_idx_type nr = dims(0);
                octave_idx_type nc = dims(1);

                // The sole purpose of the below is to force the correct
                // matrix size. This would not be necessary if the
                // octave_value resize function allowed a fill_value.
                // It also allows odd attributes in some user types
                // to be handled. With a fill_value ot should be replaced
                // with
                //
                // octave_value_list ov_idx;
                // tmp = tmp.resize(dim_vector (0,0)).resize (dims, fill_value);

                octave_value_list ov_idx;
                std::list<octave_value_list> idx_tmp;
                ov_idx(1) = static_cast<double> (nc+1);
                ov_idx(0) = Range (1, nr);
                idx_tmp.push_back (ov_idx);
                ov_idx(1) = static_cast<double> (nc);
                tmp = tmp.resize (dim_vector (0,0));
                tmp = tmp.subsasgn ("(",idx_tmp, arg.do_index_op (ov_idx));
                tmp = tmp.resize (dims);

                if (lower)
                  {
                    octave_idx_type st = nc < nr + k ? nc : nr + k;

                    for (octave_idx_type j = 1; j <= st; j++)
                      {
                        octave_idx_type nr_limit = 1 > j - k ? 1 : j - k;
                        ov_idx(1) = static_cast<double> (j);
                        ov_idx(0) = Range (nr_limit, nr);
                        std::list<octave_value_list> idx;
                        idx.push_back (ov_idx);

                        tmp = tmp.subsasgn ("(", idx, arg.do_index_op (ov_idx));

                        if (error_state)
                          return retval;
                      }
                  }
                else
                  {
                    octave_idx_type st = k + 1 > 1 ? k + 1 : 1;

                    for (octave_idx_type j = st; j <= nc; j++)
                      {
                        octave_idx_type nr_limit = nr < j - k ? nr : j - k;
                        ov_idx(1) = static_cast<double> (j);
                        ov_idx(0) = Range (1, nr_limit);
                        std::list<octave_value_list> idx;
                        idx.push_back (ov_idx);

                        tmp = tmp.subsasgn ("(", idx, arg.do_index_op (ov_idx));

                        if (error_state)
                          return retval;
                      }
                  }

                retval = tmp;
              }
            }
        }
    }

  return retval;
}

DEFUN (tril, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Function File} {} tril (@var{A})\n\
@deftypefnx {Function File} {} tril (@var{A}, @var{k})\n\
@deftypefnx {Function File} {} tril (@var{A}, @var{k}, @var{pack})\n\
@deftypefnx {Function File} {} triu (@var{A})\n\
@deftypefnx {Function File} {} triu (@var{A}, @var{k})\n\
@deftypefnx {Function File} {} triu (@var{A}, @var{k}, @var{pack})\n\
Return a new matrix formed by extracting the lower (@code{tril})\n\
or upper (@code{triu}) triangular part of the matrix @var{A}, and\n\
setting all other elements to zero.\n\
\n\
The second argument is optional, and specifies how many diagonals above or\n\
below the main diagonal should also be set to zero.\n\
\n\
The default value of @var{k} is zero, so that @code{triu} and @code{tril}\n\
normally include the main diagonal as part of the result.\n\
\n\
If the value of @var{k} is nonzero integer, the selection of elements starts\n\
at an offset of @var{k} diagonals above or below the main diagonal; above\n\
for positive @var{k} and below for negative @var{k}.\n\
\n\
The absolute value of @var{k} must not be greater than the number of\n\
subdiagonals or superdiagonals.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
tril (ones (3), -1)\n\
     @result{}  0  0  0\n\
         1  0  0\n\
         1  1  0\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
and\n\
\n\
@example\n\
@group\n\
tril (ones (3), 1)\n\
     @result{}  1  1  0\n\
         1  1  1\n\
         1  1  1\n\
@end group\n\
@end example\n\
\n\
If the option @qcode{\"pack\"} is given as third argument, the extracted\n\
elements are not inserted into a matrix, but rather stacked column-wise one\n\
above other.\n\
@seealso{diag}\n\
@end deftypefn")
{
  return do_trilu ("tril", args);
}

DEFUN (triu, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Function File} {} triu (@var{A})\n\
@deftypefnx {Function File} {} triu (@var{A}, @var{k})\n\
@deftypefnx {Function File} {} triu (@var{A}, @var{k}, @var{pack})\n\
See the documentation for the @code{tril} function (@pxref{tril}).\n\
@seealso{tril}\n\
@end deftypefn")
{
  return do_trilu ("triu", args);
}

/*
%!test
%! a = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%!
%! l0 = [1, 0, 0; 4, 5, 0; 7, 8, 9; 10, 11, 12];
%! l1 = [1, 2, 0; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! l2 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! lm1 = [0, 0, 0; 4, 0, 0; 7, 8, 0; 10, 11, 12];
%! lm2 = [0, 0, 0; 0, 0, 0; 7, 0, 0; 10, 11, 0];
%! lm3 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 10, 0, 0];
%! lm4 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%!
%! assert (tril (a, -4), lm4);
%! assert (tril (a, -3), lm3);
%! assert (tril (a, -2), lm2);
%! assert (tril (a, -1), lm1);
%! assert (tril (a), l0);
%! assert (tril (a, 1), l1);
%! assert (tril (a, 2), l2);

%!error tril ()
*/
