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

#include <cctype>
#include <iostream>

#include "lo-ieee.h"
#include "mx-base.h"

#include "mxarray.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-ch-mat.h"
#include "gripes.h"
#include "pr-output.h"

template class octave_base_matrix<charNDArray>;

idx_vector
octave_char_matrix::index_vector (bool /* require_integers */) const
{
  const char *p = matrix.data ();
  if (numel () == 1 && *p == ':')
    return idx_vector (':');
  else
    return idx_vector (array_value (true));
}

double
octave_char_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "character matrix", "real scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "real scalar");

  return retval;
}

float
octave_char_matrix::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "character matrix", "real scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "real scalar");

  return retval;
}

octave_int64
octave_char_matrix::int64_scalar_value () const
{
  octave_int64 retval = 0;

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "character matrix", "int64 scalar");

      retval = octave_int64 (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "int64 scalar");

  return retval;
}

octave_uint64
octave_char_matrix::uint64_scalar_value () const
{
  octave_uint64 retval = 0;

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "character matrix", "uint64 scalar");

      retval = octave_uint64 (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "uint64 scalar");

  return retval;
}

Complex
octave_char_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "character matrix", "complex scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "complex scalar");

  return retval;
}

FloatComplex
octave_char_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "character matrix", "complex scalar");

      retval = static_cast<unsigned char> (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("character matrix", "complex scalar");

  return retval;
}

void
octave_char_matrix::print_raw (std::ostream& os,
                               bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
                         current_print_indent_level ());
}

mxArray *
octave_char_matrix::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxCHAR_CLASS, dims (), mxREAL);

  mxChar *pr = static_cast<mxChar *> (retval->get_data ());

  mwSize nel = numel ();

  const char *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    pr[i] = p[i];

  return retval;
}

// The C++ standard guarantees cctype defines functions, not macros (and
// hence macros *CAN'T* be defined if only cctype is included) so
// there's no need to fuck around.  The exceptions are isascii and
// toascii, which are not C++.  Oddly enough, all those character
// functions are int (*) (int), even in C++.  Wicked!
static inline int xisascii (int c)
{
#ifdef HAVE_ISASCII
  return isascii (c);
#else
  return (c >= 0x00 && c <= 0x7f);
#endif
}

static inline int xtoascii (int c)
{
#ifdef HAVE_TOASCII
  return toascii (c);
#else
  return (c & 0x7F);
#endif
}

octave_value
octave_char_matrix::map (unary_mapper_t umap) const
{
  octave_value retval;

  switch (umap)
    {
#define STRING_MAPPER(UMAP,FCN,TYPE) \
    case umap_ ## UMAP: \
      return octave_value (matrix.map<TYPE, int (&) (int)> (FCN))

    STRING_MAPPER (xisalnum, std::isalnum, bool);
    STRING_MAPPER (xisalpha, std::isalpha, bool);
    STRING_MAPPER (xisascii, xisascii, bool);
    STRING_MAPPER (xiscntrl, std::iscntrl, bool);
    STRING_MAPPER (xisdigit, std::isdigit, bool);
    STRING_MAPPER (xisgraph, std::isgraph, bool);
    STRING_MAPPER (xislower, std::islower, bool);
    STRING_MAPPER (xisprint, std::isprint, bool);
    STRING_MAPPER (xispunct, std::ispunct, bool);
    STRING_MAPPER (xisspace, std::isspace, bool);
    STRING_MAPPER (xisupper, std::isupper, bool);
    STRING_MAPPER (xisxdigit, std::isxdigit, bool);
    STRING_MAPPER (xtoascii, xtoascii, double);
    STRING_MAPPER (xtolower, std::tolower, char);
    STRING_MAPPER (xtoupper, std::toupper, char);

      // For Matlab compatibility, these should work on ASCII values
      // without error or warning.
    case umap_abs:
    case umap_ceil:
    case umap_fix:
    case umap_floor:
    case umap_imag:
    case umap_isinf:
    case umap_isnan:
    case umap_real:
    case umap_round:
      {
        octave_matrix m (array_value (true));
        return m.map (umap);
      }

    default:
      error ("%s: expecting numeric argument", get_umap_name (umap));
      break;
    }

  return retval;
}
