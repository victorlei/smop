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

#include <cfloat>
#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"
#include "oct-locbuf.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "pt-exp.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"
#include "dSparse.h"

#include "ls-mat4.h"

// Read LEN elements of data from IS in the format specified by
// PRECISION, placing the result in DATA.  If SWAP is TRUE, swap
// the bytes of each element before copying to DATA.  FLT_FMT
// specifies the format of the data if we are reading floating point
// numbers.

static void
read_mat_binary_data (std::istream& is, double *data, int precision,
                      int len, bool swap,
                      oct_mach_info::float_format flt_fmt)
{
  switch (precision)
    {
    case 0:
      read_doubles (is, data, LS_DOUBLE, len, swap, flt_fmt);
      break;

    case 1:
      read_doubles (is, data, LS_FLOAT, len, swap, flt_fmt);
      break;

    case 2:
      read_doubles (is, data, LS_INT, len, swap, flt_fmt);
      break;

    case 3:
      read_doubles (is, data, LS_SHORT, len, swap, flt_fmt);
      break;

    case 4:
      read_doubles (is, data, LS_U_SHORT, len, swap, flt_fmt);
      break;

    case 5:
      read_doubles (is, data, LS_U_CHAR, len, swap, flt_fmt);
      break;

    default:
      break;
    }
}

int
read_mat_file_header (std::istream& is, bool& swap, int32_t& mopt,
                      int32_t& nr, int32_t& nc,
                      int32_t& imag, int32_t& len,
                      int quiet)
{
  swap = false;

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another mopt value should not result in an
  // error.

  is.read (reinterpret_cast<char *> (&mopt), 4);
  if (! is)
    return 1;

  if (! is.read (reinterpret_cast<char *> (&nr), 4))
    goto data_read_error;

  if (! is.read (reinterpret_cast<char *> (&nc), 4))
    goto data_read_error;

  if (! is.read (reinterpret_cast<char *> (&imag), 4))
    goto data_read_error;

  if (! is.read (reinterpret_cast<char *> (&len), 4))
    goto data_read_error;

// If mopt is nonzero and the byte order is swapped, mopt will be
// bigger than we expect, so we swap bytes.
//
// If mopt is zero, it means the file was written on a little endian
// machine, and we only need to swap if we are running on a big endian
// machine.
//
// Gag me.

  if (oct_mach_info::words_big_endian () && mopt == 0)
    swap = true;

  // mopt is signed, therefore byte swap may result in negative value.

  if (mopt > 9999 || mopt < 0)
    swap = true;

  if (swap)
    {
      swap_bytes<4> (&mopt);
      swap_bytes<4> (&nr);
      swap_bytes<4> (&nc);
      swap_bytes<4> (&imag);
      swap_bytes<4> (&len);
    }

  if (mopt > 9999 || mopt < 0 || imag > 1 || imag < 0)
    {
      if (! quiet)
        error ("load: can't read binary file");
      return -1;
    }

  return 0;

data_read_error:
  return -1;
}

// We don't just use a cast here, because we need to be able to detect
// possible errors.

oct_mach_info::float_format
mopt_digit_to_float_format (int mach)
{
  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;

  switch (mach)
    {
    case 0:
      flt_fmt = oct_mach_info::flt_fmt_ieee_little_endian;
      break;

    case 1:
      flt_fmt = oct_mach_info::flt_fmt_ieee_big_endian;
      break;

    case 2:
    case 3:
    case 4:
    default:
      flt_fmt = oct_mach_info::flt_fmt_unknown;
      break;
    }

  return flt_fmt;
}

int
float_format_to_mopt_digit (oct_mach_info::float_format flt_fmt)
{
  int retval = -1;

  switch (flt_fmt)
    {
    case oct_mach_info::flt_fmt_ieee_little_endian:
      retval = 0;
      break;

    case oct_mach_info::flt_fmt_ieee_big_endian:
      retval = 1;
      break;

    default:
      break;
    }

  return retval;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.
//
// The data is expected to be in Matlab version 4 .mat format, though
// not all the features of that format are supported.
//
// FILENAME is used for error messages.
//
// This format provides no way to tag the data as global.

std::string
read_mat_binary_data (std::istream& is, const std::string& filename,
                      octave_value& tc)
{
  std::string retval;

  // These are initialized here instead of closer to where they are
  // first used to avoid errors from gcc about goto crossing
  // initialization of variable.

  Matrix re;
  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;
  bool swap = false;
  int type = 0;
  int prec = 0;
  int order = 0;
  int mach = 0;
  int dlen = 0;

  int32_t mopt, nr, nc, imag, len;

  int err = read_mat_file_header (is, swap, mopt, nr, nc, imag, len);
  if (err)
    {
      if (err < 0)
        goto data_read_error;
      else
        return retval;
    }

  type = mopt % 10;  // Full, sparse, etc.
  mopt /= 10;        // Eliminate first digit.
  prec = mopt % 10;  // double, float, int, etc.
  mopt /= 10;        // Eliminate second digit.
  order = mopt % 10; // Row or column major ordering.
  mopt /= 10;        // Eliminate third digit.
  mach = mopt % 10;  // IEEE, VAX, etc.

  flt_fmt = mopt_digit_to_float_format (mach);

  if (flt_fmt == oct_mach_info::flt_fmt_unknown)
    {
      error ("load: unrecognized binary format!");
      return retval;
    }

  if (imag && type == 1)
    {
      error ("load: encountered complex matrix with string flag set!");
      return retval;
    }

  // LEN includes the terminating character, and the file is also
  // supposed to include it, but apparently not all files do.  Either
  // way, I think this should work.

  {
    OCTAVE_LOCAL_BUFFER (char, name, len+1);
    name[len] = '\0';
    if (! is.read (name, len))
      goto data_read_error;
    retval = name;

    dlen = nr * nc;
    if (dlen < 0)
      goto data_read_error;

    if (order)
      {
        octave_idx_type tmp = nr;
        nr = nc;
        nc = tmp;
      }

    if (type == 2)
      {
        if (nc == 4)
          {
            octave_idx_type nr_new, nc_new;
            Array<Complex> data (dim_vector (1, nr - 1));
            Array<octave_idx_type> c (dim_vector (1, nr - 1));
            Array<octave_idx_type> r (dim_vector (1, nr - 1));
            OCTAVE_LOCAL_BUFFER (double, dtmp, nr);
            OCTAVE_LOCAL_BUFFER (double, ctmp, nr);

            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              r.xelem (i) = dtmp[i] - 1;
            nr_new = dtmp[nr - 1];
            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              c.xelem (i) = dtmp[i] - 1;
            nc_new = dtmp[nr - 1];
            read_mat_binary_data (is, dtmp, prec, nr - 1, swap, flt_fmt);
            read_mat_binary_data (is, ctmp, prec, 1, swap, flt_fmt);
            read_mat_binary_data (is, ctmp, prec, nr - 1, swap, flt_fmt);

            for (octave_idx_type i = 0; i < nr - 1; i++)
              data.xelem (i) = Complex (dtmp[i], ctmp[i]);
            read_mat_binary_data (is, ctmp, prec, 1, swap, flt_fmt);

            SparseComplexMatrix smc = SparseComplexMatrix (data, r, c,
                                                           nr_new, nc_new);

            tc = order ? smc.transpose () : smc;
          }
        else
          {
            octave_idx_type nr_new, nc_new;
            Array<double> data (dim_vector (1, nr - 1));
            Array<octave_idx_type> c (dim_vector (1, nr - 1));
            Array<octave_idx_type> r (dim_vector (1, nr - 1));
            OCTAVE_LOCAL_BUFFER (double, dtmp, nr);

            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              r.xelem (i) = dtmp[i] - 1;
            nr_new = dtmp[nr - 1];
            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              c.xelem (i) = dtmp[i] - 1;
            nc_new = dtmp[nr - 1];
            read_mat_binary_data (is, data.fortran_vec (), prec, nr - 1,
                                  swap, flt_fmt);
            read_mat_binary_data (is, dtmp, prec, 1, swap, flt_fmt);

            SparseMatrix sm = SparseMatrix (data, r, c, nr_new, nc_new);

            tc = order ? sm.transpose () : sm;
          }
      }
    else
      {
        re.resize (nr, nc);

        read_mat_binary_data (is, re.fortran_vec (), prec, dlen, swap, flt_fmt);

        if (! is || error_state)
          {
            error ("load: reading matrix data for '%s'", name);
            goto data_read_error;
          }

        if (imag)
          {
            Matrix im (nr, nc);

            read_mat_binary_data (is, im.fortran_vec (), prec, dlen, swap,
                                  flt_fmt);

            if (! is || error_state)
              {
                error ("load: reading imaginary matrix data for '%s'", name);
                goto data_read_error;
              }

            ComplexMatrix ctmp (nr, nc);

            for (octave_idx_type j = 0; j < nc; j++)
              for (octave_idx_type i = 0; i < nr; i++)
                ctmp (i, j) = Complex (re (i, j), im (i, j));

            tc = order ? ctmp.transpose () : ctmp;
          }
        else
          tc = order ? re.transpose () : re;

        if (type == 1)
          tc = tc.convert_to_str (false, true, '\'');
      }

    return retval;
  }

data_read_error:
  error ("load: trouble reading binary file '%s'", filename.c_str ());
  return retval;
}

// Save the data from TC along with the corresponding NAME on stream OS
// in the MatLab version 4 binary format.

bool
save_mat_binary_data (std::ostream& os, const octave_value& tc,
                      const std::string& name)
{
  int32_t mopt = 0;

  mopt += tc.is_sparse_type () ? 2 : tc.is_string () ? 1 : 0;

  oct_mach_info::float_format flt_fmt =
    oct_mach_info::native_float_format ();;

  mopt += 1000 * float_format_to_mopt_digit (flt_fmt);

  os.write (reinterpret_cast<char *> (&mopt), 4);

  octave_idx_type len;
  int32_t nr = tc.rows ();

  int32_t nc = tc.columns ();

  if (tc.is_sparse_type ())
    {
      len = tc.nnz ();
      uint32_t nnz = len + 1;
      os.write (reinterpret_cast<char *> (&nnz), 4);

      uint32_t iscmplx = tc.is_complex_type () ? 4 : 3;
      os.write (reinterpret_cast<char *> (&iscmplx), 4);

      uint32_t tmp = 0;
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }
  else
    {
      os.write (reinterpret_cast<char *> (&nr), 4);
      os.write (reinterpret_cast<char *> (&nc), 4);

      int32_t imag = tc.is_complex_type () ? 1 : 0;
      os.write (reinterpret_cast<char *> (&imag), 4);

      len = nr * nc;
    }

  // LEN includes the terminating character, and the file is also
  // supposed to include it.

  int32_t name_len = name.length () + 1;

  os.write (reinterpret_cast<char *> (&name_len), 4);
  os << name << '\0';

  if (tc.is_string ())
    {
      unwind_protect frame;

      charMatrix chm = tc.char_matrix_value ();

      octave_idx_type nrow = chm.rows ();
      octave_idx_type ncol = chm.cols ();

      OCTAVE_LOCAL_BUFFER (double, buf, ncol*nrow);

      for (octave_idx_type i = 0; i < nrow; i++)
        {
          std::string tstr = chm.row_as_string (i);
          const char *s = tstr.data ();

          for (octave_idx_type j = 0; j < ncol; j++)
            buf[j*nrow+i] = static_cast<double> (*s++ & 0x00FF);
        }
      std::streamsize n_bytes = static_cast<std::streamsize> (nrow) *
                                static_cast<std::streamsize> (ncol) *
                                sizeof (double);
      os.write (reinterpret_cast<char *> (buf), n_bytes);
    }
  else if (tc.is_range ())
    {
      Range r = tc.range_value ();
      double base = r.base ();
      double inc = r.inc ();
      octave_idx_type nel = r.nelem ();
      for (octave_idx_type i = 0; i < nel; i++)
        {
          double x = base + i * inc;
          os.write (reinterpret_cast<char *> (&x), 8);
        }
    }
  else if (tc.is_real_scalar ())
    {
      double tmp = tc.double_value ();
      os.write (reinterpret_cast<char *> (&tmp), 8);
    }
  else if (tc.is_sparse_type ())
    {
      double ds;
      OCTAVE_LOCAL_BUFFER (double, dtmp, len);
      if (tc.is_complex_matrix ())
        {
          SparseComplexMatrix m = tc.sparse_complex_matrix_value ();

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = m.ridx (i) + 1;
          std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nr;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          octave_idx_type ii = 0;
          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
              dtmp[ii++] = j + 1;
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nc;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = std::real (m.data (i));
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = 0.;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = std::imag (m.data (i));
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          os.write (reinterpret_cast<const char *> (&ds), 8);
        }
      else
        {
          SparseMatrix m = tc.sparse_matrix_value ();

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = m.ridx (i) + 1;
          std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nr;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          octave_idx_type ii = 0;
          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
              dtmp[ii++] = j + 1;
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nc;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
          ds = 0.;
          os.write (reinterpret_cast<const char *> (&ds), 8);
        }
    }
  else if (tc.is_real_matrix ())
    {
      Matrix m = tc.matrix_value ();
      std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
      os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
    }
  else if (tc.is_complex_scalar ())
    {
      Complex tmp = tc.complex_value ();
      os.write (reinterpret_cast<char *> (&tmp), 16);
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m_cmplx = tc.complex_matrix_value ();
      Matrix m = ::real (m_cmplx);
      std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
      os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
      m = ::imag (m_cmplx);
      os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  return ! os.fail ();
}
