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

#include "DET.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "ops.h"

#include "ov-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-perm.h"

#define MAYBE_CAST(VAR, CLASS) \
  const CLASS *VAR = arg.type_id () == CLASS::static_type_id () ? \
   dynamic_cast<const CLASS *> (&arg.get_rep ()) : 0

DEFUN (det, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} det (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{d}, @var{rcond}] =} det (@var{A})\n\
Compute the determinant of @var{A}.\n\
\n\
Return an estimate of the reciprocal condition number if requested.\n\
\n\
Programming Notes: Routines from @sc{lapack} are used for full matrices and\n\
code from @sc{umfpack} is used for sparse matrices.\n\
\n\
The determinant should not be used to check a matrix for singularity.\n\
For that, use any of the condition number functions: @code{cond},\n\
@code{condest}, @code{rcond}.\n\
@seealso{cond, condest, rcond}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr == 0 && nc == 0)
    {
      retval(0) = 1.0;
      return retval;
    }

  int arg_is_empty = empty_arg ("det", nr, nc);
  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value (Matrix (1, 1, 1.0));


  if (nr != nc)
    {
      gripe_square_matrix_required ("det");
      return retval;
    }

  bool isfloat = arg.is_single_type ();

  if (arg.is_diag_matrix ())
    {
      if (arg.is_complex_type ())
        {
          if (isfloat)
            {
              retval(0) = arg.float_complex_diag_matrix_value ()
                          .determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.float_complex_diag_matrix_value ().rcond ();
            }
          else
            {
              retval(0) = arg.complex_diag_matrix_value ()
                          .determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.complex_diag_matrix_value ().rcond ();
            }
        }
      else
        {
          if (isfloat)
            {
              retval(0) = arg.float_diag_matrix_value ()
                          .determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.float_diag_matrix_value ().rcond ();
            }
          else
            {
              retval(0) = arg.diag_matrix_value ().determinant ().value ();
              if (nargout > 1)
                retval(1) = arg.diag_matrix_value ().rcond ();
            }
        }
    }
  else if (arg.is_perm_matrix ())
    {
      retval(0) = static_cast<double> (arg.perm_matrix_value ().determinant ());
      if (nargout > 1)
        retval(1) = 1.0;
    }
  else if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          octave_idx_type info;
          float rcond = 0.0;
          // Always compute rcond, so we can detect numerically
          // singular matrices.
          FloatMatrix m = arg.float_matrix_value ();
          if (! error_state)
            {
              MAYBE_CAST (rep, octave_float_matrix);
              MatrixType mtype = rep ? rep -> matrix_type () : MatrixType ();
              FloatDET det = m.determinant (mtype, info, rcond);
              retval(1) = rcond;
              retval(0) = info == -1 ? 0.0f : det.value ();
              if (rep) rep->matrix_type (mtype);
            }
        }
      else if (arg.is_complex_type ())
        {
          octave_idx_type info;
          float rcond = 0.0;
          // Always compute rcond, so we can detect numerically
          // singular matrices.
          FloatComplexMatrix m = arg.float_complex_matrix_value ();
          if (! error_state)
            {
              MAYBE_CAST (rep, octave_float_complex_matrix);
              MatrixType mtype = rep ? rep -> matrix_type () : MatrixType ();
              FloatComplexDET det = m.determinant (mtype, info, rcond);
              retval(1) = rcond;
              retval(0) = info == -1 ? FloatComplex (0.0) : det.value ();
              if (rep) rep->matrix_type (mtype);
            }
        }
    }
  else
    {
      if (arg.is_real_type ())
        {
          octave_idx_type info;
          double rcond = 0.0;
          // Always compute rcond, so we can detect numerically
          // singular matrices.
          if (arg.is_sparse_type ())
            {
              SparseMatrix m = arg.sparse_matrix_value ();
              if (! error_state)
                {
                  DET det = m.determinant (info, rcond);
                  retval(1) = rcond;
                  retval(0) = info == -1 ? 0.0 : det.value ();
                }
            }
          else
            {
              Matrix m = arg.matrix_value ();
              if (! error_state)
                {
                  MAYBE_CAST (rep, octave_matrix);
                  MatrixType mtype = rep ? rep -> matrix_type ()
                                         : MatrixType ();
                  DET det = m.determinant (mtype, info, rcond);
                  retval(1) = rcond;
                  retval(0) = info == -1 ? 0.0 : det.value ();
                  if (rep) rep->matrix_type (mtype);
                }
            }
        }
      else if (arg.is_complex_type ())
        {
          octave_idx_type info;
          double rcond = 0.0;
          // Always compute rcond, so we can detect numerically
          // singular matrices.
          if (arg.is_sparse_type ())
            {
              SparseComplexMatrix m = arg.sparse_complex_matrix_value ();
              if (! error_state)
                {
                  ComplexDET det = m.determinant (info, rcond);
                  retval(1) = rcond;
                  retval(0) = info == -1 ? Complex (0.0) : det.value ();
                }
            }
          else
            {
              ComplexMatrix m = arg.complex_matrix_value ();
              if (! error_state)
                {
                  MAYBE_CAST (rep, octave_complex_matrix);
                  MatrixType mtype = rep ? rep -> matrix_type ()
                                         : MatrixType ();
                  ComplexDET det = m.determinant (mtype, info, rcond);
                  retval(1) = rcond;
                  retval(0) = info == -1 ? Complex (0.0) : det.value ();
                  if (rep) rep->matrix_type (mtype);
                }
            }
        }
      else
        gripe_wrong_type_arg ("det", arg);
    }
  return retval;
}

/*
%!assert (det ([1, 2; 3, 4]), -2, 10*eps)
%!assert (det (single ([1, 2; 3, 4])), single (-2), 10*eps ("single"))
%!error det ()
%!error det (1, 2)
%!error <argument must be a square matrix> det ([1, 2; 3, 4; 5, 6])
*/
