/*

Copyright (C) 2015 Sébastien Villemot <sebastien@debian.org>

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

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "f77-fcn.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dtrsen, DTRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type*, const octave_idx_type&,
                             double*, const octave_idx_type&, double*, const octave_idx_type&,
                             double*, double*, octave_idx_type&, double&, double&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (ztrsen, ZTRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type*, const octave_idx_type&,
                             Complex*, const octave_idx_type&, Complex*, const octave_idx_type&,
                             Complex*, octave_idx_type&, double&, double&, Complex*,
                             const octave_idx_type&, octave_idx_type &);

  F77_RET_T
  F77_FUNC (strsen, STRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type*, const octave_idx_type&,
                             float*, const octave_idx_type&, float*, const octave_idx_type&,
                             float*, float*, octave_idx_type&, float&, float&, float*,
                             const octave_idx_type&, octave_idx_type*,
                             const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (ctrsen, CTRSEN) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&, FloatComplex*, const octave_idx_type&,
                             FloatComplex*, octave_idx_type&, float&, float&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type &);
}

DEFUN (ordschur, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{UR}, @var{SR}] =} ordschur (@var{U}, @var{S}, @var{select})\n\
Reorders the real Schur factorization (@var{U},@var{S}) obtained with the\n\
@code{schur} function, so that selected eigenvalues appear in the upper left\n\
diagonal blocks of the quasi triangular Schur matrix.\n\
\n\
The logical vector @var{select} specifies the selected eigenvalues as they\n\
appear along @var{S}'s diagonal.\n\
\n\
For example, given the matrix @code{@var{A} = [1, 2; 3, 4]}, and its Schur\n\
decomposition\n\
\n\
@example\n\
[@var{U}, @var{S}] = schur (@var{A})\n\
@end example\n\
\n\
@noindent\n\
which returns\n\
\n\
@example\n\
@group\n\
@var{U} =\n\
\n\
  -0.82456  -0.56577\n\
   0.56577  -0.82456\n\
\n\
@var{S} =\n\
\n\
  -0.37228  -1.00000\n\
   0.00000   5.37228\n\
\n\
@end group\n\
@end example\n\
\n\
It is possible to reorder the decomposition so that the positive eigenvalue\n\
is in the upper left corner, by doing:\n\
\n\
@example\n\
[@var{U}, @var{S}] = ordschur (@var{U}, @var{S}, [0,1])\n\
@end example\n\
\n\
@seealso{schur}\n\
@end deftypefn")
{
  const octave_idx_type nargin = args.length ();
  octave_value_list retval;

  if (nargin != 3)
    {
      print_usage ();
      return retval;
    }

  const Array<octave_idx_type> sel = args(2).octave_idx_type_vector_value ();
  if (error_state)
    {
      error ("ordschur: SELECT must be an array of integers");
      return retval;
    }
  const octave_idx_type n = sel.numel ();

  const dim_vector dimU = args(0).dims ();
  const dim_vector dimS = args(1).dims ();
  if (n != dimU(0))
    {
      error ("ordschur: SELECT must have same length as the sides of U and S");
      return retval;
    }
  else if (n != dimU(0) || n != dimS(0) || n != dimU(1) || n != dimS(1))
    {
      error ("ordschur: U and S must be square and of equal sizes");
      return retval;
    }

  const bool double_type  = args(0).is_double_type ()
                            || args(1).is_double_type ();
  const bool complex_type = args(0).is_complex_type ()
                            || args(1).is_complex_type ();

#define PREPARE_ARGS(TYPE, TYPE_M, TYPE_COND) \
          TYPE ## Matrix U = args(0).TYPE_M ## _value (); \
          TYPE ## Matrix S = args(1).TYPE_M ## _value (); \
          if (error_state) \
            { \
              error ("ordschur: U and S must be real or complex floating point matrices"); \
              return retval; \
            } \
          TYPE ## Matrix w (dim_vector (n, 1)); \
          TYPE ## Matrix work (dim_vector (n, 1)); \
          octave_idx_type m; \
          octave_idx_type info; \
          TYPE_COND cond1, cond2;

#define PREPARE_OUTPUT()\
          if (info != 0) \
            { \
              error ("ordschur: trsen failed"); \
              return retval; \
            } \
          retval(0) = U; \
          retval(1) = S;

  if (double_type)
    {
      if (complex_type)
        {
          PREPARE_ARGS (Complex, complex_matrix, double)

          F77_XFCN (ztrsen, ztrsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, S.fortran_vec (), n, U.fortran_vec (), n,
                     w.fortran_vec (), m, cond1, cond2, work.fortran_vec (), n,
                     info));
          PREPARE_OUTPUT()
        }
      else
        {
          PREPARE_ARGS (, matrix, double)
          Matrix wi (dim_vector (n, 1));
          Array<octave_idx_type> iwork (dim_vector (n, 1));

          F77_XFCN (dtrsen, dtrsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, S.fortran_vec (), n, U.fortran_vec (), n,
                     w.fortran_vec (), wi.fortran_vec (), m, cond1, cond2,
                     work.fortran_vec (), n, iwork.fortran_vec (), n, info));
          PREPARE_OUTPUT ()
        }
    }
  else
    {
      if (complex_type)
        {
          PREPARE_ARGS (FloatComplex, float_complex_matrix, float)

          F77_XFCN (ctrsen, ctrsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, S.fortran_vec (), n, U.fortran_vec (), n,
                     w.fortran_vec (), m, cond1, cond2, work.fortran_vec (), n,
                     info));
          PREPARE_OUTPUT ()
        }
      else
        {
          PREPARE_ARGS (Float, float_matrix, float)
          FloatMatrix wi (dim_vector (n, 1));
          Array<octave_idx_type> iwork (dim_vector (n, 1));

          F77_XFCN (strsen, strsen,
                    (F77_CONST_CHAR_ARG ("N"), F77_CONST_CHAR_ARG ("V"),
                     sel.data (), n, S.fortran_vec (), n, U.fortran_vec (), n,
                     w.fortran_vec (), wi.fortran_vec (), m, cond1, cond2,
                     work.fortran_vec (), n, iwork.fortran_vec (), n, info));
          PREPARE_OUTPUT ()
        }
    }

#undef PREPARE_ARGS
#undef PREPARE_OUTPUT

  return retval;
}

/*

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4 ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (U, T, [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps))
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps))

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4 ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (single (U), single (T), [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps ("single")))
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps ("single")))

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4+3i ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (U, T, [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps))
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps))

%!test
%! A = [1, 2, 3, -2; 4, 5, 6, -5 ; 7, 8, 9, -5; 10, 11, 12, 4+3i ];
%! [U, T] = schur (A);
%! [US, TS] = ordschur (single (U), single (T), [ 0, 0, 1, 1 ]);
%! assert (US*TS*US', A, sqrt (eps ("single")))
%! assert (diag (T)(3:4), diag (TS)(1:2), sqrt (eps ("single")))

*/
