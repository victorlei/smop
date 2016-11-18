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

#include <string>

#include "CmplxSCHUR.h"
#include "dbleSCHUR.h"
#include "fCmplxSCHUR.h"
#include "floatSCHUR.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

template <class Matrix>
static octave_value
mark_upper_triangular (const Matrix& a)
{
  octave_value retval = a;

  octave_idx_type n = a.rows ();
  assert (a.columns () == n);

  const typename Matrix::element_type zero = typename Matrix::element_type ();

  for (octave_idx_type i = 0; i < n; i++)
    if (a(i,i) == zero)
      return retval;

  retval.matrix_type (MatrixType::Upper);

  return retval;
}

DEFUN (schur, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{S} =} schur (@var{A})\n\
@deftypefnx {Built-in Function} {@var{S} =} schur (@var{A}, \"real\")\n\
@deftypefnx {Built-in Function} {@var{S} =} schur (@var{A}, \"complex\")\n\
@deftypefnx {Built-in Function} {@var{S} =} schur (@var{A}, @var{opt})\n\
@deftypefnx {Built-in Function} {[@var{U}, @var{S}] =} schur (@dots{})\n\
@cindex Schur decomposition\n\
Compute the Schur@tie{}decomposition of @var{A}.\n\
\n\
The Schur@tie{}decomposition is defined as\n\
@tex\n\
$$\n\
 S = U^T A U\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@code{@var{S} = @var{U}' * @var{A} * @var{U}}\n\
@end example\n\
\n\
@end ifnottex\n\
where @var{U} is a unitary matrix\n\
@tex\n\
($U^T U$ is identity)\n\
@end tex\n\
@ifnottex\n\
(@code{@var{U}'* @var{U}} is identity)\n\
@end ifnottex\n\
and @var{S} is upper triangular.  The eigenvalues of @var{A} (and @var{S})\n\
are the diagonal elements of @var{S}.  If the matrix @var{A} is real, then\n\
the real Schur@tie{}decomposition is computed, in which the matrix @var{U}\n\
is orthogonal and @var{S} is block upper triangular with blocks of size at\n\
most\n\
@tex\n\
$2 \\times 2$\n\
@end tex\n\
@ifnottex\n\
@code{2 x 2}\n\
@end ifnottex\n\
along the diagonal.  The diagonal elements of @var{S}\n\
(or the eigenvalues of the\n\
@tex\n\
$2 \\times 2$\n\
@end tex\n\
@ifnottex\n\
@code{2 x 2}\n\
@end ifnottex\n\
blocks, when appropriate) are the eigenvalues of @var{A} and @var{S}.\n\
\n\
The default for real matrices is a real Schur@tie{}decomposition.\n\
A complex decomposition may be forced by passing the flag\n\
@qcode{\"complex\"}.\n\
\n\
The eigenvalues are optionally ordered along the diagonal according to the\n\
value of @var{opt}.  @code{@var{opt} = \"a\"} indicates that all eigenvalues\n\
with negative real parts should be moved to the leading block of @var{S}\n\
(used in @code{are}), @code{@var{opt} = \"d\"} indicates that all\n\
eigenvalues with magnitude less than one should be moved to the leading\n\
block of @var{S} (used in @code{dare}), and @code{@var{opt} = \"u\"}, the\n\
default, indicates that no ordering of eigenvalues should occur.  The\n\
leading @var{k} columns of @var{U} always span the @var{A}-invariant\n\
subspace corresponding to the @var{k} leading eigenvalues of @var{S}.\n\
\n\
The Schur@tie{}decomposition is used to compute eigenvalues of a square\n\
matrix, and has applications in the solution of algebraic Riccati equations\n\
in control (see @code{are} and @code{dare}).\n\
@seealso{rsf2csf, ordschur, lu, chol, hess, qr, qz, svd}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  std::string ord;

  if (nargin == 2)
    {
      if (args(1).is_string ())
        ord = args(1).string_value ();
      else
        {
          error ("schur: second argument must be a string");
          return retval;
        }
    }

  bool force_complex = false;

  if (ord == "real")
    {
      ord = std::string ();
    }
  else if (ord == "complex")
    {
      force_complex = true;
      ord = std::string ();
    }
  else
    {
      char ord_char = ord.empty () ? 'U' : ord[0];

      if (ord_char != 'U' && ord_char != 'A' && ord_char != 'D'
          && ord_char != 'u' && ord_char != 'a' && ord_char != 'd')
        {
          warning ("schur: incorrect ordered schur argument '%s'",
                   ord.c_str ());
          return retval;
        }
    }

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr != nc)
    {
      gripe_square_matrix_required ("schur");
      return retval;
    }

  if (! arg.is_numeric_type ())
    gripe_wrong_type_arg ("schur", arg);
  else if (arg.is_single_type ())
    {
      if (! force_complex && arg.is_real_type ())
        {
          FloatMatrix tmp = arg.float_matrix_value ();

          if (! error_state)
            {
              if (nargout == 0 || nargout == 1)
                {
                  FloatSCHUR result (tmp, ord, false);
                  retval(0) = result.schur_matrix ();
                }
              else
                {
                  FloatSCHUR result (tmp, ord, true);
                  retval(1) = result.schur_matrix ();
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
      else
        {
          FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

          if (! error_state)
            {

              if (nargout == 0 || nargout == 1)
                {
                  FloatComplexSCHUR result (ctmp, ord, false);
                  retval(0) = mark_upper_triangular (result.schur_matrix ());
                }
              else
                {
                  FloatComplexSCHUR result (ctmp, ord, true);
                  retval(1) = mark_upper_triangular (result.schur_matrix ());
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
    }
  else
    {
      if (! force_complex && arg.is_real_type ())
        {
          Matrix tmp = arg.matrix_value ();

          if (! error_state)
            {
              if (nargout == 0 || nargout == 1)
                {
                  SCHUR result (tmp, ord, false);
                  retval(0) = result.schur_matrix ();
                }
              else
                {
                  SCHUR result (tmp, ord, true);
                  retval(1) = result.schur_matrix ();
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
      else
        {
          ComplexMatrix ctmp = arg.complex_matrix_value ();

          if (! error_state)
            {

              if (nargout == 0 || nargout == 1)
                {
                  ComplexSCHUR result (ctmp, ord, false);
                  retval(0) = mark_upper_triangular (result.schur_matrix ());
                }
              else
                {
                  ComplexSCHUR result (ctmp, ord, true);
                  retval(1) = mark_upper_triangular (result.schur_matrix ());
                  retval(0) = result.unitary_matrix ();
                }
            }
        }
    }

  return retval;
}

/*
%!test
%! a = [1, 2, 3; 4, 5, 9; 7, 8, 6];
%! [u, s] = schur (a);
%! assert (u' * a * u, s, sqrt (eps));

%!test
%! a = single ([1, 2, 3; 4, 5, 9; 7, 8, 6]);
%! [u, s] = schur (a);
%! assert (u' * a * u, s, sqrt (eps ("single")));

%!error schur ()
%!error schur (1,2,3)
%!error [a,b,c] = schur (1)
%!error <argument must be a square matrix> schur ([1, 2, 3; 4, 5, 6])
%!error <wrong type argument 'cell'> schur ({1})
%!warning <incorrect ordered schur argument> schur ([1, 2; 3, 4], "bad_opt");

*/

DEFUN (rsf2csf, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {[@var{U}, @var{T}] =} rsf2csf (@var{UR}, @var{TR})\n\
Convert a real, upper quasi-triangular Schur@tie{}form @var{TR} to a complex,\n\
upper triangular Schur@tie{}form @var{T}.\n\
\n\
Note that the following relations hold:\n\
\n\
@tex\n\
$UR \\cdot TR \\cdot {UR}^T = U T U^{\\dagger}$ and\n\
$U^{\\dagger} U$ is the identity matrix I.\n\
@end tex\n\
@ifnottex\n\
@tcode{@var{UR} * @var{TR} * @var{UR}' = @var{U} * @var{T} * @var{U}'} and\n\
@code{@var{U}' * @var{U}} is the identity matrix I.\n\
@end ifnottex\n\
\n\
Note also that @var{U} and @var{T} are not unique.\n\
@seealso{schur}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 2 && nargout <= 2)
    {
      if (! args(0).is_numeric_type ())
        gripe_wrong_type_arg ("rsf2csf", args(0));
      else if (! args(1).is_numeric_type ())
        gripe_wrong_type_arg ("rsf2csf", args(1));
      else if (args(0).is_complex_type () || args(1).is_complex_type ())
        error ("rsf2csf: UR and TR must be real matrices");
      else
        {

          if (args(0).is_single_type () || args(1).is_single_type ())
            {
              FloatMatrix u = args(0).float_matrix_value ();
              FloatMatrix t = args(1).float_matrix_value ();
              if (! error_state)
                {
                  FloatComplexSCHUR cs (FloatSCHUR (t, u));

                  retval(1) = cs.schur_matrix ();
                  retval(0) = cs.unitary_matrix ();
                }
            }
          else
            {
              Matrix u = args(0).matrix_value ();
              Matrix t = args(1).matrix_value ();
              if (! error_state)
                {
                  ComplexSCHUR cs (SCHUR (t, u));

                  retval(1) = cs.schur_matrix ();
                  retval(0) = cs.unitary_matrix ();
                }
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! A = [1, 1, 1, 2; 1, 2, 1, 1; 1, 1, 3, 1; -2, 1, 1, 1];
%! [u, t] = schur (A);
%! [U, T] = rsf2csf (u, t);
%! assert (norm (u * t * u' - U * T * U'), 0, 1e-12);
%! assert (norm (A - U * T * U'), 0, 1e-12);

%!test
%! A = rand (10);
%! [u, t] = schur (A);
%! [U, T] = rsf2csf (u, t);
%! assert (norm (tril (T, -1)), 0);
%! assert (norm (U * U'), 1, 1e-14);

%!test
%! A = [0, 1;-1, 0];
%! [u, t] = schur (A);
%! [U, T] = rsf2csf (u,t);
%! assert (U * T * U', A, 1e-14);
*/
