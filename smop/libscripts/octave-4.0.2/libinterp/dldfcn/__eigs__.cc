/*

Copyright (C) 2005-2015 David Bateman

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

#include "ov.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "quit.h"
#include "variables.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "oct-map.h"
#include "pager.h"
#include "unwind-prot.h"

#include "eigs-base.cc"

// Global pointer for user defined function.
static octave_function *eigs_fcn = 0;

// Have we warned about imaginary values returned from user function?
static bool warned_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

ColumnVector
eigs_func (const ColumnVector &x, int &eigs_error)
{
  ColumnVector retval;
  octave_value_list args;
  args(0) = x;

  if (eigs_fcn)
    {
      octave_value_list tmp = eigs_fcn->do_multi_index_op (1, args);

      if (error_state)
        {
          eigs_error = 1;
          gripe_user_supplied_eval ("eigs");
          return retval;
        }

      if (tmp.length () && tmp(0).is_defined ())
        {
          if (! warned_imaginary && tmp(0).is_complex_type ())
            {
              warning ("eigs: ignoring imaginary part returned from user-supplied function");
              warned_imaginary = true;
            }

          retval = ColumnVector (tmp(0).vector_value ());

          if (error_state)
            {
              eigs_error = 1;
              gripe_user_supplied_eval ("eigs");
            }
        }
      else
        {
          eigs_error = 1;
          gripe_user_supplied_eval ("eigs");
        }
    }

  return retval;
}

ComplexColumnVector
eigs_complex_func (const ComplexColumnVector &x, int &eigs_error)
{
  ComplexColumnVector retval;
  octave_value_list args;
  args(0) = x;

  if (eigs_fcn)
    {
      octave_value_list tmp = eigs_fcn->do_multi_index_op (1, args);

      if (error_state)
        {
          eigs_error = 1;
          gripe_user_supplied_eval ("eigs");
          return retval;
        }

      if (tmp.length () && tmp(0).is_defined ())
        {
          retval = ComplexColumnVector (tmp(0).complex_vector_value ());

          if (error_state)
            {
              eigs_error = 1;
              gripe_user_supplied_eval ("eigs");
            }
        }
      else
        {
          eigs_error = 1;
          gripe_user_supplied_eval ("eigs");
        }
    }

  return retval;
}

DEFUN_DLD (__eigs__, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{d} =} __eigs__ (@var{A})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{B})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{B}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{B}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{A}, @var{B}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{B})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{B}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{B}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {@var{d} =} __eigs__ (@var{af}, @var{n}, @var{B}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {[@var{V}, @var{d}] =} __eigs__ (@var{A}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{V}, @var{d}] =} __eigs__ (@var{af}, @var{n}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{V}, @var{d}, @var{flag}] =} __eigs__ (@var{A}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{V}, @var{d}, @var{flag}] =} __eigs__ (@var{af}, @var{n}, @dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;
#ifdef HAVE_ARPACK
  int nargin = args.length ();
  std::string fcn_name;
  octave_idx_type n = 0;
  octave_idx_type k = 6;
  Complex sigma = 0.;
  double sigmar, sigmai;
  bool have_sigma = false;
  std::string typ = "LM";
  Matrix amm, bmm, bmt;
  ComplexMatrix acm, bcm, bct;
  SparseMatrix asmm, bsmm, bsmt;
  SparseComplexMatrix ascm, bscm, bsct;
  int b_arg = 0;
  bool have_b = false;
  bool have_a_fun = false;
  bool a_is_complex = false;
  bool b_is_complex = false;
  bool symmetric = false;
  bool sym_tested = false;
  bool cholB = false;
  bool a_is_sparse = false;
  ColumnVector permB;
  int arg_offset = 0;
  double tol = std::numeric_limits<double>::epsilon ();
  int maxit = 300;
  int disp = 0;
  octave_idx_type p = -1;
  ColumnVector resid;
  ComplexColumnVector cresid;
  octave_idx_type info = 1;

  warned_imaginary = false;

  unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    {
      error ("eigs: invalid recursive call");
      if (fcn_name.length ())
        clear_function (fcn_name);
      return retval;
    }

  if (nargin == 0)
    print_usage ();
  else if (args(0).is_function_handle () || args(0).is_inline_function ()
           || args(0).is_string ())
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();
          std::string fname = "function y = ";
          fcn_name = unique_symbol_name ("__eigs_fcn__");
          fname.append (fcn_name);
          fname.append ("(x) y = ");
          eigs_fcn = extract_function (args(0), "eigs", fcn_name, fname,
                                       "; endfunction");
        }
      else
        eigs_fcn = args(0).function_value ();

      if (!eigs_fcn)
        {
          error ("eigs: unknown function");
          return retval;
        }

      if (nargin < 2)
        {
          error ("eigs: incorrect number of arguments");
          return retval;
        }
      else
        {
          n = args(1).nint_value ();
          arg_offset = 1;
          have_a_fun = true;
        }
    }
  else
    {
      if (args(0).is_complex_type ())
        {
          if (args(0).is_sparse_type ())
            {
              ascm = (args(0).sparse_complex_matrix_value ());
              a_is_sparse = true;
            }
          else
            acm = (args(0).complex_matrix_value ());
          a_is_complex = true;
          symmetric = false; // ARPACK doesn't special case complex symmetric
          sym_tested = true;
        }
      else
        {
          if (args(0).is_sparse_type ())
            {
              asmm = (args(0).sparse_matrix_value ());
              a_is_sparse = true;
            }
          else
            {
              amm = (args(0).matrix_value ());
            }
        }

    }

  // Note hold off reading B till later to avoid issues of double
  // copies of the matrix if B is full/real while A is complex.
  if (! error_state && nargin > 1 + arg_offset
      && ! (args(1 + arg_offset).is_real_scalar ()))
    {
      if (args(1+arg_offset).is_complex_type ())
        {
          b_arg = 1+arg_offset;
          have_b = true;
          b_is_complex = true;
          arg_offset++;
        }
      else
        {
          b_arg = 1+arg_offset;
          have_b = true;
          arg_offset++;
        }
    }

  if (!error_state && nargin > (1+arg_offset))
    k = args(1+arg_offset).nint_value ();

  if (!error_state && nargin > (2+arg_offset))
    {
      if (args(2+arg_offset).is_string ())
        {
          typ = args(2+arg_offset).string_value ();

          // Use STL function to convert to upper case
          transform (typ.begin (), typ.end (), typ.begin (), toupper);

          sigma = 0.;
        }
      else
        {
          sigma = args(2+arg_offset).complex_value ();

          if (! error_state)
            have_sigma = true;
          else
            {
              error ("eigs: SIGMA must be a scalar or a string");
              return retval;
            }
        }
    }

  sigmar = std::real (sigma);
  sigmai = std::imag (sigma);

  if (!error_state && nargin > (3+arg_offset))
    {
      if (args(3+arg_offset).is_map ())
        {
          octave_scalar_map map = args(3+arg_offset).scalar_map_value ();

          if (! error_state)
            {
              octave_value tmp;

              // issym is ignored for complex matrix inputs
              tmp = map.getfield ("issym");
              if (tmp.is_defined () && !sym_tested)
                {
                  symmetric = tmp.double_value () != 0.;
                  sym_tested = true;
                }

              // isreal is ignored if A is not a function
              tmp = map.getfield ("isreal");
              if (tmp.is_defined () && have_a_fun)
                a_is_complex = ! (tmp.double_value () != 0.);

              tmp = map.getfield ("tol");
              if (tmp.is_defined ())
                tol = tmp.double_value ();

              tmp = map.getfield ("maxit");
              if (tmp.is_defined ())
                maxit = tmp.nint_value ();

              tmp = map.getfield ("p");
              if (tmp.is_defined ())
                p = tmp.nint_value ();

              tmp = map.getfield ("v0");
              if (tmp.is_defined ())
                {
                  if (a_is_complex || b_is_complex)
                    cresid = ComplexColumnVector (tmp.complex_vector_value ());
                  else
                    resid = ColumnVector (tmp.vector_value ());
                }

              tmp = map.getfield ("disp");
              if (tmp.is_defined ())
                disp = tmp.nint_value ();

              tmp = map.getfield ("cholB");
              if (tmp.is_defined ())
                cholB = tmp.double_value () != 0.;

              tmp = map.getfield ("permB");
              if (tmp.is_defined ())
                permB = ColumnVector (tmp.vector_value ()) - 1.0;
            }
          else
            {
              error ("eigs: OPTS argument must be a scalar structure");
              return retval;
            }
        }
      else
        {
          error ("eigs: OPTS argument must be a structure");
          return retval;
        }
    }

  if (nargin > (4+arg_offset))
    {
      error ("eigs: incorrect number of arguments");
      return retval;
    }

  // Test undeclared (no issym) matrix inputs for symmetry
  if (!sym_tested && !have_a_fun)
    {
      if (a_is_sparse)
        symmetric = asmm.is_symmetric ();
      else
        symmetric = amm.is_symmetric ();
    }

  if (have_b)
    {
      if (a_is_complex || b_is_complex)
        {
          if (a_is_sparse)
            bscm = args(b_arg).sparse_complex_matrix_value ();
          else
            bcm = args(b_arg).complex_matrix_value ();
        }
      else
        {
          if (a_is_sparse)
            bsmm = args(b_arg).sparse_matrix_value ();
          else
            bmm = args(b_arg).matrix_value ();
        }
    }

  // Mode 1 for SM mode seems unstable for some reason.
  // Use Mode 3 instead, with sigma = 0.
  if (!error_state && !have_sigma && typ == "SM")
    have_sigma = true;

  if (!error_state)
    {
      octave_idx_type nconv;
      if (a_is_complex || b_is_complex)
        {
          ComplexMatrix eig_vec;
          ComplexColumnVector eig_val;


          if (have_a_fun)
            nconv = EigsComplexNonSymmetricFunc
                    (eigs_complex_func, n, typ, sigma, k, p, info, eig_vec,
                     eig_val, cresid, octave_stdout, tol, (nargout > 1), cholB,
                     disp, maxit);
          else if (have_sigma)
            {
              if (a_is_sparse)
                nconv = EigsComplexNonSymmetricMatrixShift
                        (ascm, sigma, k, p, info, eig_vec, eig_val, bscm, permB,
                         cresid, octave_stdout, tol, (nargout > 1), cholB, disp,
                         maxit);
              else
                nconv = EigsComplexNonSymmetricMatrixShift
                        (acm, sigma, k, p, info, eig_vec, eig_val, bcm, permB,
                         cresid, octave_stdout, tol, (nargout > 1), cholB, disp,
                         maxit);
            }
          else
            {
              if (a_is_sparse)
                nconv = EigsComplexNonSymmetricMatrix
                        (ascm, typ, k, p, info, eig_vec, eig_val, bscm, permB,
                         cresid, octave_stdout, tol, (nargout > 1), cholB, disp,
                         maxit);
              else
                nconv = EigsComplexNonSymmetricMatrix
                        (acm, typ, k, p, info, eig_vec, eig_val, bcm, permB,
                         cresid, octave_stdout, tol, (nargout > 1), cholB, disp,
                         maxit);
            }

          if (nargout < 2)
            retval(0) = eig_val;
          else
            {
              retval(2) = double (info);
              retval(1) = ComplexDiagMatrix (eig_val);
              retval(0) = eig_vec;
            }
        }
      else if (sigmai != 0.)
        {
          // Promote real problem to a complex one.
          ComplexMatrix eig_vec;
          ComplexColumnVector eig_val;

          if (have_a_fun)
            nconv = EigsComplexNonSymmetricFunc
                    (eigs_complex_func, n, typ,  sigma, k, p, info, eig_vec,
                     eig_val, cresid, octave_stdout, tol, (nargout > 1), cholB,
                     disp, maxit);
          else
            {
              if (a_is_sparse)
                nconv = EigsComplexNonSymmetricMatrixShift
                        (SparseComplexMatrix (asmm), sigma, k, p, info, eig_vec,
                         eig_val, SparseComplexMatrix (bsmm), permB, cresid,
                         octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
              else
                nconv = EigsComplexNonSymmetricMatrixShift
                        (ComplexMatrix (amm), sigma, k, p, info, eig_vec,
                         eig_val, ComplexMatrix (bmm), permB, cresid,
                         octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
            }

          if (nargout < 2)
            retval(0) = eig_val;
          else
            {
              retval(2) = double (info);
              retval(1) = ComplexDiagMatrix (eig_val);
              retval(0) = eig_vec;
            }
        }
      else
        {
          if (symmetric)
            {
              Matrix eig_vec;
              ColumnVector eig_val;

              if (have_a_fun)
                nconv = EigsRealSymmetricFunc
                        (eigs_func, n, typ, sigmar, k, p, info, eig_vec,
                         eig_val, resid, octave_stdout, tol, (nargout > 1),
                         cholB, disp, maxit);
              else if (have_sigma)
                {
                  if (a_is_sparse)
                    nconv = EigsRealSymmetricMatrixShift
                            (asmm, sigmar, k, p, info, eig_vec, eig_val, bsmm,
                             permB, resid, octave_stdout, tol, (nargout > 1),
                             cholB, disp, maxit);
                  else
                    nconv = EigsRealSymmetricMatrixShift
                            (amm, sigmar, k, p, info, eig_vec, eig_val, bmm,
                             permB, resid, octave_stdout, tol, (nargout > 1),
                             cholB, disp, maxit);
                }
              else
                {
                  if (a_is_sparse)
                    nconv = EigsRealSymmetricMatrix
                            (asmm, typ, k, p, info, eig_vec, eig_val, bsmm,
                             permB, resid, octave_stdout, tol, (nargout > 1),
                             cholB, disp, maxit);
                  else
                    nconv = EigsRealSymmetricMatrix
                            (amm, typ, k, p, info, eig_vec, eig_val, bmm, permB,
                             resid, octave_stdout, tol, (nargout > 1), cholB,
                             disp, maxit);
                }

              if (nargout < 2)
                retval(0) = eig_val;
              else
                {
                  retval(2) = double (info);
                  retval(1) = DiagMatrix (eig_val);
                  retval(0) = eig_vec;
                }
            }
          else
            {
              ComplexMatrix eig_vec;
              ComplexColumnVector eig_val;

              if (have_a_fun)
                nconv = EigsRealNonSymmetricFunc
                        (eigs_func, n, typ, sigmar, k, p, info, eig_vec,
                         eig_val, resid, octave_stdout, tol, (nargout > 1),
                         cholB, disp, maxit);
              else if (have_sigma)
                {
                  if (a_is_sparse)
                    nconv = EigsRealNonSymmetricMatrixShift
                            (asmm, sigmar, k, p, info, eig_vec, eig_val, bsmm,
                             permB, resid, octave_stdout, tol, (nargout > 1),
                             cholB, disp, maxit);
                  else
                    nconv = EigsRealNonSymmetricMatrixShift
                            (amm, sigmar, k, p, info, eig_vec, eig_val, bmm,
                             permB, resid, octave_stdout, tol, (nargout > 1),
                             cholB, disp, maxit);
                }
              else
                {
                  if (a_is_sparse)
                    nconv = EigsRealNonSymmetricMatrix
                            (asmm, typ, k, p, info, eig_vec, eig_val, bsmm,
                             permB, resid, octave_stdout, tol, (nargout > 1),
                             cholB, disp, maxit);
                  else
                    nconv = EigsRealNonSymmetricMatrix
                            (amm, typ, k, p, info, eig_vec, eig_val, bmm, permB,
                             resid, octave_stdout, tol, (nargout > 1), cholB,
                             disp, maxit);
                }

              if (nargout < 2)
                retval(0) = eig_val;
              else
                {
                  retval(2) = double (info);
                  retval(1) = ComplexDiagMatrix (eig_val);
                  retval(0) = eig_vec;
                }
            }
        }

      if (nconv <= 0)
        warning ("eigs: None of the %d requested eigenvalues converged", k);
      else if (nconv < k)
        warning ("eigs: Only %d of the %d requested eigenvalues converged",
                 nconv, k);
    }

  if (! fcn_name.empty ())
    clear_function (fcn_name);
#else
  error ("eigs: not available in this version of Octave");
#endif

  return retval;
}
