/*

Copyright (C) 1998-2015 A. S. Hodel

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

// Generalized eigenvalue balancing via LAPACK

// Author: A. S. Hodel <scotte@eng.auburn.edu>

#undef DEBUG
#undef DEBUG_SORT
#undef DEBUG_EIG

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include <iostream>
#include <iomanip>

#include "CmplxQRP.h"
#include "CmplxQR.h"
#include "dbleQR.h"
#include "f77-fcn.h"
#include "lo-math.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov.h"
#include "pager.h"
#if defined (DEBUG) || defined (DEBUG_SORT)
#include "pr-output.h"
#endif
#include "symtab.h"
#include "utils.h"
#include "variables.h"

typedef octave_idx_type (*sort_function) (const octave_idx_type& LSIZE,
                                          const double& ALPHA,
                                          const double& BETA, const double& S,
                                          const double& P);

extern "C"
{
  F77_RET_T
  F77_FUNC (dggbal, DGGBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N, double* A,
                             const octave_idx_type& LDA, double* B,
                             const octave_idx_type& LDB, octave_idx_type& ILO,
                             octave_idx_type& IHI, double* LSCALE,
                             double* RSCALE, double* WORK,
                             octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggbal, ZGGBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N, Complex* A,
                             const octave_idx_type& LDA, Complex* B,
                             const octave_idx_type& LDB, octave_idx_type& ILO,
                             octave_idx_type& IHI, double* LSCALE,
                             double* RSCALE, double* WORK,
                             octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dggbak, DGGBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N,
                             const octave_idx_type& ILO,
                             const octave_idx_type& IHI,
                             const double* LSCALE, const double* RSCALE,
                             octave_idx_type& M, double* V,
                             const octave_idx_type& LDV, octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zggbak, ZGGBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N,
                             const octave_idx_type& ILO,
                             const octave_idx_type& IHI,
                             const double* LSCALE, const double* RSCALE,
                             octave_idx_type& M, Complex* V,
                             const octave_idx_type& LDV, octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgghrd, DGGHRD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N,
                             const octave_idx_type& ILO,
                             const octave_idx_type& IHI, double* A,
                             const octave_idx_type& LDA, double* B,
                             const octave_idx_type& LDB, double* Q,
                             const octave_idx_type& LDQ, double* Z,
                             const octave_idx_type& LDZ, octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zgghrd, ZGGHRD) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N,
                             const octave_idx_type& ILO,
                             const octave_idx_type& IHI, Complex* A,
                             const octave_idx_type& LDA, Complex* B,
                             const octave_idx_type& LDB, Complex* Q,
                             const octave_idx_type& LDQ, Complex* Z,
                             const octave_idx_type& LDZ, octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dhgeqz, DHGEQZ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N,
                             const octave_idx_type& ILO,
                             const octave_idx_type& IHI,
                             double* A, const octave_idx_type& LDA, double* B,
                             const octave_idx_type& LDB, double* ALPHAR,
                             double* ALPHAI, double* BETA, double* Q,
                             const octave_idx_type& LDQ, double* Z,
                             const octave_idx_type& LDZ, double* WORK,
                             const octave_idx_type& LWORK,
                             octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zhgeqz, ZHGEQZ) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type& N,
                             const octave_idx_type& ILO,
                             const octave_idx_type& IHI,
                             Complex* A, const octave_idx_type& LDA,
                             Complex* B, const octave_idx_type& LDB,
                             Complex* ALPHA, Complex* BETA, Complex* CQ,
                             const octave_idx_type& LDQ,
                             Complex* CZ, const octave_idx_type& LDZ,
                             Complex* WORK, const octave_idx_type& LWORK,
                             double* RWORK, octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dlag2, DLAG2) (const double* A, const octave_idx_type& LDA,
                           const double* B, const octave_idx_type& LDB,
                           const double& SAFMIN, double& SCALE1,
                           double& SCALE2, double& WR1, double& WR2,
                           double& WI);

  // Van Dooren's code (netlib.org: toms/590) for reordering
  // GEP.  Only processes Z, not Q.
  F77_RET_T
  F77_FUNC (dsubsp, DSUBSP) (const octave_idx_type& NMAX,
                             const octave_idx_type& N, double* A,
                             double* B, double* Z, sort_function,
                             const double& EPS, octave_idx_type& NDIM,
                             octave_idx_type& FAIL, octave_idx_type* IND);

  // Documentation for DTGEVC incorrectly states that VR, VL are
  // complex*16; they are declared in DTGEVC as double precision
  // (probably a cut and paste problem fro ZTGEVC).
  F77_RET_T
  F77_FUNC (dtgevc, DTGEVC) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type* SELECT,
                             const octave_idx_type& N, double* A,
                             const octave_idx_type& LDA, double* B,
                             const octave_idx_type& LDB, double* VL,
                             const octave_idx_type& LDVL, double* VR,
                             const octave_idx_type& LDVR,
                             const octave_idx_type& MM, octave_idx_type& M,
                             double* WORK, octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ztgevc, ZTGEVC) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type* SELECT,
                             const octave_idx_type& N, const Complex* A,
                             const octave_idx_type& LDA,const Complex* B,
                             const octave_idx_type& LDB, Complex* xVL,
                             const octave_idx_type& LDVL, Complex* xVR,
                             const octave_idx_type& LDVR,
                             const octave_idx_type& MM, octave_idx_type& M,
                             Complex* CWORK, double* RWORK,
                             octave_idx_type& INFO
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG_DECL,
                               double& retval
                               F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xdlange, XDLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const octave_idx_type&,
                               const octave_idx_type&, const double*,
                               const octave_idx_type&, double*, double&
                               F77_CHAR_ARG_LEN_DECL);
}

// fcrhp, fin, fout, folhp:
// routines for ordering of generalized eigenvalues
// return 1 if  test is passed, 0 otherwise
//    fin: |lambda| < 1
//    fout: |lambda| >= 1
//    fcrhp: real(lambda) >= 0
//    folhp: real(lambda) < 0

static octave_idx_type
fcrhp (const octave_idx_type& lsize, const double& alpha,
       const double& beta, const double& s, const double&)
{
  if (lsize == 1)
    return (alpha * beta >= 0 ? 1 : -1);
  else
    return (s >= 0 ? 1 : -1);
}

static octave_idx_type
fin (const octave_idx_type& lsize, const double& alpha,
     const double& beta, const double&, const double& p)
{
  octave_idx_type retval;

  if (lsize == 1)
    retval = (fabs (alpha) < fabs (beta) ? 1 : -1);
  else
    retval = (fabs (p) < 1 ? 1 : -1);

#ifdef DEBUG
  std::cout << "qz: fin: retval=" << retval << std::endl;
#endif

  return retval;
}

static octave_idx_type
folhp (const octave_idx_type& lsize, const double& alpha,
       const double& beta, const double& s, const double&)
{
  if (lsize == 1)
    return (alpha * beta < 0 ? 1 : -1);
  else
    return (s < 0 ? 1 : -1);
}

static octave_idx_type
fout (const octave_idx_type& lsize, const double& alpha,
      const double& beta, const double&, const double& p)
{
  if (lsize == 1)
    return (fabs (alpha) >= fabs (beta) ? 1 : -1);
  else
    return (fabs (p) >= 1 ? 1 : -1);
}


//FIXME: Matlab does not produce lambda as the first output argument.
//       Compatibility problem?
DEFUN (qz, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{lambda} =} qz (@var{A}, @var{B})\n\
@deftypefnx {Built-in Function} {@var{lambda} =} qz (@var{A}, @var{B}, @var{opt})\n\
QZ@tie{}decomposition of the generalized eigenvalue problem\n\
(@math{A x = s B x}).\n\
\n\
There are three ways to call this function:\n\
@enumerate\n\
@item @code{@var{lambda} = qz (@var{A}, @var{B})}\n\
\n\
Computes the generalized eigenvalues\n\
@tex\n\
$\\lambda$\n\
@end tex\n\
@ifnottex\n\
@var{lambda}\n\
@end ifnottex\n\
of @math{(A - s B)}.\n\
\n\
@item @code{[AA, BB, Q, Z, V, W, @var{lambda}] = qz (@var{A}, @var{B})}\n\
\n\
Computes QZ@tie{}decomposition, generalized eigenvectors, and generalized\n\
eigenvalues of @math{(A - s B)}\n\
@tex\n\
$$ AV = BV{ \\rm diag }(\\lambda) $$\n\
$$ W^T A = { \\rm diag }(\\lambda)W^T B $$\n\
$$ AA = Q^T AZ, BB = Q^T BZ $$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
\n\
A * V = B * V * diag (@var{lambda})\n\
W' * A = diag (@var{lambda}) * W' * B\n\
AA = Q * A * Z, BB = Q * B * Z\n\
\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
with @var{Q} and @var{Z} orthogonal (unitary)= @var{I}\n\
\n\
@item @code{[AA,BB,Z@{, @var{lambda}@}] = qz (@var{A}, @var{B}, @var{opt})}\n\
\n\
As in form [2], but allows ordering of generalized eigenpairs for, e.g.,\n\
solution of discrete time algebraic Riccati equations.  Form 3 is not\n\
available for complex matrices, and does not compute the generalized\n\
eigenvectors @var{V}, @var{W}, nor the orthogonal matrix @var{Q}.\n\
\n\
@table @var\n\
@item opt\n\
for ordering eigenvalues of the @nospell{GEP} pencil.  The leading block of\n\
the revised pencil contains all eigenvalues that satisfy:\n\
\n\
@table @asis\n\
@item @qcode{\"N\"}\n\
= unordered (default)\n\
\n\
@item @qcode{\"S\"}\n\
= small: leading block has all |lambda| @leq{} 1\n\
\n\
@item @qcode{\"B\"}\n\
= big: leading block has all |lambda| @geq{} 1\n\
\n\
@item @qcode{\"-\"}\n\
= negative real part: leading block has all eigenvalues\n\
in the open left half-plane\n\
\n\
@item @qcode{\"+\"}\n\
= non-negative real part: leading block has all eigenvalues\n\
in the closed right half-plane\n\
@end table\n\
@end table\n\
@end enumerate\n\
\n\
Note: @code{qz} performs permutation balancing, but not scaling\n\
(@pxref{XREFbalance}).  The order of output arguments was selected for\n\
compatibility with @sc{matlab}.\n\
@seealso{eig, balance, lu, chol, hess, qr, qzhess, schur, svd}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

#ifdef DEBUG
  std::cout << "qz: nargin = " << nargin
            << ", nargout = " << nargout << std::endl;
#endif

  if (nargin < 2 || nargin > 3 || nargout > 7)
    {
      print_usage ();
      return retval;
    }
  else if (nargin == 3 && (nargout < 3 || nargout > 4))
    {
      error ("qz: invalid number of output arguments for form [3] call");
      return retval;
    }

#ifdef DEBUG
  std::cout << "qz: determine ordering option" << std::endl;
#endif

  // Determine ordering option.
  volatile char ord_job = 0;
  static double safmin;

  if (nargin == 2)
    ord_job = 'N';
  else if (! args(2).is_string ())
    {
      error ("qz: OPT must be a string");
      return retval;
    }
  else
    {
      std::string tmp = args(2).string_value ();

      if (! tmp.empty ())
        ord_job = tmp[0];

      if (! (ord_job == 'N' || ord_job == 'n'
             || ord_job == 'S' || ord_job == 's'
             || ord_job == 'B' || ord_job == 'b'
             || ord_job == '+' || ord_job == '-'))
        {
          error ("qz: invalid order option");
          return retval;
        }

      // overflow constant required by dlag2
      F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG2 ("S", 1),
                                   safmin
                                   F77_CHAR_ARG_LEN (1));

#ifdef DEBUG_EIG
      std::cout << "qz: initial value of safmin="
                << setiosflags (std::ios::scientific)
                << safmin << std::endl;
#endif

      // Some machines (e.g., DEC alpha) get safmin = 0;
      // for these, use eps instead to avoid problems in dlag2.
      if (safmin == 0)
        {
#ifdef DEBUG_EIG
          std::cout << "qz: DANGER WILL ROBINSON: safmin is 0!" << std::endl;
#endif

          F77_FUNC (xdlamch, XDLAMCH) (F77_CONST_CHAR_ARG2 ("E", 1),
                                       safmin
                                       F77_CHAR_ARG_LEN (1));

#ifdef DEBUG_EIG
          std::cout << "qz: safmin set to "
                    << setiosflags (std::ios::scientific)
                    << safmin << std::endl;
#endif
        }
    }

#ifdef DEBUG
  std::cout << "qz: check argument 1" << std::endl;
#endif

  // Argument 1: check if it's o.k. dimensioned.
  octave_idx_type nn = args(0).rows ();

#ifdef DEBUG
  std::cout << "argument 1 dimensions: ("
            << nn << "," << args(0).columns () << ")"
            << std::endl;
#endif

  int arg_is_empty = empty_arg ("qz", nn, args(0).columns ());

  if (arg_is_empty < 0)
    {
      gripe_empty_arg ("qz: parameter 1", 0);
      return retval;
    }
  else if (arg_is_empty > 0)
    {
      gripe_empty_arg ("qz: parameter 1; continuing", 0);
      return octave_value_list (2, Matrix ());
    }
  else if (args(0).columns () != nn)
    {
      gripe_square_matrix_required ("qz");
      return retval;
    }

  // Argument 1: dimensions look good; get the value.
  Matrix aa;
  ComplexMatrix caa;

  if (args(0).is_complex_type ())
    caa = args(0).complex_matrix_value ();
  else
    aa = args(0).matrix_value ();

  if (error_state)
    return retval;

#ifdef DEBUG
  std::cout << "qz: check argument 2" << std::endl;
#endif

  // Extract argument 2 (bb, or cbb if complex).
  if ((nn != args(1).columns ()) || (nn != args(1).rows ()))
    {
      gripe_nonconformant ();
      return retval;
    }

  Matrix bb;
  ComplexMatrix cbb;

  if (args(1).is_complex_type ())
    cbb = args(1).complex_matrix_value ();
  else
    bb = args(1).matrix_value ();

  if (error_state)
    return retval;

  // Both matrices loaded, now let's check what kind of arithmetic:
  // declared volatile to avoid compiler warnings about long jumps,
  // vforks.

  volatile int complex_case
    = (args(0).is_complex_type () || args(1).is_complex_type ());

  if (nargin == 3 && complex_case)
    {
      error ("qz: cannot re-order complex qz decomposition");
      return retval;
    }

  // First, declare variables used in both the real and complex case.
  Matrix QQ(nn,nn), ZZ(nn,nn), VR(nn,nn), VL(nn,nn);
  RowVector alphar(nn), alphai(nn), betar(nn);
  ComplexRowVector xalpha(nn), xbeta(nn);
  ComplexMatrix CQ(nn,nn), CZ(nn,nn), CVR(nn,nn), CVL(nn,nn);
  octave_idx_type ilo, ihi, info;
  char compq = (nargout >= 3 ? 'V' : 'N');
  char compz = ((nargout >= 4 || nargin == 3)? 'V' : 'N');

  // Initialize Q, Z to identity if we need either of them.
  if (compq == 'V' || compz == 'V')
    for (octave_idx_type ii = 0; ii < nn; ii++)
      for (octave_idx_type jj = 0; jj < nn; jj++)
        {
          OCTAVE_QUIT;
          QQ(ii,jj) = ZZ(ii,jj) = (ii == jj ? 1.0 : 0.0);
        }

  // Always perform permutation balancing.
  const char bal_job = 'P';
  RowVector lscale (nn), rscale (nn), work (6 * nn), rwork (nn);

  if (complex_case)
    {
#ifdef DEBUG
      if (compq == 'V')
        std::cout << "qz: performing balancing; CQ=" << std::endl
                  << CQ << std::endl;
#endif
      if (args(0).is_real_type ())
        caa = ComplexMatrix (aa);

      if (args(1).is_real_type ())
        cbb = ComplexMatrix (bb);

      if (compq == 'V')
        CQ = ComplexMatrix (QQ);

      if (compz == 'V')
        CZ = ComplexMatrix (ZZ);

      F77_XFCN (zggbal, ZGGBAL,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 nn, caa.fortran_vec (), nn, cbb.fortran_vec (),
                 nn, ilo, ihi, lscale.fortran_vec (),
                 rscale.fortran_vec (), work.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)));
    }
  else
    {
#ifdef DEBUG
      if (compq == 'V')
        std::cout << "qz: performing balancing; QQ=" << std::endl
                  << QQ << std::endl;
#endif

      F77_XFCN (dggbal, DGGBAL,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 nn, aa.fortran_vec (), nn, bb.fortran_vec (),
                 nn, ilo, ihi, lscale.fortran_vec (),
                 rscale.fortran_vec (), work.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)));
    }

  // Since we just want the balancing matrices, we can use dggbal
  // for both the real and complex cases; left first

#if 0
  if (compq == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 F77_CONST_CHAR_ARG2 ("L", 1),
                 nn, ilo, ihi, lscale.data (), rscale.data (),
                 nn, QQ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

#ifdef DEBUG
      if (compq == 'V')
        std::cout << "qz: balancing done; QQ=" << std::endl << QQ << std::endl;
#endif
  }

  // then right
  if (compz == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
                (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                 F77_CONST_CHAR_ARG2 ("R", 1),
                 nn, ilo, ihi, lscale.data (), rscale.data (),
                 nn, ZZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

#ifdef DEBUG
      if (compz == 'V')
        std::cout << "qz: balancing done; ZZ=" << std::endl << ZZ << std::endl;
#endif
    }
#endif

  static char qz_job;
  qz_job = (nargout < 2 ? 'E' : 'S');

  if (complex_case)
    {
      // Complex case.

      // The QR decomposition of cbb.
      ComplexQR cbqr (cbb);
      // The R matrix of QR decomposition for cbb.
      cbb = cbqr.R ();
      // (Q*)caa for following work.
      caa = (cbqr.Q ().hermitian ()) * caa;
      CQ = CQ * cbqr.Q ();

      F77_XFCN (zgghrd, ZGGHRD,
                (F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi, caa.fortran_vec (),
                 nn, cbb.fortran_vec (), nn, CQ.fortran_vec (), nn,
                 CZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      ComplexRowVector cwork (1 * nn);

      F77_XFCN (zhgeqz, ZHGEQZ,
                (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                 F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi,
                 caa.fortran_vec (), nn,
                 cbb.fortran_vec (),nn,
                 xalpha.fortran_vec (), xbeta.fortran_vec (),
                 CQ.fortran_vec (), nn,
                 CZ.fortran_vec (), nn,
                 cwork.fortran_vec (), nn, rwork.fortran_vec (), info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      if (compq == 'V')
        {
          // Left eigenvector.
          F77_XFCN (zggbak, ZGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("L", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, CQ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }

      // Right eigenvector.
      if (compz == 'V')
        {
          F77_XFCN (zggbak, ZGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("R", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, CZ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }

    }
  else
    {
#ifdef DEBUG
      std::cout << "qz: peforming qr decomposition of bb" << std::endl;
#endif

      // Compute the QR factorization of bb.
      QR bqr (bb);

#ifdef DEBUG
      std::cout << "qz: qr (bb) done; now peforming qz decomposition"
                << std::endl;
#endif

      bb = bqr.R ();

#ifdef DEBUG
      std::cout << "qz: extracted bb" << std::endl;
#endif

      aa = (bqr.Q ()).transpose () * aa;

#ifdef DEBUG
      std::cout << "qz: updated aa " << std::endl;
      std::cout << "bqr.Q () = " << std::endl << bqr.Q () << std::endl;

      if (compq == 'V')
        std::cout << "QQ =" << QQ << std::endl;
#endif

      if (compq == 'V')
        QQ = QQ * bqr.Q ();

#ifdef DEBUG
      std::cout << "qz: precursors done..." << std::endl;
#endif

#ifdef DEBUG
      std::cout << "qz: compq = " << compq << ", compz = " << compz
                << std::endl;
#endif

      // Reduce  to generalized hessenberg form.
      F77_XFCN (dgghrd, DGGHRD,
                (F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi, aa.fortran_vec (),
                 nn, bb.fortran_vec (), nn, QQ.fortran_vec (), nn,
                 ZZ.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      // Check if just computing generalized eigenvalues or if we're
      // actually computing the decomposition.

      // Reduce to generalized Schur form.
      F77_XFCN (dhgeqz, DHGEQZ,
                (F77_CONST_CHAR_ARG2 (&qz_job, 1),
                 F77_CONST_CHAR_ARG2 (&compq, 1),
                 F77_CONST_CHAR_ARG2 (&compz, 1),
                 nn, ilo, ihi, aa.fortran_vec (), nn, bb.fortran_vec (),
                 nn, alphar.fortran_vec (), alphai.fortran_vec (),
                 betar.fortran_vec (), QQ.fortran_vec (), nn,
                 ZZ.fortran_vec (), nn, work.fortran_vec (), nn, info
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)
                 F77_CHAR_ARG_LEN (1)));

      if (compq == 'V')
        {
          F77_XFCN (dggbak, DGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("L", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, QQ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

#ifdef DEBUG
          if (compq == 'V')
            std::cout << "qz: balancing done; QQ=" << std::endl
                      << QQ << std::endl;
#endif
        }

      // then right
      if (compz == 'V')
        {
          F77_XFCN (dggbak, DGGBAK,
                    (F77_CONST_CHAR_ARG2 (&bal_job, 1),
                     F77_CONST_CHAR_ARG2 ("R", 1),
                     nn, ilo, ihi, lscale.data (), rscale.data (),
                     nn, ZZ.fortran_vec (), nn, info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

#ifdef DEBUG
          if (compz == 'V')
            std::cout << "qz: balancing done; ZZ=" << std::endl
                      << ZZ << std::endl;
#endif
        }

    }

  // Order the QZ decomposition?
  if (! (ord_job == 'N' || ord_job == 'n'))
    {
      if (complex_case)
        {
          // Probably not needed, but better be safe.
          error ("qz: cannot re-order complex qz decomposition");
          return retval;
        }
      else
        {
#ifdef DEBUG_SORT
          std::cout << "qz: ordering eigenvalues: ord_job = "
                    << ord_job << std::endl;
#endif

          // Declared static to avoid vfork/long jump compiler complaints.
          static sort_function sort_test;
          sort_test = 0;

          switch (ord_job)
            {
            case 'S':
            case 's':
              sort_test = &fin;
              break;

            case 'B':
            case 'b':
              sort_test = &fout;
              break;

            case '+':
              sort_test = &fcrhp;
              break;

            case '-':
              sort_test = &folhp;
              break;

            default:
              // Invalid order option (should never happen, since we
              // checked the options at the top).
              panic_impossible ();
              break;
            }

          octave_idx_type ndim, fail;
          double inf_norm;

          F77_XFCN (xdlange, XDLANGE,
                    (F77_CONST_CHAR_ARG2 ("I", 1),
                     nn, nn, aa.data (), nn, work.fortran_vec (), inf_norm
                     F77_CHAR_ARG_LEN (1)));

          double eps = std::numeric_limits<double>::epsilon () * inf_norm * nn;

#ifdef DEBUG_SORT
          std::cout << "qz: calling dsubsp: aa=" << std::endl;
          octave_print_internal (std::cout, aa, 0);
          std::cout << std::endl << "bb="  << std::endl;
          octave_print_internal (std::cout, bb, 0);
          if (compz == 'V')
            {
              std::cout << std::endl << "ZZ="  << std::endl;
              octave_print_internal (std::cout, ZZ, 0);
            }
          std::cout << std::endl;
          std::cout << "alphar = " << std::endl;
          octave_print_internal (std::cout, (Matrix) alphar, 0);
          std::cout << std::endl << "alphai = " << std::endl;
          octave_print_internal (std::cout, (Matrix) alphai, 0);
          std::cout << std::endl << "beta = " << std::endl;
          octave_print_internal (std::cout, (Matrix) betar, 0);
          std::cout << std::endl;
#endif

          Array<octave_idx_type> ind (dim_vector (nn, 1));

          F77_XFCN (dsubsp, DSUBSP,
                    (nn, nn, aa.fortran_vec (), bb.fortran_vec (),
                     ZZ.fortran_vec (), sort_test, eps, ndim, fail,
                     ind.fortran_vec ()));

#ifdef DEBUG
          std::cout << "qz: back from dsubsp: aa=" << std::endl;
          octave_print_internal (std::cout, aa, 0);
          std::cout << std::endl << "bb="  << std::endl;
          octave_print_internal (std::cout, bb, 0);
          if (compz == 'V')
            {
              std::cout << std::endl << "ZZ="  << std::endl;
              octave_print_internal (std::cout, ZZ, 0);
            }
          std::cout << std::endl;
#endif

          // Manually update alphar, alphai, betar.
          static int jj;

          jj = 0;
          while (jj < nn)
            {
#ifdef DEBUG_EIG
              std::cout << "computing gen eig #" << jj << std::endl;
#endif

              // Number of zeros in this block.
              static int zcnt;

              if (jj == (nn-1))
                zcnt = 1;
              else if (aa(jj+1,jj) == 0)
                zcnt = 1;
              else zcnt = 2;

              if (zcnt == 1)
                {
                  // Real zero.
#ifdef DEBUG_EIG
                  std::cout << "  single gen eig:" << std::endl;
                  std::cout << "  alphar(" << jj << ") = " << aa(jj,jj)
                            << std::endl;
                  std::cout << "  betar(" << jj << ") = " << bb(jj,jj)
                            << std::endl;
                  std::cout << "  alphai(" << jj << ") = 0" << std::endl;
#endif

                  alphar(jj) = aa(jj,jj);
                  alphai(jj) = 0;
                  betar(jj) = bb(jj,jj);
                }
              else
                {
                  // Complex conjugate pair.
#ifdef DEBUG_EIG
                  std::cout << "qz: calling dlag2:" << std::endl;
                  std::cout << "safmin="
                            << setiosflags (std::ios::scientific)
                            << safmin << std::endl;

                  for (int idr = jj; idr <= jj+1; idr++)
                    {
                      for (int idc = jj; idc <= jj+1; idc++)
                        {
                          std::cout << "aa(" << idr << "," << idc << ")="
                                    << aa(idr,idc) << std::endl;
                          std::cout << "bb(" << idr << "," << idc << ")="
                                    << bb(idr,idc) << std::endl;
                        }
                    }
#endif

                  // FIXME: probably should be using
                  // fortran_vec instead of &aa(jj,jj) here.

                  double scale1, scale2, wr1, wr2, wi;
                  const double *aa_ptr = aa.data () + jj * nn + jj;
                  const double *bb_ptr = bb.data () + jj * nn + jj;
                  F77_XFCN (dlag2, DLAG2,
                            (aa_ptr, nn, bb_ptr, nn, safmin,
                             scale1, scale2, wr1, wr2, wi));

#ifdef DEBUG_EIG
                  std::cout << "dlag2 returns: scale1=" << scale1
                            << "\tscale2=" << scale2 << std::endl
                            << "\twr1=" << wr1 << "\twr2=" << wr2
                            << "\twi=" << wi << std::endl;
#endif

                  // Just to be safe, check if it's a real pair.
                  if (wi == 0)
                    {
                      alphar(jj) = wr1;
                      alphai(jj) = 0;
                      betar(jj) = scale1;
                      alphar(jj+1) = wr2;
                      alphai(jj+1) = 0;
                      betar(jj+1) = scale2;
                    }
                  else
                    {
                      alphar(jj) = alphar(jj+1) = wr1;
                      alphai(jj) = -(alphai(jj+1) = wi);
                      betar(jj)  = betar(jj+1) = scale1;
                    }
                }

              // Advance past this block.
              jj += zcnt;
            }

#ifdef DEBUG_SORT
          std::cout << "qz: back from dsubsp: aa=" << std::endl;
          octave_print_internal (std::cout, aa, 0);
          std::cout << std::endl << "bb="  << std::endl;
          octave_print_internal (std::cout, bb, 0);

          if (compz == 'V')
            {
              std::cout << std::endl << "ZZ="  << std::endl;
              octave_print_internal (std::cout, ZZ, 0);
            }
          std::cout << std::endl << "qz: ndim=" << ndim << std::endl
                    << "fail=" << fail << std::endl;
          std::cout << "alphar = " << std::endl;
          octave_print_internal (std::cout, (Matrix) alphar, 0);
          std::cout << std::endl << "alphai = " << std::endl;
          octave_print_internal (std::cout, (Matrix) alphai, 0);
          std::cout << std::endl << "beta = " << std::endl;
          octave_print_internal (std::cout, (Matrix) betar, 0);
          std::cout << std::endl;
#endif
        }
    }

  // Compute generalized eigenvalues?
  ComplexColumnVector gev;

  if (nargout < 2 || nargout == 7 || (nargin == 3 && nargout == 4))
    {
      if (complex_case)
        {
          int cnt = 0;

          for (int ii = 0; ii < nn; ii++)
            cnt++;

          ComplexColumnVector tmp (cnt);

          cnt = 0;
          for (int ii = 0; ii < nn; ii++)
            tmp(cnt++) = xalpha(ii) / xbeta(ii);

          gev = tmp;
        }
      else
        {
#ifdef DEBUG
          std::cout << "qz: computing generalized eigenvalues" << std::endl;
#endif

          // Return finite generalized eigenvalues.
          int cnt = 0;

          for (int ii = 0; ii < nn; ii++)
            if (betar(ii) != 0)
              cnt++;

          ComplexColumnVector tmp (cnt);

          cnt = 0;
          for (int ii = 0; ii < nn; ii++)
            if (betar(ii) != 0)
              tmp(cnt++) = Complex(alphar(ii), alphai(ii))/betar(ii);

          gev = tmp;
        }
    }

  // Right, left eigenvector matrices.
  if (nargout >= 5)
    {
      // Which side to compute?
      char side = (nargout == 5 ? 'R' : 'B');
      // Compute all of them and backtransform
      char howmny = 'B';
      // Dummy pointer; select is not used.
      octave_idx_type *select = 0;

      if (complex_case)
        {
          CVL = CQ;
          CVR = CZ;
          ComplexRowVector cwork2 (2 * nn);
          RowVector rwork2 (8 * nn);
          octave_idx_type m;

          F77_XFCN (ztgevc, ZTGEVC,
                    (F77_CONST_CHAR_ARG2 (&side, 1),
                     F77_CONST_CHAR_ARG2 (&howmny, 1),
                     select, nn, caa.fortran_vec (), nn, cbb.fortran_vec (),
                     nn, CVL.fortran_vec (), nn, CVR.fortran_vec (), nn, nn,
                     m, cwork2.fortran_vec (), rwork2.fortran_vec (), info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));
        }
      else
        {
#ifdef DEBUG
          std::cout << "qz: computing  generalized eigenvectors" << std::endl;
#endif

          VL = QQ;
          VR = ZZ;
          octave_idx_type m;

          F77_XFCN (dtgevc, DTGEVC,
                    (F77_CONST_CHAR_ARG2 (&side, 1),
                     F77_CONST_CHAR_ARG2 (&howmny, 1),
                     select, nn, aa.fortran_vec (), nn, bb.fortran_vec (),
                     nn, VL.fortran_vec (), nn, VR.fortran_vec (), nn, nn,
                     m, work.fortran_vec (), info
                     F77_CHAR_ARG_LEN (1)
                     F77_CHAR_ARG_LEN (1)));

          // Now construct the complex form of VV, WW.
          int jj = 0;

          while (jj < nn)
            {
              OCTAVE_QUIT;

              // See if real or complex eigenvalue.

              // Column increment; assume complex eigenvalue.
              int cinc = 2;

              if (jj == (nn-1))
                // Single column.
                cinc = 1;
              else if (aa(jj+1,jj) == 0)
                cinc = 1;

              // Now copy the eigenvector (s) to CVR, CVL.
              if (cinc == 1)
                {
                  for (int ii = 0; ii < nn; ii++)
                    CVR(ii,jj) = VR(ii,jj);

                  if (side == 'B')
                    for (int ii = 0; ii < nn; ii++)
                      CVL(ii,jj) = VL(ii,jj);
                }
              else
                {
                  // Double column; complex vector.

                  for (int ii = 0; ii < nn; ii++)
                    {
                      CVR(ii,jj) = Complex (VR(ii,jj), VR(ii,jj+1));
                      CVR(ii,jj+1) = Complex (VR(ii,jj), -VR(ii,jj+1));
                    }

                  if (side == 'B')
                    for (int ii = 0; ii < nn; ii++)
                      {
                        CVL(ii,jj) = Complex (VL(ii,jj), VL(ii,jj+1));
                        CVL(ii,jj+1) = Complex (VL(ii,jj), -VL(ii,jj+1));
                      }
                }

              // Advance to next eigenvectors (if any).
              jj += cinc;
            }
        }
    }

  switch (nargout)
    {
    case 7:
      retval(6) = gev;

    case 6:
      // Return eigenvectors.
      retval(5) = CVL;

    case 5:
      // Return eigenvectors.
      retval(4) = CVR;

    case 4:
      if (nargin == 3)
        {
#ifdef DEBUG
          std::cout << "qz: sort: retval(3) = gev = " << std::endl;
          octave_print_internal (std::cout, gev);
          std::cout << std::endl;
#endif
          retval(3) = gev;
        }
      else
        {
          if (complex_case)
            retval(3) = CZ;
          else
            retval(3) = ZZ;
        }

    case 3:
      if (nargin == 3)
        {
          if (complex_case)
            retval(2) = CZ;
          else
            retval(2) = ZZ;
        }
      else
        {
          if (complex_case)
            retval(2) = CQ.hermitian ();
          else
            retval(2) = QQ.transpose ();
        }

    case 2:
      {
        if (complex_case)
          {
#ifdef DEBUG
            std::cout << "qz: retval(1) = cbb = " << std::endl;
            octave_print_internal (std::cout, cbb, 0);
            std::cout << std::endl << "qz: retval(0) = caa = " <<std::endl;
            octave_print_internal (std::cout, caa, 0);
            std::cout << std::endl;
#endif
            retval(1) = cbb;
            retval(0) = caa;
          }
        else
          {
#ifdef DEBUG
            std::cout << "qz: retval(1) = bb = " << std::endl;
            octave_print_internal (std::cout, bb, 0);
            std::cout << std::endl << "qz: retval(0) = aa = " <<std::endl;
            octave_print_internal (std::cout, aa, 0);
            std::cout << std::endl;
#endif
            retval(1) = bb;
            retval(0) = aa;
          }
      }
      break;


    case 1:
    case 0:
#ifdef DEBUG
      std::cout << "qz: retval(0) = gev = " << gev << std::endl;
#endif
      retval(0) = gev;
      break;

    default:
      error ("qz: too many return arguments");
      break;
  }

#ifdef DEBUG
  std::cout << "qz: exiting (at long last)" << std::endl;
#endif

  return retval;
}

/*
%!shared a, b, c
%! a = [1 2; 0 3];
%! b = [1 0; 0 0];
%! c = [0 1; 0 0];
%!assert (qz (a,b), 1)
%!assert (isempty (qz (a,c)))

## Exaple 7.7.3 in Golub & Van Loan
%!test
%! a = [ 10  1  2;
%!        1  2 -1;
%!        1  1  2];
%! b = reshape (1:9,3,3);
%! [aa, bb, q, z, v, w, lambda] = qz (a, b);
%! sz = length (lambda);
%! observed = (b * v * diag ([lambda;0])) (:, 1:sz);
%! assert ((a*v)(:, 1:sz), observed, norm (observed) * 1e-14);
%! observed = (diag ([lambda;0]) * w' * b) (1:sz, :);
%! assert ((w'*a)(1:sz, :) , observed, norm (observed) * 1e-13);
%! assert (q * a * z, aa, norm (aa) * 1e-14);
%! assert (q * b * z, bb, norm (bb) * 1e-14);

%!test
%! A = [0, 0, -1, 0; 1, 0, 0, 0; -1, 0, -2, -1; 0, -1, 1, 0];
%! B = [0, 0, 0, 0; 0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1];
%! [AA, BB, Q, Z1] = qz (A, B);
%! [AA, BB, Z2] = qz (A, B, '-');
%! assert (Z1, Z2);
*/
