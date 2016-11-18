/*

Copyright (C) 2000-2015 Gabriele Pannocchia

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

#include "dbleCHOL.h"
#include "dbleSVD.h"
#include "mx-m-dm.h"
#include "EIG.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pr-output.h"
#include "utils.h"

static Matrix
null (const Matrix& A, octave_idx_type& rank)
{
  Matrix retval;

  rank = 0;

  if (! A.is_empty ())
    {
      SVD A_svd (A);

      DiagMatrix S = A_svd.singular_values ();

      ColumnVector s = S.extract_diag ();

      Matrix V = A_svd.right_singular_matrix ();

      octave_idx_type A_nr = A.rows ();
      octave_idx_type A_nc = A.cols ();

      octave_idx_type tmp = A_nr > A_nc ? A_nr : A_nc;

      double tol = tmp * s(0) * std::numeric_limits<double>::epsilon ();

      octave_idx_type n = s.length ();

      for (octave_idx_type i = 0; i < n; i++)
        {
          if (s(i) > tol)
            rank++;
        }

      if (rank < A_nc)
        retval = V.extract (0, rank, A_nc-1, A_nc-1);
      else
        retval.resize (A_nc, 0);

      for (octave_idx_type i = 0; i < retval.numel (); i++)
        if (std::abs (retval(i)) < std::numeric_limits<double>::epsilon ())
          retval(i) = 0;
    }

  return retval;
}

static int
qp (const Matrix& H, const ColumnVector& q,
    const Matrix& Aeq, const ColumnVector& beq,
    const Matrix& Ain, const ColumnVector& bin,
    int maxit,
    ColumnVector& x, ColumnVector& lambda, int& iter)
{
  int info = 0;

  iter = 0;

  double rtol = sqrt (std::numeric_limits<double>::epsilon ());

  // Problem dimension.
  octave_idx_type n = x.length ();

  // Dimension of constraints.
  octave_idx_type n_eq = beq.length ();
  octave_idx_type n_in = bin.length ();

  // Filling the current active set.

  octave_idx_type n_act = n_eq;

  octave_idx_type n_tot = n_eq + n_in;

  // Equality constraints come first.  We won't check the sign of the
  // Lagrange multiplier for those.

  Matrix Aact = Aeq;
  ColumnVector bact = beq;
  ColumnVector Wact;

  if (n_in > 0)
    {
      ColumnVector res = Ain*x - bin;

      for (octave_idx_type i = 0; i < n_in; i++)
        {
          res(i) /= (1.0 + std::abs (bin(i)));

          if (res(i) < rtol)
            {
              n_act++;
              Aact = Aact.stack (Ain.row (i));
              bact.resize (n_act, bin(i));
              Wact.resize (n_act-n_eq, i);
            }
        }
    }

  // Computing the ???

  EIG eigH (H);

  if (error_state)
    {
      error ("qp: failed to compute eigenvalues of H");
      return -1;
    }

  ColumnVector eigenvalH = real (eigH.eigenvalues ());
  Matrix eigenvecH = real (eigH.eigenvectors ());
  double minReal = eigenvalH.min ();
  octave_idx_type indminR = 0;
  for (octave_idx_type i = 0; i < n; i++)
    {
      if (minReal == eigenvalH(i))
        {
          indminR = i;
          break;
        }
    }

  bool done = false;

  double alpha = 0.0;

  Matrix R;
  Matrix Y (n, 0, 0.0);

  ColumnVector g (n, 0.0);
  ColumnVector p (n, 0.0);

  ColumnVector lambda_tmp (n_in, 0.0);

  while (! done)
    {
      iter++;

      // Current Gradient
      // g = q + H * x;

      g = q + H * x;

      if (n_act == 0)
        {
          // There are no active constraints.

          if (minReal > 0.0)
            {
              // Inverting the Hessian.  Using the Cholesky
              // factorization since the Hessian is positive
              // definite.

              CHOL cholH (H);

              R = cholH.chol_matrix ();

              Matrix Hinv = chol2inv (R);

              // Computing the unconstrained step.
              // p = -Hinv * g;

              p = -Hinv * g;

              info = 0;
            }
          else
            {
              // Finding the negative curvature of H.

              p = eigenvecH.column (indminR);

              // Following the negative curvature of H.

              if (p.transpose () * g > std::numeric_limits<double>::epsilon ())
                p = -p;

              info = 1;
            }

          // Multipliers are zero.
          lambda_tmp.fill (0.0);
        }
      else
        {
          // There are active constraints.

          // Computing the null space.

          octave_idx_type rank;

          Matrix Z = null (Aact, rank);

          octave_idx_type dimZ = n - rank;

          // FIXME: still remain to handle the case of
          // non-full rank active set matrix.

          // Computing the Y matrix (orthogonal to Z)
          Y = Aact.pseudo_inverse ();

          // Reduced Hessian
          Matrix Zt = Z.transpose ();
          Matrix rH = Zt * H * Z;

          octave_idx_type pR = 0;

          if (dimZ > 0)
            {
              // Computing the Cholesky factorization (pR = 0 means
              // that the reduced Hessian was positive definite).

              CHOL cholrH (rH, pR);
              Matrix tR = cholrH.chol_matrix ();
              if (pR == 0)
                R = tR;
            }

          if (pR == 0)
            {
              info = 0;

              // Computing the step pz.
              if (dimZ > 0)
                {
                  // Using the Cholesky factorization to invert rH

                  Matrix rHinv = chol2inv (R);

                  ColumnVector pz = -rHinv * Zt * g;

                  // Global step.
                  p = Z * pz;
                }
              else
                {
                  // Global step.
                  p.fill (0.0);
                }
            }
          else
            {
              info = 1;

              // Searching for the most negative curvature.

              EIG eigrH (rH);

              if (error_state)
                {
                  error ("qp: failed to compute eigenvalues of rH");
                  return -1;
                }

              ColumnVector eigenvalrH = real (eigrH.eigenvalues ());
              Matrix eigenvecrH = real (eigrH.eigenvectors ());
              double mRrH = eigenvalrH.min ();
              indminR = 0;
              for (octave_idx_type i = 0; i < n; i++)
                {
                  if (mRrH == eigenvalH(i))
                    {
                      indminR = i;
                      break;
                    }
                }

              ColumnVector eVrH = eigenvecrH.column (indminR);

              // Computing the step pz.
              p = Z * eVrH;

              if (p.transpose () * g > std::numeric_limits<double>::epsilon ())
                p = -p;
            }
        }

      // Checking the step-size.
      ColumnVector abs_p (n);
      for (octave_idx_type i = 0; i < n; i++)
        abs_p(i) = std::abs (p(i));
      double max_p = abs_p.max ();

      if (max_p < rtol)
        {
          // The step is null.  Checking constraints.
          if (n_act - n_eq == 0)
            // Solution is found because no inequality
            // constraints are active.
            done = true;
          else
            {
              // Computing the multipliers only for the inequality
              // constraints that are active.  We do NOT compute
              // multipliers for the equality constraints.
              Matrix Yt = Y.transpose ();
              Yt = Yt.extract_n (n_eq, 0, n_act-n_eq, n);
              lambda_tmp = Yt * (g + H * p);

              // Checking the multipliers.  We remove the most
              // negative from the set (if any).
              double min_lambda = lambda_tmp.min ();
              if (min_lambda >= 0)
                {
                  // Solution is found.
                  done = true;
                }
              else
                {
                  octave_idx_type which_eig = 0;
                  for (octave_idx_type i = 0; i < n_act; i++)
                    {
                      if (lambda_tmp(i) == min_lambda)
                        {
                          which_eig = i;
                          break;
                        }
                    }

                  // At least one multiplier is negative, we
                  // remove it from the set.

                  n_act--;
                  for (octave_idx_type i = which_eig; i < n_act - n_eq; i++)
                    {
                      Wact(i) = Wact(i+1);
                      for (octave_idx_type j = 0; j < n; j++)
                        Aact(n_eq+i,j) = Aact(n_eq+i+1,j);
                      bact(n_eq+i) = bact(n_eq+i+1);
                    }

                  // Resizing the active set.
                  Wact.resize (n_act-n_eq);
                  bact.resize (n_act);
                  Aact.resize (n_act, n);
                }
            }
        }
      else
        {
          // The step is not null.
          if (n_act - n_eq == n_in)
            {
              // All inequality constraints were active.  We can
              // add the whole step.
              x += p;
            }
          else
            {
              // Some constraints were not active.  Checking if
              // there is a blocking constraint.
              alpha = 1.0;
              octave_idx_type is_block = -1;

              for (octave_idx_type i = 0; i < n_in; i++)
                {
                  bool found = false;

                  for (octave_idx_type j = 0; j < n_act-n_eq; j++)
                    {
                      if (Wact(j) == i)
                        {
                          found = true;
                          break;
                        }
                    }

                  if (! found)
                    {
                      // The i-th constraint was not in the set.  Is it a
                      // blocking constraint?

                      RowVector tmp_row = Ain.row (i);
                      double tmp = tmp_row * p;
                      double res = tmp_row * x;

                      if (tmp < 0.0)
                        {
                          double alpha_tmp = (bin(i) - res) / tmp;

                          if (alpha_tmp < alpha)
                            {
                              alpha = alpha_tmp;
                              is_block = i;
                            }
                        }
                    }
                }

              // In is_block there is the index of the blocking
              // constraint (if any).
              if (is_block >= 0)
                {
                  // There is a blocking constraint (index in
                  // is_block) which is added to the active set.
                  n_act++;
                  Aact = Aact.stack (Ain.row (is_block));
                  bact.resize (n_act, bin(is_block));
                  Wact.resize (n_act-n_eq, is_block);

                  // Adding the reduced step
                  x += alpha * p;
                }
              else
                {
                  // There are no blocking constraints.  Adding the
                  // whole step.
                  x += alpha * p;
                }
            }
        }

      if (iter == maxit)
        {
          done = true;
          // warning ("qp_main: maximum number of iteration reached");
          info = 3;
        }
    }

  lambda_tmp = Y.transpose () * (g + H * p);

  // Reordering the Lagrange multipliers.

  lambda.resize (n_tot);
  lambda.fill (0.0);
  for (octave_idx_type i = 0; i < n_eq; i++)
    lambda(i) = lambda_tmp(i);

  for (octave_idx_type i = n_eq; i < n_tot; i++)
    {
      for (octave_idx_type j = 0; j < n_act-n_eq; j++)
        {
          if (Wact(j) == i - n_eq)
            {
              lambda(i) = lambda_tmp(n_eq+j);
              break;
            }
        }
    }

  return info;
}

DEFUN (__qp__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{x}, @var{lambda}, @var{info}, @var{iter}] =} __qp__ (@var{x0}, @var{H}, @var{q}, @var{Aeq}, @var{beq}, @var{Ain}, @var{bin}, @var{maxit})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 8)
    {
      const ColumnVector x0  (args(0) . vector_value ());
      const Matrix H         (args(1) . matrix_value ());
      const ColumnVector q   (args(2) . vector_value ());
      const Matrix Aeq       (args(3) . matrix_value ());
      const ColumnVector beq (args(4) . vector_value ());
      const Matrix Ain       (args(5) . matrix_value ());
      const ColumnVector bin (args(6) . vector_value ());
      const int maxit        (args(7) . int_value ());

      if (! error_state)
        {
          int iter = 0;

          // Copying the initial guess in the working variable
          ColumnVector x = x0;

          // Reordering the Lagrange multipliers
          ColumnVector lambda;

          int info = qp (H, q, Aeq, beq, Ain, bin, maxit, x, lambda, iter);

          if (! error_state)
            {
              retval(3) = iter;
              retval(2) = info;
              retval(1) = lambda;
              retval(0) = x;
            }
          else
            error ("qp: internal error");
        }
      else
        error ("__qp__: invalid arguments");
    }
  else
    print_usage ();

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
