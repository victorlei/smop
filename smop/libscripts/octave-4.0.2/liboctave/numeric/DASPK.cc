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

#include <sstream>

#include "DASPK.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-math.h"
#include "quit.h"

typedef octave_idx_type (*daspk_fcn_ptr) (const double&, const double*,
                                          const double*, const double&,
                                          double*, octave_idx_type&,
                                          double*, octave_idx_type*);

typedef octave_idx_type (*daspk_jac_ptr) (const double&, const double*,
                                          const double*, double*,
                                          const double&, double*,
                                          octave_idx_type*);

typedef octave_idx_type (*daspk_psol_ptr) (const octave_idx_type&,
                                           const double&, const double*,
                                           const double*, const double*,
                                           const double&, const double*,
                                           double*, octave_idx_type*,
                                           double*, const double&,
                                           octave_idx_type&, double*,
                                           octave_idx_type*);

extern "C"
{
  F77_RET_T
  F77_FUNC (ddaspk, DDASPK) (daspk_fcn_ptr, const octave_idx_type&,
                             double&, double*, double*, double&,
                             const octave_idx_type*, const double*,
                             const double*, octave_idx_type&,
                             double*, const octave_idx_type&,
                             octave_idx_type*, const octave_idx_type&,
                             const double*, const octave_idx_type*,
                             daspk_jac_ptr, daspk_psol_ptr);
}

static DAEFunc::DAERHSFunc user_fun;
static DAEFunc::DAEJacFunc user_jac;
static octave_idx_type nn;

static octave_idx_type
ddaspk_f (const double& time, const double *state, const double *deriv,
          const double&, double *delta, octave_idx_type& ires, double *,
          octave_idx_type *)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  ColumnVector tmp_deriv (nn);
  ColumnVector tmp_state (nn);
  ColumnVector tmp_delta (nn);

  for (octave_idx_type i = 0; i < nn; i++)
    {
      tmp_deriv.elem (i) = deriv[i];
      tmp_state.elem (i) = state[i];
    }

  tmp_delta = user_fun (tmp_state, tmp_deriv, time, ires);

  if (ires >= 0)
    {
      if (tmp_delta.length () == 0)
        ires = -2;
      else
        {
          for (octave_idx_type i = 0; i < nn; i++)
            delta[i] = tmp_delta.elem (i);
        }
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

//NEQ, T, Y, YPRIME, SAVR, WK, CJ, WGHT,
//C                          WP, IWP, B, EPLIN, IER, RPAR, IPAR)

static octave_idx_type
ddaspk_psol (const octave_idx_type&, const double&, const double *,
             const double *, const double *, const double&,
             const double *, double *, octave_idx_type *, double *,
             const double&, octave_idx_type&, double *, octave_idx_type*)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  abort ();

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}


static octave_idx_type
ddaspk_j (const double& time, const double *state, const double *deriv,
          double *pd, const double& cj, double *, octave_idx_type *)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  // FIXME: would be nice to avoid copying the data.

  ColumnVector tmp_state (nn);
  ColumnVector tmp_deriv (nn);

  for (octave_idx_type i = 0; i < nn; i++)
    {
      tmp_deriv.elem (i) = deriv[i];
      tmp_state.elem (i) = state[i];
    }

  Matrix tmp_pd = user_jac (tmp_state, tmp_deriv, time, cj);

  for (octave_idx_type j = 0; j < nn; j++)
    for (octave_idx_type i = 0; i < nn; i++)
      pd[nn * j + i] = tmp_pd.elem (i, j);

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

ColumnVector
DASPK::do_integrate (double tout)
{
  // FIXME: should handle all this option stuff just once
  // for each new problem.

  ColumnVector retval;

  if (! initialized || restart || DAEFunc::reset|| DASPK_options::reset)
    {
      integration_error = false;

      initialized = true;

      info.resize (dim_vector (20, 1));

      for (octave_idx_type i = 0; i < 20; i++)
        info(i) = 0;

      octave_idx_type n = size ();

      nn = n;

      info(0) = 0;

      if (stop_time_set)
        {
          rwork(0) = stop_time;
          info(3) = 1;
        }
      else
        info(3) = 0;

      // DAEFunc

      user_fun = DAEFunc::function ();
      user_jac = DAEFunc::jacobian_function ();

      if (user_fun)
        {
          octave_idx_type ires = 0;

          ColumnVector res = (*user_fun) (x, xdot, t, ires);

          if (res.length () != x.length ())
            {
              (*current_liboctave_error_handler)
                ("daspk: inconsistent sizes for state and residual vectors");

              integration_error = true;
              return retval;
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("daspk: no user supplied RHS subroutine!");

          integration_error = true;
          return retval;
        }

      info(4) = user_jac ? 1 : 0;

      DAEFunc::reset = false;

      octave_idx_type eiq = enforce_inequality_constraints ();
      octave_idx_type ccic = compute_consistent_initial_condition ();
      octave_idx_type eavfet = exclude_algebraic_variables_from_error_test ();

      liw = 40 + n;
      if (eiq == 1 || eiq == 3)
        liw += n;
      if (ccic == 1 || eavfet == 1)
        liw += n;

      lrw = 50 + 9*n + n*n;
      if (eavfet == 1)
        lrw += n;

      iwork.resize (dim_vector (liw, 1));
      rwork.resize (dim_vector (lrw, 1));

      // DASPK_options

      abs_tol = absolute_tolerance ();
      rel_tol = relative_tolerance ();

      octave_idx_type abs_tol_len = abs_tol.length ();
      octave_idx_type rel_tol_len = rel_tol.length ();

      if (abs_tol_len == 1 && rel_tol_len == 1)
        {
          info(1) = 0;
        }
      else if (abs_tol_len == n && rel_tol_len == n)
        {
          info(1) = 1;
        }
      else
        {
          (*current_liboctave_error_handler)
            ("daspk: inconsistent sizes for tolerance arrays");

          integration_error = true;
          return retval;
        }

      double hmax = maximum_step_size ();
      if (hmax >= 0.0)
        {
          rwork(1) = hmax;
          info(6) = 1;
        }
      else
        info(6) = 0;

      double h0 = initial_step_size ();
      if (h0 >= 0.0)
        {
          rwork(2) = h0;
          info(7) = 1;
        }
      else
        info(7) = 0;

      octave_idx_type maxord = maximum_order ();
      if (maxord >= 0)
        {
          if (maxord > 0 && maxord < 6)
            {
              info(8) = 1;
              iwork(2) = maxord;
            }
          else
            {
              (*current_liboctave_error_handler)
                ("daspk: invalid value for maximum order");
              integration_error = true;
              return retval;
            }
        }

      switch (eiq)
        {
        case 1:
        case 3:
          {
            Array<octave_idx_type> ict = inequality_constraint_types ();

            if (ict.length () == n)
              {
                for (octave_idx_type i = 0; i < n; i++)
                  {
                    octave_idx_type val = ict(i);
                    if (val < -2 || val > 2)
                      {
                        (*current_liboctave_error_handler)
                          ("daspk: invalid value for inequality constraint type");
                        integration_error = true;
                        return retval;
                      }
                    iwork(40+i) = val;
                  }
              }
            else
              {
                (*current_liboctave_error_handler)
                  ("daspk: inequality constraint types size mismatch");
                integration_error = true;
                return retval;
              }
          }
          // Fall through...

        case 0:
        case 2:
          info(9) = eiq;
          break;

        default:
          (*current_liboctave_error_handler)
            ("daspk: invalid value for enforce inequality constraints option");
          integration_error = true;
          return retval;
        }

      if (ccic)
        {
          if (ccic == 1)
            {
              // FIXME: this code is duplicated below.

              Array<octave_idx_type> av = algebraic_variables ();

              if (av.length () == n)
                {
                  octave_idx_type lid;
                  if (eiq == 0 || eiq == 2)
                    lid = 40;
                  else if (eiq == 1 || eiq == 3)
                    lid = 40 + n;
                  else
                    abort ();

                  for (octave_idx_type i = 0; i < n; i++)
                    iwork(lid+i) = av(i) ? -1 : 1;
                }
              else
                {
                  (*current_liboctave_error_handler)
                    ("daspk: algebraic variables size mismatch");
                  integration_error = true;
                  return retval;
                }
            }
          else if (ccic != 2)
            {
              (*current_liboctave_error_handler)
                ("daspk: invalid value for compute consistent initial condition option");
              integration_error = true;
              return retval;
            }

          info(10) = ccic;
        }

      if (eavfet)
        {
          info(15) = 1;

          // FIXME: this code is duplicated above.

          Array<octave_idx_type> av = algebraic_variables ();

          if (av.length () == n)
            {
              octave_idx_type lid;
              if (eiq == 0 || eiq == 2)
                lid = 40;
              else if (eiq == 1 || eiq == 3)
                lid = 40 + n;
              else
                abort ();

              for (octave_idx_type i = 0; i < n; i++)
                iwork(lid+i) = av(i) ? -1 : 1;
            }
        }

      if (use_initial_condition_heuristics ())
        {
          Array<double> ich = initial_condition_heuristics ();

          if (ich.length () == 6)
            {
              iwork(31) = NINTbig (ich(0));
              iwork(32) = NINTbig (ich(1));
              iwork(33) = NINTbig (ich(2));
              iwork(34) = NINTbig (ich(3));

              rwork(13) = ich(4);
              rwork(14) = ich(5);
            }
          else
            {
              (*current_liboctave_error_handler)
                ("daspk: invalid initial condition heuristics option");
              integration_error = true;
              return retval;
            }

          info(16) = 1;
        }

      octave_idx_type pici = print_initial_condition_info ();
      switch (pici)
        {
        case 0:
        case 1:
        case 2:
          info(17) = pici;
          break;

        default:
          (*current_liboctave_error_handler)
            ("daspk: invalid value for print initial condition info option");
          integration_error = true;
          return retval;
          break;
        }

      DASPK_options::reset = false;

      restart = false;
    }

  double *px = x.fortran_vec ();
  double *pxdot = xdot.fortran_vec ();

  octave_idx_type *pinfo = info.fortran_vec ();

  double *prel_tol = rel_tol.fortran_vec ();
  double *pabs_tol = abs_tol.fortran_vec ();

  double *prwork = rwork.fortran_vec ();
  octave_idx_type *piwork = iwork.fortran_vec ();

  double *dummy = 0;
  octave_idx_type *idummy = 0;

  F77_XFCN (ddaspk, DDASPK, (ddaspk_f, nn, t, px, pxdot, tout, pinfo,
                             prel_tol, pabs_tol, istate, prwork, lrw,
                             piwork, liw, dummy, idummy, ddaspk_j,
                             ddaspk_psol));

  switch (istate)
    {
    case 1: // A step was successfully taken in intermediate-output
            // mode. The code has not yet reached TOUT.
    case 2: // The integration to TSTOP was successfully completed
            // (T=TSTOP) by stepping exactly to TSTOP.
    case 3: // The integration to TOUT was successfully completed
            // (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
            // interpolation.  YPRIME(*) is obtained by interpolation.
    case 4: // The initial condition calculation, with
            // INFO(11) > 0, was successful, and INFO(14) = 1.
            // No integration steps were taken, and the solution
            // is not considered to have been started.
      retval = x;
      t = tout;
      break;

    case -1: // A large amount of work has been expended.  (~500 steps).
    case -2: // The error tolerances are too stringent.
    case -3: // The local error test cannot be satisfied because you
             // specified a zero component in ATOL and the
             // corresponding computed solution component is zero.
             // Thus, a pure relative error test is impossible for
             // this component.
    case -6: // DDASPK had repeated error test failures on the last
             // attempted step.
    case -7: // The corrector could not converge.
    case -8: // The matrix of partial derivatives is singular.
    case -9: // The corrector could not converge.  There were repeated
             // error test failures in this step.
    case -10: // The corrector could not converge because IRES was
              // equal to minus one.
    case -11: // IRES equal to -2 was encountered and control is being
              // returned to the calling program.
    case -12: // DDASPK failed to compute the initial YPRIME.
    case -13: // Unrecoverable error encountered inside user's
              // PSOL routine, and control is being returned to
              // the calling program.
    case -14: // The Krylov linear system solver could not
              // achieve convergence.
    case -33: // The code has encountered trouble from which it cannot
              // recover. A message is printed explaining the trouble
              // and control is returned to the calling program. For
              // example, this occurs when invalid input is detected.
      integration_error = true;
      break;

    default:
      integration_error = true;
      (*current_liboctave_error_handler)
        ("unrecognized value of istate (= %d) returned from ddaspk",
         istate);
      break;
    }

  return retval;
}

Matrix
DASPK::do_integrate (const ColumnVector& tout)
{
  Matrix dummy;
  return integrate (tout, dummy);
}

Matrix
DASPK::integrate (const ColumnVector& tout, Matrix& xdot_out)
{
  Matrix retval;

  octave_idx_type n_out = tout.capacity ();
  octave_idx_type n = size ();

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (octave_idx_type i = 0; i < n; i++)
        {
          retval.elem (0, i) = x.elem (i);
          xdot_out.elem (0, i) = xdot.elem (i);
        }

      for (octave_idx_type j = 1; j < n_out; j++)
        {
          ColumnVector x_next = do_integrate (tout.elem (j));

          if (integration_error)
            return retval;

          for (octave_idx_type i = 0; i < n; i++)
            {
              retval.elem (j, i) = x_next.elem (i);
              xdot_out.elem (j, i) = xdot.elem (i);
            }
        }
    }

  return retval;
}

Matrix
DASPK::do_integrate (const ColumnVector& tout, const ColumnVector& tcrit)
{
  Matrix dummy;
  return integrate (tout, dummy, tcrit);
}

Matrix
DASPK::integrate (const ColumnVector& tout, Matrix& xdot_out,
                  const ColumnVector& tcrit)
{
  Matrix retval;

  octave_idx_type n_out = tout.capacity ();
  octave_idx_type n = size ();

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (octave_idx_type i = 0; i < n; i++)
        {
          retval.elem (0, i) = x.elem (i);
          xdot_out.elem (0, i) = xdot.elem (i);
        }

      octave_idx_type n_crit = tcrit.capacity ();

      if (n_crit > 0)
        {
          octave_idx_type i_crit = 0;
          octave_idx_type i_out = 1;
          double next_crit = tcrit.elem (0);
          double next_out;
          while (i_out < n_out)
            {
              bool do_restart = false;

              next_out = tout.elem (i_out);
              if (i_crit < n_crit)
                next_crit = tcrit.elem (i_crit);

              bool save_output;
              double t_out;

              if (next_crit == next_out)
                {
                  set_stop_time (next_crit);
                  t_out = next_out;
                  save_output = true;
                  i_out++;
                  i_crit++;
                  do_restart = true;
                }
              else if (next_crit < next_out)
                {
                  if (i_crit < n_crit)
                    {
                      set_stop_time (next_crit);
                      t_out = next_crit;
                      save_output = false;
                      i_crit++;
                      do_restart = true;
                    }
                  else
                    {
                      clear_stop_time ();
                      t_out = next_out;
                      save_output = true;
                      i_out++;
                    }
                }
              else
                {
                  set_stop_time (next_crit);
                  t_out = next_out;
                  save_output = true;
                  i_out++;
                }

              ColumnVector x_next = do_integrate (t_out);

              if (integration_error)
                return retval;

              if (save_output)
                {
                  for (octave_idx_type i = 0; i < n; i++)
                    {
                      retval.elem (i_out-1, i) = x_next.elem (i);
                      xdot_out.elem (i_out-1, i) = xdot.elem (i);
                    }
                }

              if (do_restart)
                force_restart ();
            }
        }
      else
        {
          retval = integrate (tout, xdot_out);

          if (integration_error)
            return retval;
        }
    }

  return retval;
}

std::string
DASPK::error_message (void) const
{
  std::string retval;

  std::ostringstream buf;
  buf << t;
  std::string t_curr = buf.str ();

  switch (istate)
    {
    case 1:
      retval = "a step was successfully taken in intermediate-output mode.";
      break;

    case 2:
      retval = "integration completed by stepping exactly to TOUT";
      break;

    case 3:
      retval = "integration to tout completed by stepping past TOUT";
      break;

    case 4:
      retval = "initial condition calculation completed successfully";
      break;

    case -1:
      retval = std::string ("a large amount of work has been expended (t =")
               + t_curr + ")";
      break;

    case -2:
      retval = "the error tolerances are too stringent";
      break;

    case -3:
      retval = std::string ("error weight became zero during problem. (t = ")
               + t_curr
               + "; solution component i vanished, and atol or atol(i) == 0)";
      break;

    case -6:
      retval = std::string ("repeated error test failures on the last attempted step (t = ")
               + t_curr + ")";
      break;

    case -7:
      retval = std::string ("the corrector could not converge (t = ")
               + t_curr + ")";
      break;

    case -8:
      retval = std::string ("the matrix of partial derivatives is singular (t = ")
               + t_curr + ")";
      break;

    case -9:
      retval = std::string ("the corrector could not converge (t = ")
               + t_curr + "; repeated test failures)";
      break;

    case -10:
      retval = std::string ("corrector could not converge because IRES was -1 (t = ")
               + t_curr + ")";
      break;

    case -11:
      retval = std::string ("return requested in user-supplied function (t = ")
               + t_curr + ")";
      break;

    case -12:
      retval = "failed to compute consistent initial conditions";
      break;

    case -13:
      retval = std::string ("unrecoverable error encountered inside user's PSOL function (t = ")
               + t_curr + ")";
      break;

    case -14:
      retval = std::string ("the Krylov linear system solver failed to converge (t = ")
               + t_curr + ")";
      break;

    case -33:
      retval = "unrecoverable error (see printed message)";
      break;

    default:
      retval = "unknown error state";
      break;
    }

  return retval;
}
