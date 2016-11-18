/*

Copyright (C) 2002-2015 John W. Eaton

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

#include "DASRT.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-math.h"
#include "quit.h"

typedef octave_idx_type (*dasrt_fcn_ptr) (const double&, const double*,
                                          const double*, double*,
                                          octave_idx_type&, double*,
                                          octave_idx_type*);

typedef octave_idx_type (*dasrt_jac_ptr) (const double&, const double*,
                                          const double*, double*,
                                          const double&, double*,
                                          octave_idx_type*);

typedef octave_idx_type (*dasrt_constr_ptr) (const octave_idx_type&,
                                             const double&, const double*,
                                             const octave_idx_type&,
                                             double*, double*,
                                             octave_idx_type*);

extern "C"
{
  F77_RET_T
  F77_FUNC (ddasrt, DDASRT) (dasrt_fcn_ptr, const octave_idx_type&,
                             double&, double*, double*, const double&,
                             octave_idx_type*, const double*,
                             const double*, octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             const octave_idx_type&, double*,
                             octave_idx_type*, dasrt_jac_ptr,
                             dasrt_constr_ptr, const octave_idx_type&,
                             octave_idx_type*);
}

static DAEFunc::DAERHSFunc user_fsub;
static DAEFunc::DAEJacFunc user_jsub;
static DAERTFunc::DAERTConstrFunc user_csub;

static octave_idx_type nn;

static octave_idx_type
ddasrt_f (const double& t, const double *state, const double *deriv,
          double *delta, octave_idx_type& ires, double *, octave_idx_type *)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  ColumnVector tmp_state (nn);
  ColumnVector tmp_deriv (nn);

  for (octave_idx_type i = 0; i < nn; i++)
    {
      tmp_state(i) = state[i];
      tmp_deriv(i) = deriv[i];
    }

  ColumnVector tmp_fval = (*user_fsub) (tmp_state, tmp_deriv, t, ires);

  if (tmp_fval.length () == 0)
    ires = -2;
  else
    {
      for (octave_idx_type i = 0; i < nn; i++)
        delta[i] = tmp_fval(i);
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

octave_idx_type
ddasrt_j (const double& time, const double *state, const double *deriv,
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

  Matrix tmp_pd = (*user_jsub) (tmp_state, tmp_deriv, time, cj);

  for (octave_idx_type j = 0; j < nn; j++)
    for (octave_idx_type i = 0; i < nn; i++)
      pd[nn * j + i] = tmp_pd.elem (i, j);

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

static octave_idx_type
ddasrt_g (const octave_idx_type& neq, const double& t, const double *state,
          const octave_idx_type& ng, double *gout, double *, octave_idx_type *)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  octave_idx_type n = neq;

  ColumnVector tmp_state (n);
  for (octave_idx_type i = 0; i < n; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_fval = (*user_csub) (tmp_state, t);

  for (octave_idx_type i = 0; i < ng; i++)
    gout[i] = tmp_fval(i);

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

void
DASRT::integrate (double tout)
{
  DASRT_result retval;

  // I suppose this is the safe thing to do.  If this is the first
  // call, or if anything about the problem has changed, we should
  // start completely fresh.

  if (! initialized || restart
      || DAEFunc::reset || DAERTFunc::reset || DASRT_options::reset)
    {
      integration_error = false;

      initialized = true;

      info.resize (dim_vector (15, 1));

      for (octave_idx_type i = 0; i < 15; i++)
        info(i) = 0;

      octave_idx_type n = size ();

      nn = n;

      // DAERTFunc

      user_csub = DAERTFunc::constraint_function ();

      if (user_csub)
        {
          ColumnVector tmp = (*user_csub) (x, t);
          ng = tmp.length ();
        }
      else
        ng = 0;

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
                ("dassl: invalid value for maximum order");
              integration_error = true;
              return;
            }
        }

      liw = 21 + n;
      lrw = 50 + 9*n + n*n + 3*ng;

      iwork.resize (dim_vector (liw, 1));
      rwork.resize (dim_vector (lrw, 1));

      info(0) = 0;

      if (stop_time_set)
        {
          info(3) = 1;
          rwork(0) = stop_time;
        }
      else
        info(3) = 0;

      restart = false;

      // DAEFunc

      user_fsub = DAEFunc::function ();
      user_jsub = DAEFunc::jacobian_function ();

      if (user_fsub)
        {
          octave_idx_type ires = 0;

          ColumnVector fval = (*user_fsub) (x, xdot, t, ires);

          if (fval.length () != x.length ())
            {
              (*current_liboctave_error_handler)
                ("dasrt: inconsistent sizes for state and residual vectors");

              integration_error = true;
              return;
            }
        }
      else
        {
          (*current_liboctave_error_handler)
            ("dasrt: no user supplied RHS subroutine!");

          integration_error = true;
          return;
        }

      info(4) = user_jsub ? 1 : 0;

      DAEFunc::reset = false;

      jroot.resize (dim_vector (ng, 1), 1);

      DAERTFunc::reset = false;

      // DASRT_options

      double mss = maximum_step_size ();
      if (mss >= 0.0)
        {
          rwork(1) = mss;
          info(6) = 1;
        }
      else
        info(6) = 0;

      double iss = initial_step_size ();
      if (iss >= 0.0)
        {
          rwork(2) = iss;
          info(7) = 1;
        }
      else
        info(7) = 0;

      if (step_limit () >= 0)
        {
          info(11) = 1;
          iwork(20) = step_limit ();
        }
      else
        info(11) = 0;

      abs_tol = absolute_tolerance ();
      rel_tol = relative_tolerance ();

      octave_idx_type abs_tol_len = abs_tol.length ();
      octave_idx_type rel_tol_len = rel_tol.length ();

      if (abs_tol_len == 1 && rel_tol_len == 1)
        {
          info.elem (1) = 0;
        }
      else if (abs_tol_len == n && rel_tol_len == n)
        {
          info.elem (1) = 1;
        }
      else
        {
          (*current_liboctave_error_handler)
            ("dasrt: inconsistent sizes for tolerance arrays");

          integration_error = true;
          return;
        }

      DASRT_options::reset = false;
    }

  double *px = x.fortran_vec ();
  double *pxdot = xdot.fortran_vec ();

  octave_idx_type *pinfo = info.fortran_vec ();

  double *prel_tol = rel_tol.fortran_vec ();
  double *pabs_tol = abs_tol.fortran_vec ();

  double *prwork = rwork.fortran_vec ();
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type *pjroot = jroot.fortran_vec ();

  double *dummy = 0;
  octave_idx_type *idummy = 0;

  F77_XFCN (ddasrt, DDASRT, (ddasrt_f, nn, t, px, pxdot, tout, pinfo,
                             prel_tol, pabs_tol, istate, prwork, lrw,
                             piwork, liw, dummy, idummy, ddasrt_j,
                             ddasrt_g, ng, pjroot));

  switch (istate)
    {
    case 1: // A step was successfully taken in intermediate-output
            // mode. The code has not yet reached TOUT.
    case 2: // The integration to TOUT was successfully completed
            // (T=TOUT) by stepping exactly to TOUT.
    case 3: // The integration to TOUT was successfully completed
            // (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
            // interpolation.  YPRIME(*) is obtained by interpolation.
      t = tout;
      break;

    case 4: //  The integration was successfully completed
            // by finding one or more roots of G at T.
      break;

    case -1: // A large amount of work has been expended.
    case -2: // The error tolerances are too stringent.
    case -3: // The local error test cannot be satisfied because you
             // specified a zero component in ATOL and the
             // corresponding computed solution component is zero.
             // Thus, a pure relative error test is impossible for
             // this component.
    case -6: // DDASRT had repeated error test failures on the last
             // attempted step.
    case -7: // The corrector could not converge.
    case -8: // The matrix of partial derivatives is singular.
    case -9: // The corrector could not converge.  There were repeated
             // error test failures in this step.
    case -10: // The corrector could not converge because IRES was
              // equal to minus one.
    case -11: // IRES equal to -2 was encountered and control is being
              // returned to the calling program.
    case -12: // DASSL failed to compute the initial YPRIME.
    case -33: // The code has encountered trouble from which it cannot
              // recover. A message is printed explaining the trouble
              // and control is returned to the calling program. For
              // example, this occurs when invalid input is detected.
      integration_error = true;
      break;

    default:
      integration_error = true;
      (*current_liboctave_error_handler)
        ("unrecognized value of istate (= %d) returned from ddasrt",
         istate);
      break;
    }
}

DASRT_result
DASRT::integrate (const ColumnVector& tout)
{
  DASRT_result retval;

  Matrix x_out;
  Matrix xdot_out;
  ColumnVector t_out = tout;

  octave_idx_type n_out = tout.capacity ();
  octave_idx_type n = size ();

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (octave_idx_type i = 0; i < n; i++)
        {
          x_out(0,i) = x(i);
          xdot_out(0,i) = xdot(i);
        }

      for (octave_idx_type j = 1; j < n_out; j++)
        {
          integrate (tout(j));

          if (integration_error)
            {
              retval = DASRT_result (x_out, xdot_out, t_out);
              return retval;
            }

          if (istate == 4)
            t_out(j) = t;
          else
            t_out(j) = tout(j);

          for (octave_idx_type i = 0; i < n; i++)
            {
              x_out(j,i) = x(i);
              xdot_out(j,i) = xdot(i);
            }

          if (istate == 4)
            {
              x_out.resize (j+1, n);
              xdot_out.resize (j+1, n);
              t_out.resize (j+1);
              break;
            }
        }
    }

  retval = DASRT_result (x_out, xdot_out, t_out);

  return retval;
}

DASRT_result
DASRT::integrate (const ColumnVector& tout, const ColumnVector& tcrit)
{
  DASRT_result retval;

  Matrix x_out;
  Matrix xdot_out;
  ColumnVector t_outs = tout;

  octave_idx_type n_out = tout.capacity ();
  octave_idx_type n = size ();

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);
      xdot_out.resize (n_out, n);

      octave_idx_type n_crit = tcrit.capacity ();

      if (n_crit > 0)
        {
          octave_idx_type i_crit = 0;
          octave_idx_type i_out = 1;
          double next_crit = tcrit(0);
          double next_out;
          while (i_out < n_out)
            {
              bool do_restart = false;

              next_out = tout(i_out);
              if (i_crit < n_crit)
                next_crit = tcrit(i_crit);

              octave_idx_type save_output;
              double t_out;

              if (next_crit == next_out)
                {
                  set_stop_time (next_crit);
                  t_out = next_out;
                  save_output = 1;
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
                      save_output = 0;
                      i_crit++;
                      do_restart = true;
                    }
                  else
                    {
                      clear_stop_time ();
                      t_out = next_out;
                      save_output = 1;
                      i_out++;
                    }
                }
              else
                {
                  set_stop_time (next_crit);
                  t_out = next_out;
                  save_output = 1;
                  i_out++;
                }

              integrate (t_out);

              if (integration_error)
                {
                  retval = DASRT_result (x_out, xdot_out, t_outs);
                  return retval;
                }

              if (istate == 4)
                t_out = t;

              if (save_output)
                {
                  for (octave_idx_type i = 0; i < n; i++)
                    {
                      x_out(i_out-1,i) = x(i);
                      xdot_out(i_out-1,i) = xdot(i);
                    }

                  t_outs(i_out-1) = t_out;

                  if (istate == 4)
                    {
                      x_out.resize (i_out, n);
                      xdot_out.resize (i_out, n);
                      t_outs.resize (i_out);
                      i_out = n_out;
                    }
                }

              if (do_restart)
                force_restart ();
            }

          retval = DASRT_result (x_out, xdot_out, t_outs);
        }
      else
        {
          retval = integrate (tout);

          if (integration_error)
            return retval;
        }
    }

  return retval;
}

std::string
DASRT::error_message (void) const
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
      retval = "integration completed by finding one or more roots of G at T";
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

    case -33:
      retval = "unrecoverable error (see printed message)";
      break;

    default:
      retval = "unknown error state";
      break;
    }

  return retval;
}
