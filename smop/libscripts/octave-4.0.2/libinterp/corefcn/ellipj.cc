/*

Copyright (C) 2013-2015 Leopoldo Cerbaro <redbliss@libero.it>

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
#include "lo-specfun.h"

static void
gripe_ellipj_arg (const char *arg)
{
  error ("ellipj: expecting scalar or matrix as %s argument", arg);
}

DEFUN (ellipj, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{sn}, @var{cn}, @var{dn}, @var{err}] =} ellipj (@var{u}, @var{m})\n\
@deftypefnx {Built-in Function} {[@var{sn}, @var{cn}, @var{dn}, @var{err}] =} ellipj (@var{u}, @var{m}, @var{tol})\n\
Compute the Jacobi elliptic functions @var{sn}, @var{cn}, and @var{dn}\n\
of complex argument @var{u} and real parameter @var{m}.\n\
\n\
If @var{m} is a scalar, the results are the same size as @var{u}.\n\
If @var{u} is a scalar, the results are the same size as @var{m}.\n\
If @var{u} is a column vector and @var{m} is a row vector, the\n\
results are matrices with @code{length (@var{u})} rows and\n\
@code{length (@var{m})} columns.  Otherwise, @var{u} and\n\
@var{m} must conform in size and the results will be the same size as the\n\
inputs.\n\
\n\
The value of @var{u} may be complex.\n\
The value of @var{m} must be 0 @leq{} @var{m} @leq{} 1.\n\
\n\
The optional input @var{tol} is currently ignored (@sc{matlab} uses this to\n\
allow faster, less accurate approximation).\n\
\n\
If requested, @var{err} contains the following status information\n\
and is the same size as the result.\n\
\n\
@enumerate 0\n\
@item\n\
Normal return.\n\
\n\
@item\n\
Error---no computation, algorithm termination condition not met,\n\
return @code{NaN}.\n\
@end enumerate\n\
\n\
Reference: Milton @nospell{Abramowitz} and Irene A @nospell{Stegun},\n\
@cite{Handbook of Mathematical Functions}, Chapter 16 (Sections 16.4, 16.13,\n\
and 16.15), Dover, 1965.\n\
\n\
@seealso{ellipke}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value u_arg = args(0);
  octave_value m_arg = args(1);

  if (m_arg.is_scalar_type ())
    {
      double m = args(1).double_value ();

      if (error_state)
        {
          gripe_ellipj_arg ("second");
          return retval;
        }

      if (u_arg.is_scalar_type ())
        {
          if (u_arg.is_real_type ())
            {
              // u real, m scalar
              double u = args(0).double_value ();

              if (error_state)
                {
                  gripe_ellipj_arg ("first");
                  return retval;
                }

              double sn, cn, dn;
              double err = 0;

              ellipj (u, m, sn, cn, dn, err);

              if (nargout > 3)
                retval(3) = err;
              retval(2) = dn;
              retval(1) = cn;
              retval(0) = sn;
            }
          else
            {
              // u complex, m scalar
              Complex u = u_arg.complex_value ();

              if (error_state)
                {
                  gripe_ellipj_arg ("first");
                  return retval;
                }

              Complex sn, cn, dn;
              double err = 0;

              ellipj (u, m, sn, cn, dn, err);

              if (nargout > 3)
                retval(3) = err;
              retval(2) = dn;
              retval(1) = cn;
              retval(0) = sn;
            }
        }
      else
        {
          // u is matrix, m is scalar
          ComplexNDArray u = u_arg.complex_array_value ();

          if (error_state)
            {
              gripe_ellipj_arg ("first");
              return retval;
            }

          dim_vector sz_u = u.dims ();

          ComplexNDArray sn (sz_u), cn (sz_u), dn (sz_u);
          NDArray err (sz_u);

          const Complex *pu = u.data ();
          Complex *psn = sn.fortran_vec ();
          Complex *pcn = cn.fortran_vec ();
          Complex *pdn = dn.fortran_vec ();
          double *perr = err.fortran_vec ();
          octave_idx_type nel = u.numel ();

          for (octave_idx_type i = 0; i < nel; i++)
            ellipj (pu[i], m, psn[i], pcn[i], pdn[i], perr[i]);

          if (nargout > 3)
            retval(3) = err;
          retval(2) = dn;
          retval(1) = cn;
          retval(0) = sn;
        }
    }
  else
    {
      NDArray m = args(1).array_value ();

      if (error_state)
        {
          gripe_ellipj_arg ("second");
          return retval;
        }

      dim_vector sz_m = m.dims ();

      if (u_arg.is_scalar_type ())
        {
          // u is scalar, m is array
          if (u_arg.is_real_type ())
            {
              // u is real scalar, m is array
              double u = u_arg.double_value ();

              if (error_state)
                {
                  gripe_ellipj_arg ("first");
                  return retval;
                }

              NDArray sn (sz_m), cn (sz_m), dn (sz_m);
              NDArray err (sz_m);

              const double *pm = m.data ();
              double *psn = sn.fortran_vec ();
              double *pcn = cn.fortran_vec ();
              double *pdn = dn.fortran_vec ();
              double *perr = err.fortran_vec ();
              octave_idx_type nel = m.numel ();

              for (octave_idx_type i = 0; i < nel; i++)
                ellipj (u, pm[i], psn[i], pcn[i], pdn[i], perr[i]);

              if (nargout > 3)
                retval(3) = err;
              retval(2) = dn;
              retval(1) = cn;
              retval(0) = sn;
            }
          else
            {
              // u is complex scalar, m is array
              Complex u = u_arg.complex_value ();

              if (error_state)
                {
                  gripe_ellipj_arg ("first");
                  return retval;
                }

              ComplexNDArray sn (sz_m), cn (sz_m), dn (sz_m);
              NDArray err (sz_m);

              const double *pm = m.data ();
              Complex *psn = sn.fortran_vec ();
              Complex *pcn = cn.fortran_vec ();
              Complex *pdn = dn.fortran_vec ();
              double *perr = err.fortran_vec ();
              octave_idx_type nel = m.numel ();

              for (octave_idx_type i = 0; i < nel; i++)
                ellipj (u, pm[i], psn[i], pcn[i], pdn[i], perr[i]);

              if (nargout > 3)
                retval(3) = err;
              retval(2) = dn;
              retval(1) = cn;
              retval(0) = sn;
            }
        }
      else
        {
          // u is array, m is array
          if (u_arg.is_real_type ())
            {
              // u is real array, m is array
              NDArray u = u_arg.array_value ();

              if (error_state)
                {
                  gripe_ellipj_arg ("first");
                  return retval;
                }

              dim_vector sz_u = u.dims ();

              if (sz_u.length () == 2 && sz_m.length () == 2
                  && sz_u(1) == 1 && sz_m(0) == 1)
                {
                  // u is real column vector, m is row vector
                  octave_idx_type ur = sz_u(0);
                  octave_idx_type mc = sz_m(1);
                  dim_vector sz_out (ur, mc);

                  NDArray sn (sz_out), cn (sz_out), dn (sz_out);
                  NDArray err (sz_out);

                  const double *pu = u.data ();
                  const double *pm = m.data ();

                  for (octave_idx_type j = 0; j < mc; j++)
                    for (octave_idx_type i = 0; i < ur; i++)
                      ellipj (pu[i], pm[j], sn(i,j), cn(i,j), dn(i,j),
                              err(i,j));

                  if (nargout > 3)
                    retval(3) = err;
                  retval(2) = dn;
                  retval(1) = cn;
                  retval(0) = sn;
                }
              else if (sz_m == sz_u)
                {
                  NDArray sn (sz_m), cn (sz_m), dn (sz_m);
                  NDArray err (sz_m);

                  const double *pu = u.data ();
                  const double *pm = m.data ();
                  double *psn = sn.fortran_vec ();
                  double *pcn = cn.fortran_vec ();
                  double *pdn = dn.fortran_vec ();
                  double *perr = err.fortran_vec ();
                  octave_idx_type nel = m.numel ();

                  for (octave_idx_type i = 0; i < nel; i++)
                    ellipj (pu[i], pm[i], psn[i], pcn[i], pdn[i], perr[i]);

                  if (nargout > 3)
                    retval(3) = err;
                  retval(2) = dn;
                  retval(1) = cn;
                  retval(0) = sn;
                }
              else
                error ("ellipj: Invalid size combination for U and M");
            }
          else
            {
              // u is complex array, m is array
              ComplexNDArray u = u_arg.complex_array_value ();

              if (error_state)
                {
                  gripe_ellipj_arg ("second");
                  return retval;
                }

              dim_vector sz_u = u.dims ();

              if (sz_u.length () == 2 && sz_m.length () == 2
                  && sz_u(1) == 1 && sz_m(0) == 1)
                {
                  // u is complex column vector, m is row vector
                  octave_idx_type ur = sz_u(0);
                  octave_idx_type mc = sz_m(1);
                  dim_vector sz_out (ur, mc);

                  ComplexNDArray sn (sz_out), cn (sz_out), dn (sz_out);
                  NDArray err (sz_out);

                  const Complex *pu = u.data ();
                  const double  *pm = m.data ();

                  for (octave_idx_type j = 0; j < mc; j++)
                    for (octave_idx_type i = 0; i < ur; i++)
                      ellipj (pu[i], pm[j], sn(i,j), cn(i,j), dn(i,j),
                              err(i,j));

                  if (nargout > 3)
                    retval(3) = err;
                  retval(2) = dn;
                  retval(1) = cn;
                  retval(0) = sn;
                }
              else if (sz_m == sz_u)
                {
                  ComplexNDArray sn (sz_m), cn (sz_m), dn (sz_m);
                  NDArray err (sz_m);

                  const Complex *pu = u.data ();
                  const double  *pm = m.data ();
                  Complex *psn = sn.fortran_vec ();
                  Complex *pcn = cn.fortran_vec ();
                  Complex *pdn = dn.fortran_vec ();
                  double *perr = err.fortran_vec ();
                  octave_idx_type nel = m.numel ();

                  for (octave_idx_type i = 0; i < nel; i++)
                    ellipj (pu[i], pm[i], psn[i], pcn[i], pdn[i], perr[i]);

                  if (nargout > 3)
                    retval(3) = err;
                  retval(2) = dn;
                  retval(1) = cn;
                  retval(0) = sn;
                }
              else
                error ("ellipj: Invalid size combination for U and M");
            }
        }
    }  // m matrix

  return retval;
}

/*
## demos taken from inst/ellipj.m

%!demo
%! N = 150;
%! # m = [1-logspace(0,log(eps),N-1), 1]; # m near 1
%! # m = [0, logspace(log(eps),0,N-1)];   # m near 0
%!   m = linspace (0,1,N);                # m equally spaced
%! u = linspace (-20, 20, N);
%! M = ones (length (u), 1) * m;
%! U = u' * ones (1, length (m));
%! [sn, cn, dn] = ellipj (U,M);
%!
%! ## Plotting
%! data = {sn,cn,dn};
%! dname = {"sn","cn","dn"};
%! for i=1:3
%!   subplot (1,3,i);
%!   data{i}(data{i} > 1) = 1;
%!   data{i}(data{i} < -1) = -1;
%!   image (m,u,32*data{i}+32);
%!   title (dname{i});
%! endfor
%! colormap (hot (64));

%!demo
%! N = 200;
%! # m = [1-logspace(0,log(eps),N-1), 1]; # m near 1
%! # m = [0, logspace(log(eps),0,N-1)];   # m near 0
%!   m = linspace (0,1,N);                # m equally spaced
%! u = linspace (0,20,5);
%! M = ones (length (u), 1) * m;
%! U = u' * ones (1, length (m));
%! [sn, cn, dn] = ellipj (U,M);
%!
%! ## Plotting
%! data = {sn,cn,dn};
%! dname = {"sn","cn","dn"};
%! for i=1:3
%!   subplot (1,3,i);
%!   plot (m, data{i});
%!   title (dname{i});
%!   grid on;
%! endfor
*/

/*
## tests taken from inst/test_sncndn.m

%!test
%! k = (tan(pi/8.))^2;  m = k*k;
%! SN = [
%! -1. + I * 0. ,  -0.8392965923 + 0. * I
%! -1. + I * 0.2 ,  -0.8559363407 + 0.108250955 * I
%! -1. + I * 0.4 ,  -0.906529758 + 0.2204040232 * I
%! -1. + I * 0.6 ,  -0.9931306727 + 0.3403783409 * I
%! -1. + I * 0.8 ,  -1.119268095 + 0.4720784944 * I
%! -1. + I * 1. ,  -1.29010951 + 0.6192468708 * I
%! -1. + I * 1.2 ,  -1.512691987 + 0.7850890595 * I
%! -1. + I * 1.4 ,  -1.796200374 + 0.9714821804 * I
%! -1. + I * 1.6 ,  -2.152201882 + 1.177446413 * I
%! -1. + I * 1.8 ,  -2.594547417 + 1.396378892 * I
%! -1. + I * 2. ,  -3.138145339 + 1.611394819 * I
%! -0.8 + I * 0. ,  -0.7158157937 + 0. * I
%! -0.8 + I * 0.2 ,  -0.7301746722 + 0.1394690862 * I
%! -0.8 + I * 0.4 ,  -0.7738940898 + 0.2841710966 * I
%! -0.8 + I * 0.6 ,  -0.8489542135 + 0.4394411376 * I
%! -0.8 + I * 0.8 ,  -0.9588386397 + 0.6107824358 * I
%! -0.8 + I * 1. ,  -1.108848724 + 0.8038415767 * I
%! -0.8 + I * 1.2 ,  -1.306629972 + 1.024193359 * I
%! -0.8 + I * 1.4 ,  -1.563010199 + 1.276740951 * I
%! -0.8 + I * 1.6 ,  -1.893274688 + 1.564345558 * I
%! -0.8 + I * 1.8 ,  -2.318944084 + 1.88491973 * I
%! -0.8 + I * 2. ,  -2.869716809 + 2.225506523 * I
%! -0.6 + I * 0. ,  -0.5638287208 + 0. * I
%! -0.6 + I * 0.2 ,  -0.5752723012 + 0.1654722474 * I
%! -0.6 + I * 0.4 ,  -0.610164314 + 0.3374004736 * I
%! -0.6 + I * 0.6 ,  -0.6702507087 + 0.5224614298 * I
%! -0.6 + I * 0.8 ,  -0.7586657365 + 0.7277663879 * I
%! -0.6 + I * 1. ,  -0.8803349115 + 0.9610513652 * I
%! -0.6 + I * 1.2 ,  -1.042696526 + 1.230800819 * I
%! -0.6 + I * 1.4 ,  -1.256964505 + 1.546195843 * I
%! -0.6 + I * 1.6 ,  -1.540333527 + 1.916612621 * I
%! -0.6 + I * 1.8 ,  -1.919816065 + 2.349972151 * I
%! -0.6 + I * 2. ,  -2.438761841 + 2.848129496 * I
%! -0.4 + I * 0. ,  -0.3891382858 + 0. * I
%! -0.4 + I * 0.2 ,  -0.3971152026 + 0.1850563793 * I
%! -0.4 + I * 0.4 ,  -0.4214662882 + 0.3775700801 * I
%! -0.4 + I * 0.6 ,  -0.4635087491 + 0.5853434119 * I
%! -0.4 + I * 0.8 ,  -0.5256432877 + 0.8168992398 * I
%! -0.4 + I * 1. ,  -0.611733177 + 1.081923504 * I
%! -0.4 + I * 1.2 ,  -0.7278102331 + 1.391822501 * I
%! -0.4 + I * 1.4 ,  -0.8833807998 + 1.760456461 * I
%! -0.4 + I * 1.6 ,  -1.093891878 + 2.205107766 * I
%! -0.4 + I * 1.8 ,  -1.385545188 + 2.747638761 * I
%! -0.4 + I * 2. ,  -1.805081271 + 3.41525351 * I
%! -0.2 + I * 0. ,  -0.1986311721 + 0. * I
%! -0.2 + I * 0.2 ,  -0.2027299916 + 0.1972398665 * I
%! -0.2 + I * 0.4 ,  -0.2152524522 + 0.402598347 * I
%! -0.2 + I * 0.6 ,  -0.2369100139 + 0.6246336356 * I
%! -0.2 + I * 0.8 ,  -0.2690115146 + 0.8728455227 * I
%! -0.2 + I * 1. ,  -0.3136938773 + 1.158323088 * I
%! -0.2 + I * 1.2 ,  -0.3743615191 + 1.494672508 * I
%! -0.2 + I * 1.4 ,  -0.4565255082 + 1.899466033 * I
%! -0.2 + I * 1.6 ,  -0.5694611346 + 2.39667232 * I
%! -0.2 + I * 1.8 ,  -0.7296612675 + 3.020990664 * I
%! -0.2 + I * 2. ,  -0.9685726188 + 3.826022536 * I
%! 0. + I * 0. ,  0. + 0. * I
%! 0. + I * 0.2 ,  0. + 0.201376364 * I
%! 0. + I * 0.4 ,  0. + 0.4111029248 * I
%! 0. + I * 0.6 ,  0. + 0.6380048435 * I
%! 0. + I * 0.8 ,  0. + 0.8919321473 * I
%! 0. + I * 1. ,  0. + 1.184486615 * I
%! 0. + I * 1.2 ,  0. + 1.530096023 * I
%! 0. + I * 1.4 ,  0. + 1.947754612 * I
%! 0. + I * 1.6 ,  0. + 2.464074356 * I
%! 0. + I * 1.8 ,  0. + 3.119049475 * I
%! 0. + I * 2. ,  0. + 3.97786237 * I
%! 0.2 + I * 0. ,  0.1986311721 + 0. * I
%! 0.2 + I * 0.2 ,  0.2027299916 + 0.1972398665 * I
%! 0.2 + I * 0.4 ,  0.2152524522 + 0.402598347 * I
%! 0.2 + I * 0.6 ,  0.2369100139 + 0.6246336356 * I
%! 0.2 + I * 0.8 ,  0.2690115146 + 0.8728455227 * I
%! 0.2 + I * 1. ,  0.3136938773 + 1.158323088 * I
%! 0.2 + I * 1.2 ,  0.3743615191 + 1.494672508 * I
%! 0.2 + I * 1.4 ,  0.4565255082 + 1.899466033 * I
%! 0.2 + I * 1.6 ,  0.5694611346 + 2.39667232 * I
%! 0.2 + I * 1.8 ,  0.7296612675 + 3.020990664 * I
%! 0.2 + I * 2. ,  0.9685726188 + 3.826022536 * I
%! 0.4 + I * 0. ,  0.3891382858 + 0. * I
%! 0.4 + I * 0.2 ,  0.3971152026 + 0.1850563793 * I
%! 0.4 + I * 0.4 ,  0.4214662882 + 0.3775700801 * I
%! 0.4 + I * 0.6 ,  0.4635087491 + 0.5853434119 * I
%! 0.4 + I * 0.8 ,  0.5256432877 + 0.8168992398 * I
%! 0.4 + I * 1. ,  0.611733177 + 1.081923504 * I
%! 0.4 + I * 1.2 ,  0.7278102331 + 1.391822501 * I
%! 0.4 + I * 1.4 ,  0.8833807998 + 1.760456461 * I
%! 0.4 + I * 1.6 ,  1.093891878 + 2.205107766 * I
%! 0.4 + I * 1.8 ,  1.385545188 + 2.747638761 * I
%! 0.4 + I * 2. ,  1.805081271 + 3.41525351 * I
%! 0.6 + I * 0. ,  0.5638287208 + 0. * I
%! 0.6 + I * 0.2 ,  0.5752723012 + 0.1654722474 * I
%! 0.6 + I * 0.4 ,  0.610164314 + 0.3374004736 * I
%! 0.6 + I * 0.6 ,  0.6702507087 + 0.5224614298 * I
%! 0.6 + I * 0.8 ,  0.7586657365 + 0.7277663879 * I
%! 0.6 + I * 1. ,  0.8803349115 + 0.9610513652 * I
%! 0.6 + I * 1.2 ,  1.042696526 + 1.230800819 * I
%! 0.6 + I * 1.4 ,  1.256964505 + 1.546195843 * I
%! 0.6 + I * 1.6 ,  1.540333527 + 1.916612621 * I
%! 0.6 + I * 1.8 ,  1.919816065 + 2.349972151 * I
%! 0.6 + I * 2. ,  2.438761841 + 2.848129496 * I
%! 0.8 + I * 0. ,  0.7158157937 + 0. * I
%! 0.8 + I * 0.2 ,  0.7301746722 + 0.1394690862 * I
%! 0.8 + I * 0.4 ,  0.7738940898 + 0.2841710966 * I
%! 0.8 + I * 0.6 ,  0.8489542135 + 0.4394411376 * I
%! 0.8 + I * 0.8 ,  0.9588386397 + 0.6107824358 * I
%! 0.8 + I * 1. ,  1.108848724 + 0.8038415767 * I
%! 0.8 + I * 1.2 ,  1.306629972 + 1.024193359 * I
%! 0.8 + I * 1.4 ,  1.563010199 + 1.276740951 * I
%! 0.8 + I * 1.6 ,  1.893274688 + 1.564345558 * I
%! 0.8 + I * 1.8 ,  2.318944084 + 1.88491973 * I
%! 0.8 + I * 2. ,  2.869716809 + 2.225506523 * I
%! 1. + I * 0. ,  0.8392965923 + 0. * I
%! 1. + I * 0.2 ,  0.8559363407 + 0.108250955 * I
%! 1. + I * 0.4 ,  0.906529758 + 0.2204040232 * I
%! 1. + I * 0.6 ,  0.9931306727 + 0.3403783409 * I
%! 1. + I * 0.8 ,  1.119268095 + 0.4720784944 * I
%! 1. + I * 1. ,  1.29010951 + 0.6192468708 * I
%! 1. + I * 1.2 ,  1.512691987 + 0.7850890595 * I
%! 1. + I * 1.4 ,  1.796200374 + 0.9714821804 * I
%! 1. + I * 1.6 ,  2.152201882 + 1.177446413 * I
%! 1. + I * 1.8 ,  2.594547417 + 1.396378892 * I
%! 1. + I * 2. ,  3.138145339 + 1.611394819 * I
%! ];
%! CN = [
%! -1. + I * 0. , 0.5436738271 + 0. * I
%! -1. + I * 0.2 , 0.5541219664 + 0.1672121517 * I
%! -1. + I * 0.4 , 0.5857703552 + 0.3410940893 * I
%! -1. + I * 0.6 , 0.6395034233 + 0.5285979063 * I
%! -1. + I * 0.8 , 0.716688504 + 0.7372552987 * I
%! -1. + I * 1. , 0.8189576795 + 0.9755037374 * I
%! -1. + I * 1.2 , 0.9477661951 + 1.253049471 * I
%! -1. + I * 1.4 , 1.103540657 + 1.581252712 * I
%! -1. + I * 1.6 , 1.284098214 + 1.973449038 * I
%! -1. + I * 1.8 , 1.481835651 + 2.4449211 * I
%! -1. + I * 2. , 1.679032464 + 3.011729224 * I
%! -0.8 + I * 0. , 0.6982891589 + 0. * I
%! -0.8 + I * 0.2 , 0.71187169 + 0.1430549855 * I
%! -0.8 + I * 0.4 , 0.7530744458 + 0.2920273465 * I
%! -0.8 + I * 0.6 , 0.8232501212 + 0.4531616768 * I
%! -0.8 + I * 0.8 , 0.9245978896 + 0.6334016187 * I
%! -0.8 + I * 1. , 1.060030206 + 0.8408616109 * I
%! -0.8 + I * 1.2 , 1.232861756 + 1.085475913 * I
%! -0.8 + I * 1.4 , 1.446126965 + 1.379933558 * I
%! -0.8 + I * 1.6 , 1.701139468 + 1.741030588 * I
%! -0.8 + I * 1.8 , 1.994526268 + 2.191509596 * I
%! -0.8 + I * 2. , 2.312257188 + 2.762051518 * I
%! -0.6 + I * 0. , 0.8258917445 + 0. * I
%! -0.6 + I * 0.2 , 0.842151698 + 0.1130337928 * I
%! -0.6 + I * 0.4 , 0.8915487431 + 0.2309124769 * I
%! -0.6 + I * 0.6 , 0.975948103 + 0.3588102098 * I
%! -0.6 + I * 0.8 , 1.098499209 + 0.5026234141 * I
%! -0.6 + I * 1. , 1.263676101 + 0.6695125973 * I
%! -0.6 + I * 1.2 , 1.477275851 + 0.8687285705 * I
%! -0.6 + I * 1.4 , 1.746262523 + 1.112955966 * I
%! -0.6 + I * 1.6 , 2.078179075 + 1.420581466 * I
%! -0.6 + I * 1.8 , 2.479425208 + 1.819580713 * I
%! -0.6 + I * 2. , 2.950586798 + 2.354077344 * I
%! -0.4 + I * 0. , 0.9211793498 + 0. * I
%! -0.4 + I * 0.2 , 0.9395019377 + 0.07822091534 * I
%! -0.4 + I * 0.4 , 0.9952345231 + 0.1598950363 * I
%! -0.4 + I * 0.6 , 1.090715991 + 0.2487465067 * I
%! -0.4 + I * 0.8 , 1.229998843 + 0.34910407 * I
%! -0.4 + I * 1. , 1.419103868 + 0.4663848201 * I
%! -0.4 + I * 1.2 , 1.666426377 + 0.607877235 * I
%! -0.4 + I * 1.4 , 1.983347336 + 0.7841054404 * I
%! -0.4 + I * 1.6 , 2.385101684 + 1.01134031 * I
%! -0.4 + I * 1.8 , 2.89185416 + 1.316448705 * I
%! -0.4 + I * 2. , 3.529393374 + 1.74670531 * I
%! -0.2 + I * 0. , 0.9800743122 + 0. * I
%! -0.2 + I * 0.2 , 0.9997019476 + 0.03999835809 * I
%! -0.2 + I * 0.4 , 1.059453907 + 0.08179712295 * I
%! -0.2 + I * 0.6 , 1.16200643 + 0.1273503824 * I
%! -0.2 + I * 0.8 , 1.312066413 + 0.1789585449 * I
%! -0.2 + I * 1. , 1.516804331 + 0.2395555269 * I
%! -0.2 + I * 1.2 , 1.786613221 + 0.313189147 * I
%! -0.2 + I * 1.4 , 2.136422971 + 0.405890925 * I
%! -0.2 + I * 1.6 , 2.588021972 + 0.527357091 * I
%! -0.2 + I * 1.8 , 3.174302819 + 0.6944201617 * I
%! -0.2 + I * 2. , 3.947361147 + 0.9387994989 * I
%! 0. + I * 0. , 1. + 0. * I
%! 0. + I * 0.2 , 1.020074723 + 0. * I
%! 0. + I * 0.4 , 1.08120563 + 0. * I
%! 0. + I * 0.6 , 1.18619146 + 0. * I
%! 0. + I * 0.8 , 1.339978715 + 0. * I
%! 0. + I * 1. , 1.550164037 + 0. * I
%! 0. + I * 1.2 , 1.827893279 + 0. * I
%! 0. + I * 1.4 , 2.189462954 + 0. * I
%! 0. + I * 1.6 , 2.659259752 + 0. * I
%! 0. + I * 1.8 , 3.275434266 + 0. * I
%! 0. + I * 2. , 4.101632484 + 0. * I
%! 0.2 + I * 0. , 0.9800743122 + 0. * I
%! 0.2 + I * 0.2 , 0.9997019476 - 0.03999835809 * I
%! 0.2 + I * 0.4 , 1.059453907 - 0.08179712295 * I
%! 0.2 + I * 0.6 , 1.16200643 - 0.1273503824 * I
%! 0.2 + I * 0.8 , 1.312066413 - 0.1789585449 * I
%! 0.2 + I * 1. , 1.516804331 - 0.2395555269 * I
%! 0.2 + I * 1.2 , 1.786613221 - 0.313189147 * I
%! 0.2 + I * 1.4 , 2.136422971 - 0.405890925 * I
%! 0.2 + I * 1.6 , 2.588021972 - 0.527357091 * I
%! 0.2 + I * 1.8 , 3.174302819 - 0.6944201617 * I
%! 0.2 + I * 2. , 3.947361147 - 0.9387994989 * I
%! 0.4 + I * 0. , 0.9211793498 + 0. * I
%! 0.4 + I * 0.2 , 0.9395019377 - 0.07822091534 * I
%! 0.4 + I * 0.4 , 0.9952345231 - 0.1598950363 * I
%! 0.4 + I * 0.6 , 1.090715991 - 0.2487465067 * I
%! 0.4 + I * 0.8 , 1.229998843 - 0.34910407 * I
%! 0.4 + I * 1. , 1.419103868 - 0.4663848201 * I
%! 0.4 + I * 1.2 , 1.666426377 - 0.607877235 * I
%! 0.4 + I * 1.4 , 1.983347336 - 0.7841054404 * I
%! 0.4 + I * 1.6 , 2.385101684 - 1.01134031 * I
%! 0.4 + I * 1.8 , 2.89185416 - 1.316448705 * I
%! 0.4 + I * 2. , 3.529393374 - 1.74670531 * I
%! 0.6 + I * 0. , 0.8258917445 + 0. * I
%! 0.6 + I * 0.2 , 0.842151698 - 0.1130337928 * I
%! 0.6 + I * 0.4 , 0.8915487431 - 0.2309124769 * I
%! 0.6 + I * 0.6 , 0.975948103 - 0.3588102098 * I
%! 0.6 + I * 0.8 , 1.098499209 - 0.5026234141 * I
%! 0.6 + I * 1. , 1.263676101 - 0.6695125973 * I
%! 0.6 + I * 1.2 , 1.477275851 - 0.8687285705 * I
%! 0.6 + I * 1.4 , 1.746262523 - 1.112955966 * I
%! 0.6 + I * 1.6 , 2.078179075 - 1.420581466 * I
%! 0.6 + I * 1.8 , 2.479425208 - 1.819580713 * I
%! 0.6 + I * 2. , 2.950586798 - 2.354077344 * I
%! 0.8 + I * 0. , 0.6982891589 + 0. * I
%! 0.8 + I * 0.2 , 0.71187169 - 0.1430549855 * I
%! 0.8 + I * 0.4 , 0.7530744458 - 0.2920273465 * I
%! 0.8 + I * 0.6 , 0.8232501212 - 0.4531616768 * I
%! 0.8 + I * 0.8 , 0.9245978896 - 0.6334016187 * I
%! 0.8 + I * 1. , 1.060030206 - 0.8408616109 * I
%! 0.8 + I * 1.2 , 1.232861756 - 1.085475913 * I
%! 0.8 + I * 1.4 , 1.446126965 - 1.379933558 * I
%! 0.8 + I * 1.6 , 1.701139468 - 1.741030588 * I
%! 0.8 + I * 1.8 , 1.994526268 - 2.191509596 * I
%! 0.8 + I * 2. , 2.312257188 - 2.762051518 * I
%! 1. + I * 0. , 0.5436738271 + 0. * I
%! 1. + I * 0.2 , 0.5541219664 - 0.1672121517 * I
%! 1. + I * 0.4 , 0.5857703552 - 0.3410940893 * I
%! 1. + I * 0.6 , 0.6395034233 - 0.5285979063 * I
%! 1. + I * 0.8 , 0.716688504 - 0.7372552987 * I
%! 1. + I * 1. , 0.8189576795 - 0.9755037374 * I
%! 1. + I * 1.2 , 0.9477661951 - 1.253049471 * I
%! 1. + I * 1.4 , 1.103540657 - 1.581252712 * I
%! 1. + I * 1.6 , 1.284098214 - 1.973449038 * I
%! 1. + I * 1.8 , 1.481835651 - 2.4449211 * I
%! 1. + I * 2. , 1.679032464 - 3.011729224 * I
%! ];
%! DN = [
%! -1. + I * 0. , 0.9895776106 + 0. * I
%! -1. + I * 0.2 , 0.9893361555 + 0.002756935338 * I
%! -1. + I * 0.4 , 0.9885716856 + 0.005949639805 * I
%! -1. + I * 0.6 , 0.9871564855 + 0.01008044183 * I
%! -1. + I * 0.8 , 0.9848512162 + 0.01579337596 * I
%! -1. + I * 1. , 0.9812582484 + 0.02396648455 * I
%! -1. + I * 1.2 , 0.9757399152 + 0.0358288294 * I
%! -1. + I * 1.4 , 0.9672786056 + 0.0531049859 * I
%! -1. + I * 1.6 , 0.954237868 + 0.0781744383 * I
%! -1. + I * 1.8 , 0.933957524 + 0.1141918269 * I
%! -1. + I * 2. , 0.9020917489 + 0.1650142936 * I
%! -0.8 + I * 0. , 0.992429635 + 0. * I
%! -0.8 + I * 0.2 , 0.9924147861 + 0.003020708044 * I
%! -0.8 + I * 0.4 , 0.99236555 + 0.00652359532 * I
%! -0.8 + I * 0.6 , 0.9922655715 + 0.0110676219 * I
%! -0.8 + I * 0.8 , 0.9920785856 + 0.01737733806 * I
%! -0.8 + I * 1. , 0.9917291795 + 0.02645738598 * I
%! -0.8 + I * 1.2 , 0.9910606387 + 0.03974949378 * I
%! -0.8 + I * 1.4 , 0.9897435004 + 0.05935252515 * I
%! -0.8 + I * 1.6 , 0.987077644 + 0.08832675281 * I
%! -0.8 + I * 1.8 , 0.9815667458 + 0.1310872821 * I
%! -0.8 + I * 2. , 0.970020127 + 0.1938136793 * I
%! -0.6 + I * 0. , 0.9953099088 + 0. * I
%! -0.6 + I * 0.2 , 0.995526009 + 0.002814772354 * I
%! -0.6 + I * 0.4 , 0.9962071136 + 0.006083312292 * I
%! -0.6 + I * 0.6 , 0.9974557125 + 0.01033463525 * I
%! -0.6 + I * 0.8 , 0.9994560563 + 0.01626207722 * I
%! -0.6 + I * 1. , 1.00249312 + 0.02484336286 * I
%! -0.6 + I * 1.2 , 1.006973922 + 0.0375167093 * I
%! -0.6 + I * 1.4 , 1.013436509 + 0.05645315628 * I
%! -0.6 + I * 1.6 , 1.022504295 + 0.08499262247 * I
%! -0.6 + I * 1.8 , 1.034670023 + 0.1283564595 * I
%! -0.6 + I * 2. , 1.049599899 + 0.194806122 * I
%! -0.4 + I * 0. , 0.9977686897 + 0. * I
%! -0.4 + I * 0.2 , 0.9981836165 + 0.002167241934 * I
%! -0.4 + I * 0.4 , 0.9994946045 + 0.004686808612 * I
%! -0.4 + I * 0.6 , 1.001910789 + 0.00797144174 * I
%! -0.4 + I * 0.8 , 1.005817375 + 0.01256717724 * I
%! -0.4 + I * 1. , 1.011836374 + 0.01925509038 * I
%! -0.4 + I * 1.2 , 1.020923572 + 0.02920828367 * I
%! -0.4 + I * 1.4 , 1.034513743 + 0.04425213602 * I
%! -0.4 + I * 1.6 , 1.054725746 + 0.06732276244 * I
%! -0.4 + I * 1.8 , 1.08462027 + 0.1033236812 * I
%! -0.4 + I * 2. , 1.128407402 + 0.1608240664 * I
%! -0.2 + I * 0. , 0.9994191176 + 0. * I
%! -0.2 + I * 0.2 , 0.9999683719 + 0.001177128019 * I
%! -0.2 + I * 0.4 , 1.001705496 + 0.00254669712 * I
%! -0.2 + I * 0.6 , 1.004913944 + 0.004334880912 * I
%! -0.2 + I * 0.8 , 1.010120575 + 0.006842775622 * I
%! -0.2 + I * 1. , 1.018189543 + 0.01050520136 * I
%! -0.2 + I * 1.2 , 1.030482479 + 0.01598431001 * I
%! -0.2 + I * 1.4 , 1.049126108 + 0.02433134655 * I
%! -0.2 + I * 1.6 , 1.077466003 + 0.0372877718 * I
%! -0.2 + I * 1.8 , 1.120863308 + 0.05789156398 * I
%! -0.2 + I * 2. , 1.188162088 + 0.09181238708 * I
%! 0. + I * 0. , 1. + 0. * I
%! 0. + I * 0.2 , 1.000596698 + 0. * I
%! 0. + I * 0.4 , 1.002484444 + 0. * I
%! 0. + I * 0.6 , 1.005973379 + 0. * I
%! 0. + I * 0.8 , 1.011641536 + 0. * I
%! 0. + I * 1. , 1.020441432 + 0. * I
%! 0. + I * 1.2 , 1.033885057 + 0. * I
%! 0. + I * 1.4 , 1.054361188 + 0. * I
%! 0. + I * 1.6 , 1.085694733 + 0. * I
%! 0. + I * 1.8 , 1.134186672 + 0. * I
%! 0. + I * 2. , 1.210701071 + 0. * I
%! 0.2 + I * 0. , 0.9994191176 + 0. * I
%! 0.2 + I * 0.2 , 0.9999683719 - 0.001177128019 * I
%! 0.2 + I * 0.4 , 1.001705496 - 0.00254669712 * I
%! 0.2 + I * 0.6 , 1.004913944 - 0.004334880912 * I
%! 0.2 + I * 0.8 , 1.010120575 - 0.006842775622 * I
%! 0.2 + I * 1. , 1.018189543 - 0.01050520136 * I
%! 0.2 + I * 1.2 , 1.030482479 - 0.01598431001 * I
%! 0.2 + I * 1.4 , 1.049126108 - 0.02433134655 * I
%! 0.2 + I * 1.6 , 1.077466003 - 0.0372877718 * I
%! 0.2 + I * 1.8 , 1.120863308 - 0.05789156398 * I
%! 0.2 + I * 2. , 1.188162088 - 0.09181238708 * I
%! 0.4 + I * 0. , 0.9977686897 + 0. * I
%! 0.4 + I * 0.2 , 0.9981836165 - 0.002167241934 * I
%! 0.4 + I * 0.4 , 0.9994946045 - 0.004686808612 * I
%! 0.4 + I * 0.6 , 1.001910789 - 0.00797144174 * I
%! 0.4 + I * 0.8 , 1.005817375 - 0.01256717724 * I
%! 0.4 + I * 1. , 1.011836374 - 0.01925509038 * I
%! 0.4 + I * 1.2 , 1.020923572 - 0.02920828367 * I
%! 0.4 + I * 1.4 , 1.034513743 - 0.04425213602 * I
%! 0.4 + I * 1.6 , 1.054725746 - 0.06732276244 * I
%! 0.4 + I * 1.8 , 1.08462027 - 0.1033236812 * I
%! 0.4 + I * 2. , 1.128407402 - 0.1608240664 * I
%! 0.6 + I * 0. , 0.9953099088 + 0. * I
%! 0.6 + I * 0.2 , 0.995526009 - 0.002814772354 * I
%! 0.6 + I * 0.4 , 0.9962071136 - 0.006083312292 * I
%! 0.6 + I * 0.6 , 0.9974557125 - 0.01033463525 * I
%! 0.6 + I * 0.8 , 0.9994560563 - 0.01626207722 * I
%! 0.6 + I * 1. , 1.00249312 - 0.02484336286 * I
%! 0.6 + I * 1.2 , 1.006973922 - 0.0375167093 * I
%! 0.6 + I * 1.4 , 1.013436509 - 0.05645315628 * I
%! 0.6 + I * 1.6 , 1.022504295 - 0.08499262247 * I
%! 0.6 + I * 1.8 , 1.034670023 - 0.1283564595 * I
%! 0.6 + I * 2. , 1.049599899 - 0.194806122 * I
%! 0.8 + I * 0. , 0.992429635 + 0. * I
%! 0.8 + I * 0.2 , 0.9924147861 - 0.003020708044 * I
%! 0.8 + I * 0.4 , 0.99236555 - 0.00652359532 * I
%! 0.8 + I * 0.6 , 0.9922655715 - 0.0110676219 * I
%! 0.8 + I * 0.8 , 0.9920785856 - 0.01737733806 * I
%! 0.8 + I * 1. , 0.9917291795 - 0.02645738598 * I
%! 0.8 + I * 1.2 , 0.9910606387 - 0.03974949378 * I
%! 0.8 + I * 1.4 , 0.9897435004 - 0.05935252515 * I
%! 0.8 + I * 1.6 , 0.987077644 - 0.08832675281 * I
%! 0.8 + I * 1.8 , 0.9815667458 - 0.1310872821 * I
%! 0.8 + I * 2. , 0.970020127 - 0.1938136793 * I
%! 1. + I * 0. , 0.9895776106 + 0. * I
%! 1. + I * 0.2 , 0.9893361555 - 0.002756935338 * I
%! 1. + I * 0.4 , 0.9885716856 - 0.005949639805 * I
%! 1. + I * 0.6 , 0.9871564855 - 0.01008044183 * I
%! 1. + I * 0.8 , 0.9848512162 - 0.01579337596 * I
%! 1. + I * 1. , 0.9812582484 - 0.02396648455 * I
%! 1. + I * 1.2 , 0.9757399152 - 0.0358288294 * I
%! 1. + I * 1.4 , 0.9672786056 - 0.0531049859 * I
%! 1. + I * 1.6 , 0.954237868 - 0.0781744383 * I
%! 1. + I * 1.8 , 0.933957524 - 0.1141918269 * I
%! 1. + I * 2. , 0.9020917489 - 0.1650142936 * I
%! ];
%! tol = 1e-9;
%! for x = 0:10
%!   for y = 0:10
%!     ur = -1 + x * 0.2;
%!     ui =  y * 0.2;
%!     ii = 1 + y + x*11;
%!     [sn, cn, dn] = ellipj (ur + I * ui, m);
%!     assert (sn, SN(ii, 2), tol);
%!     assert (cn, CN(ii, 2), tol);
%!     assert (dn, DN(ii, 2), tol);
%!   endfor
%! endfor

## tests taken from test_ellipj.m
%!test
%! u1 = pi/3; m1 = 0;
%! res1 = [sin(pi/3), cos(pi/3), 1];
%! [sn,cn,dn] = ellipj (u1,m1);
%! assert ([sn,cn,dn], res1, 10*eps);

%!test
%! u2 = log(2); m2 = 1;
%! res2 = [ 3/5, 4/5, 4/5 ];
%! [sn,cn,dn] = ellipj (u2,m2);
%! assert ([sn,cn,dn], res2, 10*eps);

%!test
%! u3 = log(2)*1i; m3 = 0;
%! res3 = [3i/4,5/4,1];
%! [sn,cn,dn] = ellipj (u3,m3);
%! assert ([sn,cn,dn], res3, 10*eps);

%!test
%! u4 = -1; m4 = tan (pi/8)^4;
%! res4 = [-0.8392965923,0.5436738271,0.9895776106];
%! [sn,cn,dn] = ellipj (u4, m4);
%! assert ([sn,cn,dn], res4, 1e-10);

%!test
%! u5 = -0.2 + 0.4i; m5 = tan(pi/8)^4;
%! res5 = [ -0.2152524522 + 0.402598347i, ...
%!           1.059453907  + 0.08179712295i, ...
%!           1.001705496  + 0.00254669712i ];
%! [sn,cn,dn] = ellipj (u5,m5);
%! assert ([sn,cn,dn], res5, 1e-9);

%!test
%! u6 = 0.2 + 0.6i; m6 = tan(pi/8)^4;
%! res6 = [ 0.2369100139 + 0.624633635i, ...
%!          1.16200643   - 0.1273503824i, ...
%!          1.004913944  - 0.004334880912i ];
%! [sn,cn,dn] = ellipj (u6,m6);
%! assert ([sn,cn,dn], res6, 1e-8);

%!test
%! u7 = 0.8 + 0.8i; m7 = tan (pi/8)^4;
%! res7 = [0.9588386397 + 0.6107824358i, ...
%!         0.9245978896 - 0.6334016187i, ...
%!         0.9920785856 - 0.01737733806i ];
%! [sn,cn,dn] = ellipj (u7,m7);
%! assert ([sn,cn,dn], res7, 1e-10);

%!test
%! u = [0,pi/6,pi/4,pi/2]; m=0;
%! res = [0,1/2,1/sqrt(2),1;1,cos(pi/6),1/sqrt(2),0;1,1,1,1];
%! [sn,cn,dn] = ellipj (u,m);
%! assert ([sn;cn;dn], res, 100*eps);
%! [sn,cn,dn] = ellipj (u',0);
%! assert ([sn,cn,dn], res', 100*eps);

## FIXME: need to check [real,complex]x[scalar,rowvec,colvec,matrix]x[u,m]

## One test for u column vector x m row vector
%!test
%! u = [0,pi/6,pi/4,pi/2]';  m = [0 0 0 0];
%! res = [0,1/2,1/sqrt(2),1;1,cos(pi/6),1/sqrt(2),0;1,1,1,1]';
%! [sn,cn,dn] = ellipj (u,m);
%! assert (sn, repmat (res(:,1), [1,4]), 100*eps);
%! assert (cn, repmat (res(:,2), [1,4]), 100*eps);
%! assert (dn, repmat (res(:,3), [1,4]), 100*eps);

%!test
%! ## Test Jacobi elliptic functions
%! ## against "exact" solution from Mathematica 3.0
%! ## David Billinghurst <David.Billinghurst@riotinto.com>
%! ## 1 February 2001
%! u = [ 0.25; 0.25; 0.20; 0.20; 0.672; 0.5];
%! m = [ 0.0;  1.0;  0.19; 0.81; 0.36;  0.9999999999];
%! S = [ sin(0.25);
%!       tanh(0.25);
%!       0.19842311013970879516;
%!       0.19762082367187648571;
%!       0.6095196917919021945;
%!       0.4621171572617320908 ];
%! C = [ cos(0.25);
%!       sech(0.25);
%!       0.9801164570409401062;
%!       0.9802785369736752032;
%!       0.7927709286533560550;
%!       0.8868188839691764094 ];
%! D = [ 1.0;
%!       sech(0.25);
%!       0.9962526643271134302;
%!       0.9840560289645665155;
%!       0.9307281387786906491;
%!       0.8868188839812167635 ];
%! [sn,cn,dn] = ellipj (u,m);
%! assert (sn, S, 8*eps);
%! assert (cn, C, 8*eps);
%! assert (dn, D, 8*eps);

%!test
%! ## Test continuity of dn when cn is near zero (bug #43344)
%! m = 0.5;
%! u = ellipke (0.5);
%! x = [-1e-3, -1e-12, 0, 1e-12, 1e-3];
%! [~, ~, dn] = ellipj (u + x, m);
%! D = 1/sqrt (2) * ones (size (x));
%! assert (dn, D, 1e-6);

%!error ellipj ()
%!error ellipj (1)
%!error ellipj (1,2,3,4)
%!warning <expecting 0 <= M <= 1> ellipj (1,2);
## FIXME: errors commented out untill lasterr() truly returns the last error.
%!#error <expecting scalar or matrix as second argument> ellipj (1, "1")
%!#error <expecting scalar or matrix as first argument> ellipj ("1", 1)
%!#error <expecting scalar or matrix as first argument> ellipj ({1}, 1)
%!#error <expecting scalar or matrix as first argument> ellipj ({1, 2}, 1)
%!#error <expecting scalar or matrix as second argument> ellipj (1, {1, 2})
%!#error <expecting scalar or matrix as first argument> ellipj ("1", [1, 2])
%!#error <expecting scalar or matrix as first argument> ellipj ({1}, [1, 2])
%!#error <expecting scalar or matrix as first argument> ellipj ({1}, [1, 2])
%!#error <expecting scalar or matrix as first argument> ellipj ("1,2", [1, 2])
%!#error <expecting scalar or matrix as first argument> ellipj ({1, 2}, [1, 2])
%!error <Invalid size combination for U and M> ellipj ([1:4], [1:3])
%!error <Invalid size combination for U and M> ellipj (complex (1:4,1:4), [1:3])

*/

