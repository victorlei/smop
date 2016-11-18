/*

Copyright (C) 2006-2015 John W. Eaton

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

/* Original version written by Paul Kienzle distributed as free
   software in the in the public domain.  */

/*

double randg (a)
void fill_randg (a,n,x)

Generate a series of standard gamma distributions.

See: Marsaglia G and Tsang W (2000), "A simple method for generating
gamma variables", ACM Transactions on Mathematical Software 26(3) 363-372

Needs the following defines:
* NAN: value to return for Not-A-Number
* RUNI: uniform generator on (0,1)
* RNOR: normal generator
* REXP: exponential generator, or -log(RUNI) if one isn't available
* INFINITE: function to test whether a value is infinite

Test using:
  mean = a
  variance = a
  skewness = 2/sqrt(a)
  kurtosis = 3 + 6/sqrt(a)

Note that randg can be used to generate many distributions:

gamma(a,b) for a>0, b>0 (from R)
  r = b*randg(a)
beta(a,b) for a>0, b>0
  r1 = randg(a,1)
  r = r1 / (r1 + randg(b,1))
Erlang(a,n)
  r = a*randg(n)
chisq(df) for df>0
  r = 2*randg(df/2)
t(df) for 0<df<inf (use randn if df is infinite)
  r = randn () / sqrt(2*randg(df/2)/df)
F(n1,n2) for 0<n1, 0<n2
  r1 = 2*randg(n1/2)/n1 or 1 if n1 is infinite
  r2 = 2*randg(n2/2)/n2 or 1 if n2 is infinite
  r = r1 / r2
negative binonial (n, p) for n>0, 0<p<=1
  r = randp((1-p)/p * randg(n))
  (from R, citing Devroye(1986), Non-Uniform Random Variate Generation)
non-central chisq(df,L), for df>=0 and L>0 (use chisq if L=0)
  r = randp(L/2)
  r(r>0) = 2*randg(r(r>0))
  r(df>0) += 2*randg(df(df>0)/2)
  (from R, citing formula 29.5b-c in Johnson, Kotz, Balkrishnan(1995))
Dirichlet(a1,...,ak) for ai > 0
  r = (randg(a1),...,randg(ak))
  r = r / sum(r)
  (from GSL, citing Law & Kelton(1991), Simulation Modeling and Analysis)
*/

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#include <stdio.h>

#include "lo-ieee.h"
#include "lo-math.h"
#include "randmtzig.h"
#include "randgamma.h"

#undef NAN
#define NAN octave_NaN
#define INFINITE lo_ieee_isinf
#define RUNI oct_randu()
#define RNOR oct_randn()
#define REXP oct_rande()

void
oct_fill_randg (double a, octave_idx_type n, double *r)
{
  octave_idx_type i;
  /* If a < 1, start by generating gamma (1+a) */
  const double d =  (a < 1. ? 1.+a : a) - 1./3.;
  const double c = 1./sqrt (9.*d);

  /* Handle invalid cases */
  if (a <= 0 || INFINITE(a))
    {
      for (i=0; i < n; i++)
        r[i] = NAN;
      return;
    }

  for (i=0; i < n; i++)
    {
      double x, xsq, v, u;
    restart:
      x = RNOR;
      v = (1+c*x);
      v *= v*v;
      if (v <= 0)
        goto restart; /* rare, so don't bother moving up */
      u = RUNI;
      xsq = x*x;
      if (u >= 1.-0.0331*xsq*xsq && log (u) >= 0.5*xsq + d*(1-v+log (v)))
        goto restart;
      r[i] = d*v;
    }
  if (a < 1)
    {
      /* Use gamma(a) = gamma(1+a)*U^(1/a) */
      /* Given REXP = -log(U) then U^(1/a) = exp(-REXP/a) */
      for (i = 0; i < n; i++)
        r[i] *= exp (-REXP/a);
    }
}

double
oct_randg (double a)
{
  double ret;
  oct_fill_randg (a,1,&ret);
  return ret;
}

#undef NAN
#undef RUNI
#undef RNOR
#undef REXP
#define NAN octave_Float_NaN
#define RUNI oct_float_randu()
#define RNOR oct_float_randn()
#define REXP oct_float_rande()

void
oct_fill_float_randg (float a, octave_idx_type n, float *r)
{
  octave_idx_type i;
  /* If a < 1, start by generating gamma(1+a) */
  const float d =  (a < 1. ? 1.+a : a) - 1./3.;
  const float c = 1./sqrt (9.*d);

  /* Handle invalid cases */
  if (a <= 0 || INFINITE(a))
    {
      for (i=0; i < n; i++)
        r[i] = NAN;
      return;
    }

  for (i=0; i < n; i++)
    {
      float x, xsq, v, u;
    frestart:
      x = RNOR;
      v = (1+c*x);
      v *= v*v;
      if (v <= 0)
        goto frestart; /* rare, so don't bother moving up */
      u = RUNI;
      xsq = x*x;
      if (u >= 1.-0.0331*xsq*xsq && log (u) >= 0.5*xsq + d*(1-v+log (v)))
        goto frestart;
      r[i] = d*v;
    }
  if (a < 1)
    {
      /* Use gamma(a) = gamma(1+a)*U^(1/a) */
      /* Given REXP = -log(U) then U^(1/a) = exp(-REXP/a) */
      for (i = 0; i < n; i++)
        r[i] *= exp (-REXP/a);
    }
}

float
oct_float_randg (float a)
{
  float ret;
  oct_fill_float_randg (a,1,&ret);
  return ret;
}
