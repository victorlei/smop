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

/* Needs the following defines:
 * NAN: value to return for Not-A-Number
 * RUNI: uniform generator on (0,1)
 * RNOR: normal generator
 * LGAMMA: log gamma function
 * INFINITE: function to test whether a value is infinite
 */

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#include <stdio.h>

#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-math.h"
#include "randmtzig.h"
#include "randpoisson.h"

#undef NAN
#define NAN octave_NaN
#undef INFINITE
#define INFINITE lo_ieee_isinf
#define RUNI oct_randu()
#define RNOR oct_randn()
#define LGAMMA xlgamma

F77_RET_T
F77_FUNC (dlgams, DLGAMS) (const double *, double *, double *);

static double
xlgamma (double x)
{
  double result;
#ifdef HAVE_LGAMMA
  result = lgamma (x);
#else
  double sgngam;

  if (lo_ieee_isnan (x))
    result = x;
  else if (x <= 0 || lo_ieee_isinf (x))
    result = octave_Inf;
  else
    F77_XFCN (dlgams, DLGAMS, (&x, &result, &sgngam));
#endif
  return result;
}

/* ---- pprsc.c from Stadloeber's winrand --- */

/* flogfak(k) = ln(k!) */
static double
flogfak (double k)
{
#define C0  9.18938533204672742e-01
#define C1  8.33333333333333333e-02
#define C3 -2.77777777777777778e-03
#define C5  7.93650793650793651e-04
#define C7 -5.95238095238095238e-04

  static double logfak[30L] =
  {
    0.00000000000000000,   0.00000000000000000,   0.69314718055994531,
    1.79175946922805500,   3.17805383034794562,   4.78749174278204599,
    6.57925121201010100,   8.52516136106541430,  10.60460290274525023,
    12.80182748008146961,  15.10441257307551530,  17.50230784587388584,
    19.98721449566188615,  22.55216385312342289,  25.19122118273868150,
    27.89927138384089157,  30.67186010608067280,  33.50507345013688888,
    36.39544520803305358,  39.33988418719949404,  42.33561646075348503,
    45.38013889847690803,  48.47118135183522388,  51.60667556776437357,
    54.78472939811231919,  58.00360522298051994,  61.26170176100200198,
    64.55753862700633106,  67.88974313718153498,  71.25703896716800901
  };

  double  r, rr;

  if (k >= 30.0)
    {
      r  = 1.0 / k;
      rr = r * r;
      return ((k + 0.5)*log (k) - k + C0 + r*(C1 + rr*(C3 + rr*(C5 + rr*C7))));
    }
  else
    return (logfak[(int)k]);
}


/******************************************************************
 *                                                                *
 * Poisson Distribution - Patchwork Rejection/Inversion  *
 *                                                                *
 ******************************************************************
 *                                                                *
 * For parameter  my < 10  Tabulated Inversion is applied.        *
 * For my >= 10  Patchwork Rejection is employed:                 *
 * The area below the histogram function f(x) is rearranged in    *
 * its body by certain point reflections. Within a large center   *
 * interval variates are sampled efficiently by rejection from    *
 * uniform hats. Rectangular immediate acceptance regions speed   *
 * up the generation. The remaining tails are covered by          *
 * exponential functions.                                         *
 *                                                                *
 ******************************************************************
 *                                                                *
 * FUNCTION :   - pprsc samples a random number from the Poisson  *
 *                distribution with parameter my > 0.             *
 * REFERENCE :  - H. Zechner (1994): Efficient sampling from      *
 *                continuous and discrete unimodal distributions, *
 *                Doctoral Dissertation, 156 pp., Technical       *
 *                University Graz, Austria.                       *
 * SUBPROGRAM : - drand(seed) ... (0,1)-Uniform generator with    *
 *                unsigned long integer *seed.                    *
 *                                                                *
 * Implemented by H. Zechner, January 1994                        *
 * Revised by F. Niederl, July 1994                               *
 *                                                                *
 ******************************************************************/

static double
f (double k, double l_nu, double c_pm)
{
  return exp (k * l_nu - flogfak (k) - c_pm);
}

static double
pprsc (double my)
{
  static double my_last = -1.0;
  static double m,  k2, k4, k1, k5;
  static double dl, dr, r1, r2, r4, r5, ll, lr, l_my, c_pm,
                f1, f2, f4, f5, p1, p2, p3, p4, p5, p6;
  double        Dk, X, Y;
  double        Ds, U, V, W;

  if (my != my_last)
    {                               /* set-up           */
      my_last = my;
      /* approximate deviation of reflection points k2, k4 from my - 1/2 */
      Ds = sqrt (my + 0.25);

      /* mode m, reflection points k2 and k4, and points k1 and k5,      */
      /* which delimit the centre region of h(x)                         */
      m  = floor (my);
      k2 = ceil (my - 0.5 - Ds);
      k4 = floor (my - 0.5 + Ds);
      k1 = k2 + k2 - m + 1L;
      k5 = k4 + k4 - m;

      /* range width of the critical left and right centre region        */
      dl = (k2 - k1);
      dr = (k5 - k4);

      /* recurrence constants r(k)=p(k)/p(k-1) at k = k1, k2, k4+1, k5+1 */
      r1 = my / k1;
      r2 = my / k2;
      r4 = my / (k4 + 1.0);
      r5 = my / (k5 + 1.0);

      /* reciprocal values of the scale parameters of exp. tail envelope */
      ll =  log (r1);                                /* expon. tail left */
      lr = -log (r5);                                /* expon. tail right*/

      /* Poisson constants, necessary for computing function values f(k) */
      l_my = log (my);
      c_pm = m * l_my - flogfak (m);

      /* function values f(k) = p(k)/p(m) at k = k2, k4, k1, k5          */
      f2 = f (k2, l_my, c_pm);
      f4 = f (k4, l_my, c_pm);
      f1 = f (k1, l_my, c_pm);
      f5 = f (k5, l_my, c_pm);

      /* area of the two centre and the two exponential tail regions     */
      /* area of the two immediate acceptance regions between k2, k4     */
      p1 = f2 * (dl + 1.0);                            /* immed. left    */
      p2 = f2 * dl         + p1;                       /* centre left    */
      p3 = f4 * (dr + 1.0) + p2;                       /* immed. right   */
      p4 = f4 * dr         + p3;                       /* centre right   */
      p5 = f1 / ll         + p4;                       /* exp. tail left */
      p6 = f5 / lr         + p5;                       /* exp. tail right*/
    }

  for (;;)
    {
      /* generate uniform number U -- U(0, p6)                           */
      /* case distinction corresponding to U                             */
      if ((U = RUNI * p6) < p2)
        {                                            /* centre left      */

          /* immediate acceptance region
             R2 = [k2, m) *[0, f2),  X = k2, ... m -1 */
          if ((V = U - p1) < 0.0)  return (k2 + floor (U/f2));
          /* immediate acceptance region
             R1 = [k1, k2)*[0, f1),  X = k1, ... k2-1 */
          if ((W = V / dl) < f1 )  return (k1 + floor (V/f1));

          /* computation of candidate X < k2, and its counterpart Y > k2 */
          /* either squeeze-acceptance of X or acceptance-rejection of Y */
          Dk = floor (dl * RUNI) + 1.0;
          if (W <= f2 - Dk * (f2 - f2/r2))
            {                                        /* quick accept of  */
              return (k2 - Dk);                      /* X = k2 - Dk      */
            }
          if ((V = f2 + f2 - W) < 1.0)
            {                                        /* quick reject of Y*/
              Y = k2 + Dk;
              if (V <= f2 + Dk * (1.0 - f2)/(dl + 1.0))
                {                                    /* quick accept of  */
                  return (Y);                        /* Y = k2 + Dk      */
                }
              if (V <= f (Y, l_my, c_pm))  return (Y); /* final accept of Y*/
            }
          X = k2 - Dk;
        }
      else if (U < p4)
        {                                            /* centre right     */
          /*  immediate acceptance region
              R3 = [m, k4+1)*[0, f4), X = m, ... k4    */
          if ((V = U - p3) < 0.0)  return (k4 - floor ((U - p2)/f4));
          /* immediate acceptance region
             R4 = [k4+1, k5+1)*[0, f5)                */
          if ((W = V / dr) < f5 )  return (k5 - floor (V/f5));

          /* computation of candidate X > k4, and its counterpart Y < k4 */
          /* either squeeze-acceptance of X or acceptance-rejection of Y */
          Dk = floor (dr * RUNI) + 1.0;
          if (W <= f4 - Dk * (f4 - f4*r4))
            {                                        /* quick accept of  */
              return (k4 + Dk);                      /* X = k4 + Dk      */
            }
          if ((V = f4 + f4 - W) < 1.0)
            {                                        /* quick reject of Y*/
              Y = k4 - Dk;
              if (V <= f4 + Dk * (1.0 - f4)/ dr)
                {                                    /* quick accept of  */
                  return (Y);                        /* Y = k4 - Dk      */
                }
              if (V <= f (Y, l_my, c_pm))  return (Y); /* final accept of Y*/
            }
          X = k4 + Dk;
        }
      else
        {
          W = RUNI;
          if (U < p5)
            {                                        /* expon. tail left */
              Dk = floor (1.0 - log (W)/ll);
              if ((X = k1 - Dk) < 0L)  continue;     /* 0 <= X <= k1 - 1 */
              W *= (U - p4) * ll;                    /* W -- U(0, h(x))  */
              if (W <= f1 - Dk * (f1 - f1/r1))
                return (X);                          /* quick accept of X*/
            }
          else
            {                                        /* expon. tail right*/
              Dk = floor (1.0 - log (W)/lr);
              X  = k5 + Dk;                          /* X >= k5 + 1      */
              W *= (U - p5) * lr;                    /* W -- U(0, h(x))  */
              if (W <= f5 - Dk * (f5 - f5*r5))
                return (X);                          /* quick accept of X*/
            }
        }

      /* acceptance-rejection test of candidate X from the original area */
      /* test, whether  W <= f(k),    with  W = U*h(x)  and  U -- U(0, 1)*/
      /* log f(X) = (X - m)*log(my) - log X! + log m!                    */
      if (log (W) <= X * l_my - flogfak (X) - c_pm)  return (X);
    }
}
/* ---- pprsc.c end ------ */


/* The remainder of the file is by Paul Kienzle */

/* Given uniform u, find x such that CDF(L,x)==u.  Return x. */
static void
poisson_cdf_lookup (double lambda, double *p, size_t n)
{
  /* Table size is predicated on the maximum value of lambda
   * we want to store in the table, and the maximum value of
   * returned by the uniform random number generator on [0,1).
   * With lambda==10 and u_max = 1 - 1/(2^32+1), we
   * have poisson_pdf(lambda,36) < 1-u_max.  If instead our
   * generator uses more bits of mantissa or returns a value
   * in the range [0,1], then for lambda==10 we need a table
   * size of 46 instead.  For long doubles, the table size
   * will need to be longer still.  */
#define TABLESIZE 46
  double t[TABLESIZE];

  /* Precompute the table for the u up to and including 0.458.
   * We will almost certainly need it. */
  int intlambda = (int)floor (lambda);
  double P;
  int tableidx;
  size_t i = n;

  t[0] = P = exp (-lambda);
  for (tableidx = 1; tableidx <= intlambda; tableidx++)
    {
      P = P*lambda/(double)tableidx;
      t[tableidx] = t[tableidx-1] + P;
    }

  while (i-- > 0)
    {
      double u = RUNI;

      /* If u > 0.458 we know we can jump to floor(lambda) before
       * comparing (this observation is based on Stadlober's winrand
       * code). For lambda >= 1, this will be a win.  Lambda < 1
       * is already fast, so adding an extra comparison is not a
       * problem. */
      int k = (u > 0.458 ? intlambda : 0);

      /* We aren't using a for loop here because when we find the
       * right k we want to jump to the next iteration of the
       * outer loop, and the continue statement will only work for
       * the inner loop. */
    nextk:
      if (u <= t[k])
        {
          p[i] = (double) k;
          continue;
        }
      if (++k < tableidx) goto nextk;

      /* We only need high values of the table very rarely so we
       * don't automatically compute the entire table. */
      while (tableidx < TABLESIZE)
        {
          P = P*lambda/(double)tableidx;
          t[tableidx] = t[tableidx-1] + P;
          /* Make sure we converge to 1.0 just in case u is uniform
           * on [0,1] rather than [0,1). */
          if (t[tableidx] == t[tableidx-1]) t[tableidx] = 1.0;
          tableidx++;
          if (u <= t[tableidx-1]) break;
        }

      /* We are assuming that the table size is big enough here.
       * This should be true even if RUNI is returning values in
       * the range [0,1] rather than [0,1). */
      p[i] = (double)(tableidx-1);
    }
}

static void
poisson_cdf_lookup_float (double lambda, float *p, size_t n)
{
  double t[TABLESIZE];

  /* Precompute the table for the u up to and including 0.458.
   * We will almost certainly need it. */
  int intlambda = (int)floor (lambda);
  double P;
  int tableidx;
  size_t i = n;

  t[0] = P = exp (-lambda);
  for (tableidx = 1; tableidx <= intlambda; tableidx++)
    {
      P = P*lambda/(double)tableidx;
      t[tableidx] = t[tableidx-1] + P;
    }

  while (i-- > 0)
    {
      double u = RUNI;
      int k = (u > 0.458 ? intlambda : 0);
    nextk:
      if (u <= t[k])
        {
          p[i] = (float) k;
          continue;
        }
      if (++k < tableidx) goto nextk;

      while (tableidx < TABLESIZE)
        {
          P = P*lambda/(double)tableidx;
          t[tableidx] = t[tableidx-1] + P;
          if (t[tableidx] == t[tableidx-1]) t[tableidx] = 1.0;
          tableidx++;
          if (u <= t[tableidx-1]) break;
        }

      p[i] = (float)(tableidx-1);
    }
}

/* From Press, et al., Numerical Recipes */
static void
poisson_rejection (double lambda, double *p, size_t n)
{
  double sq = sqrt (2.0*lambda);
  double alxm = log (lambda);
  double g = lambda*alxm - LGAMMA(lambda+1.0);
  size_t i;

  for (i = 0; i < n; i++)
    {
      double y, em, t;
      do
        {
          do
            {
              y = tan (M_PI*RUNI);
              em = sq * y + lambda;
            } while (em < 0.0);
          em = floor (em);
          t = 0.9*(1.0+y*y)*exp (em*alxm-flogfak (em)-g);
        } while (RUNI > t);
      p[i] = em;
    }
}

/* From Press, et al., Numerical Recipes */
static void
poisson_rejection_float (double lambda, float *p, size_t n)
{
  double sq = sqrt (2.0*lambda);
  double alxm = log (lambda);
  double g = lambda*alxm - LGAMMA(lambda+1.0);
  size_t i;

  for (i = 0; i < n; i++)
    {
      double y, em, t;
      do
        {
          do
            {
              y = tan (M_PI*RUNI);
              em = sq * y + lambda;
            } while (em < 0.0);
          em = floor (em);
          t = 0.9*(1.0+y*y)*exp (em*alxm-flogfak (em)-g);
        } while (RUNI > t);
      p[i] = em;
    }
}

/* The cutoff of L <= 1e8 in the following two functions before using
 * the normal approximation is based on:
 *   > L=1e8; x=floor(linspace(0,2*L,1000));
 *   > max(abs(normal_pdf(x,L,L)-poisson_pdf(x,L)))
 *   ans =  1.1376e-28
 * For L=1e7, the max is around 1e-9, which is within the step size of RUNI.
 * For L>1e10 the pprsc function breaks down, as I saw from the histogram
 * of a large sample, so 1e8 is both small enough and large enough. */

/* Generate a set of poisson numbers with the same distribution */
void
oct_fill_randp (double L, octave_idx_type n, double *p)
{
  octave_idx_type i;
  if (L < 0.0 || INFINITE(L))
    {
      for (i=0; i<n; i++)
        p[i] = NAN;
    }
  else if (L <= 10.0)
    {
      poisson_cdf_lookup (L, p, n);
    }
  else if (L <= 1e8)
    {
      for (i=0; i<n; i++)
        p[i] = pprsc (L);
    }
  else
    {
      /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
      const double sqrtL = sqrt (L);
      for (i = 0; i < n; i++)
        {
          p[i] = floor (RNOR*sqrtL + L + 0.5);
          if (p[i] < 0.0)
            p[i] = 0.0; /* will probably never happen */
        }
    }
}

/* Generate one poisson variate */
double
oct_randp (double L)
{
  double ret;
  if (L < 0.0) ret = NAN;
  else if (L <= 12.0)
    {
      /* From Press, et al. Numerical recipes */
      double g = exp (-L);
      int em = -1;
      double t = 1.0;
      do
        {
          ++em;
          t *= RUNI;
        } while (t > g);
      ret = em;
    }
  else if (L <= 1e8)
    {
      /* numerical recipes */
      poisson_rejection (L, &ret, 1);
    }
  else if (INFINITE(L))
    {
      /* FIXME R uses NaN, but the normal approx. suggests that as
       * limit should be inf. Which is correct? */
      ret = NAN;
    }
  else
    {
      /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
      ret = floor (RNOR*sqrt (L) + L + 0.5);
      if (ret < 0.0) ret = 0.0; /* will probably never happen */
    }
  return ret;
}

/* Generate a set of poisson numbers with the same distribution */
void
oct_fill_float_randp (float FL, octave_idx_type n, float *p)
{
  double L = FL;
  octave_idx_type i;
  if (L < 0.0 || INFINITE(L))
    {
      for (i=0; i<n; i++)
        p[i] = NAN;
    }
  else if (L <= 10.0)
    {
      poisson_cdf_lookup_float (L, p, n);
    }
  else if (L <= 1e8)
    {
      for (i=0; i<n; i++)
        p[i] = pprsc (L);
    }
  else
    {
      /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
      const double sqrtL = sqrt (L);
      for (i = 0; i < n; i++)
        {
          p[i] = floor (RNOR*sqrtL + L + 0.5);
          if (p[i] < 0.0)
            p[i] = 0.0; /* will probably never happen */
        }
    }
}

/* Generate one poisson variate */
float
oct_float_randp (float FL)
{
  double L = FL;
  float ret;
  if (L < 0.0) ret = NAN;
  else if (L <= 12.0)
    {
      /* From Press, et al. Numerical recipes */
      double g = exp (-L);
      int em = -1;
      double t = 1.0;
      do
        {
          ++em;
          t *= RUNI;
        } while (t > g);
      ret = em;
    }
  else if (L <= 1e8)
    {
      /* numerical recipes */
      poisson_rejection_float (L, &ret, 1);
    }
  else if (INFINITE(L))
    {
      /* FIXME R uses NaN, but the normal approx. suggests that as
       * limit should be inf. Which is correct? */
      ret = NAN;
    }
  else
    {
      /* normal approximation: from Phys. Rev. D (1994) v50 p1284 */
      ret = floor (RNOR*sqrt (L) + L + 0.5);
      if (ret < 0.0) ret = 0.0; /* will probably never happen */
    }
  return ret;
}
