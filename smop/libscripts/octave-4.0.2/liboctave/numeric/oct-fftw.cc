/*

Copyright (C) 2001-2015 John W. Eaton

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

#if defined (HAVE_FFTW)

#include <iostream>
#include <vector>

#include "lo-error.h"
#include "oct-fftw.h"
#include "quit.h"
#include "oct-locbuf.h"
#include "singleton-cleanup.h"

#if defined (HAVE_FFTW3_THREADS)
#include "nproc.h"
#endif

octave_fftw_planner *octave_fftw_planner::instance = 0;

// Helper class to create and cache FFTW plans for both 1D and
// 2D. This implementation defaults to using FFTW_ESTIMATE to create
// the plans, which in theory is suboptimal, but provides quite
// reasonable performance in practice.

// Also note that if FFTW_ESTIMATE is not used then the planner in FFTW3
// will destroy the input and output arrays. We must, therefore, create a
// temporary input array with the same size and 16-byte alignment as
// the original array when using a different planner strategy.
// Note that we also use any wisdom that is available, either in a
// FFTW3 system wide file or as supplied by the user.

// FIXME: if we can ensure 16 byte alignment in Array<T>
// (<T> *data) the FFTW3 can use SIMD instructions for further
// acceleration.

// Note that it is profitable to store the FFTW3 plans, for small FFTs.

octave_fftw_planner::octave_fftw_planner (void)
  : meth (ESTIMATE), rplan (0), rd (0), rs (0), rr (0), rh (0), rn (),
    rsimd_align (false)
{
  plan[0] = plan[1] = 0;
  d[0] = d[1] = s[0] = s[1] = r[0] = r[1] = h[0] = h[1] = 0;
  simd_align[0] = simd_align[1] = false;
  inplace[0] = inplace[1] = false;
  n[0] = n[1] = dim_vector ();

#if defined (HAVE_FFTW3_THREADS)
  int init_ret = fftw_init_threads ();
  if (!init_ret)
    (*current_liboctave_error_handler) ("Error initializing FFTW threads");
  //Use number of processors available to the current process
  //This can be later changed with fftw ("threads", nthreads)
  nthreads = num_processors (NPROC_CURRENT);
  fftw_plan_with_nthreads (nthreads);
#endif

  // If we have a system wide wisdom file, import it.
  fftw_import_system_wisdom ();
}

octave_fftw_planner::~octave_fftw_planner (void)
{
  fftw_plan *plan_p;

  plan_p = &rplan;
  if (*plan_p)
    fftw_destroy_plan (*plan_p);

  plan_p = &plan[0];
  if (*plan_p)
    fftw_destroy_plan (*plan_p);

  plan_p = &plan[1];
  if (*plan_p)
    fftw_destroy_plan (*plan_p);
}

bool
octave_fftw_planner::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_fftw_planner ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create octave_fftw_planner object!");

      retval = false;
    }

  return retval;
}

#define CHECK_SIMD_ALIGNMENT(x) \
  (((reinterpret_cast<ptrdiff_t> (x)) & 0xF) == 0)

fftw_plan
octave_fftw_planner::do_create_plan (int dir, const int rank,
                                     const dim_vector dims,
                                     octave_idx_type howmany,
                                     octave_idx_type stride,
                                     octave_idx_type dist,
                                     const Complex *in, Complex *out)
{
  int which = (dir == FFTW_FORWARD) ? 0 : 1;
  fftw_plan *cur_plan_p = &plan[which];
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);
  bool ioinplace = (in == out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (plan[which] == 0 || d[which] != dist || s[which] != stride
      || r[which] != rank || h[which] != howmany
      || ioinplace != inplace[which]
      || ((ioalign != simd_align[which]) ? !ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != n[which](i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      d[which] = dist;
      s[which] = stride;
      r[which] = rank;
      h[which] = howmany;
      simd_align[which] = ioalign;
      inplace[which] = ioinplace;
      n[which] = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftw_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (Complex, itmp, nn * howmany + 32);
          itmp = reinterpret_cast<Complex *>
                 (((reinterpret_cast<ptrdiff_t>(itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<ptrdiff_t> (in)) & 0xF));

          *cur_plan_p =
            fftw_plan_many_dft (rank, tmp, howmany,
                                reinterpret_cast<fftw_complex *> (itmp),
                                0, stride, dist,
                                reinterpret_cast<fftw_complex *> (out),
                                0, stride, dist, dir, plan_flags);
        }
      else
        {
          *cur_plan_p =
            fftw_plan_many_dft (rank, tmp, howmany,
                                reinterpret_cast<fftw_complex *> (const_cast<Complex *> (in)),
                                0, stride, dist,
                                reinterpret_cast<fftw_complex *> (out),
                                0, stride, dist, dir, plan_flags);
        }

      if (*cur_plan_p == 0)
        (*current_liboctave_error_handler) ("Error creating fftw plan");
    }

  return *cur_plan_p;
}

fftw_plan
octave_fftw_planner::do_create_plan (const int rank, const dim_vector dims,
                                     octave_idx_type howmany,
                                     octave_idx_type stride,
                                     octave_idx_type dist,
                                     const double *in, Complex *out)
{
  fftw_plan *cur_plan_p = &rplan;
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (rplan == 0 || rd != dist || rs != stride || rr != rank
      || rh != howmany || ((ioalign != rsimd_align) ? !ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != rn(i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      rd = dist;
      rs = stride;
      rr = rank;
      rh = howmany;
      rsimd_align = ioalign;
      rn = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftw_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (double, itmp, nn + 32);
          itmp = reinterpret_cast<double *>
                 (((reinterpret_cast<ptrdiff_t>(itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<ptrdiff_t> (in)) & 0xF));

          *cur_plan_p =
            fftw_plan_many_dft_r2c (rank, tmp, howmany, itmp,
                                    0, stride, dist,
                                    reinterpret_cast<fftw_complex *> (out),
                                    0, stride, dist, plan_flags);
        }
      else
        {
          *cur_plan_p =
            fftw_plan_many_dft_r2c (rank, tmp, howmany,
                                    (const_cast<double *> (in)),
                                    0, stride, dist,
                                    reinterpret_cast<fftw_complex *> (out),
                                    0, stride, dist, plan_flags);
        }

      if (*cur_plan_p == 0)
        (*current_liboctave_error_handler) ("Error creating fftw plan");
    }

  return *cur_plan_p;
}

octave_fftw_planner::FftwMethod
octave_fftw_planner::do_method (void)
{
  return meth;
}

octave_fftw_planner::FftwMethod
octave_fftw_planner::do_method (FftwMethod _meth)
{
  FftwMethod ret = meth;
  if (_meth == ESTIMATE || _meth == MEASURE
      || _meth == PATIENT || _meth == EXHAUSTIVE
      || _meth == HYBRID)
    {
      if (meth != _meth)
        {
          meth = _meth;
          if (rplan)
            fftw_destroy_plan (rplan);
          if (plan[0])
            fftw_destroy_plan (plan[0]);
          if (plan[1])
            fftw_destroy_plan (plan[1]);
          rplan = plan[0] = plan[1] = 0;
        }
    }
  else
    ret = UNKNOWN;
  return ret;
}

octave_float_fftw_planner *octave_float_fftw_planner::instance = 0;

octave_float_fftw_planner::octave_float_fftw_planner (void)
  : meth (ESTIMATE), rplan (0), rd (0), rs (0), rr (0), rh (0), rn (),
    rsimd_align (false)
{
  plan[0] = plan[1] = 0;
  d[0] = d[1] = s[0] = s[1] = r[0] = r[1] = h[0] = h[1] = 0;
  simd_align[0] = simd_align[1] = false;
  inplace[0] = inplace[1] = false;
  n[0] = n[1] = dim_vector ();

#if defined (HAVE_FFTW3F_THREADS)
  int init_ret = fftwf_init_threads ();
  if (!init_ret)
    (*current_liboctave_error_handler) ("Error initializing FFTW3F threads");
  //Use number of processors available to the current process
  //This can be later changed with fftw ("threads", nthreads)
  nthreads = num_processors (NPROC_CURRENT);
  fftwf_plan_with_nthreads (nthreads);
#endif

  // If we have a system wide wisdom file, import it.
  fftwf_import_system_wisdom ();
}

octave_float_fftw_planner::~octave_float_fftw_planner (void)
{
  fftwf_plan *plan_p;

  plan_p = &rplan;
  if (*plan_p)
    fftwf_destroy_plan (*plan_p);

  plan_p = &plan[0];
  if (*plan_p)
    fftwf_destroy_plan (*plan_p);

  plan_p = &plan[1];
  if (*plan_p)
    fftwf_destroy_plan (*plan_p);
}

bool
octave_float_fftw_planner::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_float_fftw_planner ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create octave_fftw_planner object!");

      retval = false;
    }

  return retval;
}

fftwf_plan
octave_float_fftw_planner::do_create_plan (int dir, const int rank,
                                           const dim_vector dims,
                                           octave_idx_type howmany,
                                           octave_idx_type stride,
                                           octave_idx_type dist,
                                           const FloatComplex *in,
                                           FloatComplex *out)
{
  int which = (dir == FFTW_FORWARD) ? 0 : 1;
  fftwf_plan *cur_plan_p = &plan[which];
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);
  bool ioinplace = (in == out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (plan[which] == 0 || d[which] != dist || s[which] != stride
      || r[which] != rank || h[which] != howmany
      || ioinplace != inplace[which]
      || ((ioalign != simd_align[which]) ? !ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != n[which](i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      d[which] = dist;
      s[which] = stride;
      r[which] = rank;
      h[which] = howmany;
      simd_align[which] = ioalign;
      inplace[which] = ioinplace;
      n[which] = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftwf_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (FloatComplex, itmp, nn * howmany + 32);
          itmp = reinterpret_cast<FloatComplex *>
                 (((reinterpret_cast<ptrdiff_t>(itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<ptrdiff_t> (in)) & 0xF));

          *cur_plan_p =
            fftwf_plan_many_dft (rank, tmp, howmany,
                                 reinterpret_cast<fftwf_complex *> (itmp),
                                 0, stride, dist,
                                 reinterpret_cast<fftwf_complex *> (out),
                                 0, stride, dist, dir, plan_flags);
        }
      else
        {
          *cur_plan_p =
            fftwf_plan_many_dft (rank, tmp, howmany,
                                 reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *> (in)),
                                 0, stride, dist,
                                 reinterpret_cast<fftwf_complex *> (out),
                                 0, stride, dist, dir, plan_flags);
        }

      if (*cur_plan_p == 0)
        (*current_liboctave_error_handler) ("Error creating fftw plan");
    }

  return *cur_plan_p;
}

fftwf_plan
octave_float_fftw_planner::do_create_plan (const int rank,
                                           const dim_vector dims,
                                           octave_idx_type howmany,
                                           octave_idx_type stride,
                                           octave_idx_type dist,
                                           const float *in, FloatComplex *out)
{
  fftwf_plan *cur_plan_p = &rplan;
  bool create_new_plan = false;
  bool ioalign = CHECK_SIMD_ALIGNMENT (in) && CHECK_SIMD_ALIGNMENT (out);

  // Don't create a new plan if we have a non SIMD plan already but
  // can do SIMD.  This prevents endlessly recreating plans if we
  // change the alignment.

  if (rplan == 0 || rd != dist || rs != stride || rr != rank
      || rh != howmany || ((ioalign != rsimd_align) ? !ioalign : false))
    create_new_plan = true;
  else
    {
      // We still might not have the same shape of array.

      for (int i = 0; i < rank; i++)
        if (dims(i) != rn(i))
          {
            create_new_plan = true;
            break;
          }
    }

  if (create_new_plan)
    {
      rd = dist;
      rs = stride;
      rr = rank;
      rh = howmany;
      rsimd_align = ioalign;
      rn = dims;

      // Note reversal of dimensions for column major storage in FFTW.
      octave_idx_type nn = 1;
      OCTAVE_LOCAL_BUFFER (int, tmp, rank);

      for (int i = 0, j = rank-1; i < rank; i++, j--)
        {
          tmp[i] = dims(j);
          nn *= dims(j);
        }

      int plan_flags = 0;
      bool plan_destroys_in = true;

      switch (meth)
        {
        case UNKNOWN:
        case ESTIMATE:
          plan_flags |= FFTW_ESTIMATE;
          plan_destroys_in = false;
          break;
        case MEASURE:
          plan_flags |= FFTW_MEASURE;
          break;
        case PATIENT:
          plan_flags |= FFTW_PATIENT;
          break;
        case EXHAUSTIVE:
          plan_flags |= FFTW_EXHAUSTIVE;
          break;
        case HYBRID:
          if (nn < 8193)
            plan_flags |= FFTW_MEASURE;
          else
            {
              plan_flags |= FFTW_ESTIMATE;
              plan_destroys_in = false;
            }
          break;
        }

      if (ioalign)
        plan_flags &= ~FFTW_UNALIGNED;
      else
        plan_flags |= FFTW_UNALIGNED;

      if (*cur_plan_p)
        fftwf_destroy_plan (*cur_plan_p);

      if (plan_destroys_in)
        {
          // Create matrix with the same size and 16-byte alignment as input
          OCTAVE_LOCAL_BUFFER (float, itmp, nn + 32);
          itmp = reinterpret_cast<float *>
                 (((reinterpret_cast<ptrdiff_t>(itmp) + 15) & ~ 0xF) +
                  ((reinterpret_cast<ptrdiff_t> (in)) & 0xF));

          *cur_plan_p =
            fftwf_plan_many_dft_r2c (rank, tmp, howmany, itmp,
                                     0, stride, dist,
                                     reinterpret_cast<fftwf_complex *> (out),
                                     0, stride, dist, plan_flags);
        }
      else
        {
          *cur_plan_p =
            fftwf_plan_many_dft_r2c (rank, tmp, howmany,
                                     (const_cast<float *> (in)),
                                     0, stride, dist,
                                     reinterpret_cast<fftwf_complex *> (out),
                                     0, stride, dist, plan_flags);
        }

      if (*cur_plan_p == 0)
        (*current_liboctave_error_handler) ("Error creating fftw plan");
    }

  return *cur_plan_p;
}

octave_float_fftw_planner::FftwMethod
octave_float_fftw_planner::do_method (void)
{
  return meth;
}

octave_float_fftw_planner::FftwMethod
octave_float_fftw_planner::do_method (FftwMethod _meth)
{
  FftwMethod ret = meth;
  if (_meth == ESTIMATE || _meth == MEASURE
      || _meth == PATIENT || _meth == EXHAUSTIVE
      || _meth == HYBRID)
    {
      if (meth != _meth)
        {
          meth = _meth;
          if (rplan)
            fftwf_destroy_plan (rplan);
          if (plan[0])
            fftwf_destroy_plan (plan[0]);
          if (plan[1])
            fftwf_destroy_plan (plan[1]);
          rplan = plan[0] = plan[1] = 0;
        }
    }
  else
    ret = UNKNOWN;
  return ret;
}

template <class T>
static inline void
convert_packcomplex_1d (T *out, size_t nr, size_t nc,
                        octave_idx_type stride, octave_idx_type dist)
{
  octave_quit ();

  // Fill in the missing data.

  for (size_t i = 0; i < nr; i++)
    for (size_t j = nc/2+1; j < nc; j++)
      out[j*stride + i*dist] = conj (out[(nc - j)*stride + i*dist]);

  octave_quit ();
}

template <class T>
static inline void
convert_packcomplex_Nd (T *out, const dim_vector &dv)
{
  size_t nc = dv(0);
  size_t nr = dv(1);
  size_t np = (dv.length () > 2 ? dv.numel () / nc / nr : 1);
  size_t nrp = nr * np;
  T *ptr1, *ptr2;

  octave_quit ();

  // Create space for the missing elements.

  for (size_t i = 0; i < nrp; i++)
    {
      ptr1 = out + i * (nc/2 + 1) + nrp*((nc-1)/2);
      ptr2 = out + i * nc;
      for (size_t j = 0; j < nc/2+1; j++)
        *ptr2++ = *ptr1++;
    }

  octave_quit ();

  // Fill in the missing data for the rank = 2 case directly for speed.

  for (size_t i = 0; i < np; i++)
    {
      for (size_t j = 1; j < nr; j++)
        for (size_t k = nc/2+1; k < nc; k++)
          out[k + (j + i*nr)*nc] = conj (out[nc - k + ((i+1)*nr - j)*nc]);

      for (size_t j = nc/2+1; j < nc; j++)
        out[j + i*nr*nc] = conj (out[(i*nr+1)*nc - j]);
    }

  octave_quit ();

  // Now do the permutations needed for rank > 2 cases.

  size_t jstart = dv(0) * dv(1);
  size_t kstep = dv(0);
  size_t nel = dv.numel ();

  for (int inner = 2; inner < dv.length (); inner++)
    {
      size_t jmax = jstart * dv(inner);
      for (size_t i = 0; i < nel; i+=jmax)
        for (size_t j = jstart, jj = jmax-jstart; j < jj;
             j+=jstart, jj-=jstart)
          for (size_t k = 0; k < jstart; k+= kstep)
            for (size_t l = nc/2+1; l < nc; l++)
              {
                T tmp = out[i+ j + k + l];
                out[i + j + k + l] = out[i + jj + k + l];
                out[i + jj + k + l] = tmp;
              }
      jstart = jmax;
    }

  octave_quit ();
}

int
octave_fftw::fft (const double *in, Complex *out, size_t npts,
                  size_t nsamples, octave_idx_type stride, octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  fftw_plan plan = octave_fftw_planner::create_plan (1, dv, nsamples,
                                                     stride, dist, in, out);

  fftw_execute_dft_r2c (plan, (const_cast<double *>(in)),
                        reinterpret_cast<fftw_complex *> (out));

  // Need to create other half of the transform.

  convert_packcomplex_1d (out, nsamples, npts, stride, dist);

  return 0;
}

int
octave_fftw::fft (const Complex *in, Complex *out, size_t npts,
                  size_t nsamples, octave_idx_type stride, octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  fftw_plan plan = octave_fftw_planner::create_plan (FFTW_FORWARD, 1, dv,
                                                     nsamples, stride,
                                                     dist, in, out);

  fftw_execute_dft (plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  return 0;
}

int
octave_fftw::ifft (const Complex *in, Complex *out, size_t npts,
                   size_t nsamples, octave_idx_type stride,
                   octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  fftw_plan plan = octave_fftw_planner::create_plan (FFTW_BACKWARD, 1, dv,
                                                     nsamples, stride,
                                                     dist, in, out);

  fftw_execute_dft (plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  const Complex scale = npts;
  for (size_t j = 0; j < nsamples; j++)
    for (size_t i = 0; i < npts; i++)
      out[i*stride + j*dist] /= scale;

  return 0;
}

int
octave_fftw::fftNd (const double *in, Complex *out, const int rank,
                    const dim_vector &dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  // Fool with the position of the start of the output matrix, so that
  // creating other half of the matrix won't cause cache problems.

  octave_idx_type offset = (dv.numel () / dv(0)) * ((dv(0) - 1) / 2);

  fftw_plan plan = octave_fftw_planner::create_plan (rank, dv, 1, 1, dist,
                                                     in, out + offset);

  fftw_execute_dft_r2c (plan, (const_cast<double *>(in)),
                        reinterpret_cast<fftw_complex *> (out+ offset));

  // Need to create other half of the transform.

  convert_packcomplex_Nd (out, dv);

  return 0;
}

int
octave_fftw::fftNd (const Complex *in, Complex *out, const int rank,
                    const dim_vector &dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  fftw_plan plan = octave_fftw_planner::create_plan (FFTW_FORWARD, rank,
                                                     dv, 1, 1, dist, in, out);

  fftw_execute_dft (plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  return 0;
}

int
octave_fftw::ifftNd (const Complex *in, Complex *out, const int rank,
                     const dim_vector &dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  fftw_plan plan = octave_fftw_planner::create_plan (FFTW_BACKWARD, rank,
                                                     dv, 1, 1, dist, in, out);

  fftw_execute_dft (plan,
                    reinterpret_cast<fftw_complex *> (const_cast<Complex *>(in)),
                    reinterpret_cast<fftw_complex *> (out));

  const size_t npts = dv.numel ();
  const Complex scale = npts;
  for (size_t i = 0; i < npts; i++)
    out[i] /= scale;

  return 0;
}

int
octave_fftw::fft (const float *in, FloatComplex *out, size_t npts,
                  size_t nsamples, octave_idx_type stride, octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  fftwf_plan plan = octave_float_fftw_planner::create_plan (1, dv, nsamples,
                                                            stride, dist,
                                                            in, out);

  fftwf_execute_dft_r2c (plan, (const_cast<float *>(in)),
                         reinterpret_cast<fftwf_complex *> (out));

  // Need to create other half of the transform.

  convert_packcomplex_1d (out, nsamples, npts, stride, dist);

  return 0;
}

int
octave_fftw::fft (const FloatComplex *in, FloatComplex *out, size_t npts,
                  size_t nsamples, octave_idx_type stride, octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  fftwf_plan plan = octave_float_fftw_planner::create_plan (FFTW_FORWARD, 1,
                                                            dv, nsamples,
                                                            stride, dist,
                                                            in, out);

  fftwf_execute_dft (plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  return 0;
}

int
octave_fftw::ifft (const FloatComplex *in, FloatComplex *out, size_t npts,
                   size_t nsamples, octave_idx_type stride,
                   octave_idx_type dist)
{
  dist = (dist < 0 ? npts : dist);

  dim_vector dv (npts, 1);
  fftwf_plan plan = octave_float_fftw_planner::create_plan (FFTW_BACKWARD, 1,
                                                            dv, nsamples,
                                                            stride, dist,
                                                            in, out);

  fftwf_execute_dft (plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  const FloatComplex scale = npts;
  for (size_t j = 0; j < nsamples; j++)
    for (size_t i = 0; i < npts; i++)
      out[i*stride + j*dist] /= scale;

  return 0;
}

int
octave_fftw::fftNd (const float *in, FloatComplex *out, const int rank,
                    const dim_vector &dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  // Fool with the position of the start of the output matrix, so that
  // creating other half of the matrix won't cause cache problems.

  octave_idx_type offset = (dv.numel () / dv(0)) * ((dv(0) - 1) / 2);

  fftwf_plan plan = octave_float_fftw_planner::create_plan (rank, dv, 1, 1,
                                                            dist, in,
                                                            out + offset);

  fftwf_execute_dft_r2c (plan, (const_cast<float *>(in)),
                         reinterpret_cast<fftwf_complex *> (out+ offset));

  // Need to create other half of the transform.

  convert_packcomplex_Nd (out, dv);

  return 0;
}

int
octave_fftw::fftNd (const FloatComplex *in, FloatComplex *out, const int rank,
                    const dim_vector &dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  fftwf_plan plan = octave_float_fftw_planner::create_plan (FFTW_FORWARD,
                                                            rank, dv, 1, 1,
                                                            dist, in, out);

  fftwf_execute_dft (plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  return 0;
}

int
octave_fftw::ifftNd (const FloatComplex *in, FloatComplex *out, const int rank,
                     const dim_vector &dv)
{
  octave_idx_type dist = 1;
  for (int i = 0; i < rank; i++)
    dist *= dv(i);

  fftwf_plan plan = octave_float_fftw_planner::create_plan (FFTW_BACKWARD,
                                                            rank, dv, 1, 1,
                                                            dist, in, out);

  fftwf_execute_dft (plan,
                     reinterpret_cast<fftwf_complex *> (const_cast<FloatComplex *>(in)),
                     reinterpret_cast<fftwf_complex *> (out));

  const size_t npts = dv.numel ();
  const FloatComplex scale = npts;
  for (size_t i = 0; i < npts; i++)
    out[i] /= scale;

  return 0;
}

#endif
