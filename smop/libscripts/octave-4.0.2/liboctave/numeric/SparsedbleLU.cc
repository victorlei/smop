/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#include <vector>

#include "lo-error.h"
#include "oct-locbuf.h"

#include "SparsedbleLU.h"
#include "oct-spparms.h"

// Instantiate the base LU class for the types we need.

#include "sparse-base-lu.h"
#include "sparse-base-lu.cc"

template class sparse_base_lu <SparseMatrix, double, SparseMatrix, double>;

#include "oct-sparse.h"

SparseLU::SparseLU (const SparseMatrix& a, const Matrix& piv_thres, bool scale)
{
#ifdef HAVE_UMFPACK
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  UMFPACK_DNAME (defaults) (control);

  double tmp = octave_sparse_params::get_key ("spumoni");
  if (!xisnan (tmp))
    Control (UMFPACK_PRL) = tmp;

  if (piv_thres.nelem () == 2)
    {
      tmp = (piv_thres (0) > 1. ? 1. : piv_thres (0));
      if (!xisnan (tmp))
        Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
      tmp = (piv_thres (1) > 1. ? 1. : piv_thres (1));
      if (!xisnan (tmp))
        Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
    }
  else
    {
      tmp = octave_sparse_params::get_key ("piv_tol");
      if (!xisnan (tmp))
        Control (UMFPACK_PIVOT_TOLERANCE) = tmp;

      tmp = octave_sparse_params::get_key ("sym_tol");
      if (!xisnan (tmp))
        Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
    }

  // Set whether we are allowed to modify Q or not
  tmp = octave_sparse_params::get_key ("autoamd");
  if (!xisnan (tmp))
    Control (UMFPACK_FIXQ) = tmp;

  if (scale)
    Control (UMFPACK_SCALE) = UMFPACK_SCALE_SUM;
  else
    Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

  UMFPACK_DNAME (report_control) (control);

  const octave_idx_type *Ap = a.cidx ();
  const octave_idx_type *Ai = a.ridx ();
  const double *Ax = a.data ();

  UMFPACK_DNAME (report_matrix) (nr, nc, Ap, Ai, Ax, 1, control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status = UMFPACK_DNAME (qsymbolic) (nr, nc, Ap, Ai, Ax, 0,
                                          &Symbolic, control, info);

  if (status < 0)
    {
      (*current_liboctave_error_handler)
        ("SparseLU::SparseLU symbolic factorization failed");

      UMFPACK_DNAME (report_status) (control, status);
      UMFPACK_DNAME (report_info) (control, info);

      UMFPACK_DNAME (free_symbolic) (&Symbolic) ;
    }
  else
    {
      UMFPACK_DNAME (report_symbolic) (Symbolic, control);

      void *Numeric;
      status = UMFPACK_DNAME (numeric) (Ap, Ai, Ax, Symbolic,
                                        &Numeric, control, info) ;
      UMFPACK_DNAME (free_symbolic) (&Symbolic) ;

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
        {
          (*current_liboctave_error_handler)
            ("SparseLU::SparseLU numeric factorization failed");

          UMFPACK_DNAME (report_status) (control, status);
          UMFPACK_DNAME (report_info) (control, info);

          UMFPACK_DNAME (free_numeric) (&Numeric);
        }
      else
        {
          UMFPACK_DNAME (report_numeric) (Numeric, control);

          octave_idx_type lnz, unz, ignore1, ignore2, ignore3;
          status = UMFPACK_DNAME (get_lunz) (&lnz, &unz, &ignore1,
                                             &ignore2, &ignore3, Numeric) ;

          if (status < 0)
            {
              (*current_liboctave_error_handler)
                ("SparseLU::SparseLU extracting LU factors failed");

              UMFPACK_DNAME (report_status) (control, status);
              UMFPACK_DNAME (report_info) (control, info);

              UMFPACK_DNAME (free_numeric) (&Numeric);
            }
          else
            {
              octave_idx_type n_inner = (nr < nc ? nr : nc);

              if (lnz < 1)
                Lfact = SparseMatrix (n_inner, nr,
                                      static_cast<octave_idx_type> (1));
              else
                Lfact = SparseMatrix (n_inner, nr, lnz);

              octave_idx_type *Ltp = Lfact.cidx ();
              octave_idx_type *Ltj = Lfact.ridx ();
              double *Ltx = Lfact.data ();

              if (unz < 1)
                Ufact = SparseMatrix (n_inner, nc,
                                      static_cast<octave_idx_type> (1));
              else
                Ufact = SparseMatrix (n_inner, nc, unz);

              octave_idx_type *Up = Ufact.cidx ();
              octave_idx_type *Uj = Ufact.ridx ();
              double *Ux = Ufact.data ();

              Rfact = SparseMatrix (nr, nr, nr);
              for (octave_idx_type i = 0; i < nr; i++)
                {
                  Rfact.xridx (i) = i;
                  Rfact.xcidx (i) = i;
                }
              Rfact.xcidx (nr) = nr;
              double *Rx = Rfact.data ();

              P.resize (dim_vector (nr, 1));
              octave_idx_type *p = P.fortran_vec ();

              Q.resize (dim_vector (nc, 1));
              octave_idx_type *q = Q.fortran_vec ();

              octave_idx_type do_recip;
              status = UMFPACK_DNAME (get_numeric) (Ltp, Ltj, Ltx,
                                                    Up, Uj, Ux, p, q, 0,
                                                    &do_recip, Rx,
                                                    Numeric) ;

              UMFPACK_DNAME (free_numeric) (&Numeric) ;

              if (status < 0)
                {
                  (*current_liboctave_error_handler)
                    ("SparseLU::SparseLU extracting LU factors failed");

                  UMFPACK_DNAME (report_status) (control, status);
                }
              else
                {
                  Lfact = Lfact.transpose ();

                  if (do_recip)
                    for (octave_idx_type i = 0; i < nr; i++)
                      Rx[i] = 1.0 / Rx[i];

                  UMFPACK_DNAME (report_matrix) (nr, n_inner,
                                                 Lfact.cidx (), Lfact.ridx (),
                                                 Lfact.data (), 1, control);
                  UMFPACK_DNAME (report_matrix) (n_inner, nc,
                                                 Ufact.cidx (), Ufact.ridx (),
                                                 Ufact.data (), 1, control);
                  UMFPACK_DNAME (report_perm) (nr, p, control);
                  UMFPACK_DNAME (report_perm) (nc, q, control);
                }

              UMFPACK_DNAME (report_info) (control, info);
            }
        }
    }
#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
}

SparseLU::SparseLU (const SparseMatrix& a, const ColumnVector& Qinit,
                    const Matrix& piv_thres, bool scale, bool FixedQ,
                    double droptol, bool milu, bool udiag)
{
#ifdef HAVE_UMFPACK
  if (milu)
    (*current_liboctave_error_handler)
      ("Modified incomplete LU not implemented");
  else
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.cols ();

      // Setup the control parameters
      Matrix Control (UMFPACK_CONTROL, 1);
      double *control = Control.fortran_vec ();
      UMFPACK_DNAME (defaults) (control);

      double tmp = octave_sparse_params::get_key ("spumoni");
      if (!xisnan (tmp))
        Control (UMFPACK_PRL) = tmp;

      if (piv_thres.nelem () == 2)
        {
          tmp = (piv_thres (0) > 1. ? 1. : piv_thres (0));
          if (!xisnan (tmp))
            Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
          tmp = (piv_thres (1) > 1. ? 1. : piv_thres (1));
          if (!xisnan (tmp))
            Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
        }
      else
        {
          tmp = octave_sparse_params::get_key ("piv_tol");
          if (!xisnan (tmp))
            Control (UMFPACK_PIVOT_TOLERANCE) = tmp;

          tmp = octave_sparse_params::get_key ("sym_tol");
          if (!xisnan (tmp))
            Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
        }

      if (droptol >= 0.)
        Control (UMFPACK_DROPTOL) = droptol;


      // Set whether we are allowed to modify Q or not
      if (FixedQ)
        Control (UMFPACK_FIXQ) = 1.0;
      else
        {
          tmp = octave_sparse_params::get_key ("autoamd");
          if (!xisnan (tmp))
            Control (UMFPACK_FIXQ) = tmp;
        }

      if (scale)
        Control (UMFPACK_SCALE) = UMFPACK_SCALE_SUM;
      else
        Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

      UMFPACK_DNAME (report_control) (control);

      const octave_idx_type *Ap = a.cidx ();
      const octave_idx_type *Ai = a.ridx ();
      const double *Ax = a.data ();

      UMFPACK_DNAME (report_matrix) (nr, nc, Ap, Ai, Ax, 1,
                                     control);

      void *Symbolic;
      Matrix Info (1, UMFPACK_INFO);
      double *info = Info.fortran_vec ();
      int status;

      // Null loop so that qinit is imediately deallocated when not needed
      do
        {
          OCTAVE_LOCAL_BUFFER (octave_idx_type, qinit, nc);

          for (octave_idx_type i = 0; i < nc; i++)
            qinit[i] = static_cast<octave_idx_type> (Qinit (i));

          status = UMFPACK_DNAME (qsymbolic) (nr, nc, Ap, Ai, Ax,
                                              qinit, &Symbolic, control, info);
        }
      while (0);

      if (status < 0)
        {
          (*current_liboctave_error_handler)
            ("SparseLU::SparseLU symbolic factorization failed");

          UMFPACK_DNAME (report_status) (control, status);
          UMFPACK_DNAME (report_info) (control, info);

          UMFPACK_DNAME (free_symbolic) (&Symbolic) ;
        }
      else
        {
          UMFPACK_DNAME (report_symbolic) (Symbolic, control);

          void *Numeric;
          status = UMFPACK_DNAME (numeric) (Ap, Ai, Ax, Symbolic,
                                            &Numeric, control, info) ;
          UMFPACK_DNAME (free_symbolic) (&Symbolic) ;

          cond = Info (UMFPACK_RCOND);

          if (status < 0)
            {
              (*current_liboctave_error_handler)
                ("SparseLU::SparseLU numeric factorization failed");

              UMFPACK_DNAME (report_status) (control, status);
              UMFPACK_DNAME (report_info) (control, info);

              UMFPACK_DNAME (free_numeric) (&Numeric);
            }
          else
            {
              UMFPACK_DNAME (report_numeric) (Numeric, control);

              octave_idx_type lnz, unz, ignore1, ignore2, ignore3;
              status = UMFPACK_DNAME (get_lunz) (&lnz, &unz,
                                                 &ignore1, &ignore2, &ignore3,
                                                 Numeric) ;

              if (status < 0)
                {
                  (*current_liboctave_error_handler)
                    ("SparseLU::SparseLU extracting LU factors failed");

                  UMFPACK_DNAME (report_status) (control, status);
                  UMFPACK_DNAME (report_info) (control, info);

                  UMFPACK_DNAME (free_numeric) (&Numeric);
                }
              else
                {
                  octave_idx_type n_inner = (nr < nc ? nr : nc);

                  if (lnz < 1)
                    Lfact = SparseMatrix (n_inner, nr,
                                          static_cast<octave_idx_type> (1));
                  else
                    Lfact = SparseMatrix (n_inner, nr, lnz);

                  octave_idx_type *Ltp = Lfact.cidx ();
                  octave_idx_type *Ltj = Lfact.ridx ();
                  double *Ltx = Lfact.data ();

                  if (unz < 1)
                    Ufact = SparseMatrix (n_inner, nc,
                                          static_cast<octave_idx_type> (1));
                  else
                    Ufact = SparseMatrix (n_inner, nc, unz);

                  octave_idx_type *Up = Ufact.cidx ();
                  octave_idx_type *Uj = Ufact.ridx ();
                  double *Ux = Ufact.data ();

                  Rfact = SparseMatrix (nr, nr, nr);
                  for (octave_idx_type i = 0; i < nr; i++)
                    {
                      Rfact.xridx (i) = i;
                      Rfact.xcidx (i) = i;
                    }
                  Rfact.xcidx (nr) = nr;
                  double *Rx = Rfact.data ();

                  P.resize (dim_vector (nr, 1));
                  octave_idx_type *p = P.fortran_vec ();

                  Q.resize (dim_vector (nc, 1));
                  octave_idx_type *q = Q.fortran_vec ();

                  octave_idx_type do_recip;
                  status = UMFPACK_DNAME (get_numeric) (Ltp, Ltj,
                                                        Ltx, Up, Uj, Ux, p, q,
                                                        0, &do_recip,
                                                        Rx, Numeric) ;

                  UMFPACK_DNAME (free_numeric) (&Numeric) ;

                  if (status < 0)
                    {
                      (*current_liboctave_error_handler)
                        ("SparseLU::SparseLU extracting LU factors failed");

                      UMFPACK_DNAME (report_status) (control, status);
                    }
                  else
                    {
                      Lfact = Lfact.transpose ();

                      if (do_recip)
                        for (octave_idx_type i = 0; i < nr; i++)
                          Rx[i] = 1.0 / Rx[i];

                      UMFPACK_DNAME (report_matrix) (nr, n_inner,
                                                     Lfact.cidx (),
                                                     Lfact.ridx (),
                                                     Lfact.data (),
                                                     1, control);
                      UMFPACK_DNAME (report_matrix) (n_inner, nc,
                                                     Ufact.cidx (),
                                                     Ufact.ridx (),
                                                     Ufact.data (),
                                                     1, control);
                      UMFPACK_DNAME (report_perm) (nr, p, control);
                      UMFPACK_DNAME (report_perm) (nc, q, control);
                    }

                  UMFPACK_DNAME (report_info) (control, info);
                }
            }
        }

      if (udiag)
        (*current_liboctave_error_handler)
          ("Option udiag of incomplete LU not implemented");
    }
#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
}
