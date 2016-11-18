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

#include <functional>

#include "quit.h"
#include "lo-error.h"
#include "MArray.h"
#include "Array-util.h"

#include "MSparse.h"
#include "MSparse-defs.h"

// sparse array with math ops.

// Element by element MSparse by MSparse ops.

template <class T, class OP>
MSparse<T>&
plus_or_minus (MSparse<T>& a, const MSparse<T>& b, OP op, const char* op_name)
{
  MSparse<T> r;

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr != b_nr || a_nc != b_nc)
    gripe_nonconformant (op_name , a_nr, a_nc, b_nr, b_nc);
  else
    {
      r = MSparse<T> (a_nr, a_nc, (a.nnz () + b.nnz ()));

      octave_idx_type jx = 0;
      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type  ja = a.cidx (i);
          octave_idx_type  ja_max = a.cidx (i+1);
          bool ja_lt_max= ja < ja_max;

          octave_idx_type  jb = b.cidx (i);
          octave_idx_type  jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.ridx (jx) = a.ridx (ja);
                  r.data (jx) = op (a.data (ja), 0.);
                  jx++;
                  ja++;
                  ja_lt_max= ja < ja_max;
                }
              else if ((! ja_lt_max)
                       || (jb_lt_max && (b.ridx (jb) < a.ridx (ja))))
                {
                  r.ridx (jx) = b.ridx (jb);
                  r.data (jx) = op (0., b.data (jb));
                  jx++;
                  jb++;
                  jb_lt_max= jb < jb_max;
                }
              else
                {
                  if (op (a.data (ja), b.data (jb)) != 0.)
                    {
                      r.data (jx) = op (a.data (ja), b.data (jb));
                      r.ridx (jx) = a.ridx (ja);
                      jx++;
                    }
                  ja++;
                  ja_lt_max= ja < ja_max;
                  jb++;
                  jb_lt_max= jb < jb_max;
                }
            }
          r.cidx (i+1) = jx;
        }

      a = r.maybe_compress ();
    }

  return a;
}

template <typename T>
MSparse<T>&
operator += (MSparse<T>& a, const MSparse<T>& b)
{
  return plus_or_minus (a, b, std::plus<T> (), "operator +=");
}

template <typename T>
MSparse<T>&
operator -= (MSparse<T>& a, const MSparse<T>& b)
{
  return plus_or_minus (a, b, std::minus<T> (), "operator -=");
}


// Element by element MSparse by scalar ops.

template <class T, class OP>
MArray<T>
plus_or_minus (const MSparse<T>& a, const T& s, OP op)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  MArray<T> r (dim_vector (nr, nc), op (0.0, s));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
      r.elem (a.ridx (i), j) = op (a.data (i), s);
  return r;
}

template <typename T>
MArray<T>
operator + (const MSparse<T>& a, const T& s)
{
  return plus_or_minus (a, s, std::plus<T> ());
}

template <typename T>
MArray<T>
operator - (const MSparse<T>& a, const T& s)
{
  return plus_or_minus (a, s, std::minus<T> ());
}


template <class T, class OP>
MSparse<T>
times_or_divide (const MSparse<T>& a, const T& s, OP op)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nnz ();

  MSparse<T> r (nr, nc, nz);

  for (octave_idx_type i = 0; i < nz; i++)
    {
      r.data (i) = op (a.data (i), s);
      r.ridx (i) = a.ridx (i);
    }
  for (octave_idx_type i = 0; i < nc + 1; i++)
    r.cidx (i) = a.cidx (i);
  r.maybe_compress (true);
  return r;
}

template <typename T>
MSparse<T>
operator * (const MSparse<T>& a, const T& s)
{
  return times_or_divide (a, s, std::multiplies<T> ());
}

template <typename T>
MSparse<T>
operator / (const MSparse<T>& a, const T& s)
{
  return times_or_divide (a, s, std::divides<T> ());
}


// Element by element scalar by MSparse ops.

template <class T, class OP>
MArray<T>
plus_or_minus (const T& s, const MSparse<T>& a, OP op)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  MArray<T> r (dim_vector (nr, nc), op (s, 0.0));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
      r.elem (a.ridx (i), j) = op (s, a.data (i));
  return r;
}

template <typename T>
MArray<T>
operator + (const T& s, const MSparse<T>& a)
{
  return plus_or_minus (s, a, std::plus<T> ());
}

template <typename T>
MArray<T>
operator - (const T& s, const MSparse<T>& a)
{
  return plus_or_minus (s, a, std::minus<T> ());
}

template <class T, class OP>
MSparse<T>
times_or_divides (const T& s, const MSparse<T>& a, OP op)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();
  octave_idx_type nz = a.nnz ();

  MSparse<T> r (nr, nc, nz);

  for (octave_idx_type i = 0; i < nz; i++)
    {
      r.data (i) = op (s, a.data (i));
      r.ridx (i) = a.ridx (i);
    }
  for (octave_idx_type i = 0; i < nc + 1; i++)
    r.cidx (i) = a.cidx (i);
  r.maybe_compress (true);
  return r;
}

template <class T>
MSparse<T>
operator * (const T& s, const MSparse<T>& a)
{
  return times_or_divides (s, a, std::multiplies<T> ());
}

template <class T>
MSparse<T>
operator / (const T& s, const MSparse<T>& a)
{
  return times_or_divides (s, a, std::divides<T> ());
}


// Element by element MSparse by MSparse ops.

template <class T, class OP>
MSparse<T>
plus_or_minus (const MSparse<T>& a, const MSparse<T>& b, OP op,
               const char* op_name, bool negate)
{
  MSparse<T> r;

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 1 && a_nc == 1)
    {
      if (a.elem (0,0) == 0.)
        if (negate)
          r = -MSparse<T> (b);
        else
          r = MSparse<T> (b);
      else
        {
          r = MSparse<T> (b_nr, b_nc, op (a.data (0), 0.));

          for (octave_idx_type j = 0 ; j < b_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * b_nr;
              for (octave_idx_type i = b.cidx (j) ; i < b.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + b.ridx (i)) = op (a.data (0), b.data (i));
                }
            }
          r.maybe_compress ();
        }
    }
  else if (b_nr == 1 && b_nc == 1)
    {
      if (b.elem (0,0) == 0.)
        r = MSparse<T> (a);
      else
        {
          r = MSparse<T> (a_nr, a_nc, op (0.0, b.data (0)));

          for (octave_idx_type j = 0 ; j < a_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * a_nr;
              for (octave_idx_type i = a.cidx (j) ; i < a.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + a.ridx (i)) = op (a.data (i), b.data (0));
                }
            }
          r.maybe_compress ();
        }
    }
  else if (a_nr != b_nr || a_nc != b_nc)
    gripe_nonconformant (op_name, a_nr, a_nc, b_nr, b_nc);
  else
    {
      r = MSparse<T> (a_nr, a_nc, (a.nnz () + b.nnz ()));

      octave_idx_type jx = 0;
      r.cidx (0) = 0;
      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type  ja = a.cidx (i);
          octave_idx_type  ja_max = a.cidx (i+1);
          bool ja_lt_max= ja < ja_max;

          octave_idx_type  jb = b.cidx (i);
          octave_idx_type  jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.ridx (jx) = a.ridx (ja);
                  r.data (jx) = op (a.data (ja), 0.);
                  jx++;
                  ja++;
                  ja_lt_max= ja < ja_max;
                }
              else if ((! ja_lt_max)
                       || (jb_lt_max && (b.ridx (jb) < a.ridx (ja))))
                {
                  r.ridx (jx) = b.ridx (jb);
                  r.data (jx) = op (0.,  b.data (jb));
                  jx++;
                  jb++;
                  jb_lt_max= jb < jb_max;
                }
              else
                {
                  if (op (a.data (ja), b.data (jb)) != 0.)
                    {
                      r.data (jx) = op (a.data (ja), b.data (jb));
                      r.ridx (jx) = a.ridx (ja);
                      jx++;
                    }
                  ja++;
                  ja_lt_max= ja < ja_max;
                  jb++;
                  jb_lt_max= jb < jb_max;
                }
            }
          r.cidx (i+1) = jx;
        }

      r.maybe_compress ();
    }

  return r;
}

template <class T>
MSparse<T>
operator+ (const MSparse<T>& a, const MSparse<T>& b)
{
  return plus_or_minus (a, b, std::plus<T> (), "operator +", false);
}

template <class T>
MSparse<T>
operator- (const MSparse<T>& a, const MSparse<T>& b)
{
  return plus_or_minus (a, b, std::minus<T> (), "operator -", true);
}

template <class T>
MSparse<T>
product (const MSparse<T>& a, const MSparse<T>& b)
{
  MSparse<T> r;

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 1 && a_nc == 1)
    {
      if (a.elem (0,0) == 0.)
        r = MSparse<T> (b_nr, b_nc);
      else
        {
          r = MSparse<T> (b);
          octave_idx_type b_nnz = b.nnz ();

          for (octave_idx_type i = 0 ; i < b_nnz ; i++)
            {
              octave_quit ();
              r.data (i) = a.data (0) * r.data (i);
            }
          r.maybe_compress ();
        }
    }
  else if (b_nr == 1 && b_nc == 1)
    {
      if (b.elem (0,0) == 0.)
        r = MSparse<T> (a_nr, a_nc);
      else
        {
          r = MSparse<T> (a);
          octave_idx_type a_nnz = a.nnz ();

          for (octave_idx_type i = 0 ; i < a_nnz ; i++)
            {
              octave_quit ();
              r.data (i) = r.data (i) * b.data (0);
            }
          r.maybe_compress ();
        }
    }
  else if (a_nr != b_nr || a_nc != b_nc)
    gripe_nonconformant ("product", a_nr, a_nc, b_nr, b_nc);
  else
    {
      r = MSparse<T> (a_nr, a_nc, (a.nnz () > b.nnz () ? a.nnz () : b.nnz ()));

      octave_idx_type jx = 0;
      r.cidx (0) = 0;
      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type  ja = a.cidx (i);
          octave_idx_type  ja_max = a.cidx (i+1);
          bool ja_lt_max= ja < ja_max;

          octave_idx_type  jb = b.cidx (i);
          octave_idx_type  jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  ja++; ja_lt_max= ja < ja_max;
                }
              else if ((! ja_lt_max)
                       || (jb_lt_max && (b.ridx (jb) < a.ridx (ja))))
                {
                  jb++; jb_lt_max= jb < jb_max;
                }
              else
                {
                  if ((a.data (ja) * b.data (jb)) != 0.)
                    {
                      r.data (jx) = a.data (ja) * b.data (jb);
                      r.ridx (jx) = a.ridx (ja);
                      jx++;
                    }
                  ja++; ja_lt_max= ja < ja_max;
                  jb++; jb_lt_max= jb < jb_max;
                }
            }
          r.cidx (i+1) = jx;
        }

      r.maybe_compress ();
    }

  return r;
}

template <class T>
MSparse<T>
quotient (const MSparse<T>& a, const MSparse<T>& b)
{
  MSparse<T> r;
  T Zero = T ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (a_nr == 1 && a_nc == 1)
    {
      T val = a.elem (0,0);
      T fill = val / T ();
      if (fill == T ())
        {
          octave_idx_type b_nnz = b.nnz ();
          r = MSparse<T> (b);
          for (octave_idx_type i = 0 ; i < b_nnz ; i++)
            r.data (i) = val / r.data (i);
          r.maybe_compress ();
        }
      else
        {
          r = MSparse<T> (b_nr, b_nc, fill);
          for (octave_idx_type j = 0 ; j < b_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * b_nr;
              for (octave_idx_type i = b.cidx (j) ; i < b.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + b.ridx (i)) = val / b.data (i);
                }
            }
          r.maybe_compress ();
        }
    }
  else if (b_nr == 1 && b_nc == 1)
    {
      T val = b.elem (0,0);
      T fill = T () / val;
      if (fill == T ())
        {
          octave_idx_type a_nnz = a.nnz ();
          r = MSparse<T> (a);
          for (octave_idx_type i = 0 ; i < a_nnz ; i++)
            r.data (i) = r.data (i) / val;
          r.maybe_compress ();
        }
      else
        {
          r = MSparse<T> (a_nr, a_nc, fill);
          for (octave_idx_type j = 0 ; j < a_nc ; j++)
            {
              octave_quit ();
              octave_idx_type idxj = j * a_nr;
              for (octave_idx_type i = a.cidx (j) ; i < a.cidx (j+1) ; i++)
                {
                  octave_quit ();
                  r.data (idxj + a.ridx (i)) = a.data (i) / val;
                }
            }
          r.maybe_compress ();
        }
    }
  else if (a_nr != b_nr || a_nc != b_nc)
    gripe_nonconformant ("quotient", a_nr, a_nc, b_nr, b_nc);
  else
    {
      r = MSparse<T> (a_nr, a_nc, (Zero / Zero));

      for (octave_idx_type i = 0 ; i < a_nc ; i++)
        {
          octave_idx_type  ja = a.cidx (i);
          octave_idx_type  ja_max = a.cidx (i+1);
          bool ja_lt_max= ja < ja_max;

          octave_idx_type  jb = b.cidx (i);
          octave_idx_type  jb_max = b.cidx (i+1);
          bool jb_lt_max = jb < jb_max;

          while (ja_lt_max || jb_lt_max)
            {
              octave_quit ();
              if ((! jb_lt_max) || (ja_lt_max && (a.ridx (ja) < b.ridx (jb))))
                {
                  r.elem (a.ridx (ja),i) = a.data (ja) / Zero;
                  ja++; ja_lt_max= ja < ja_max;
                }
              else if ((! ja_lt_max)
                       || (jb_lt_max && (b.ridx (jb) < a.ridx (ja))))
                {
                  r.elem (b.ridx (jb),i) = Zero / b.data (jb);
                  jb++; jb_lt_max= jb < jb_max;
                }
              else
                {
                  r.elem (a.ridx (ja),i) = a.data (ja) / b.data (jb);
                  ja++; ja_lt_max= ja < ja_max;
                  jb++; jb_lt_max= jb < jb_max;
                }
            }
        }

      r.maybe_compress (true);
    }

  return r;
}



// Unary MSparse ops.

template <class T>
MSparse<T>
operator + (const MSparse<T>& a)
{
  return a;
}

template <class T>
MSparse<T>
operator - (const MSparse<T>& a)
{
  MSparse<T> retval (a);
  octave_idx_type nz = a.nnz ();
  for (octave_idx_type i = 0; i < nz; i++)
    retval.data (i) = - retval.data (i);
  return retval;
}
