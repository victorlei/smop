/* -*- C++ -*-

Copyright (C) 2009-2015 Jason Riedy, Jaroslav Hajek

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

#if !defined (octave_Sparse_diag_op_defs_h)
#define octave_Sparse_diag_op_defs_h 1

// Matrix multiplication

template <typename RT, typename DM, typename SM>
RT do_mul_dm_sm (const DM& d, const SM& a)
{
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  if (nc != a_nr)
    {
      gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc);
      return RT ();
    }
  else
    {
      RT r (nr, a_nc, a.nnz ());

      octave_idx_type l = 0;

      for (octave_idx_type j = 0; j < a_nc; j++)
        {
          r.xcidx (j) = l;
          const octave_idx_type colend = a.cidx (j+1);
          for (octave_idx_type k = a.cidx (j); k < colend; k++)
            {
              const octave_idx_type i = a.ridx (k);
              if (i >= nr) break;
              r.xdata (l) = d.dgelem (i) * a.data (k);
              r.xridx (l) = i;
              l++;
            }
        }

      r.xcidx (a_nc) = l;

      r.maybe_compress (true);
      return r;
    }
}

template <typename RT, typename SM, typename DM>
RT do_mul_sm_dm (const SM& a, const DM& d)
{
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  if (nr != a_nc)
    {
      gripe_nonconformant ("operator *", a_nr, a_nc, nr, nc);
      return RT ();
    }
  else
    {

      const octave_idx_type mnc = nc < a_nc ? nc: a_nc;
      RT r (a_nr, nc, a.cidx (mnc));

      for (octave_idx_type j = 0; j < mnc; ++j)
        {
          const typename DM::element_type s = d.dgelem (j);
          const octave_idx_type colend = a.cidx (j+1);
          r.xcidx (j) = a.cidx (j);
          for (octave_idx_type k = a.cidx (j); k < colend; ++k)
            {
              r.xdata (k) = s * a.data (k);
              r.xridx (k) = a.ridx (k);
            }
        }
      for (octave_idx_type j = mnc; j <= nc; ++j)
        r.xcidx (j) = a.cidx (mnc);

      r.maybe_compress (true);
      return r;
    }
}

// FIXME: functors such as this should be gathered somewhere
template <typename T>
struct identity_val
  : public std::unary_function <T, T>
{
  T operator () (const T x) { return x; }
};

// Matrix addition

template <typename RT, typename SM, typename DM, typename OpA, typename OpD>
RT inner_do_add_sm_dm (const SM& a, const DM& d, OpA opa, OpD opd)
{
  using std::min;
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();
  const octave_idx_type n = min (nr, nc);

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  const octave_idx_type nz = a.nnz ();
  RT r (a_nr, a_nc, nz + n);
  octave_idx_type k = 0;

  for (octave_idx_type j = 0; j < nc; ++j)
    {
      octave_quit ();
      const octave_idx_type colend = a.cidx (j+1);
      r.xcidx (j) = k;
      octave_idx_type k_src = a.cidx (j), k_split;

      for (k_split = k_src; k_split < colend; k_split++)
        if (a.ridx (k_split) >= j)
          break;

      for (; k_src < k_split; k_src++, k++)
        {
          r.xridx (k) = a.ridx (k_src);
          r.xdata (k) = opa (a.data (k_src));
        }

      if (k_src < colend && a.ridx (k_src) == j)
        {
          r.xridx (k) = j;
          r.xdata (k) = opa (a.data (k_src)) + opd (d.dgelem (j));
          k++; k_src++;
        }
      else
        {
          r.xridx (k) = j;
          r.xdata (k) = opd (d.dgelem (j));
          k++;
        }

      for (; k_src < colend; k_src++, k++)
        {
          r.xridx (k) = a.ridx (k_src);
          r.xdata (k) = opa (a.data (k_src));
        }

    }
  r.xcidx (nc) = k;

  r.maybe_compress (true);
  return r;
}

template <typename RT, typename DM, typename SM>
RT do_commutative_add_dm_sm (const DM& d, const SM& a)
{
  // Extra function to ensure this is only emitted once.
  return inner_do_add_sm_dm<RT> (a, d,
                                 identity_val<typename SM::element_type> (),
                                 identity_val<typename DM::element_type> ());
}

template <typename RT, typename DM, typename SM>
RT do_add_dm_sm (const DM& d, const SM& a)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    {
      gripe_nonconformant ("operator +", d.rows (), d.cols (), a.rows (), a.cols ());
      return RT ();
    }
  else
    return do_commutative_add_dm_sm<RT> (d, a);
}

template <typename RT, typename DM, typename SM>
RT do_sub_dm_sm (const DM& d, const SM& a)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    {
      gripe_nonconformant ("operator -", d.rows (), d.cols (), a.rows (), a.cols ());
      return RT ();
    }
  else
    return inner_do_add_sm_dm<RT> (a, d, std::negate<typename SM::element_type> (),
                                   identity_val<typename DM::element_type> ());
}

template <typename RT, typename SM, typename DM>
RT do_add_sm_dm (const SM& a, const DM& d)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    {
      gripe_nonconformant ("operator +", a.rows (), a.cols (), d.rows (), d.cols ());
      return RT ();
    }
  else
    return do_commutative_add_dm_sm<RT> (d, a);
}

template <typename RT, typename SM, typename DM>
RT do_sub_sm_dm (const SM& a, const DM& d)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    {
      gripe_nonconformant ("operator -", a.rows (), a.cols (), d.rows (), d.cols ());
      return RT ();
    }
  else
    return inner_do_add_sm_dm<RT> (a, d,
                                   identity_val<typename SM::element_type> (),
                                   std::negate<typename DM::element_type> ());
}

#endif // octave_Sparse_diag_op_defs_h
