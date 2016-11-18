/*

Copyright (C) 2003-2015 David Bateman
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009-2010 VZLU Prague

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

Code stolen in large part from Python's, listobject.c, which itself had
no license header. However, thanks to Tim Peters for the parts of the
code I ripped-off.

As required in the Python license the short description of the changes
made are

* convert the sorting code in listobject.cc into a generic class,
  replacing PyObject* with the type of the class T.

* replaced usages of malloc, free, memcpy and memmove by standard C++
  new [], delete [] and std::copy and std::copy_backward. Note that replacing
  memmove by std::copy is possible if the destination starts before the source.
  If not, std::copy_backward needs to be used.

* templatize comparison operator in most methods, provide possible dispatch

* duplicate methods to avoid by-the-way indexed sorting

* add methods for verifying sortedness of array

* row sorting via breadth-first tree subsorting

* binary lookup and sequential binary lookup optimized for dense downsampling.

* NOTE: the memory management routines rely on the fact that delete [] silently
  ignores null pointers. Don't gripe about the missing checks - they're there.


The Python license is

  PSF LICENSE AGREEMENT FOR PYTHON 2.3
  --------------------------------------

  1. This LICENSE AGREEMENT is between the Python Software Foundation
  ("PSF"), and the Individual or Organization ("Licensee") accessing and
  otherwise using Python 2.3 software in source or binary form and its
  associated documentation.

  2. Subject to the terms and conditions of this License Agreement, PSF
  hereby grants Licensee a nonexclusive, royalty-free, world-wide
  license to reproduce, analyze, test, perform and/or display publicly,
  prepare derivative works, distribute, and otherwise use Python 2.3
  alone or in any derivative version, provided, however, that PSF's
  License Agreement and PSF's notice of copyright, i.e., "Copyright (c)
  2001, 2002, 2003 Python Software Foundation; All Rights Reserved" are
  retained in Python 2.3 alone or in any derivative version prepared by
  Licensee.

  3. In the event Licensee prepares a derivative work that is based on
  or incorporates Python 2.3 or any part thereof, and wants to make
  the derivative work available to others as provided herein, then
  Licensee hereby agrees to include in any such work a brief summary of
  the changes made to Python 2.3.

  4. PSF is making Python 2.3 available to Licensee on an "AS IS"
  basis.  PSF MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
  IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PSF MAKES NO AND
  DISCLAIMS ANY REPRESENTATION OR WARRANTY OF MERCHANTABILITY OR FITNESS
  FOR ANY PARTICULAR PURPOSE OR THAT THE USE OF PYTHON 2.3 WILL NOT
  INFRINGE ANY THIRD PARTY RIGHTS.

  5. PSF SHALL NOT BE LIABLE TO LICENSEE OR ANY OTHER USERS OF PYTHON
  2.3 FOR ANY INCIDENTAL, SPECIAL, OR CONSEQUENTIAL DAMAGES OR LOSS AS
  A RESULT OF MODIFYING, DISTRIBUTING, OR OTHERWISE USING PYTHON 2.3,
  OR ANY DERIVATIVE THEREOF, EVEN IF ADVISED OF THE POSSIBILITY THEREOF.

  6. This License Agreement will automatically terminate upon a material
  breach of its terms and conditions.

  7. Nothing in this License Agreement shall be deemed to create any
  relationship of agency, partnership, or joint venture between PSF and
  Licensee.  This License Agreement does not grant permission to use PSF
  trademarks or trade name in a trademark sense to endorse or promote
  products or services of Licensee, or any third party.

  8. By copying, installing or otherwise using Python 2.3, Licensee
  agrees to be bound by the terms and conditions of this License
  Agreement.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <algorithm>
#include <functional>
#include <cstring>
#include <stack>

#include "lo-mappers.h"
#include "quit.h"
#include "oct-sort.h"
#include "oct-locbuf.h"

template <class T>
octave_sort<T>::octave_sort (void) :
  compare (ascending_compare), ms (0)
{
}

template <class T>
octave_sort<T>::octave_sort (compare_fcn_type comp)
  : compare (comp), ms (0)
{
}

template <class T>
octave_sort<T>::~octave_sort ()
{
  delete ms;
}

template <class T>
void
octave_sort<T>::set_compare (sortmode mode)
{
  if (mode == ASCENDING)
    compare = ascending_compare;
  else if (mode == DESCENDING)
    compare = descending_compare;
  else
    compare = 0;
}

template <class T>
template <class Comp>
void
octave_sort<T>::binarysort (T *data, octave_idx_type nel,
                            octave_idx_type start, Comp comp)
{
  if (start == 0)
    ++start;

  for (; start < nel; ++start)
    {
      /* set l to where *start belongs */
      octave_idx_type l = 0;
      octave_idx_type r = start;
      T pivot = data[start];
      /* Invariants:
       * pivot >= all in [lo, l).
       * pivot  < all in [r, start).
       * The second is vacuously true at the start.
       */
      do
        {
          octave_idx_type p = l + ((r - l) >> 1);
          if (comp (pivot, data[p]))
            r = p;
          else
            l = p+1;
        }
      while (l < r);
      /* The invariants still hold, so pivot >= all in [lo, l) and
         pivot < all in [l, start), so pivot belongs at l.  Note
         that if there are elements equal to pivot, l points to the
         first slot after them -- that's why this sort is stable.
         Slide over to make room.
         Caution: using memmove is much slower under MSVC 5;
         we're not usually moving many slots. */
      // NOTE: using swap and going upwards appears to be faster.
      for (octave_idx_type p = l; p < start; p++)
        std::swap (pivot, data[p]);
      data[start] = pivot;
    }

  return;
}

template <class T>
template <class Comp>
void
octave_sort<T>::binarysort (T *data, octave_idx_type *idx, octave_idx_type nel,
                            octave_idx_type start, Comp comp)
{
  if (start == 0)
    ++start;

  for (; start < nel; ++start)
    {
      /* set l to where *start belongs */
      octave_idx_type l = 0;
      octave_idx_type r = start;
      T pivot = data[start];
      /* Invariants:
       * pivot >= all in [lo, l).
       * pivot  < all in [r, start).
       * The second is vacuously true at the start.
       */
      do
        {
          octave_idx_type p = l + ((r - l) >> 1);
          if (comp (pivot, data[p]))
            r = p;
          else
            l = p+1;
        }
      while (l < r);
      /* The invariants still hold, so pivot >= all in [lo, l) and
         pivot < all in [l, start), so pivot belongs at l.  Note
         that if there are elements equal to pivot, l points to the
         first slot after them -- that's why this sort is stable.
         Slide over to make room.
         Caution: using memmove is much slower under MSVC 5;
         we're not usually moving many slots. */
      // NOTE: using swap and going upwards appears to be faster.
      for (octave_idx_type p = l; p < start; p++)
        std::swap (pivot, data[p]);
      data[start] = pivot;
      octave_idx_type ipivot = idx[start];
      for (octave_idx_type p = l; p < start; p++)
        std::swap (ipivot, idx[p]);
      idx[start] = ipivot;
    }

  return;
}

/*
Return the length of the run beginning at lo, in the slice [lo, hi).  lo < hi
is required on entry.  "A run" is the longest ascending sequence, with

    lo[0] <= lo[1] <= lo[2] <= ...

or the longest descending sequence, with

    lo[0] > lo[1] > lo[2] > ...

DESCENDING is set to false in the former case, or to true in the latter.
For its intended use in a stable mergesort, the strictness of the defn of
"descending" is needed so that the caller can safely reverse a descending
sequence without violating stability (strict > ensures there are no equal
elements to get out of order).

Returns -1 in case of error.
*/
template <class T>
template <class Comp>
octave_idx_type
octave_sort<T>::count_run (T *lo, octave_idx_type nel, bool& descending,
                           Comp comp)
{
  octave_idx_type n;
  T *hi = lo + nel;

  descending = false;
  ++lo;
  if (lo == hi)
    return 1;

  n = 2;

  if (comp (*lo, *(lo-1)))
    {
      descending = true;
      for (lo = lo+1; lo < hi; ++lo, ++n)
        {
          if (comp (*lo, *(lo-1)))
            ;
          else
            break;
        }
    }
  else
    {
      for (lo = lo+1; lo < hi; ++lo, ++n)
        {
          if (comp (*lo, *(lo-1)))
            break;
        }
    }

  return n;
}

/*
Locate the proper position of key in a sorted vector; if the vector contains
an element equal to key, return the position immediately to the left of
the leftmost equal element.  [gallop_right() does the same except returns
the position to the right of the rightmost equal element (if any).]

"a" is a sorted vector with n elements, starting at a[0].  n must be > 0.

"hint" is an index at which to begin the search, 0 <= hint < n.  The closer
hint is to the final result, the faster this runs.

The return value is the int k in 0..n such that

    a[k-1] < key <= a[k]

pretending that *(a-1) is minus infinity and a[n] is plus infinity.  IOW,
key belongs at index k; or, IOW, the first k elements of a should precede
key, and the last n-k should follow key.

Returns -1 on error.  See listsort.txt for info on the method.
*/
template <class T>
template <class Comp>
octave_idx_type
octave_sort<T>::gallop_left (T key, T *a, octave_idx_type n,
                             octave_idx_type hint,
                             Comp comp)
{
  octave_idx_type ofs;
  octave_idx_type lastofs;
  octave_idx_type k;

  a += hint;
  lastofs = 0;
  ofs = 1;
  if (comp (*a, key))
    {
      /* a[hint] < key -- gallop right, until
       * a[hint + lastofs] < key <= a[hint + ofs]
       */
      const octave_idx_type maxofs = n - hint;  /* &a[n-1] is highest */
      while (ofs < maxofs)
        {
          if (comp (a[ofs], key))
            {
              lastofs = ofs;
              ofs = (ofs << 1) + 1;
              if (ofs <= 0)     /* int overflow */
                ofs = maxofs;
            }
          else  /* key <= a[hint + ofs] */
            break;
        }
      if (ofs > maxofs)
        ofs = maxofs;
      /* Translate back to offsets relative to &a[0]. */
      lastofs += hint;
      ofs += hint;
    }
  else
    {
      /* key <= a[hint] -- gallop left, until
       * a[hint - ofs] < key <= a[hint - lastofs]
       */
      const octave_idx_type maxofs = hint + 1;  /* &a[0] is lowest */
      while (ofs < maxofs)
        {
          if (comp (*(a-ofs), key))
            break;
          /* key <= a[hint - ofs] */
          lastofs = ofs;
          ofs = (ofs << 1) + 1;
          if (ofs <= 0) /* int overflow */
            ofs = maxofs;
        }
      if (ofs > maxofs)
        ofs = maxofs;
      /* Translate back to positive offsets relative to &a[0]. */
      k = lastofs;
      lastofs = hint - ofs;
      ofs = hint - k;
    }
  a -= hint;

  /* Now a[lastofs] < key <= a[ofs], so key belongs somewhere to the
   * right of lastofs but no farther right than ofs.  Do a binary
   * search, with invariant a[lastofs-1] < key <= a[ofs].
   */
  ++lastofs;
  while (lastofs < ofs)
    {
      octave_idx_type m = lastofs + ((ofs - lastofs) >> 1);

      if (comp (a[m], key))
        lastofs = m+1;  /* a[m] < key */
      else
        ofs = m;        /* key <= a[m] */
    }

  return ofs;
}

/*
Exactly like gallop_left(), except that if key already exists in a[0:n],
finds the position immediately to the right of the rightmost equal value.

The return value is the int k in 0..n such that

    a[k-1] <= key < a[k]

or -1 if error.

The code duplication is massive, but this is enough different given that
we're sticking to "<" comparisons that it's much harder to follow if
written as one routine with yet another "left or right?" flag.
*/
template <class T>
template <class Comp>
octave_idx_type
octave_sort<T>::gallop_right (T key, T *a, octave_idx_type n,
                              octave_idx_type hint,
                              Comp comp)
{
  octave_idx_type ofs;
  octave_idx_type lastofs;
  octave_idx_type k;

  a += hint;
  lastofs = 0;
  ofs = 1;
  if (comp (key, *a))
    {
      /* key < a[hint] -- gallop left, until
       * a[hint - ofs] <= key < a[hint - lastofs]
       */
      const octave_idx_type maxofs = hint + 1;  /* &a[0] is lowest */
      while (ofs < maxofs)
        {
          if (comp (key, *(a-ofs)))
            {
              lastofs = ofs;
              ofs = (ofs << 1) + 1;
              if (ofs <= 0)     /* int overflow */
                ofs = maxofs;
            }
          else  /* a[hint - ofs] <= key */
            break;
        }
      if (ofs > maxofs)
        ofs = maxofs;
      /* Translate back to positive offsets relative to &a[0]. */
      k = lastofs;
      lastofs = hint - ofs;
      ofs = hint - k;
    }
  else
    {
      /* a[hint] <= key -- gallop right, until
       * a[hint + lastofs] <= key < a[hint + ofs]
       */
      const octave_idx_type maxofs = n - hint;  /* &a[n-1] is highest */
      while (ofs < maxofs)
        {
          if (comp (key, a[ofs]))
            break;
          /* a[hint + ofs] <= key */
          lastofs = ofs;
          ofs = (ofs << 1) + 1;
          if (ofs <= 0) /* int overflow */
            ofs = maxofs;
        }
      if (ofs > maxofs)
        ofs = maxofs;
      /* Translate back to offsets relative to &a[0]. */
      lastofs += hint;
      ofs += hint;
    }
  a -= hint;

  /* Now a[lastofs] <= key < a[ofs], so key belongs somewhere to the
   * right of lastofs but no farther right than ofs.  Do a binary
   * search, with invariant a[lastofs-1] <= key < a[ofs].
   */
  ++lastofs;
  while (lastofs < ofs)
    {
      octave_idx_type m = lastofs + ((ofs - lastofs) >> 1);

      if (comp (key, a[m]))
        ofs = m;        /* key < a[m] */
      else
        lastofs = m+1;  /* a[m] <= key */
    }

  return ofs;
}

static inline octave_idx_type
roundupsize (octave_idx_type n)
{
  unsigned int nbits = 3;
  octave_idx_type n2 = static_cast<octave_idx_type> (n) >> 8;

  /* Round up:
   * If n <       256, to a multiple of        8.
   * If n <      2048, to a multiple of       64.
   * If n <     16384, to a multiple of      512.
   * If n <    131072, to a multiple of     4096.
   * If n <   1048576, to a multiple of    32768.
   * If n <   8388608, to a multiple of   262144.
   * If n <  67108864, to a multiple of  2097152.
   * If n < 536870912, to a multiple of 16777216.
   * ...
   * If n < 2**(5+3*i), to a multiple of 2**(3*i).
   *
   * This over-allocates proportional to the list size, making room
   * for additional growth.  The over-allocation is mild, but is
   * enough to give linear-time amortized behavior over a long
   * sequence of appends() in the presence of a poorly-performing
   * system realloc() (which is a reality, e.g., across all flavors
   * of Windows, with Win9x behavior being particularly bad -- and
   * we've still got address space fragmentation problems on Win9x
   * even with this scheme, although it requires much longer lists to
   * provoke them than it used to).
   */
  while (n2)
    {
      n2 >>= 3;
      nbits += 3;
    }

  return ((n >> nbits) + 1) << nbits;
}

/* Ensure enough temp memory for 'need' array slots is available.
 * Returns 0 on success and -1 if the memory can't be gotten.
 */
template <class T>
void
octave_sort<T>::MergeState::getmem (octave_idx_type need)
{
  if (need <= alloced)
    return;

  need = roundupsize (need);
  /* Don't realloc!  That can cost cycles to copy the old data, but
   * we don't care what's in the block.
   */
  delete [] a;
  delete [] ia; // Must do this or fool possible next getmemi.
  a = new T [need];
  alloced = need;

}

template <class T>
void
octave_sort<T>::MergeState::getmemi (octave_idx_type need)
{
  if (ia && need <= alloced)
    return;

  need = roundupsize (need);
  /* Don't realloc!  That can cost cycles to copy the old data, but
   * we don't care what's in the block.
   */
  delete [] a;
  delete [] ia;

  a = new T [need];
  ia = new octave_idx_type [need];
  alloced = need;
}

/* Merge the na elements starting at pa with the nb elements starting at pb
 * in a stable way, in-place.  na and nb must be > 0, and pa + na == pb.
 * Must also have that *pb < *pa, that pa[na-1] belongs at the end of the
 * merge, and should have na <= nb.  See listsort.txt for more info.
 * Return 0 if successful, -1 if error.
 */
template <class T>
template <class Comp>
int
octave_sort<T>::merge_lo (T *pa, octave_idx_type na,
                          T *pb, octave_idx_type nb,
                          Comp comp)
{
  octave_idx_type k;
  T *dest;
  int result = -1;      /* guilty until proved innocent */
  octave_idx_type min_gallop = ms->min_gallop;

  ms->getmem (na);

  std::copy (pa, pa + na, ms->a);
  dest = pa;
  pa = ms->a;

  *dest++ = *pb++;
  --nb;
  if (nb == 0)
    goto Succeed;
  if (na == 1)
    goto CopyB;

  for (;;)
    {
      octave_idx_type acount = 0;       /* # of times A won in a row */
      octave_idx_type bcount = 0;       /* # of times B won in a row */

      /* Do the straightforward thing until (if ever) one run
       * appears to win consistently.
       */
      for (;;)
        {

          // FIXME: these loops are candidates for further optimizations.
          // Rather than testing everything in each cycle, it may be more
          // efficient to do it in hunks.
          if (comp (*pb, *pa))
            {
              *dest++ = *pb++;
              ++bcount;
              acount = 0;
              --nb;
              if (nb == 0)
                goto Succeed;
              if (bcount >= min_gallop)
                break;
            }
          else
            {
              *dest++ = *pa++;
              ++acount;
              bcount = 0;
              --na;
              if (na == 1)
                goto CopyB;
              if (acount >= min_gallop)
                break;
            }
        }

      /* One run is winning so consistently that galloping may
       * be a huge win.  So try that, and continue galloping until
       * (if ever) neither run appears to be winning consistently
       * anymore.
       */
      ++min_gallop;
      do
        {
          min_gallop -= min_gallop > 1;
          ms->min_gallop = min_gallop;
          k = gallop_right (*pb, pa, na, 0, comp);
          acount = k;
          if (k)
            {
              if (k < 0)
                goto Fail;
              dest = std::copy (pa, pa + k, dest);
              pa += k;
              na -= k;
              if (na == 1)
                goto CopyB;
              /* na==0 is impossible now if the comparison
               * function is consistent, but we can't assume
               * that it is.
               */
              if (na == 0)
                goto Succeed;
            }
          *dest++ = *pb++;
          --nb;
          if (nb == 0)
            goto Succeed;

          k = gallop_left (*pa, pb, nb, 0, comp);
          bcount = k;
          if (k)
            {
              if (k < 0)
                goto Fail;
              dest = std::copy (pb, pb + k, dest);
              pb += k;
              nb -= k;
              if (nb == 0)
                goto Succeed;
            }
          *dest++ = *pa++;
          --na;
          if (na == 1)
            goto CopyB;
        }
      while (acount >= MIN_GALLOP || bcount >= MIN_GALLOP);

      ++min_gallop;     /* penalize it for leaving galloping mode */
      ms->min_gallop = min_gallop;
    }

Succeed:
  result = 0;

Fail:
  if (na)
    std::copy (pa, pa + na, dest);
  return result;

CopyB:
  /* The last element of pa belongs at the end of the merge. */
  std::copy (pb, pb + nb, dest);
  dest[nb] = *pa;

  return 0;
}

template <class T>
template <class Comp>
int
octave_sort<T>::merge_lo (T *pa, octave_idx_type *ipa, octave_idx_type na,
                          T *pb, octave_idx_type *ipb, octave_idx_type nb,
                          Comp comp)
{
  octave_idx_type k;
  T *dest;
  octave_idx_type *idest;
  int result = -1;      /* guilty until proved innocent */
  octave_idx_type min_gallop = ms->min_gallop;

  ms->getmemi (na);

  std::copy (pa, pa + na, ms->a);
  std::copy (ipa, ipa + na, ms->ia);
  dest = pa; idest = ipa;
  pa = ms->a; ipa = ms->ia;

  *dest++ = *pb++; *idest++ = *ipb++;
  --nb;
  if (nb == 0)
    goto Succeed;
  if (na == 1)
    goto CopyB;

  for (;;)
    {
      octave_idx_type acount = 0;       /* # of times A won in a row */
      octave_idx_type bcount = 0;       /* # of times B won in a row */

      /* Do the straightforward thing until (if ever) one run
       * appears to win consistently.
       */
      for (;;)
        {

          if (comp (*pb, *pa))
            {
              *dest++ = *pb++; *idest++ = *ipb++;
              ++bcount;
              acount = 0;
              --nb;
              if (nb == 0)
                goto Succeed;
              if (bcount >= min_gallop)
                break;
            }
          else
            {
              *dest++ = *pa++; *idest++ = *ipa++;
              ++acount;
              bcount = 0;
              --na;
              if (na == 1)
                goto CopyB;
              if (acount >= min_gallop)
                break;
            }
        }

      /* One run is winning so consistently that galloping may
       * be a huge win.  So try that, and continue galloping until
       * (if ever) neither run appears to be winning consistently
       * anymore.
       */
      ++min_gallop;
      do
        {
          min_gallop -= min_gallop > 1;
          ms->min_gallop = min_gallop;
          k = gallop_right (*pb, pa, na, 0, comp);
          acount = k;
          if (k)
            {
              if (k < 0)
                goto Fail;
              dest = std::copy (pa, pa + k, dest);
              idest = std::copy (ipa, ipa + k, idest);
              pa += k; ipa += k;
              na -= k;
              if (na == 1)
                goto CopyB;
              /* na==0 is impossible now if the comparison
               * function is consistent, but we can't assume
               * that it is.
               */
              if (na == 0)
                goto Succeed;
            }
          *dest++ = *pb++; *idest++ = *ipb++;
          --nb;
          if (nb == 0)
            goto Succeed;

          k = gallop_left (*pa, pb, nb, 0, comp);
          bcount = k;
          if (k)
            {
              if (k < 0)
                goto Fail;
              dest = std::copy (pb, pb + k, dest);
              idest = std::copy (ipb, ipb + k, idest);
              pb += k; ipb += k;
              nb -= k;
              if (nb == 0)
                goto Succeed;
            }
          *dest++ = *pa++; *idest++ = *ipa++;
          --na;
          if (na == 1)
            goto CopyB;
        }
      while (acount >= MIN_GALLOP || bcount >= MIN_GALLOP);

      ++min_gallop;     /* penalize it for leaving galloping mode */
      ms->min_gallop = min_gallop;
    }

Succeed:
  result = 0;

Fail:
  if (na)
    {
      std::copy (pa, pa + na, dest);
      std::copy (ipa, ipa + na, idest);
    }
  return result;

CopyB:
  /* The last element of pa belongs at the end of the merge. */
  std::copy (pb, pb + nb, dest);
  std::copy (ipb, ipb + nb, idest);
  dest[nb] = *pa;
  idest[nb] = *ipa;

  return 0;
}

/* Merge the na elements starting at pa with the nb elements starting at pb
 * in a stable way, in-place.  na and nb must be > 0, and pa + na == pb.
 * Must also have that *pb < *pa, that pa[na-1] belongs at the end of the
 * merge, and should have na >= nb.  See listsort.txt for more info.
 * Return 0 if successful, -1 if error.
 */
template <class T>
template <class Comp>
int
octave_sort<T>::merge_hi (T *pa, octave_idx_type na,
                          T *pb, octave_idx_type nb,
                          Comp comp)
{
  octave_idx_type k;
  T *dest;
  int result = -1;      /* guilty until proved innocent */
  T *basea, *baseb;
  octave_idx_type min_gallop = ms->min_gallop;

  ms->getmem (nb);

  dest = pb + nb - 1;
  std::copy (pb, pb + nb, ms->a);
  basea = pa;
  baseb = ms->a;
  pb = ms->a + nb - 1;
  pa += na - 1;

  *dest-- = *pa--;
  --na;
  if (na == 0)
    goto Succeed;
  if (nb == 1)
    goto CopyA;

  for (;;)
    {
      octave_idx_type acount = 0;       /* # of times A won in a row */
      octave_idx_type bcount = 0;       /* # of times B won in a row */

      /* Do the straightforward thing until (if ever) one run
       * appears to win consistently.
       */
      for (;;)
        {
          if (comp (*pb, *pa))
            {
              *dest-- = *pa--;
              ++acount;
              bcount = 0;
              --na;
              if (na == 0)
                goto Succeed;
              if (acount >= min_gallop)
                break;
            }
          else
            {
              *dest-- = *pb--;
              ++bcount;
              acount = 0;
              --nb;
              if (nb == 1)
                goto CopyA;
              if (bcount >= min_gallop)
                break;
            }
        }

      /* One run is winning so consistently that galloping may
       * be a huge win.  So try that, and continue galloping until
       * (if ever) neither run appears to be winning consistently
       * anymore.
       */
      ++min_gallop;
      do
        {
          min_gallop -= min_gallop > 1;
          ms->min_gallop = min_gallop;
          k = gallop_right (*pb, basea, na, na-1, comp);
          if (k < 0)
            goto Fail;
          k = na - k;
          acount = k;
          if (k)
            {
              dest = std::copy_backward (pa+1 - k, pa+1, dest+1) - 1;
              pa -= k;
              na -= k;
              if (na == 0)
                goto Succeed;
            }
          *dest-- = *pb--;
          --nb;
          if (nb == 1)
            goto CopyA;

          k = gallop_left (*pa, baseb, nb, nb-1, comp);
          if (k < 0)
            goto Fail;
          k = nb - k;
          bcount = k;
          if (k)
            {
              dest -= k;
              pb -= k;
              std::copy (pb+1, pb+1 + k, dest+1);
              nb -= k;
              if (nb == 1)
                goto CopyA;
              /* nb==0 is impossible now if the comparison
               * function is consistent, but we can't assume
               * that it is.
               */
              if (nb == 0)
                goto Succeed;
            }
          *dest-- = *pa--;
          --na;
          if (na == 0)
            goto Succeed;
        } while (acount >= MIN_GALLOP || bcount >= MIN_GALLOP);
      ++min_gallop;     /* penalize it for leaving galloping mode */
      ms->min_gallop = min_gallop;
    }

Succeed:
  result = 0;

Fail:
  if (nb)
    std::copy (baseb, baseb + nb, dest-(nb-1));
  return result;

CopyA:
  /* The first element of pb belongs at the front of the merge. */
  dest = std::copy_backward (pa+1 - na, pa+1, dest+1) - 1;
  pa -= na;
  *dest = *pb;

  return 0;
}

template <class T>
template <class Comp>
int
octave_sort<T>::merge_hi (T *pa, octave_idx_type *ipa, octave_idx_type na,
                          T *pb, octave_idx_type *ipb, octave_idx_type nb,
                          Comp comp)
{
  octave_idx_type k;
  T *dest;
  octave_idx_type *idest;
  int result = -1;      /* guilty until proved innocent */
  T *basea, *baseb;
  octave_idx_type *ibaseb;
  octave_idx_type min_gallop = ms->min_gallop;

  ms->getmemi (nb);

  dest = pb + nb - 1;
  idest = ipb + nb - 1;
  std::copy (pb, pb + nb, ms->a);
  std::copy (ipb, ipb + nb, ms->ia);
  basea = pa;
  baseb = ms->a; ibaseb = ms->ia;
  pb = ms->a + nb - 1; ipb = ms->ia + nb - 1;
  pa += na - 1; ipa += na - 1;

  *dest-- = *pa--; *idest-- = *ipa--;
  --na;
  if (na == 0)
    goto Succeed;
  if (nb == 1)
    goto CopyA;

  for (;;)
    {
      octave_idx_type acount = 0;       /* # of times A won in a row */
      octave_idx_type bcount = 0;       /* # of times B won in a row */

      /* Do the straightforward thing until (if ever) one run
       * appears to win consistently.
       */
      for (;;)
        {
          if (comp (*pb, *pa))
            {
              *dest-- = *pa--; *idest-- = *ipa--;
              ++acount;
              bcount = 0;
              --na;
              if (na == 0)
                goto Succeed;
              if (acount >= min_gallop)
                break;
            }
          else
            {
              *dest-- = *pb--; *idest-- = *ipb--;
              ++bcount;
              acount = 0;
              --nb;
              if (nb == 1)
                goto CopyA;
              if (bcount >= min_gallop)
                break;
            }
        }

      /* One run is winning so consistently that galloping may
       * be a huge win.  So try that, and continue galloping until
       * (if ever) neither run appears to be winning consistently
       * anymore.
       */
      ++min_gallop;
      do
        {
          min_gallop -= min_gallop > 1;
          ms->min_gallop = min_gallop;
          k = gallop_right (*pb, basea, na, na-1, comp);
          if (k < 0)
            goto Fail;
          k = na - k;
          acount = k;
          if (k)
            {
              dest = std::copy_backward (pa+1 - k, pa+1, dest+1) - 1;
              idest = std::copy_backward (ipa+1 - k, ipa+1, idest+1) - 1;
              pa -= k; ipa -= k;
              na -= k;
              if (na == 0)
                goto Succeed;
            }
          *dest-- = *pb--; *idest-- = *ipb--;
          --nb;
          if (nb == 1)
            goto CopyA;

          k = gallop_left (*pa, baseb, nb, nb-1, comp);
          if (k < 0)
            goto Fail;
          k = nb - k;
          bcount = k;
          if (k)
            {
              dest -= k; idest -= k;
              pb -= k; ipb -= k;
              std::copy (pb+1, pb+1 + k, dest+1);
              std::copy (ipb+1, ipb+1 + k, idest+1);
              nb -= k;
              if (nb == 1)
                goto CopyA;
              /* nb==0 is impossible now if the comparison
               * function is consistent, but we can't assume
               * that it is.
               */
              if (nb == 0)
                goto Succeed;
            }
          *dest-- = *pa--; *idest-- = *ipa--;
          --na;
          if (na == 0)
            goto Succeed;
        } while (acount >= MIN_GALLOP || bcount >= MIN_GALLOP);
      ++min_gallop;     /* penalize it for leaving galloping mode */
      ms->min_gallop = min_gallop;
    }

Succeed:
  result = 0;

Fail:
  if (nb)
    {
      std::copy (baseb, baseb + nb, dest-(nb-1));
      std::copy (ibaseb, ibaseb + nb, idest-(nb-1));
    }
  return result;

CopyA:
  /* The first element of pb belongs at the front of the merge. */
  dest = std::copy_backward (pa+1 - na, pa+1, dest+1) - 1;
  idest = std::copy_backward (ipa+1 - na, ipa+1, idest+1) - 1;
  pa -= na; ipa -= na;
  *dest = *pb; *idest = *ipb;

  return 0;
}

/* Merge the two runs at stack indices i and i+1.
 * Returns 0 on success, -1 on error.
 */
template <class T>
template <class Comp>
int
octave_sort<T>::merge_at (octave_idx_type i, T *data,
                          Comp comp)
{
  T *pa, *pb;
  octave_idx_type na, nb;
  octave_idx_type k;

  pa = data + ms->pending[i].base;
  na = ms->pending[i].len;
  pb = data + ms->pending[i+1].base;
  nb = ms->pending[i+1].len;

  /* Record the length of the combined runs; if i is the 3rd-last
   * run now, also slide over the last run (which isn't involved
   * in this merge).  The current run i+1 goes away in any case.
   */
  ms->pending[i].len = na + nb;
  if (i == ms->n - 3)
    ms->pending[i+1] = ms->pending[i+2];
  ms->n--;

  /* Where does b start in a?  Elements in a before that can be
   * ignored (already in place).
   */
  k = gallop_right (*pb, pa, na, 0, comp);
  if (k < 0)
    return -1;
  pa += k;
  na -= k;
  if (na == 0)
    return 0;

  /* Where does a end in b?  Elements in b after that can be
   * ignored (already in place).
   */
  nb = gallop_left (pa[na-1], pb, nb, nb-1, comp);
  if (nb <= 0)
    return nb;

  /* Merge what remains of the runs, using a temp array with
   * min (na, nb) elements.
   */
  if (na <= nb)
    return merge_lo (pa, na, pb, nb, comp);
  else
    return merge_hi (pa, na, pb, nb, comp);
}

template <class T>
template <class Comp>
int
octave_sort<T>::merge_at (octave_idx_type i, T *data, octave_idx_type *idx,
                          Comp comp)
{
  T *pa, *pb;
  octave_idx_type *ipa, *ipb;
  octave_idx_type na, nb;
  octave_idx_type k;

  pa = data + ms->pending[i].base;
  ipa = idx + ms->pending[i].base;
  na = ms->pending[i].len;
  pb = data + ms->pending[i+1].base;
  ipb = idx + ms->pending[i+1].base;
  nb = ms->pending[i+1].len;

  /* Record the length of the combined runs; if i is the 3rd-last
   * run now, also slide over the last run (which isn't involved
   * in this merge).  The current run i+1 goes away in any case.
   */
  ms->pending[i].len = na + nb;
  if (i == ms->n - 3)
    ms->pending[i+1] = ms->pending[i+2];
  ms->n--;

  /* Where does b start in a?  Elements in a before that can be
   * ignored (already in place).
   */
  k = gallop_right (*pb, pa, na, 0, comp);
  if (k < 0)
    return -1;
  pa += k; ipa += k;
  na -= k;
  if (na == 0)
    return 0;

  /* Where does a end in b?  Elements in b after that can be
   * ignored (already in place).
   */
  nb = gallop_left (pa[na-1], pb, nb, nb-1, comp);
  if (nb <= 0)
    return nb;

  /* Merge what remains of the runs, using a temp array with
   * min (na, nb) elements.
   */
  if (na <= nb)
    return merge_lo (pa, ipa, na, pb, ipb, nb, comp);
  else
    return merge_hi (pa, ipa, na, pb, ipb, nb, comp);
}

/* Examine the stack of runs waiting to be merged, merging adjacent runs
 * until the stack invariants are re-established:
 *
 * 1. len[-3] > len[-2] + len[-1]
 * 2. len[-2] > len[-1]
 *
 * See listsort.txt for more info.
 *
 * Returns 0 on success, -1 on error.
 */
template <class T>
template <class Comp>
int
octave_sort<T>::merge_collapse (T *data, Comp comp)
{
  struct s_slice *p = ms->pending;

  while (ms->n > 1)
    {
      octave_idx_type n = ms->n - 2;
      if (n > 0 && p[n-1].len <= p[n].len + p[n+1].len)
        {
          if (p[n-1].len < p[n+1].len)
            --n;
          if (merge_at (n, data, comp) < 0)
            return -1;
        }
      else if (p[n].len <= p[n+1].len)
        {
          if (merge_at (n, data, comp) < 0)
            return -1;
        }
      else
        break;
    }

  return 0;
}

template <class T>
template <class Comp>
int
octave_sort<T>::merge_collapse (T *data, octave_idx_type *idx, Comp comp)
{
  struct s_slice *p = ms->pending;

  while (ms->n > 1)
    {
      octave_idx_type n = ms->n - 2;
      if (n > 0 && p[n-1].len <= p[n].len + p[n+1].len)
        {
          if (p[n-1].len < p[n+1].len)
            --n;
          if (merge_at (n, data, idx, comp) < 0)
            return -1;
        }
      else if (p[n].len <= p[n+1].len)
        {
          if (merge_at (n, data, idx, comp) < 0)
            return -1;
        }
      else
        break;
    }

  return 0;
}

/* Regardless of invariants, merge all runs on the stack until only one
 * remains.  This is used at the end of the mergesort.
 *
 * Returns 0 on success, -1 on error.
 */
template <class T>
template <class Comp>
int
octave_sort<T>::merge_force_collapse (T *data, Comp comp)
{
  struct s_slice *p = ms->pending;

  while (ms->n > 1)
    {
      octave_idx_type n = ms->n - 2;
      if (n > 0 && p[n-1].len < p[n+1].len)
        --n;
      if (merge_at (n, data, comp) < 0)
        return -1;
    }

  return 0;
}

template <class T>
template <class Comp>
int
octave_sort<T>::merge_force_collapse (T *data, octave_idx_type *idx, Comp comp)
{
  struct s_slice *p = ms->pending;

  while (ms->n > 1)
    {
      octave_idx_type n = ms->n - 2;
      if (n > 0 && p[n-1].len < p[n+1].len)
        --n;
      if (merge_at (n, data, idx, comp) < 0)
        return -1;
    }

  return 0;
}

/* Compute a good value for the minimum run length; natural runs shorter
 * than this are boosted artificially via binary insertion.
 *
 * If n < 64, return n (it's too small to bother with fancy stuff).
 * Else if n is an exact power of 2, return 32.
 * Else return an int k, 32 <= k <= 64, such that n/k is close to, but
 * strictly less than, an exact power of 2.
 *
 * See listsort.txt for more info.
 */
template <class T>
octave_idx_type
octave_sort<T>::merge_compute_minrun (octave_idx_type n)
{
  octave_idx_type r = 0;        /* becomes 1 if any 1 bits are shifted off */

  while (n >= 64)
    {
      r |= n & 1;
      n >>= 1;
    }

  return n + r;
}

template <class T>
template <class Comp>
void
octave_sort<T>::sort (T *data, octave_idx_type nel, Comp comp)
{
  /* Re-initialize the Mergestate as this might be the second time called */
  if (! ms) ms = new MergeState;

  ms->reset ();
  ms->getmem (1024);

  if (nel > 1)
    {
      octave_idx_type nremaining = nel;
      octave_idx_type lo = 0;

      /* March over the array once, left to right, finding natural runs,
       * and extending short natural runs to minrun elements.
       */
      octave_idx_type minrun = merge_compute_minrun (nremaining);
      do
        {
          bool descending;
          octave_idx_type n;

          /* Identify next run. */
          n = count_run (data + lo, nremaining, descending, comp);
          if (n < 0)
            goto fail;
          if (descending)
            std::reverse (data + lo, data + lo + n);
          /* If short, extend to min (minrun, nremaining). */
          if (n < minrun)
            {
              const octave_idx_type force = nremaining <= minrun ? nremaining
                                                                 : minrun;
              binarysort (data + lo, force, n, comp);
              n = force;
            }
          /* Push run onto pending-runs stack, and maybe merge. */
          assert (ms->n < MAX_MERGE_PENDING);
          ms->pending[ms->n].base = lo;
          ms->pending[ms->n].len = n;
          ms->n++;
          if (merge_collapse (data, comp) < 0)
            goto fail;
          /* Advance to find next run. */
          lo += n;
          nremaining -= n;
        }
      while (nremaining);

      merge_force_collapse (data, comp);
    }

fail:
  return;
}

template <class T>
template <class Comp>
void
octave_sort<T>::sort (T *data, octave_idx_type *idx, octave_idx_type nel,
                      Comp comp)
{
  /* Re-initialize the Mergestate as this might be the second time called */
  if (! ms) ms = new MergeState;

  ms->reset ();
  ms->getmemi (1024);

  if (nel > 1)
    {
      octave_idx_type nremaining = nel;
      octave_idx_type lo = 0;

      /* March over the array once, left to right, finding natural runs,
       * and extending short natural runs to minrun elements.
       */
      octave_idx_type minrun = merge_compute_minrun (nremaining);
      do
        {
          bool descending;
          octave_idx_type n;

          /* Identify next run. */
          n = count_run (data + lo, nremaining, descending, comp);
          if (n < 0)
            goto fail;
          if (descending)
            {
              std::reverse (data + lo, data + lo + n);
              std::reverse (idx + lo, idx + lo + n);
            }
          /* If short, extend to min (minrun, nremaining). */
          if (n < minrun)
            {
              const octave_idx_type force = nremaining <= minrun ? nremaining
                                                                 : minrun;
              binarysort (data + lo, idx + lo, force, n, comp);
              n = force;
            }
          /* Push run onto pending-runs stack, and maybe merge. */
          assert (ms->n < MAX_MERGE_PENDING);
          ms->pending[ms->n].base = lo;
          ms->pending[ms->n].len = n;
          ms->n++;
          if (merge_collapse (data, idx, comp) < 0)
            goto fail;
          /* Advance to find next run. */
          lo += n;
          nremaining -= n;
        }
      while (nremaining);

      merge_force_collapse (data, idx, comp);
    }

fail:
  return;
}

template <class T>
void
octave_sort<T>::sort (T *data, octave_idx_type nel)
{
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    sort (data, nel, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      sort (data, nel, std::greater<T> ());
    else
#endif
      if (compare)
        sort (data, nel, compare);
}

template <class T>
void
octave_sort<T>::sort (T *data, octave_idx_type *idx, octave_idx_type nel)
{
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    sort (data, idx, nel, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      sort (data, idx, nel, std::greater<T> ());
    else
#endif
      if (compare)
        sort (data, idx, nel, compare);
}

template <class T> template <class Comp>
bool
octave_sort<T>::is_sorted (const T *data, octave_idx_type nel, Comp comp)
{
  const T *end = data + nel;
  if (data != end)
    {
      const T *next = data;
      while (++next != end)
        {
          if (comp (*next, *data))
            break;
          data = next;
        }
      data = next;
    }

  return data == end;
}

template <class T>
bool
octave_sort<T>::is_sorted (const T *data, octave_idx_type nel)
{
  bool retval = false;
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    retval = is_sorted (data, nel, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      retval = is_sorted (data, nel, std::greater<T> ());
    else
#endif
      if (compare)
        retval = is_sorted (data, nel, compare);

  return retval;
}

// FIXME: is there really no way to make this local to the following function?
struct sortrows_run_t
{
  sortrows_run_t (octave_idx_type c, octave_idx_type o, octave_idx_type n)
    : col (c), ofs (o), nel (n) { }
  octave_idx_type col, ofs, nel;
};


template <class T> template <class Comp>
void
octave_sort<T>::sort_rows (const T *data, octave_idx_type *idx,
                           octave_idx_type rows, octave_idx_type cols,
                           Comp comp)
{
  OCTAVE_LOCAL_BUFFER (T, buf, rows);
  for (octave_idx_type i = 0; i < rows; i++)
    idx[i] = i;

  if (cols == 0 || rows <= 1)
    return;


  // This is a breadth-first traversal.
  typedef sortrows_run_t run_t;
  std::stack<run_t> runs;

  runs.push (run_t (0, 0, rows));

  while (! runs.empty ())
    {
      octave_idx_type col = runs.top ().col;
      octave_idx_type ofs = runs.top ().ofs;
      octave_idx_type nel = runs.top ().nel;
      runs.pop ();
      assert (nel > 1);

      T *lbuf = buf + ofs;
      const T *ldata = data + rows*col;
      octave_idx_type *lidx = idx + ofs;

      // Gather.
      for (octave_idx_type i = 0; i < nel; i++)
        lbuf[i] = ldata[lidx[i]];

      // Sort.
      sort (lbuf, lidx, nel, comp);

      // Identify constant runs and schedule subsorts.
      if (col < cols-1)
        {
          octave_idx_type lst = 0;
          for (octave_idx_type i = 0; i < nel; i++)
            {
              if (comp (lbuf[lst], lbuf[i]))
                {
                  if (i > lst + 1)
                    runs.push (run_t (col+1, ofs + lst, i - lst));
                  lst = i;
                }
            }
          if (nel > lst + 1)
            runs.push (run_t (col+1, ofs + lst, nel - lst));
        }
    }
}

template <class T>
void
octave_sort<T>::sort_rows (const T *data, octave_idx_type *idx,
                           octave_idx_type rows, octave_idx_type cols)
{
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    sort_rows (data, idx, rows, cols, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      sort_rows (data, idx, rows, cols, std::greater<T> ());
    else
#endif
      if (compare)
        sort_rows (data, idx, rows, cols, compare);
}

template <class T> template <class Comp>
bool
octave_sort<T>::is_sorted_rows (const T *data, octave_idx_type rows,
                                octave_idx_type cols, Comp comp)
{
  if (rows <= 1 || cols == 0)
    return true;

  // This is a breadth-first traversal.
  const T *lastrow = data + rows*(cols - 1);
  typedef std::pair<const T *, octave_idx_type> run_t;
  std::stack<run_t> runs;

  bool sorted = true;
  runs.push (run_t (data, rows));
  while (sorted && ! runs.empty ())
    {
      const T *lo = runs.top ().first;
      octave_idx_type n = runs.top ().second;
      runs.pop ();
      if (lo < lastrow)
        {
          // Not the final column.
          assert (n > 1);
          const T *hi = lo + n;
          const T *lst = lo;
          for (lo++; lo < hi; lo++)
            {
              if (comp (*lst, *lo))
                {
                  if (lo > lst + 1)
                    runs.push (run_t (lst + rows, lo - lst));
                  lst = lo;
                }
              else if (comp (*lo, *lst))
                break;

            }
          if (lo == hi)
            {
              if (lo > lst + 1)
                runs.push (run_t (lst + rows, lo - lst));
            }
          else
            {
              sorted = false;
              break;
            }
        }
      else
        // The final column - use fast code.
        sorted = is_sorted (lo, n, comp);
    }

  return sorted;
}

template <class T>
bool
octave_sort<T>::is_sorted_rows (const T *data, octave_idx_type rows,
                                octave_idx_type cols)
{
  bool retval = false;

#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    retval = is_sorted_rows (data, rows, cols, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      retval = is_sorted_rows (data, rows, cols, std::greater<T> ());
    else
#endif
      if (compare)
        retval = is_sorted_rows (data, rows, cols, compare);

  return retval;
}

// The simple binary lookup.

template <class T> template <class Comp>
octave_idx_type
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T& value, Comp comp)
{
  octave_idx_type lo = 0;
  octave_idx_type hi = nel;

  while (lo < hi)
    {
      octave_idx_type mid = lo + ((hi-lo) >> 1);
      if (comp (value, data[mid]))
        hi = mid;
      else
        lo = mid + 1;
    }

  return lo;
}

template <class T>
octave_idx_type
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T& value)
{
  octave_idx_type retval = 0;

#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    retval = lookup (data, nel, value, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      retval = lookup (data, nel, value, std::greater<T> ());
    else
#endif
      if (compare)
        retval = lookup (data, nel, value, std::ptr_fun (compare));

  return retval;
}

template <class T> template <class Comp>
void
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T *values, octave_idx_type nvalues,
                        octave_idx_type *idx, Comp comp)
{
  // Use a sequence of binary lookups.
  // TODO: Can this be sped up generally? The sorted merge case is dealt with
  // elsewhere.
  for (octave_idx_type j = 0; j < nvalues; j++)
    idx[j] = lookup (data, nel, values[j], comp);
}

template <class T>
void
octave_sort<T>::lookup (const T *data, octave_idx_type nel,
                        const T* values, octave_idx_type nvalues,
                        octave_idx_type *idx)
{
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    lookup (data, nel, values, nvalues, idx, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      lookup (data, nel, values, nvalues, idx, std::greater<T> ());
    else
#endif
      if (compare)
        lookup (data, nel, values, nvalues, idx, std::ptr_fun (compare));
}

template <class T> template <class Comp>
void
octave_sort<T>::lookup_sorted (const T *data, octave_idx_type nel,
                               const T *values, octave_idx_type nvalues,
                               octave_idx_type *idx, bool rev, Comp comp)
{
  if (rev)
    {
      octave_idx_type i = 0;
      octave_idx_type j = nvalues - 1;

      if (nvalues > 0 && nel > 0)
        {
          while (true)
            {
              if (comp (values[j], data[i]))
                {
                  idx[j] = i;
                  if (--j < 0)
                    break;
                }
              else if (++i == nel)
                break;
            }
        }

      for (; j >= 0; j--)
        idx[j] = i;
    }
  else
    {
      octave_idx_type i = 0;
      octave_idx_type j = 0;

      if (nvalues > 0 && nel > 0)
        {
          while (true)
            {
              if (comp (values[j], data[i]))
                {
                  idx[j] = i;
                  if (++j == nvalues)
                    break;
                }
              else if (++i == nel)
                break;
            }
        }

      for (; j != nvalues; j++)
        idx[j] = i;
    }
}

template <class T>
void
octave_sort<T>::lookup_sorted (const T *data, octave_idx_type nel,
                               const T* values, octave_idx_type nvalues,
                               octave_idx_type *idx, bool rev)
{
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    lookup_sorted (data, nel, values, nvalues, idx, rev, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      lookup_sorted (data, nel, values, nvalues, idx, rev, std::greater<T> ());
    else
#endif
      if (compare)
        lookup_sorted (data, nel, values, nvalues, idx, rev,
                       std::ptr_fun (compare));
}

template <class T> template <class Comp>
void
octave_sort<T>::nth_element (T *data, octave_idx_type nel,
                             octave_idx_type lo, octave_idx_type up,
                             Comp comp)
{
  // Simply wrap the STL algorithms.
  // FIXME: this will fail if we attempt to inline <,> for Complex.
  if (up == lo+1)
    std::nth_element (data, data + lo, data + nel, comp);
  else if (lo == 0)
    std::partial_sort (data, data + up, data + nel, comp);
  else
    {
      std::nth_element (data, data + lo, data + nel, comp);
      if (up == lo + 2)
        {
          // Finding two subsequent elements.
          std::swap (data[lo+1],
                     *std::min_element (data + lo + 1, data + nel, comp));
        }
      else
        std::partial_sort (data + lo + 1, data + up, data + nel, comp);
    }
}

template <class T>
void
octave_sort<T>::nth_element (T *data, octave_idx_type nel,
                             octave_idx_type lo, octave_idx_type up)
{
  if (up < 0)
    up = lo + 1;
#ifdef INLINE_ASCENDING_SORT
  if (compare == ascending_compare)
    nth_element (data, nel, lo, up, std::less<T> ());
  else
#endif
#ifdef INLINE_DESCENDING_SORT
    if (compare == descending_compare)
      nth_element (data, nel, lo, up, std::greater<T> ());
    else
#endif
      if (compare)
        nth_element (data, nel, lo, up, std::ptr_fun (compare));
}

template <class T>
bool
octave_sort<T>::ascending_compare (typename ref_param<T>::type x,
                                   typename ref_param<T>::type y)
{
  return x < y;
}

template <class T>
bool
octave_sort<T>::descending_compare (typename ref_param<T>::type x,
                                    typename ref_param<T>::type y)
{
  return x > y;
}
