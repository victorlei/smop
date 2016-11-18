/*
Copyright (C) 2003-2015 David Bateman
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

#if !defined (octave_oct_sort_h)
#define octave_oct_sort_h 1

#include "lo-traits.h"

// The maximum number of entries in a MergeState's pending-runs stack.
// This is enough to sort arrays of size up to about
//     32 * phi ** MAX_MERGE_PENDING
// where phi ~= 1.618.  85 is ridiculously large enough, good for an array
// with 2**64 elements.
#define MAX_MERGE_PENDING 85

// When we get into galloping mode, we stay there until both runs win less
// often than MIN_GALLOP consecutive times.  See listsort.txt for more info.
#define MIN_GALLOP 7

// Avoid malloc for small temp arrays.
#define MERGESTATE_TEMP_SIZE 1024

// Enum for type of sort function
enum sortmode { UNSORTED = 0, ASCENDING, DESCENDING };

template <class T>
class
octave_sort
{
public:

  typedef bool (*compare_fcn_type) (typename ref_param<T>::type,
                                    typename ref_param<T>::type);

  octave_sort (void);

  octave_sort (compare_fcn_type);

  ~octave_sort (void);

  void set_compare (compare_fcn_type comp) { compare = comp; }

  void set_compare (sortmode mode);

  // Sort an array in-place.
  void sort (T *data, octave_idx_type nel);

  // Ditto, but also permute the passed indices (may not be valid indices).
  void sort (T *data, octave_idx_type *idx, octave_idx_type nel);

  // Check whether an array is sorted.
  bool is_sorted (const T *data, octave_idx_type nel);

  // Sort a matrix by rows, return a permutation
  // vector.
  void sort_rows (const T *data, octave_idx_type *idx,
                  octave_idx_type rows, octave_idx_type cols);

  // Determine whether a matrix (as a contiguous block) is sorted by rows.
  bool is_sorted_rows (const T *data,
                       octave_idx_type rows, octave_idx_type cols);

  // Do a binary lookup in a sorted array.
  octave_idx_type lookup (const T *data, octave_idx_type nel,
                          const T& value);

  // Ditto, but for an array.
  void lookup (const T *data, octave_idx_type nel,
               const T* values, octave_idx_type nvalues,
               octave_idx_type *idx);

  // A linear merge of two sorted tables. rev indicates the second table is
  // in reverse order.
  void lookup_sorted (const T *data, octave_idx_type nel,
                      const T* values, octave_idx_type nvalues,
                      octave_idx_type *idx, bool rev = false);

  // Rearranges the array so that the elements with indices
  // lo..up-1 are in their correct place.
  void nth_element (T *data, octave_idx_type nel,
                    octave_idx_type lo, octave_idx_type up = -1);

  static bool ascending_compare (typename ref_param<T>::type,
                                 typename ref_param<T>::type);

  static bool descending_compare (typename ref_param<T>::type,
                                  typename ref_param<T>::type);

private:

  // One MergeState exists on the stack per invocation of mergesort.
  // It's just a convenient way to pass state around among the helper
  // functions.
  //
  // DGB: This isn't needed with mergesort in a class, but it doesn't
  // slow things up, and it is likely to make my life easier for any
  // potential backporting of changes in the Python code.

  struct s_slice
  {
    octave_idx_type base, len;
  };

  struct MergeState
  {
    MergeState (void)
      : min_gallop (), a (0), ia (0), alloced (0), n (0)
    { reset (); }

    ~MergeState (void)
    { delete [] a; delete [] ia; }

    void reset (void)
    { min_gallop = MIN_GALLOP; n = 0; }

    void getmem (octave_idx_type need);

    void getmemi (octave_idx_type need);

    // This controls when we get *into* galloping mode.  It's
    // initialized to MIN_GALLOP.  merge_lo and merge_hi tend to nudge
    // it higher for random data, and lower for highly structured
    // data.
    octave_idx_type min_gallop;

    // 'a' is temp storage to help with merges.  It contains room for
    // alloced entries.
    T *a;               // may point to temparray below
    octave_idx_type *ia;
    octave_idx_type alloced;

    // A stack of n pending runs yet to be merged.  Run #i starts at
    // address base[i] and extends for len[i] elements.  It's always
    // true (so long as the indices are in bounds) that
    //
    //   pending[i].base + pending[i].len == pending[i+1].base
    //
    // so we could cut the storage for this, but it's a minor amount,
    // and keeping all the info explicit simplifies the code.
    octave_idx_type n;
    struct s_slice pending[MAX_MERGE_PENDING];

    // No copying!

    MergeState (const MergeState&);

    MergeState& operator = (const MergeState&);
  };

  compare_fcn_type compare;

  MergeState *ms;


  template <class Comp>
  void binarysort (T *data, octave_idx_type nel,
                   octave_idx_type start, Comp comp);

  template <class Comp>
  void binarysort (T *data, octave_idx_type *idx, octave_idx_type nel,
                   octave_idx_type start, Comp comp);

  template <class Comp>
  octave_idx_type count_run (T *lo, octave_idx_type n, bool& descending,
                             Comp comp);

  template <class Comp>
  octave_idx_type gallop_left (T key, T *a, octave_idx_type n,
                               octave_idx_type hint, Comp comp);

  template <class Comp>
  octave_idx_type gallop_right (T key, T *a, octave_idx_type n,
                                octave_idx_type hint, Comp comp);

  template <class Comp>
  int merge_lo (T *pa, octave_idx_type na,
                T *pb, octave_idx_type nb,
                Comp comp);

  template <class Comp>
  int merge_lo (T *pa, octave_idx_type *ipa, octave_idx_type na,
                T *pb, octave_idx_type *ipb, octave_idx_type nb,
                Comp comp);

  template <class Comp>
  int merge_hi (T *pa, octave_idx_type na,
                T *pb, octave_idx_type nb,
                Comp comp);

  template <class Comp>
  int merge_hi (T *pa, octave_idx_type *ipa, octave_idx_type na,
                T *pb, octave_idx_type *ipb, octave_idx_type nb,
                Comp comp);

  template <class Comp>
  int merge_at (octave_idx_type i, T *data, Comp comp);

  template <class Comp>
  int merge_at (octave_idx_type i, T *data, octave_idx_type *idx, Comp comp);

  template <class Comp>
  int merge_collapse (T *data, Comp comp);

  template <class Comp>
  int merge_collapse (T *data, octave_idx_type *idx, Comp comp);

  template <class Comp>
  int merge_force_collapse (T *data, Comp comp);

  template <class Comp>
  int merge_force_collapse (T *data, octave_idx_type *idx, Comp comp);

  octave_idx_type merge_compute_minrun (octave_idx_type n);

  template <class Comp>
  void sort (T *data, octave_idx_type nel, Comp comp);

  template <class Comp>
  void sort (T *data, octave_idx_type *idx, octave_idx_type nel, Comp comp);

  template <class Comp>
  bool is_sorted (const T *data, octave_idx_type nel, Comp comp);

  template <class Comp>
  void sort_rows (const T *data, octave_idx_type *idx,
                  octave_idx_type rows, octave_idx_type cols,
                  Comp comp);

  template <class Comp>
  bool is_sorted_rows (const T *data, octave_idx_type rows,
                       octave_idx_type cols, Comp comp);

  template <class Comp>
  octave_idx_type lookup (const T *data, octave_idx_type nel,
                          const T& value, Comp comp);

  template <class Comp>
  void lookup (const T *data, octave_idx_type nel,
               const T* values, octave_idx_type nvalues,
               octave_idx_type *idx, Comp comp);

  template <class Comp>
  void lookup_sorted (const T *data, octave_idx_type nel,
                      const T* values, octave_idx_type nvalues,
                      octave_idx_type *idx, bool rev, Comp comp);

  template <class Comp>
  void nth_element (T *data, octave_idx_type nel,
                    octave_idx_type lo, octave_idx_type up,
                    Comp comp);

  // No copying!

  octave_sort (const octave_sort&);

  octave_sort& operator = (const octave_sort&);
};

template <class T>
class
vec_index
{
public:
  T vec;
  octave_idx_type indx;
};
#endif
