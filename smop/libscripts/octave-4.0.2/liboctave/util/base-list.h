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

#if !defined (octave_base_list_h)
#define octave_base_list_h 1

#include <cstdlib>

#include <list>

template <typename elt_type>
class
octave_base_list
{
public:

  typedef typename std::list<elt_type>::iterator iterator;
  typedef typename std::list<elt_type>::const_iterator const_iterator;

  typedef typename std::list<elt_type>::reverse_iterator reverse_iterator;
  typedef typename std::list<elt_type>::const_reverse_iterator const_reverse_iterator;

  bool empty (void) const { return lst.empty (); }

  size_t size (void) const { return lst.size (); }
  size_t length (void) const { return size (); }

  iterator erase (iterator pos) { return lst.erase (pos); }

  template <class P>
  void remove_if (P pred)
  {
    lst.remove_if (pred);

    // FIXME: kluge removed 8/7/13.  Eventually this commented
    //        code should be deleted.
    //
    // FIXME: this kluge should be removed at some point.
    // We would like to simply call
    //
    //   lst.remove_if (pred);
    //
    // but the Sun Studio compiler chokes on that.
    //
    // iterator b = lst.begin ();
    // iterator e = lst.end ();
    // while (b != e)
    //   {
    //     iterator n = b;
    //     n++;
    //     if (pred (*b))
    //       lst.erase (b);
    //     b = n;
    //   }
  }

  void clear (void) { lst.clear (); }

  iterator begin (void) { return iterator (lst.begin ()); }
  const_iterator begin (void) const { return const_iterator (lst.begin ()); }

  iterator end (void) { return iterator (lst.end ()); }
  const_iterator end (void) const { return const_iterator (lst.end ()); }

  reverse_iterator rbegin (void) { return reverse_iterator (lst.rbegin ()); }
  const_reverse_iterator rbegin (void) const
  { return const_reverse_iterator (lst.rbegin ()); }

  reverse_iterator rend (void) { return reverse_iterator (lst.rend ()); }
  const_reverse_iterator rend (void) const
  { return const_reverse_iterator (lst.rend ()); }

  elt_type& front (void) { return lst.front (); }
  elt_type& back (void) { return lst.back (); }

  const elt_type& front (void) const { return lst.front (); }
  const elt_type& back (void) const { return lst.back (); }

  void push_front (const elt_type& s) { lst.push_front (s); }
  void push_back (const elt_type& s) { lst.push_back (s); }

  void pop_front (void) { lst.pop_front (); }
  void pop_back (void) { lst.pop_back (); }

  // For backward compatibility.
  void append (const elt_type& s) { lst.push_back (s); }

  octave_base_list (void) : lst () { }

  octave_base_list (const std::list<elt_type>& l) : lst (l) { }

  octave_base_list (const octave_base_list& bl) : lst (bl.lst) { }

  octave_base_list& operator = (const octave_base_list& bl)
  {
    if (this != &bl)
      {
        lst = bl.lst;
      }
    return *this;
  }

  ~octave_base_list (void) { }

private:

  std::list<elt_type> lst;
};

#endif
