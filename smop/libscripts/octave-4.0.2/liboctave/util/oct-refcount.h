/*

Copyright (C) 2012-2015 Jaroslav Hajek

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

#if !defined (octave_oct_refcount_h)
#define octave_oct_refcount_h 1

#ifndef OCTAVE_CONFIG_INCLUDED
# error "The file <octave/config.h> must be included before oct-refcount.h."
#endif

#if defined (USE_ATOMIC_REFCOUNT) && (defined (_MSC_VER) || defined (__GNUC__))
# if defined (_MSC_VER)
#  include <intrin.h>
#  define OCTREFCOUNT_ATOMIC_INCREMENT(x) _InterlockedIncrement((long*)x)
#  define OCTREFCOUNT_ATOMIC_DECREMENT(x) _InterlockedDecrement((long*)x)
#  define OCTREFCOUNT_ATOMIC_INCREMENT_POST(x) _InterlockedExchangeAdd((long*)x,  1)
#  define OCTREFCOUNT_ATOMIC_DECREMENT_POST(x) _InterlockedExchangeAdd((long*)x, -1)
# elif defined (__GNUC__)
#  define OCTREFCOUNT_ATOMIC_INCREMENT(x) __sync_add_and_fetch(x,  1)
#  define OCTREFCOUNT_ATOMIC_DECREMENT(x) __sync_add_and_fetch(x, -1)
#  define OCTREFCOUNT_ATOMIC_INCREMENT_POST(x) __sync_fetch_and_add(x,  1)
#  define OCTREFCOUNT_ATOMIC_DECREMENT_POST(x) __sync_fetch_and_add(x, -1)
# endif
#else // Generic non-locking versions
# define OCTREFCOUNT_ATOMIC_INCREMENT(x) ++(*(x))
# define OCTREFCOUNT_ATOMIC_DECREMENT(x) --(*(x))
# define OCTREFCOUNT_ATOMIC_INCREMENT_POST(x) (*(x))++
# define OCTREFCOUNT_ATOMIC_DECREMENT_POST(x) (*(x))--
#endif

// Encapsulates a reference counter.
template <class T>
class octave_refcount
{
public:
  typedef T count_type;

  octave_refcount(count_type initial_count) : count(initial_count) { }

  // Increment/Decrement. int is postfix.
  count_type operator++(void)
  {
    return OCTREFCOUNT_ATOMIC_INCREMENT (&count);
  }

  count_type operator++(int)
  {
    return OCTREFCOUNT_ATOMIC_INCREMENT_POST (&count);
  }

  count_type operator--(void)
  {
    return OCTREFCOUNT_ATOMIC_DECREMENT (&count);
  }

  count_type operator--(int)
  {
    return OCTREFCOUNT_ATOMIC_DECREMENT_POST (&count);
  }

  operator count_type (void) const
  {
    return static_cast<count_type const volatile&> (count);
  }

  count_type *get (void)
  {
    return &count;
  }

private:
  count_type count;
};

#endif
