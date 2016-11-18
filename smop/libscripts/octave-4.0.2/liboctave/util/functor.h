/*

Copyright (C) 2008-2015 John W. Eaton

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

#if !defined (octave_functor_h)
#define octave_functor_h 1

template <typename RT, typename PT>
class fcn_ptr
{
public:
  typedef RT (*TYPE) (PT);
};

template <typename RT, typename PT>
class functor
{
private:
  typedef typename fcn_ptr<RT, PT>::TYPE fcn_ptr_type;
  fcn_ptr_type fptr;

public:

  functor (fcn_ptr_type p) : fptr (p) { }

  RT operator () (PT arg) { return fptr (arg); }
};

template <typename CT, typename RT, typename PT>
class functor_with_conversion
{
private:
  typedef typename fcn_ptr<RT, PT>::TYPE fcn_ptr_type;
  fcn_ptr_type fptr;

public:

  functor_with_conversion (fcn_ptr_type p) : fptr (p) { }

  CT operator () (PT arg) { return CT (fptr (arg)); }
};

template <typename RT, typename PT>
functor<RT, PT>
func_ptr (RT (*f) (PT))
{
  return functor<RT, PT> (f);
}

template <typename CT, typename RT, typename PT>
functor_with_conversion<CT, RT, PT>
func_ptr_with_conversion (RT (*f) (PT))
{
  return functor_with_conversion<CT, RT, PT> (f);
}

#endif
