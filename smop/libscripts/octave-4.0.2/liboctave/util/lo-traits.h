/*

Copyright (C) 2009-2015 John W. Eaton

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

#if !defined (octave_lo_traits_h)
#define octave_lo_traits_h 1

// Ideas for these classes taken from C++ Templates, The Complete
// Guide by David Vandevoorde and Nicolai M. Josuttis, Addison-Wesley
// (2003).

// Select a type based on the value of a constant expression.

template <bool cond, typename T1, typename T2>
class if_then_else;

template<typename T1, typename T2>
class if_then_else<true, T1, T2>
{
public:

  typedef T1 result;
};

template<typename T1, typename T2>
class if_then_else<false, T1, T2>
{
public:

  typedef T2 result;
};

// Determine whether two types are equal.
template <class T1, class T2>
class equal_types
{
public:

  static const bool value = false;
};

template <class T>
class equal_types <T, T>
{
public:

  static const bool value = true;
};

// Determine whether a type is an instance of a template.

template <template <class> class Template, class T>
class is_instance
{
public:

  static const bool value = false;
};

template <template <class> class Template, class T>
class is_instance <Template, Template<T> >
{
public:

  static const bool value = true;
};

// Determine whether a template paramter is a class type.

template<typename T1>
class is_class_type
{
private:

  typedef char one;
  typedef struct { char c[2]; } two;

  // Classes can have pointers to members.
  template<typename T2> static one is_class_type_test (int T2::*);

  // Catch everything else.
  template<typename T2> static two is_class_type_test (...);

public:

  enum { yes = sizeof (is_class_type_test<T1> (0)) == 1 };
  enum { no = ! yes };
};

// Define typename ref_param<T>::type as T const& if T is a class
// type.  Otherwise, define it to be T.

template<typename T>
class ref_param
{
public:

  typedef typename if_then_else<is_class_type<T>::no, T, T const&>::result type;
};

// Will turn TemplatedClass<T> to T, leave T otherwise.
// Useful for stripping wrapper classes, like octave_int.

template<template<typename> class TemplatedClass, typename T>
class strip_template_param
{
public:
  typedef T type;
};

template<template<typename> class TemplatedClass, typename T>
class strip_template_param<TemplatedClass, TemplatedClass<T> >
{
public:
  typedef T type;
};

// Will turn TemplatedClass<T> to TemplatedClass<S>, T to S otherwise.
// Useful for generic promotions.

template<template<typename> class TemplatedClass, typename T, typename S>
class subst_template_param
{
public:
  typedef S type;
};

template<template<typename> class TemplatedClass, typename T, typename S>
class subst_template_param<TemplatedClass, TemplatedClass<T>, S>
{
public:
  typedef TemplatedClass<S> type;
};

#endif
