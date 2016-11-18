/*

Copyright (C) 2004-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek

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

#if !defined (octave_oct_inttypes_h)
#define octave_oct_inttypes_h 1

#include <cstdlib>

#include <limits>
#include <iosfwd>

#include "lo-traits.h"
#include "lo-math.h"
#include "lo-mappers.h"

#ifdef OCTAVE_INT_USE_LONG_DOUBLE
inline long double xround (long double x) { return roundl (x); }
inline long double xisnan (long double x)
{ return xisnan (static_cast<double> (x)); }
#endif

// FIXME: we define this by our own because some compilers, such as
// MSVC, do not provide std::abs (int64_t) and std::abs (uint64_t).  In
// the future, it should go away in favor of std::abs.
template <class T>
inline T octave_int_abs (T x) { return x >= 0 ? x : -x; }

// Query for an integer type of certain sizeof, and signedness.
template<int qsize, bool qsigned>
struct query_integer_type
{
public:
  static const bool registered = false;
  typedef void type; // Void shall result in a compile-time error if we
                     // attempt to use it in computations.
};

#define REGISTER_INT_TYPE(TYPE) \
template <> \
class query_integer_type<sizeof (TYPE), std::numeric_limits<TYPE>::is_signed> \
{ \
public: \
  static const bool registered = true; \
  typedef TYPE type; \
}

// No two registered integers can share sizeof and signedness.
REGISTER_INT_TYPE (int8_t);
REGISTER_INT_TYPE (uint8_t);
REGISTER_INT_TYPE (int16_t);
REGISTER_INT_TYPE (uint16_t);
REGISTER_INT_TYPE (int32_t);
REGISTER_INT_TYPE (uint32_t);
REGISTER_INT_TYPE (int64_t);
REGISTER_INT_TYPE (uint64_t);

// Rationale: Comparators have a single static method, rel(), that returns the
// result of the binary relation. They also have two static boolean fields:
// ltval, gtval determine the value of x OP y if x < y, x > y, respectively.
#define REGISTER_OCTAVE_CMP_OP(NM,OP) \
  class NM \
    { \
    public: \
      static const bool ltval = (0 OP 1); \
      static const bool gtval = (1 OP 0); \
      template <class T> \
      static bool op (T x, T y) { return x OP y; } \
    }

// We also provide two special relations: ct, yielding always true, and cf,
// yielding always false.
#define REGISTER_OCTAVE_CONST_OP(NM,value) \
  class NM \
    { \
    public: \
      static const bool ltval = value; \
      static const bool gtval = value; \
      template <class T> \
      static bool op (T, T) { return value; } \
    }

// Handles non-homogeneous integer comparisons. Avoids doing useless tests.
class octave_int_cmp_op
{
  // This determines a suitable promotion type for T1 when meeting T2 in a
  // binary relation. If promotion to int or T2 is safe, it is used. Otherwise,
  // the signedness of T1 is preserved and it is widened if T2 is wider.
  // Notice that if this is applied to both types, they must end up with equal
  // size.
  template <class T1, class T2>
  class prom
  {
    // Promote to int?
    static const bool pint = (sizeof (T1) < sizeof (int)
                              && sizeof (T2) < sizeof (int));
    static const bool t1sig = std::numeric_limits<T1>::is_signed;
    static const bool t2sig = std::numeric_limits<T2>::is_signed;
    static const bool psig =
      (pint || (sizeof (T2) > sizeof (T1) && t2sig) || t1sig);
    static const int psize =
      (pint ? sizeof (int) : (sizeof (T2) > sizeof (T1)
                              ? sizeof (T2) : sizeof (T1)));
  public:
    typedef typename query_integer_type<psize, psig>::type type;
  };

  // Implements comparisons between two types of equal size but
  // possibly different signedness.
  template<class xop, int size>
  class uiop
  {
    typedef typename query_integer_type<size, false>::type utype;
    typedef typename query_integer_type<size, true>::type stype;
  public:
    static bool op (utype x, utype y)
    { return xop::op (x, y); }
    static bool op (stype x, stype y)
    { return xop::op (x, y); }
    static bool op (stype x, utype y)
    { return (x < 0) ? xop::ltval : xop::op (static_cast<utype> (x), y); }
    static bool op (utype x, stype y)
    { return (y < 0) ? xop::gtval : xop::op (x, static_cast<utype> (y)); }
  };

public:
  REGISTER_OCTAVE_CMP_OP (lt, <);
  REGISTER_OCTAVE_CMP_OP (le, <=);
  REGISTER_OCTAVE_CMP_OP (gt, >);
  REGISTER_OCTAVE_CMP_OP (ge, >=);
  REGISTER_OCTAVE_CMP_OP (eq, ==);
  REGISTER_OCTAVE_CMP_OP (ne, !=);
  REGISTER_OCTAVE_CONST_OP (ct, true);
  REGISTER_OCTAVE_CONST_OP (cf, false);

  // Universal comparison operation.
  template<class xop, class T1, class T2>
  static bool
  op (T1 x, T2 y)
  {
    typedef typename prom<T1, T2>::type PT1;
    typedef typename prom<T2, T1>::type PT2;
    return uiop<xop, sizeof (PT1)>::op (static_cast<PT1> (x),
                                        static_cast<PT2> (y));
  }

public:

  // Mixed comparisons
  template <class xop, class T>
  static bool
  mop (T x, double y)
  { return xop::op (static_cast<double> (x), y); }

  template <class xop, class T>
  static bool
  mop (double x, T y)
  { return xop::op (x, static_cast<double> (y)); }

#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED
#define DECLARE_EXTERNAL_LONG_DOUBLE_CMP_OPS(T) \
  template <class xop> static OCTAVE_API bool \
  external_mop (double, T); \
  template <class xop> static OCTAVE_API bool \
  external_mop (T, double)

  DECLARE_EXTERNAL_LONG_DOUBLE_CMP_OPS (int64_t);
  DECLARE_EXTERNAL_LONG_DOUBLE_CMP_OPS (uint64_t);
#endif

  // Typecasting to doubles won't work properly for 64-bit integers --
  // they lose precision.
  // If we have long doubles, use them...
#ifdef OCTAVE_INT_USE_LONG_DOUBLE
#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED
#define DEFINE_LONG_DOUBLE_CMP_OP(T) \
  template <class xop> \
  static bool \
  mop (double x, T y) \
  { \
    return external_mop<xop> (x, y); \
  } \
  template <class xop> \
  static bool \
  mop (T x, double y) \
  { \
    return external_mop<xop> (x, y); \
  }
#else
#define DEFINE_LONG_DOUBLE_CMP_OP(T) \
  template <class xop> \
  static bool \
  mop (double x, T y) \
  { \
    return xop::op (static_cast<long double> (x), \
                    static_cast<long double> (y)); \
  } \
  template <class xop> \
  static bool \
  mop (T x, double y) \
  { \
    return xop::op (static_cast<long double> (x), \
                    static_cast<long double> (y)); \
  }
#endif
#else
  // ... otherwise, use external handlers

  // FIXME: We could declare directly the mop methods as external,
  // but we can't do this because bugs in gcc (<= 4.3) prevent
  // explicit instantiations later in that case.
#define DEFINE_LONG_DOUBLE_CMP_OP(T) \
  template <class xop> static OCTAVE_API bool \
  emulate_mop (double, T); \
  template <class xop> \
  static bool \
  mop (double x, T y) \
    { \
      return emulate_mop<xop> (x, y); \
    } \
  template <class xop> static OCTAVE_API bool \
  emulate_mop (T, double); \
  template <class xop> \
  static bool \
  mop (T x, double y) \
    { \
      return emulate_mop<xop> (x, y); \
    }
#endif

  DEFINE_LONG_DOUBLE_CMP_OP(int64_t)
  DEFINE_LONG_DOUBLE_CMP_OP(uint64_t)

#undef DEFINE_LONG_DOUBLE_CMP_OP
};

// Base integer class. No data, just conversion methods and exception flags.
template <class T>
class octave_int_base
{
public:

  static T min_val () { return std::numeric_limits<T>:: min (); }
  static T max_val () { return std::numeric_limits<T>:: max (); }

  // Convert integer value.
  template <class S>
  static T
  truncate_int (const S& value)
  {
    // An exhaustive test whether the max and/or min check can be omitted.
    static const bool t_is_signed = std::numeric_limits<T>::is_signed;
    static const bool s_is_signed = std::numeric_limits<S>::is_signed;
    static const int t_size = sizeof (T);
    static const int s_size = sizeof (S);

    static const bool omit_chk_min =
      (! s_is_signed || (t_is_signed && t_size >= s_size));
    static const bool omit_chk_max =
      (t_size > s_size || (t_size == s_size
                           && (! t_is_signed || s_is_signed)));
    // If the check can be omitted, substitute constant false relation.
    typedef octave_int_cmp_op::cf cf;
    typedef octave_int_cmp_op::lt lt;
    typedef octave_int_cmp_op::gt gt;
    typedef typename if_then_else<omit_chk_min, cf, lt>::result chk_min;
    typedef typename if_then_else<omit_chk_max, cf, gt>::result chk_max;

    // Efficiency of the following depends on inlining and dead code
    // elimination, but that should be a piece of cake for most compilers.
    if (chk_min::op (value, static_cast<S> (min_val ())))
      {
        return min_val ();
      }
    else if (chk_max::op (value, static_cast<S> (max_val ())))
      {
        return max_val ();
      }
    else
      return static_cast<T> (value);
  }

private:

  // Computes a real-valued threshold for a max/min check.
  template <class S>
  static S
  compute_threshold (S val, T orig_val)
  {
    val = xround (val); // Fool optimizations (maybe redundant)
    // If val is even, but orig_val is odd, we're one unit off.
    if (orig_val % 2 && val / 2 == xround (val / 2))
      // FIXME: is this always correct?
      val *= (static_cast<S> (1) - (std::numeric_limits<S>::epsilon () / 2));
    return val;
  }

public:

  // Convert a real number (check NaN and non-int).
  template <class S>
  static T
  convert_real (const S& value);
};

// Saturated (homogeneous) integer arithmetics. The signed and unsigned
// implementations are significantly different, so we implement another layer
// and completely specialize. Arithmetics inherits from octave_int_base so that
// it can use its exceptions and truncation functions.

template <class T, bool is_signed>
class octave_int_arith_base
{ };

// Unsigned arithmetics. C++ standard requires it to be modular, so the
// overflows can be handled efficiently and reliably.
template <class T>
class octave_int_arith_base<T, false> : octave_int_base<T>
{
public:

  static T
  abs (T x) { return x; }

  static T
  signum (T x) { return x ? static_cast<T> (1) : static_cast<T> (0); }

  // Shifts do not overflow.
  static T
  rshift (T x, int n) { return x >> n; }

  static T
  lshift (T x, int n) { return x << n; }

  static T
  minus (T)
  {
    return static_cast<T> (0);
  }

  // the overflow behaviour for unsigned integers is guaranteed by C/C++,
  // so the following should always work.
  static T
  add (T x, T y)
  {
    T u = x + y;
    if (u < x)
      {
        u = octave_int_base<T>::max_val ();
      }
    return u;
  }

  static T
  sub (T x, T y)
  {
    T u = x - y;
    if (u > x)
      {
        u = 0;
      }
    return u;
  }

  // Multiplication is done using promotion to wider integer type. If there is
  // no suitable promotion type, this operation *MUST* be specialized.
  static T mul (T x, T y) { return mul_internal (x, y); }

  static T
  mul_internal (T x, T y)
  {
    // Promotion type for multiplication (if exists).
    typedef typename query_integer_type<2*sizeof (T), false>::type mptype;
    return octave_int_base<T>::truncate_int (static_cast<mptype> (x)
           * static_cast<mptype> (y));
  }

  // Division with rounding to nearest. Note that / and % are probably
  // computed by a single instruction.
  static T
  div (T x, T y)
  {
    if (y != 0)
      {
        T z = x / y;
        T w = x % y;
        if (w >= y-w) z += 1;
        return z;
      }
    else
      {
        return x ? octave_int_base<T>::max_val () : 0;
      }
  }

  // Remainder.
  static T
  rem (T x, T y)
  {
    return y != 0 ? x % y : 0;
  }

  // Modulus. Note the weird y = 0 case for Matlab compatibility.
  static T
  mod (T x, T y)
  {
    return y != 0 ? x % y : x;
  }
};

#ifdef OCTAVE_INT_USE_LONG_DOUBLE

// Handle 64-bit multiply using long double

#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED

extern OCTAVE_API uint64_t
octave_external_uint64_uint64_mul (uint64_t, uint64_t);

#endif

template <>
inline uint64_t
octave_int_arith_base<uint64_t, false>::mul_internal (uint64_t x, uint64_t y)
{
  uint64_t retval;

  long double p = static_cast<long double> (x) * static_cast<long double> (y);

  if (p > static_cast<long double> (octave_int_base<uint64_t>::max_val ()))
    retval = octave_int_base<uint64_t>::max_val ();
  else
    retval = static_cast<uint64_t> (p);

  return retval;
}

template <>
inline uint64_t
octave_int_arith_base<uint64_t, false>::mul (uint64_t x, uint64_t y)
{
#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED
  return octave_external_uint64_uint64_mul (x, y);
#else
  return mul_internal (x, y);
#endif
}

#else

// Special handler for 64-bit integer multiply.
template <>
OCTAVE_API uint64_t
octave_int_arith_base<uint64_t, false>::mul_internal (uint64_t, uint64_t);

#endif

// Signed integer arithmetics.
// Rationale: If HAVE_FAST_INT_OPS is defined, the following conditions
// should hold:
// 1. Signed numbers are represented by twos complement
//    (see <http://en.wikipedia.org/wiki/Two%27s_complement>)
// 2. static_cast to unsigned int counterpart works like interpreting
//    the signed bit pattern as unsigned (and is thus zero-cost).
// 3. Signed addition and subtraction yield the same bit results as unsigned.
//    (We use casts to prevent optimization interference, so there is no
//     need for things like -ftrapv).
// 4. Bit operations on signed integers work like on unsigned integers,
//    except for the shifts. Shifts are arithmetic.
//
// The above conditions are satisfied by most modern platforms. If
// HAVE_FAST_INT_OPS is defined, bit tricks and wraparound arithmetics are used
// to avoid conditional jumps as much as possible, thus being friendly to
// modern pipeline processor architectures.
// Otherwise, we fall back to a bullet-proof code that only uses assumptions
// guaranteed by the standard.

template <class T>
class octave_int_arith_base<T, true> : octave_int_base<T>
{
  // The corresponding unsigned type.
  typedef typename query_integer_type<sizeof (T), false>::type UT;
public:

  // Returns 1 for negative number, 0 otherwise.
  static T
  __signbit (T x)
  {
#ifdef HAVE_FAST_INT_OPS
    return static_cast<UT> (x) >> std::numeric_limits<T>::digits;
#else
    return (x < 0) ? 1 : 0;
#endif
  }

  static T
  abs (T x)
  {
#ifdef HAVE_FAST_INT_OPS
    // This is close to how GCC does std::abs, but we can't just use std::abs,
    // because its behaviour for INT_MIN is undefined and the compiler could
    // discard the following test.
    T m = x >> std::numeric_limits<T>::digits;
    T y = (x ^ m) - m;
    if (y < 0)
      {
        y = octave_int_base<T>::max_val ();
      }
    return y;
#else
    // -INT_MAX is safe because C++ actually allows only three implementations
    // of integers: sign & magnitude, ones complement and twos complement.
    // The first test will, with modest optimizations, evaluate at compile
    // time, and maybe eliminate the branch completely.
    T y;
    if (octave_int_base<T>::min_val () < -octave_int_base<T>::max_val ()
        && x == octave_int_base<T>::min_val ())
      {
        y = octave_int_base<T>::max_val ();
      }
    else
      y = (x < 0) ? -x : x;
    return y;
#endif
  }

  static T
  signum (T x)
  {
    // With modest optimizations, this will compile without a jump.
    return ((x > 0) ? 1 : 0) - __signbit (x);
  }

  // FIXME: we do not have an authority what signed shifts should
  // exactly do, so we define them the easy way. Note that Matlab does
  // not define signed shifts.

  static T
  rshift (T x, int n) { return x >> n; }

  static T
  lshift (T x, int n) { return x << n; }

  // Minus has problems similar to abs.
  static T
  minus (T x)
  {
#ifdef HAVE_FAST_INT_OPS
    T y = -x;
    if (y == octave_int_base<T>::min_val ())
      {
        --y;
      }
    return y;
#else
    T y;
    if (octave_int_base<T>::min_val () < -octave_int_base<T>::max_val ()
        && x == octave_int_base<T>::min_val ())
      {
        y = octave_int_base<T>::max_val ();
      }
    else
      y = -x;
    return y;
#endif
  }

  static T
  add (T x, T y)
  {
#ifdef HAVE_FAST_INT_OPS
    // The typecasts do nothing, but they are here to prevent an optimizing
    // compiler from interfering. Also, the signed operations on small types
    // actually return int.
    T u = static_cast<UT> (x) + static_cast<UT> (y);
    T ux = u ^ x;
    T uy = u ^ y;
    if ((ux & uy) < 0)
      {
        u = octave_int_base<T>::max_val () + __signbit (~u);
      }
    return u;
#else
    // We shall carefully avoid anything that may overflow.
    T u;
    if (y < 0)
      {
        if (x < octave_int_base<T>::min_val () - y)
          {
            u = octave_int_base<T>::min_val ();
          }
        else
          u = x + y;
      }
    else
      {
        if (x > octave_int_base<T>::max_val () - y)
          {
            u = octave_int_base<T>::max_val ();
          }
        else
          u = x + y;
      }

    return u;
#endif
  }

  // This is very similar to addition.
  static T
  sub (T x, T y)
  {
#ifdef HAVE_FAST_INT_OPS
    // The typecasts do nothing, but they are here to prevent an optimizing
    // compiler from interfering. Also, the signed operations on small types
    // actually return int.
    T u = static_cast<UT> (x) - static_cast<UT> (y);
    T ux = u ^ x;
    T uy = u ^ ~y;
    if ((ux & uy) < 0)
      {
        u = octave_int_base<T>::max_val () + __signbit (~u);
      }
    return u;
#else
    // We shall carefully avoid anything that may overflow.
    T u;
    if (y < 0)
      {
        if (x > octave_int_base<T>::max_val () + y)
          {
            u = octave_int_base<T>::max_val ();
          }
        else
          u = x - y;
      }
    else
      {
        if (x < octave_int_base<T>::min_val () + y)
          {
            u = octave_int_base<T>::min_val ();
          }
        else
          u = x - y;
      }

    return u;
#endif
  }

  // Multiplication is done using promotion to wider integer type. If there is
  // no suitable promotion type, this operation *MUST* be specialized.
  static T mul (T x, T y) { return mul_internal (x, y); }

  static T
  mul_internal (T x, T y)
  {
    // Promotion type for multiplication (if exists).
    typedef typename query_integer_type<2*sizeof (T), true>::type mptype;
    return octave_int_base<T>::truncate_int (static_cast<mptype> (x)
           * static_cast<mptype> (y));
  }

  // Division.
  static T
  div (T x, T y)
  {
    T z;
    if (y == 0)
      {
        if (x < 0)
          z = octave_int_base<T>::min_val ();
        else if (x != 0)
          z = octave_int_base<T>::max_val ();
        else
          z = 0;
      }
    else if (y < 0)
      {
        // This is a special case that overflows as well.
        if (y == -1 && x == octave_int_base<T>::min_val ())
          {
            z = octave_int_base<T>::max_val ();
          }
        else
          {
            z = x / y;
            // Can't overflow, but std::abs (x) can!
            T w = -octave_int_abs (x % y);
            if (w <= y - w)
              z -= 1 - (__signbit (x) << 1);
          }
      }
    else
      {
        z = x / y;
        // FIXME: this is a workaround due to MSVC's absence of
        // std::abs (int64_t).  The call to octave_int_abs can't
        // overflow, but std::abs (x) can!
        T w = octave_int_abs (x % y);

        if (w >= y - w)
          z += 1 - (__signbit (x) << 1);
      }
    return z;
  }

  // Remainder.
  static T
  rem (T x, T y)
  {
    return y != 0 ? x % y : 0;
  }

  // Modulus. Note the weird y = 0 case for Matlab compatibility.
  static T
  mod (T x, T y)
  {
    if (y != 0)
      {
        T r = x % y;
        return ((r < 0) != (y < 0)) ? r + y : r;
      }
    else
      return x;
  }
};

#ifdef OCTAVE_INT_USE_LONG_DOUBLE

// Handle 64-bit multiply using long double

#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED

extern OCTAVE_API int64_t
octave_external_int64_int64_mul (int64_t, int64_t);

#endif

template <>
inline int64_t
octave_int_arith_base<int64_t, true>::mul_internal (int64_t x, int64_t y)
{
  int64_t retval;

  long double p = static_cast<long double> (x) * static_cast<long double> (y);

  // NOTE: We could maybe do it with a single branch if HAVE_FAST_INT_OPS,
  // but it would require one more runtime conversion, so the question is
  // whether it would really be faster.
  if (p > static_cast<long double> (octave_int_base<int64_t>::max_val ()))
    retval = octave_int_base<int64_t>::max_val ();
  else if (p < static_cast<long double> (octave_int_base<int64_t>::min_val ()))
    retval = octave_int_base<int64_t>::min_val ();
  else
    retval = static_cast<int64_t> (p);

  return retval;
}

template <>
inline int64_t
octave_int_arith_base<int64_t, true>::mul (int64_t x, int64_t y)
{
#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED
  return octave_external_int64_int64_mul (x, y);
#else
  return mul_internal (x, y);
#endif
}

#else

// Special handler for 64-bit integer multiply.
template <>
OCTAVE_API int64_t
octave_int_arith_base<int64_t, true>::mul_internal (int64_t, int64_t);

#endif

// This class simply selects the proper arithmetics.
template<class T>
class octave_int_arith
 : public octave_int_arith_base<T, std::numeric_limits<T>::is_signed>
{ };

template <class T>
class
octave_int : public octave_int_base<T>
{
public:
  typedef T val_type;

  octave_int (void) : ival () { }

  octave_int (T i) : ival (i) { }

#if defined (HAVE_OVERLOAD_CHAR_INT8_TYPES)
  // Always treat characters as unsigned.
  octave_int (char c)
    : ival (octave_int_base<T>::truncate_int (static_cast<unsigned char> (c)))
  { }
#endif

  octave_int (double d) : ival (octave_int_base<T>::convert_real (d)) { }

  octave_int (float d) : ival (octave_int_base<T>::convert_real (d)) { }

#ifdef OCTAVE_INT_USE_LONG_DOUBLE
  octave_int (long double d) : ival (octave_int_base<T>::convert_real (d)) { }
#endif

  octave_int (bool b) : ival (b) { }

  template <class U>
  octave_int (const U& i) : ival(octave_int_base<T>::truncate_int (i)) { }

  template <class U>
  octave_int (const octave_int<U>& i)
    : ival (octave_int_base<T>::truncate_int (i.value ())) { }

  octave_int (const octave_int<T>& i) : ival (i.ival) { }

  octave_int& operator = (const octave_int<T>& i)
  {
    ival = i.ival;
    return *this;
  }

  T value (void) const { return ival; }

  const unsigned char * iptr (void) const
  { return reinterpret_cast<const unsigned char *> (& ival); }

  bool operator ! (void) const { return ! ival; }

  bool bool_value (void) const { return static_cast<bool> (value ()); }

  char char_value (void) const { return static_cast<char> (value ()); }

  double double_value (void) const { return static_cast<double> (value ()); }

  float float_value (void) const { return static_cast<float> (value ()); }

  operator T (void) const { return value (); }

  // char and bool operators intentionally omitted.

  operator double (void) const { return double_value (); }

  operator float (void) const { return float_value (); }

  octave_int<T>
  operator + () const
  { return *this; }

  // unary operators & mappers
#define OCTAVE_INT_UN_OP(OPNAME,NAME) \
  inline octave_int<T> \
  OPNAME () const \
  { return octave_int_arith<T>::NAME (ival); }

  OCTAVE_INT_UN_OP(operator -, minus)
  OCTAVE_INT_UN_OP(abs, abs)
  OCTAVE_INT_UN_OP(signum, signum)

#undef OCTAVE_INT_UN_OP

// Homogeneous binary integer operations.
#define OCTAVE_INT_BIN_OP(OP, NAME, ARGT) \
  inline octave_int<T> \
  operator OP (const ARGT& y) const \
  { return octave_int_arith<T>::NAME (ival, y); } \
  inline octave_int<T>& \
  operator OP##= (const ARGT& y) \
  { \
    ival = octave_int_arith<T>::NAME (ival, y); \
    return *this; \
  }

  OCTAVE_INT_BIN_OP(+, add, octave_int<T>)
  OCTAVE_INT_BIN_OP(-, sub, octave_int<T>)
  OCTAVE_INT_BIN_OP(*, mul, octave_int<T>)
  OCTAVE_INT_BIN_OP(/, div, octave_int<T>)
  OCTAVE_INT_BIN_OP(%, rem, octave_int<T>)
  OCTAVE_INT_BIN_OP(<<, lshift, int)
  OCTAVE_INT_BIN_OP(>>, rshift, int)

#undef OCTAVE_INT_BIN_OP

  static octave_int<T> min (void) { return std::numeric_limits<T>::min (); }
  static octave_int<T> max (void) { return std::numeric_limits<T>::max (); }

  static int nbits (void) { return std::numeric_limits<T>::digits; }

  static int byte_size (void) { return sizeof (T); }

  static const char *type_name ();

  // The following are provided for convenience.
  static const octave_int zero, one;

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return const_cast<T *> (&ival); }

private:

  T ival;
};

template <class T>
inline octave_int<T>
rem (const octave_int<T>& x, const octave_int<T>& y)
{ return octave_int_arith<T>::rem (x.value (), y.value ()); }

template <class T>
inline octave_int<T>
mod (const octave_int<T>& x, const octave_int<T>& y)
{ return octave_int_arith<T>::mod (x.value (), y.value ()); }

// No mixed integer binary operations!

template <class T>
inline bool
xisnan (const octave_int<T>&)
{ return false; }

// FIXME: can/should any of these be inline?

template <class T>
extern OCTAVE_API octave_int<T>
pow (const octave_int<T>&, const octave_int<T>&);

template <class T>
extern OCTAVE_API octave_int<T>
pow (const double& a, const octave_int<T>& b);

template <class T>
extern OCTAVE_API octave_int<T>
pow (const octave_int<T>& a, const double& b);

template <class T>
extern OCTAVE_API octave_int<T>
pow (const float& a, const octave_int<T>& b);

template <class T>
extern OCTAVE_API octave_int<T>
pow (const octave_int<T>& a, const float& b);

// FIXME: Do we really need a differently named single-precision
//        function integer power function here instead of an overloaded
//        one?
template <class T>
extern OCTAVE_API octave_int<T>
powf (const float& a, const octave_int<T>& b);

template <class T>
extern OCTAVE_API octave_int<T>
powf (const octave_int<T>& a, const float& b);

// Binary relations

#define OCTAVE_INT_CMP_OP(OP, NAME) \
  template<class T1, class T2> \
  inline bool \
  operator OP (const octave_int<T1>& x, const octave_int<T2>& y) \
  { return octave_int_cmp_op::op<octave_int_cmp_op::NAME, T1, T2> \
    (x.value (), y.value ()); }

OCTAVE_INT_CMP_OP (<, lt)
OCTAVE_INT_CMP_OP (<=, le)
OCTAVE_INT_CMP_OP (>, gt)
OCTAVE_INT_CMP_OP (>=, ge)
OCTAVE_INT_CMP_OP (==, eq)
OCTAVE_INT_CMP_OP (!=, ne)

#undef OCTAVE_INT_CMP_OP

template <class T>
inline std::ostream&
operator << (std::ostream& os, const octave_int<T>& ival)
{
  os << ival.value ();
  return os;
}

template <class T>
inline std::istream&
operator >> (std::istream& is, octave_int<T>& ival)
{
  T tmp = 0;
  is >> tmp;
  ival = tmp;
  return is;
}

// We need to specialise for char and unsigned char because
// std::operator<< and std::operator>> are overloaded to input and
// output the ASCII character values instead of a representation of
// their numerical value (e.g. os << char(10) outputs a space instead
// of outputting the characters '1' and '0')

template <>
inline std::ostream&
operator << (std::ostream& os, const octave_int<int8_t>& ival)
{
  os << static_cast<int> (ival.value ());
  return os;
}

template <>
inline std::ostream&
operator << (std::ostream& os, const octave_int<uint8_t>& ival)
{
  os << static_cast<unsigned int> (ival.value ());
  return os;
}


template <>
inline std::istream&
operator >> (std::istream& is, octave_int<int8_t>& ival)
{
  int tmp = 0;
  is >> tmp;
  ival = static_cast<int8_t> (tmp);
  return is;
}

template <>
inline std::istream&
operator >> (std::istream& is, octave_int<uint8_t>& ival)
{
  unsigned int tmp = 0;
  is >> tmp;
  ival = static_cast<uint8_t> (tmp);
  return is;
}


// Bitwise operations

#define OCTAVE_INT_BITCMP_OP(OP) \
  template <class T> \
  octave_int<T> \
  operator OP (const octave_int<T>& x, const octave_int<T>& y) \
  { return x.value () OP y.value (); }

OCTAVE_INT_BITCMP_OP (&)
OCTAVE_INT_BITCMP_OP (|)
OCTAVE_INT_BITCMP_OP (^)

#undef OCTAVE_INT_BITCMP_OP

// General bit shift.
template <class T>
octave_int<T>
bitshift (const octave_int<T>& a, int n,
          const octave_int<T>& mask = std::numeric_limits<T>::max ())
{
  if (n > 0)
    return (a << n) & mask;
  else if (n < 0)
    return (a >> -n) & mask;
  else
    return a & mask;
}

typedef octave_int<int8_t> octave_int8;
typedef octave_int<int16_t> octave_int16;
typedef octave_int<int32_t> octave_int32;
typedef octave_int<int64_t> octave_int64;

typedef octave_int<uint8_t> octave_uint8;
typedef octave_int<uint16_t> octave_uint16;
typedef octave_int<uint32_t> octave_uint32;
typedef octave_int<uint64_t> octave_uint64;

#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED

#define DECLARE_EXTERNAL_LONG_DOUBLE_OP(T, OP) \
  extern OCTAVE_API T \
  external_double_ ## T ## _ ## OP (double x, T y); \
  extern OCTAVE_API T \
  external_ ## T ## _double_ ## OP (T x, double y)

#define DECLARE_EXTERNAL_LONG_DOUBLE_OPS(T) \
  DECLARE_EXTERNAL_LONG_DOUBLE_OP (T, add); \
  DECLARE_EXTERNAL_LONG_DOUBLE_OP (T, sub); \
  DECLARE_EXTERNAL_LONG_DOUBLE_OP (T, mul); \
  DECLARE_EXTERNAL_LONG_DOUBLE_OP (T, div)

DECLARE_EXTERNAL_LONG_DOUBLE_OPS (octave_int64);
DECLARE_EXTERNAL_LONG_DOUBLE_OPS (octave_uint64);

#endif

#define OCTAVE_INT_DOUBLE_BIN_OP0(OP) \
  template <class T> \
  inline octave_int<T> \
  operator OP (const octave_int<T>& x, const double& y) \
  { return octave_int<T> (static_cast<double> (x) OP y); } \
  template <class T> \
  inline octave_int<T> \
  operator OP (const double& x, const octave_int<T>& y) \
  { return octave_int<T> (x OP static_cast<double> (y)); }

#ifdef OCTAVE_INT_USE_LONG_DOUBLE
// Handle mixed op using long double
#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED
#define OCTAVE_INT_DOUBLE_BIN_OP(OP, NAME) \
  OCTAVE_INT_DOUBLE_BIN_OP0(OP) \
  template <> \
  inline octave_int64 \
  operator OP (const double& x, const octave_int64& y) \
  { \
    return external_double_octave_int64_ ## NAME (x, y); \
  } \
  template <> \
  inline octave_uint64 \
  operator OP (const double& x, const octave_uint64& y) \
  { \
    return external_double_octave_uint64_ ## NAME (x, y); \
  } \
  template <> \
  inline octave_int64 \
  operator OP (const octave_int64& x, const double& y) \
  { \
    return external_octave_int64_double_ ## NAME (x, y); \
  } \
  template <> \
  inline octave_uint64 \
  operator OP (const octave_uint64& x, const double& y) \
  { \
    return external_octave_uint64_double_ ## NAME (x, y); \
  }
#else
#define OCTAVE_INT_DOUBLE_BIN_OP(OP, NAME) \
  OCTAVE_INT_DOUBLE_BIN_OP0(OP) \
  template <> \
  inline octave_int64 \
  operator OP (const double& x, const octave_int64& y) \
  { \
    return octave_int64 (x OP static_cast<long double> (y.value ())); \
  } \
  template <> \
  inline octave_uint64 \
  operator OP (const double& x, const octave_uint64& y) \
  { \
    return octave_uint64 (x OP static_cast<long double> (y.value ())); \
  } \
  template <> \
  inline octave_int64 \
  operator OP (const octave_int64& x, const double& y) \
  { \
    return octave_int64 (static_cast<long double> (x.value ()) OP y);   \
  } \
  template <> \
  inline octave_uint64 \
  operator OP (const octave_uint64& x, const double& y) \
  { \
    return octave_uint64 (static_cast<long double> (x.value ()) OP y); \
  }
#endif
#else
// external handlers
#define OCTAVE_INT_DOUBLE_BIN_OP(OP, NAME) \
  OCTAVE_INT_DOUBLE_BIN_OP0(OP) \
  template <> \
  OCTAVE_API octave_int64 \
  operator OP (const double&, const octave_int64&); \
  template <> \
  OCTAVE_API octave_uint64 \
  operator OP (const double&, const octave_uint64&); \
  template <> \
  OCTAVE_API octave_int64 \
  operator OP (const octave_int64&, const double&); \
  template <> \
  OCTAVE_API octave_uint64 \
  operator OP (const octave_uint64&, const double&);

#endif

OCTAVE_INT_DOUBLE_BIN_OP (+, add)
OCTAVE_INT_DOUBLE_BIN_OP (-, sub)
OCTAVE_INT_DOUBLE_BIN_OP (*, mul)
OCTAVE_INT_DOUBLE_BIN_OP (/, div)

#undef OCTAVE_INT_DOUBLE_BIN_OP0
#undef OCTAVE_INT_DOUBLE_BIN_OP
#undef DECLARE_EXTERNAL_LONG_DOUBLE_OP
#undef DECLARE_EXTERNAL_LONG_DOUBLE_OPS

#define OCTAVE_INT_DOUBLE_CMP_OP(OP,NAME) \
  template <class T> \
  inline bool \
  operator OP (const octave_int<T>& x, const double& y) \
  { return octave_int_cmp_op::mop<octave_int_cmp_op::NAME> (x.value (), y); } \
  template <class T> \
  inline bool \
  operator OP (const double& x, const octave_int<T>& y) \
  { return octave_int_cmp_op::mop<octave_int_cmp_op::NAME> (x, y.value ()); }

OCTAVE_INT_DOUBLE_CMP_OP (<, lt)
OCTAVE_INT_DOUBLE_CMP_OP (<=, le)
OCTAVE_INT_DOUBLE_CMP_OP (>=, ge)
OCTAVE_INT_DOUBLE_CMP_OP (>, gt)
OCTAVE_INT_DOUBLE_CMP_OP (==, eq)
OCTAVE_INT_DOUBLE_CMP_OP (!=, ne)

#undef OCTAVE_INT_DOUBLE_CMP_OP

// Floats are handled by simply converting to doubles.

#define OCTAVE_INT_FLOAT_BIN_OP(OP) \
  template <class T> \
  inline octave_int<T> \
  operator OP (const octave_int<T>& x, float y) \
  { return x OP static_cast<double> (y); } \
  template <class T> \
  inline octave_int<T> \
  operator OP (float x, const octave_int<T>& y) \
  { return static_cast<double> (x) OP y; }

OCTAVE_INT_FLOAT_BIN_OP (+)
OCTAVE_INT_FLOAT_BIN_OP (-)
OCTAVE_INT_FLOAT_BIN_OP (*)
OCTAVE_INT_FLOAT_BIN_OP (/)

#undef OCTAVE_INT_FLOAT_BIN_OP

#define OCTAVE_INT_FLOAT_CMP_OP(OP) \
  template <class T> \
  inline bool \
  operator OP (const octave_int<T>& x, const float& y) \
  { return x OP static_cast<double> (y); } \
  template <class T> \
  bool \
  operator OP (const float& x, const octave_int<T>& y) \
  { return static_cast<double> (x) OP y; }

OCTAVE_INT_FLOAT_CMP_OP (<)
OCTAVE_INT_FLOAT_CMP_OP (<=)
OCTAVE_INT_FLOAT_CMP_OP (>=)
OCTAVE_INT_FLOAT_CMP_OP (>)
OCTAVE_INT_FLOAT_CMP_OP (==)
OCTAVE_INT_FLOAT_CMP_OP (!=)

#undef OCTAVE_INT_FLOAT_CMP_OP

template <class T>
octave_int<T>
xmax (const octave_int<T>& x, const octave_int<T>& y)
{
  const T xv = x.value ();
  const T yv = y.value ();
  return octave_int<T> (xv >= yv ? xv : yv);
}

template <class T>
octave_int<T>
xmin (const octave_int<T>& x, const octave_int<T>& y)
{
  const T xv = x.value ();
  const T yv = y.value ();
  return octave_int<T> (xv <= yv ? xv : yv);
}

#endif
