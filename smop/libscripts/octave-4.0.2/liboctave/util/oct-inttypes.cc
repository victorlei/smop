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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <fpucw.h>

#include "lo-error.h"

#include "oct-inttypes.h"

template<class T>
const octave_int<T> octave_int<T>::zero (static_cast<T> (0));

template<class T>
const octave_int<T> octave_int<T>::one (static_cast<T> (1));

// define type names.
#define DECLARE_OCTAVE_INT_TYPENAME(TYPE, TYPENAME) \
  template <> \
  OCTAVE_API const char * \
  octave_int<TYPE>::type_name () { return TYPENAME; }

DECLARE_OCTAVE_INT_TYPENAME (int8_t, "int8")
DECLARE_OCTAVE_INT_TYPENAME (int16_t, "int16")
DECLARE_OCTAVE_INT_TYPENAME (int32_t, "int32")
DECLARE_OCTAVE_INT_TYPENAME (int64_t, "int64")
DECLARE_OCTAVE_INT_TYPENAME (uint8_t, "uint8")
DECLARE_OCTAVE_INT_TYPENAME (uint16_t, "uint16")
DECLARE_OCTAVE_INT_TYPENAME (uint32_t, "uint32")
DECLARE_OCTAVE_INT_TYPENAME (uint64_t, "uint64")

template <class T>
template <class S>
T
octave_int_base<T>::convert_real (const S& value)
{
  // Compute proper thresholds.
  static const S thmin = compute_threshold (static_cast<S> (min_val ()),
                                            min_val ());
  static const S thmax = compute_threshold (static_cast<S> (max_val ()),
                                            max_val ());
  if (xisnan (value))
    {
      return static_cast<T> (0);
    }
  else if (value < thmin)
    {
      return min_val ();
    }
  else if (value > thmax)
    {
      return max_val ();
    }
  else
    {
      S rvalue = xround (value);
      return static_cast<T> (rvalue);
    }
}

#define INSTANTIATE_CONVERT_REAL_1(T, S) \
  template \
  OCTAVE_API \
  T \
  octave_int_base<T>::convert_real (const S&)

#define INSTANTIATE_CONVERT_REAL(S) \
  INSTANTIATE_CONVERT_REAL_1 (int8_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (uint8_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (int16_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (uint16_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (int32_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (uint32_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (int64_t, S); \
  INSTANTIATE_CONVERT_REAL_1 (uint64_t, S)

INSTANTIATE_CONVERT_REAL (double);
INSTANTIATE_CONVERT_REAL (float);
#if defined (OCTAVE_INT_USE_LONG_DOUBLE)
INSTANTIATE_CONVERT_REAL (long double);
#endif

#ifdef OCTAVE_INT_USE_LONG_DOUBLE

#ifdef OCTAVE_ENSURE_LONG_DOUBLE_OPERATIONS_ARE_NOT_TRUNCATED

#define DEFINE_OCTAVE_LONG_DOUBLE_CMP_OP_TEMPLATES(T) \
  template <class xop> \
  bool \
  octave_int_cmp_op::external_mop (double x, T y) \
  { \
     DECL_LONG_DOUBLE_ROUNDING \
   \
     BEGIN_LONG_DOUBLE_ROUNDING (); \
   \
     bool retval = xop::op (static_cast<long double> (x), \
                            static_cast<long double> (y)); \
   \
     END_LONG_DOUBLE_ROUNDING (); \
   \
     return retval; \
  } \
   \
  template <class xop> \
  bool \
  octave_int_cmp_op::external_mop (T x, double y) \
  { \
     DECL_LONG_DOUBLE_ROUNDING \
   \
     BEGIN_LONG_DOUBLE_ROUNDING (); \
   \
     bool retval = xop::op (static_cast<long double> (x), \
                            static_cast<long double> (y)); \
   \
     END_LONG_DOUBLE_ROUNDING (); \
   \
     return retval; \
  }

DEFINE_OCTAVE_LONG_DOUBLE_CMP_OP_TEMPLATES (int64_t)
DEFINE_OCTAVE_LONG_DOUBLE_CMP_OP_TEMPLATES (uint64_t)

#define INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP(OP, T) \
  template OCTAVE_API bool \
  octave_int_cmp_op::external_mop<octave_int_cmp_op::OP> (double, T); \
  template OCTAVE_API bool \
  octave_int_cmp_op::external_mop<octave_int_cmp_op::OP> (T, double)

#define INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OPS(T) \
  INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP (lt, T); \
  INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP (le, T); \
  INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP (gt, T); \
  INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP (ge, T); \
  INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP (eq, T); \
  INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OP (ne, T)

INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OPS (int64_t);
INSTANTIATE_LONG_DOUBLE_LONG_DOUBLE_CMP_OPS (uint64_t);

uint64_t
octave_external_uint64_uint64_mul (uint64_t x, uint64_t y)
{
  DECL_LONG_DOUBLE_ROUNDING

  BEGIN_LONG_DOUBLE_ROUNDING ();

  uint64_t retval = octave_int_arith_base<uint64_t, false>::mul_internal (x, y);

  END_LONG_DOUBLE_ROUNDING ();

  return retval;
}

int64_t
octave_external_int64_int64_mul (int64_t x, int64_t y)
{
  DECL_LONG_DOUBLE_ROUNDING

  BEGIN_LONG_DOUBLE_ROUNDING ();

  int64_t retval = octave_int_arith_base<int64_t, true>::mul_internal (x, y);

  END_LONG_DOUBLE_ROUNDING ();

  return retval;
}

// Note that if we return long double it is apparently possible for
// truncation to happen at the point of storing the result in retval,
// which can happen after we end long double rounding.  Attempt to avoid
// that problem by storing the full precision temporary value in the
// integer value before we end the long double rounding mode.
// Similarly, the conversion from the 64-bit integer type to long double
// must also occur in long double rounding mode.

#define OCTAVE_LONG_DOUBLE_OP(T, OP, NAME) \
  T \
  external_double_ ## T ## _ ## NAME (double x, T y) \
  { \
    DECL_LONG_DOUBLE_ROUNDING \
 \
    BEGIN_LONG_DOUBLE_ROUNDING (); \
 \
    T retval = T (x OP static_cast<long double> (y.value ())); \
 \
    END_LONG_DOUBLE_ROUNDING (); \
 \
    return retval; \
  } \
 \
  T \
  external_ ## T ## _double_ ## NAME (T x, double y) \
  { \
    DECL_LONG_DOUBLE_ROUNDING \
 \
    BEGIN_LONG_DOUBLE_ROUNDING (); \
 \
    T retval = T (static_cast<long double> (x.value ()) OP y); \
 \
    END_LONG_DOUBLE_ROUNDING (); \
 \
    return retval; \
  }

#define OCTAVE_LONG_DOUBLE_OPS(T) \
  OCTAVE_LONG_DOUBLE_OP (T, +, add); \
  OCTAVE_LONG_DOUBLE_OP (T, -, sub); \
  OCTAVE_LONG_DOUBLE_OP (T, *, mul); \
  OCTAVE_LONG_DOUBLE_OP (T, /, div)

OCTAVE_LONG_DOUBLE_OPS(octave_int64);
OCTAVE_LONG_DOUBLE_OPS(octave_uint64);

#endif

#else

// Define comparison operators

template <class xop>
bool
octave_int_cmp_op::emulate_mop (uint64_t x, double y)
{
  static const double xxup = std::numeric_limits<uint64_t>::max ();
  // This converts to the nearest double. Unless there's an equality, the
  // result is clear.
  double xx = x;
  if (xx != y)
    return xop::op (xx, y);
  else
    {
      // If equality occurred we compare as integers.
      if (xx == xxup)
        return xop::gtval;
      else
        return xop::op (x, static_cast<uint64_t> (xx));
    }
}

template <class xop>
bool
octave_int_cmp_op::emulate_mop (int64_t x, double y)
{
  static const double xxup = std::numeric_limits<int64_t>::max ();
  static const double xxlo = std::numeric_limits<int64_t>::min ();
  // This converts to the nearest double. Unless there's an equality, the
  // result is clear.
  double xx = x;
  if (xx != y)
    return xop::op (xx, y);
  else
    {
      // If equality occurred we compare as integers.
      if (xx == xxup)
        return xop::gtval;
      else if (xx == xxlo)
        return xop::ltval;
      else
        return xop::op (x, static_cast<int64_t> (xx));
    }

}

// We define double-int operations by reverting the operator

// A trait class reverting the operator
template <class xop>
class rev_op
{
public:
  typedef xop op;
};

#define DEFINE_REVERTED_OPERATOR(OP1,OP2) \
  template <> \
  class rev_op<octave_int_cmp_op::OP1> \
  { \
  public: \
    typedef octave_int_cmp_op::OP2 op; \
  }

DEFINE_REVERTED_OPERATOR(lt,gt);
DEFINE_REVERTED_OPERATOR(gt,lt);
DEFINE_REVERTED_OPERATOR(le,ge);
DEFINE_REVERTED_OPERATOR(ge,le);

template <class xop>
bool
octave_int_cmp_op::emulate_mop (double x, uint64_t y)
{
  typedef typename rev_op<xop>::op rop;
  return mop<rop> (y, x);
}

template <class xop>
bool
octave_int_cmp_op::emulate_mop (double x, int64_t y)
{
  typedef typename rev_op<xop>::op rop;
  return mop<rop> (y, x);
}


// Define handlers for int64 multiplication

template <>
uint64_t
octave_int_arith_base<uint64_t, false>::mul_internal (uint64_t x, uint64_t y)
{
  // Get upper words
  uint64_t ux = x >> 32;
  uint64_t uy = y >> 32;
  uint64_t res;
  if (ux)
    {
      if (uy)
        goto overflow;
      else
        {
          uint64_t ly = static_cast<uint32_t> (y);
          uint64_t uxly = ux*ly;
          if (uxly >> 32)
            goto overflow;
          uxly <<= 32; // never overflows
          uint64_t lx = static_cast<uint32_t> (x);
          uint64_t lxly = lx*ly;
          res = add (uxly, lxly);
        }
    }
  else if (uy)
    {
      uint64_t lx = static_cast<uint32_t> (x);
      uint64_t uylx = uy*lx;
      if (uylx >> 32)
        goto overflow;
      uylx <<= 32; // never overflows
      uint64_t ly = static_cast<uint32_t> (y);
      uint64_t lylx = ly*lx;
      res = add (uylx, lylx);
    }
  else
    {
      uint64_t lx = static_cast<uint32_t> (x);
      uint64_t ly = static_cast<uint32_t> (y);
      res = lx*ly;
    }

  return res;

overflow:
  return max_val ();
}

template <>
int64_t
octave_int_arith_base<int64_t, true>::mul_internal (int64_t x, int64_t y)
{
  // The signed case is far worse. The problem is that
  // even if neither integer fits into signed 32-bit range, the result may
  // still be OK. Uh oh.

  // Essentially, what we do is compute sign, multiply absolute values
  // (as above) and impose the sign.
  // FIXME: can we do something faster if we HAVE_FAST_INT_OPS?

  uint64_t usx = octave_int_abs (x);
  uint64_t usy = octave_int_abs (y);
  bool positive = (x < 0) == (y < 0);

  // Get upper words
  uint64_t ux = usx >> 32;
  uint64_t uy = usy >> 32;
  uint64_t res;
  if (ux)
    {
      if (uy)
        goto overflow;
      else
        {
          uint64_t ly = static_cast<uint32_t> (usy);
          uint64_t uxly = ux*ly;
          if (uxly >> 32)
            goto overflow;
          uxly <<= 32; // never overflows
          uint64_t lx = static_cast<uint32_t> (usx);
          uint64_t lxly = lx*ly;
          res = uxly + lxly;
          if (res < uxly)
            goto overflow;
        }
    }
  else if (uy)
    {
      uint64_t lx = static_cast<uint32_t> (usx);
      uint64_t uylx = uy*lx;
      if (uylx >> 32)
        goto overflow;
      uylx <<= 32; // never overflows
      uint64_t ly = static_cast<uint32_t> (usy);
      uint64_t lylx = ly*lx;
      res = uylx + lylx;
      if (res < uylx)
        goto overflow;
    }
  else
    {
      uint64_t lx = static_cast<uint32_t> (usx);
      uint64_t ly = static_cast<uint32_t> (usy);
      res = lx*ly;
    }

  if (positive)
    {
      if (res > static_cast<uint64_t> (max_val ()))
        {
          return max_val ();
        }
      else
        return static_cast<int64_t> (res);
    }
  else
    {
      if (res > static_cast<uint64_t> (-min_val ()))
        {
          return min_val ();
        }
      else
        return -static_cast<int64_t> (res);
    }


overflow:
  return positive ? max_val () : min_val ();

}

#define INT_DOUBLE_BINOP_DECL(OP,SUFFIX) \
  template <> \
  OCTAVE_API octave_ ## SUFFIX \
  operator OP (const octave_ ## SUFFIX & x, const double& y)

#define DOUBLE_INT_BINOP_DECL(OP,SUFFIX) \
  template <> \
  OCTAVE_API octave_ ## SUFFIX \
  operator OP (const double& x, const octave_ ## SUFFIX & y)

INT_DOUBLE_BINOP_DECL (+, uint64)
{
  return (y < 0) ? x - octave_uint64 (-y) : x + octave_uint64 (y);
}

DOUBLE_INT_BINOP_DECL (+, uint64)
{ return y + x; }

INT_DOUBLE_BINOP_DECL (+, int64)
{
  if (fabs (y) < static_cast<double> (octave_int64::max ()))
    return x + octave_int64 (y);
  else
    {
      // If the number is within the int64 range (the most common case,
      // probably), the above will work as expected. If not, it's more
      // complicated - as long as y is within _twice_ the signed range, the
      // result may still be an integer. An instance of such an operation is
      // 3*2**62 + (1+intmin ('int64')) that should yield int64 (2**62) + 1.  So
      // what we do is to try to convert y/2 and add it twice. Note that if y/2
      // overflows, the result must overflow as well, and that y/2 cannot be a
      // fractional number.
      octave_int64 y2 (y / 2);
      return (x + y2) + y2;
    }
}

DOUBLE_INT_BINOP_DECL (+, int64)
{
  return y + x;
}

INT_DOUBLE_BINOP_DECL (-, uint64)
{
  return x + (-y);
}

DOUBLE_INT_BINOP_DECL (-, uint64)
{
  if (x <= static_cast<double> (octave_uint64::max ()))
    return octave_uint64 (x) - y;
  else
    {
      // Again a trick to get the corner cases right. Things like
      // 3**2**63 - intmax ('uint64') should produce the correct result, i.e.
      // int64 (2**63) + 1.
      const double p2_64 = std::pow (2.0, 64);
      if (y.bool_value ())
        {
          const uint64_t p2_64my = (~y.value ()) + 1; // Equals 2**64 - y
          return octave_uint64 (x - p2_64) + octave_uint64 (p2_64my);
        }
      else
        return octave_uint64 (p2_64);
    }
}

INT_DOUBLE_BINOP_DECL (-, int64)
{
  return x + (-y);
}

DOUBLE_INT_BINOP_DECL (-, int64)
{
  static const bool twosc = (std::numeric_limits<int64_t>::min ()
                             < -std::numeric_limits<int64_t>::max ());
  // In case of symmetric integers (not two's complement), this will probably
  // be eliminated at compile time.
  if (twosc && y.value () == std::numeric_limits<int64_t>::min ())
    {
      return octave_int64 (x + std::pow (2.0, 63));
    }
  else
    return x + (-y);
}

// NOTE:
// Emulated mixed multiplications are tricky due to possible precision loss.
// Here, after sorting out common cases for speed, we follow the strategy
// of converting the double number into the form sign * 64-bit integer *
// 2**exponent, multiply the 64-bit integers to get a 128-bit number, split that
// number into 32-bit words and form 4 double-valued summands (none of which
// loses precision), then convert these into integers and sum them. Though it is
// not immediately obvious, this should work even w.r.t. rounding (none of the
// summands lose precision).

// Multiplies two unsigned 64-bit ints to get a 128-bit number represented
// as four 32-bit words.
static void
umul128 (uint64_t x, uint64_t y, uint32_t w[4])
{
  uint64_t lx = static_cast<uint32_t> (x);
  uint64_t ux = x >> 32;
  uint64_t ly = static_cast<uint32_t> (y);
  uint64_t uy = y >> 32;
  uint64_t a = lx * ly;
  w[0] = a; a >>= 32;
  uint64_t uxly = ux*ly;
  uint64_t uylx = uy*lx;
  a += static_cast<uint32_t> (uxly); uxly >>= 32;
  a += static_cast<uint32_t> (uylx); uylx >>= 32;
  w[1] = a; a >>= 32;
  uint64_t uxuy = ux * uy;
  a += uxly; a += uylx; a += uxuy;
  w[2] = a; a >>= 32;
  w[3] = a;
}

// Splits a double into bool sign, unsigned 64-bit mantissa and int exponent
static void
dblesplit (double x, bool& sign, uint64_t& mtis, int& exp)
{
  sign = x < 0; x = fabs (x);
  x = gnulib::frexp (x, &exp);
  exp -= 52;
  mtis = static_cast<uint64_t> (ldexp (x, 52));
}

// Gets a double number from a
// 32-bit unsigned integer mantissa, exponent, and sign.
static double
dbleget (bool sign, uint32_t mtis, int exp)
{
  double x = ldexp (static_cast<double> (mtis), exp);
  return sign ? -x : x;
}

INT_DOUBLE_BINOP_DECL (*, uint64)
{
  if (y >= 0 && y < octave_uint64::max () && y == xround (y))
    {
      return x * octave_uint64 (static_cast<uint64_t> (y));
    }
  else if (y == 0.5)
    {
      return x / octave_uint64 (static_cast<uint64_t> (2));
    }
  else if (y < 0 || xisnan (y) || xisinf (y))
    {
      return octave_uint64 (x.value () * y);
    }
  else
    {
      bool sign;
      uint64_t my;
      int e;
      dblesplit (y, sign, my, e);
      uint32_t w[4];
      umul128 (x.value (), my, w);
      octave_uint64 res = octave_uint64::zero;
      for (short i = 0; i < 4; i++)
        {
          res += octave_uint64 (dbleget (sign, w[i], e));
          e += 32;
        }
      return res;
    }
}

DOUBLE_INT_BINOP_DECL (*, uint64)
{ return y * x; }

INT_DOUBLE_BINOP_DECL (*, int64)
{
  if (fabs (y) < octave_int64::max () && y == xround (y))
    {
      return x * octave_int64 (static_cast<int64_t> (y));
    }
  else if (fabs (y) == 0.5)
    {
      return x / octave_int64 (static_cast<uint64_t> (4*y));
    }
  else if (xisnan (y) || xisinf (y))
    {
      return octave_int64 (x.value () * y);
    }
  else
    {
      bool sign;
      uint64_t my;
      int e;
      dblesplit (y, sign, my, e);
      uint32_t w[4];
      sign = (sign != (x.value () < 0));
      umul128 (octave_int_abs (x.value ()), my, w);
      octave_int64 res = octave_int64::zero;
      for (short i = 0; i < 4; i++)
        {
          res += octave_int64 (dbleget (sign, w[i], e));
          e += 32;
        }
      return res;
    }
}

DOUBLE_INT_BINOP_DECL (*, int64)
{ return y * x; }

DOUBLE_INT_BINOP_DECL (/, uint64)
{
  return octave_uint64 (x / static_cast<double> (y));
}

DOUBLE_INT_BINOP_DECL (/, int64)
{
  return octave_int64 (x / static_cast<double> (y));
}

INT_DOUBLE_BINOP_DECL (/, uint64)
{
  if (y >= 0 && y < octave_uint64::max () && y == xround (y))
    {
      return x / octave_uint64 (y);
    }
  else
    return x * (1.0/y);
}

INT_DOUBLE_BINOP_DECL (/, int64)
{
  if (fabs (y) < octave_int64::max () && y == xround (y))
    {
      return x / octave_int64 (y);
    }
  else
    return x * (1.0/y);
}

#define INSTANTIATE_INT64_DOUBLE_CMP_OP0(OP,T1,T2) \
  template OCTAVE_API bool \
  octave_int_cmp_op::emulate_mop<octave_int_cmp_op::OP> (T1 x, T2 y)

#define INSTANTIATE_INT64_DOUBLE_CMP_OP(OP) \
  INSTANTIATE_INT64_DOUBLE_CMP_OP0(OP, double, int64_t); \
  INSTANTIATE_INT64_DOUBLE_CMP_OP0(OP, double, uint64_t); \
  INSTANTIATE_INT64_DOUBLE_CMP_OP0(OP, int64_t, double); \
  INSTANTIATE_INT64_DOUBLE_CMP_OP0(OP, uint64_t, double)

INSTANTIATE_INT64_DOUBLE_CMP_OP(lt);
INSTANTIATE_INT64_DOUBLE_CMP_OP(le);
INSTANTIATE_INT64_DOUBLE_CMP_OP(gt);
INSTANTIATE_INT64_DOUBLE_CMP_OP(ge);
INSTANTIATE_INT64_DOUBLE_CMP_OP(eq);
INSTANTIATE_INT64_DOUBLE_CMP_OP(ne);

#endif

//template <class T>
//bool
//xisnan (const octave_int<T>&)
//{
//  return false;
//}

template <class T>
octave_int<T>
pow (const octave_int<T>& a, const octave_int<T>& b)
{
  octave_int<T> retval;

  octave_int<T> zero = static_cast<T> (0);
  octave_int<T> one = static_cast<T> (1);

  if (b == zero || a == one)
    retval = one;
  else if (b < zero)
    {
      if (a == -one)
        retval = (b.value () % 2) ? a : one;
      else
        retval = zero;
    }
  else
    {
      octave_int<T> a_val = a;
      T b_val = b; // no need to do saturation on b

      retval = a;

      b_val -= 1;

      while (b_val != 0)
        {
          if (b_val & 1)
            retval = retval * a_val;

          b_val = b_val >> 1;

          if (b_val)
            a_val = a_val * a_val;
        }
    }

  return retval;
}

template <class T>
octave_int<T>
pow (const double& a, const octave_int<T>& b)
{ return octave_int<T> (pow (a, b.double_value ())); }

template <class T>
octave_int<T>
pow (const octave_int<T>& a, const double& b)
{
  return ((b >= 0 && b < std::numeric_limits<T>::digits && b == xround (b))
          ? pow (a, octave_int<T> (static_cast<T> (b)))
          : octave_int<T> (pow (a.double_value (), b)));
}

template <class T>
octave_int<T>
pow (const float& a, const octave_int<T>& b)
{ return octave_int<T> (pow (a, b.float_value ())); }

template <class T>
octave_int<T>
pow (const octave_int<T>& a, const float& b)
{
  return ((b >= 0 && b < std::numeric_limits<T>::digits && b == xround (b))
          ? pow (a, octave_int<T> (static_cast<T> (b)))
          : octave_int<T> (pow (a.double_value (), static_cast<double> (b))));
}

// FIXME: Do we really need a differently named single-precision
//        function integer power function here instead of an overloaded
//        one?
template <class T>
octave_int<T>
powf (const float& a, const octave_int<T>& b)
{ return octave_int<T> (pow (a, b.float_value ())); }

template <class T>
octave_int<T>
powf (const octave_int<T>& a, const float& b)
{
  return ((b >= 0 && b < std::numeric_limits<T>::digits && b == xround (b))
          ? pow (a, octave_int<T> (static_cast<T> (b)))
          : octave_int<T> (pow (a.double_value (), static_cast<double> (b))));
}

#define INSTANTIATE_INTTYPE(T) \
  template class OCTAVE_API octave_int<T>; \
  template OCTAVE_API octave_int<T> pow (const octave_int<T>&, const octave_int<T>&); \
  template OCTAVE_API octave_int<T> pow (const double&, const octave_int<T>&); \
  template OCTAVE_API octave_int<T> pow (const octave_int<T>&, const double&); \
  template OCTAVE_API octave_int<T> pow (const float&, const octave_int<T>&);  \
  template OCTAVE_API octave_int<T> pow (const octave_int<T>&, const float&);  \
  template OCTAVE_API octave_int<T> powf (const float&, const octave_int<T>&); \
  template OCTAVE_API octave_int<T> powf (const octave_int<T>&, const float&); \
  template OCTAVE_API octave_int<T> \
  bitshift (const octave_int<T>&, int, const octave_int<T>&);

INSTANTIATE_INTTYPE (int8_t);
INSTANTIATE_INTTYPE (int16_t);
INSTANTIATE_INTTYPE (int32_t);
INSTANTIATE_INTTYPE (int64_t);

INSTANTIATE_INTTYPE (uint8_t);
INSTANTIATE_INTTYPE (uint16_t);
INSTANTIATE_INTTYPE (uint32_t);
INSTANTIATE_INTTYPE (uint64_t);


/*

%!assert (intmax ("int64") / intmin ("int64"), int64 (-1))
%!assert (intmin ("int64") / int64 (-1), intmax ("int64"))
%!assert (int64 (2**63), intmax ("int64"))
%!assert (uint64 (2**64), intmax ("uint64"))
%!test
%! a = 1.9*2^61; b = uint64 (a); b++; assert (b > a);
%!test
%! a = -1.9*2^61; b = int64 (a); b++; assert (b > a);
%!test
%! a = int64 (-2**60) + 2; assert (1.25*a == (5*a)/4);
%!test
%! a = uint64 (2**61) + 2; assert (1.25*a == (5*a)/4);
%!assert (int32 (2**31+0.5), intmax ("int32"))
%!assert (int32 (-2**31-0.5), intmin ("int32"))
%!assert ((int64 (2**62)+1)**1, int64 (2**62)+1)
%!assert ((int64 (2**30)+1)**2, int64 (2**60+2**31) + 1)

%!assert (uint8 (char (128)), uint8 (128));
%!assert (uint8 (char (255)), uint8 (255));
%!assert (int8 (char (128)), int8 (128));
%!assert (int8 (char (255)), int8 (255));

%!assert (uint16 (char (128)), uint16 (128));
%!assert (uint16 (char (255)), uint16 (255));
%!assert (int16 (char (128)), int16 (128));
%!assert (int16 (char (255)), int16 (255));

%!assert (uint32 (char (128)), uint32 (128));
%!assert (uint32 (char (255)), uint32 (255));
%!assert (int32 (char (128)), int32 (128));
%!assert (int32 (char (255)), int32 (255));

%!assert (uint64 (char (128)), uint64 (128));
%!assert (uint64 (char (255)), uint64 (255));
%!assert (int64 (char (128)), int64 (128));
%!assert (int64 (char (255)), int64 (255));
*/
