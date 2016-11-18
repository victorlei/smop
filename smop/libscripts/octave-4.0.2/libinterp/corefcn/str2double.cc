/*

Copyright (C) 2010-2015 Jaroslav Hajek
Copyright (C) 2010 VZLU Prague

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

#include <string>
#include <cctype>
#include <sstream>
#include <algorithm>

#include "lo-ieee.h"

#include "Cell.h"
#include "ov.h"
#include "defun.h"
#include "gripes.h"
#include "utils.h"

static inline bool
is_imag_unit (int c)
{ return c == 'i' || c == 'j'; }

static double
single_num (std::istringstream& is)
{
  double num = 0.0;

  char c = is.peek ();

  // Skip spaces.
  while (isspace (c))
    {
      is.get ();
      c = is.peek ();
    }

  if (std::toupper (c) == 'I')
    {
      // It's infinity.
      is.get ();
      char c1 = is.get ();
      char c2 = is.get ();
      if (std::tolower (c1) == 'n' && std::tolower (c2) == 'f')
        {
          num = octave_Inf;
          is.peek (); // May set EOF bit.
        }
      else
        is.setstate (std::ios::failbit); // indicate that read has failed.
    }
  else if (c == 'N')
    {
      // It's NA or NaN
      is.get ();
      char c1 = is.get ();
      if (c1 == 'A')
        {
          num = octave_NA;
          is.peek (); // May set EOF bit.
        }
      else
        {
          char c2 = is.get ();
          if (c1 == 'a' && c2 == 'N')
            {
              num = octave_NaN;
              is.peek (); // May set EOF bit.
            }
          else
            is.setstate (std::ios::failbit); // indicate that read has failed.
        }
    }
  else
    is >> num;

  return num;
}

static std::istringstream&
extract_num (std::istringstream& is, double& num, bool& imag, bool& have_sign)
{
  have_sign = imag = false;

  char c = is.peek ();

  // Skip leading spaces.
  while (isspace (c))
    {
      is.get ();
      c = is.peek ();
    }

  bool negative = false;

  // Accept leading sign.
  if (c == '+' || c == '-')
    {
      have_sign = true;
      negative = c == '-';
      is.get ();
      c = is.peek ();
    }

  // Skip spaces after sign.
  while (isspace (c))
    {
      is.get ();
      c = is.peek ();
    }

  // Imaginary number (i*num or just i), or maybe 'inf'.
  if (c == 'i')
    {
      // possible infinity.
      is.get ();
      c = is.peek ();

      if (is.eof ())
        {
          // just 'i' and string is finished.  Return immediately.
          imag = true;
          num = negative ? -1.0 : 1.0;
          return is;
        }
      else
        {
          if (std::tolower (c) != 'n')
            imag = true;
          is.unget ();
        }
    }
  else if (c == 'j')
    imag = true;

  // It's i*num or just i
  if (imag)
    {
      is.get ();
      c = is.peek ();
      // Skip spaces after imaginary unit.
      while (isspace (c))
        {
          is.get ();
          c = is.peek ();
        }

      if (c == '*')
        {
          // Multiplier follows, we extract it as a number.
          is.get ();
          num = single_num (is);
          if (is.good ())
            c = is.peek ();
        }
      else
        num = 1.0;
    }
  else
    {
      // It's num, num*i, or numi.
      num = single_num (is);
      if (is.good ())
        {
          c = is.peek ();

          // Skip spaces after number.
          while (isspace (c))
            {
              is.get ();
              c = is.peek ();
            }

          if (c == '*')
            {
              is.get ();
              c = is.peek ();

              // Skip spaces after operator.
              while (isspace (c))
                {
                  is.get ();
                  c = is.peek ();
                }

              if (is_imag_unit (c))
                {
                  imag = true;
                  is.get ();
                  c = is.peek ();
                }
              else
                is.setstate (std::ios::failbit); // indicate read has failed.
            }
          else if (is_imag_unit (c))
            {
              imag = true;
              is.get ();
              c = is.peek ();
            }
        }
    }

  if (is.good ())
    {
      // Skip trailing spaces.
      while (isspace (c))
        {
          is.get ();
          c = is.peek ();
        }
    }

  if (negative)
    num = -num;

  return is;
}

static inline void
set_component (Complex& c, double num, bool imag)
{
#if defined (HAVE_CXX_COMPLEX_SETTERS)
  if (imag)
    c.imag (num);
  else
    c.real (num);
#elif defined (HAVE_CXX_COMPLEX_REFERENCE_ACCESSORS)
  if (imag)
    c.imag () = num;
  else
    c.real () = num;
#else
  if (imag)
    c = Complex (c.real (), num);
  else
    c = Complex (num, c.imag ());
#endif
}

static Complex
str2double1 (const std::string& str_arg)
{
  Complex val (0.0, 0.0);

  std::string str = str_arg;

  // FIXME: removing all commas doesn't allow actual parsing.
  //        Example: "1,23.45" is wrong, but passes Octave.
  str.erase (std::remove (str.begin (), str.end(), ','), str.end ());
  std::istringstream is (str);

  double num;
  bool i1, i2, s1, s2;

  if (is.eof ())
    val = octave_NaN;
  else if (! extract_num (is, num, i1, s1))
    val = octave_NaN;
  else
    {
      set_component (val, num, i1);

      if (! is.eof ())
        {
          if (! extract_num (is, num, i2, s2) || i1 == i2 || ! s2)
            val = octave_NaN;
          else
            set_component (val, num, i2);
        }
    }

  return val;
}

DEFUN (str2double, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} str2double (@var{s})\n\
Convert a string to a real or complex number.\n\
\n\
The string must be in one of the following formats where a and b are real\n\
numbers and the complex unit is @qcode{'i'} or @qcode{'j'}:\n\
\n\
@itemize\n\
@item a + bi\n\
\n\
@item a + b*i\n\
\n\
@item a + i*b\n\
\n\
@item bi + a\n\
\n\
@item b*i + a\n\
\n\
@item i*b + a\n\
@end itemize\n\
\n\
If present, a and/or b are of the form @nospell{[+-]d[,.]d[[eE][+-]d]} where\n\
the brackets indicate optional arguments and @qcode{'d'} indicates zero or\n\
more digits.  The special input values @code{Inf}, @code{NaN}, and @code{NA}\n\
are also accepted.\n\
\n\
@var{s} may be a character string, character matrix, or cell array.  For\n\
character arrays the conversion is repeated for every row, and a double or\n\
complex array is returned.  Empty rows in @var{s} are deleted and not\n\
returned in the numeric array.  For cell arrays each character string\n\
element is processed and a double or complex array of the same dimensions as\n\
@var{s} is returned.\n\
\n\
For unconvertible scalar or character string input @code{str2double} returns\n\
a NaN@.  Similarly, for character array input @code{str2double} returns a\n\
NaN for any row of @var{s} that could not be converted.  For a cell array,\n\
@code{str2double} returns a NaN for any element of @var{s} for which\n\
conversion fails.  Note that numeric elements in a mixed string/numeric\n\
cell array are not strings and the conversion will fail for these elements\n\
and return NaN.\n\
\n\
@code{str2double} can replace @code{str2num}, and it avoids the security\n\
risk of using @code{eval} on unknown data.\n\
@seealso{str2num}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () != 1)
    print_usage ();
  else if (args(0).is_string ())
    {
      if (args(0).rows () == 0 || args(0).columns () == 0)
        {
          retval = Matrix (1, 1, octave_NaN);
        }
      else if (args(0).rows () == 1 && args(0).ndims () == 2)
        {
          retval = str2double1 (args(0).string_value ());
        }
      else
        {
          const string_vector sv = args(0).all_strings ();
          if (! error_state)
            retval = sv.map<Complex> (str2double1);
        }
    }
  else if (args(0).is_cell ())
    {
      const Cell cell = args(0).cell_value ();

      if (! error_state)
        {
          ComplexNDArray output (cell.dims (), octave_NaN);
          for (octave_idx_type i = 0; i < cell.numel (); i++)
            {
              if (cell(i).is_string ())
                output(i) = str2double1 (cell(i).string_value ());
            }
          retval = output;
        }
    }
  else
    retval = Matrix (1, 1, octave_NaN);


  return retval;
}

/*
%!assert (str2double ("1"), 1)
%!assert (str2double ("-.1e-5"), -1e-6)
%!assert (str2double (char ("1", "2 3", "4i")), [1; NaN; 4i])
%!assert (str2double ("1,222.5"), 1222.5)
%!assert (str2double ("i"), i)
%!assert (str2double ("2j"), 2i)
%!assert (str2double ("2 + j"), 2+j)
%!assert (str2double ("i*2 + 3"), 3+2i)
%!assert (str2double (".5*i + 3.5"), 3.5+0.5i)
%!assert (str2double ("1e-3 + i*.25"), 1e-3 + 0.25i)
%!assert (str2double (["2 + j";"1.25e-3";"-05"]), [2+i; 1.25e-3; -5])
%!assert (str2double ({"2 + j","1.25e-3","-05"}), [2+i, 1.25e-3, -5])
%!assert (str2double (1), NaN)
%!assert (str2double ("1 2 3 4"), NaN)
%!assert (str2double ("Hello World"), NaN)
%!assert (str2double ("NaN"), NaN)
%!assert (str2double ("NA"), NA)
%!assert (str2double ("Inf"), Inf)
%!assert (str2double ("iNF"), Inf)
%!assert (str2double ("-Inf"), -Inf)
%!assert (str2double ("Inf*i"), complex (0, Inf))
%!assert (str2double ("iNF*i"), complex (0, Inf))
%!assert (str2double ("NaN + Inf*i"), complex (NaN, Inf))
%!assert (str2double ("Inf - Inf*i"), complex (Inf, -Inf))
%!assert (str2double ("-i*NaN - Inf"), complex (-Inf, -NaN))
%!assert (str2double ({"abc", "4i"}), [NaN + 0i, 4i])
%!assert (str2double ({2, "4i"}), [NaN + 0i, 4i])
%!assert (str2double (zeros (3,1,2)), NaN)
%!assert (str2double (''), NaN)
%!assert (str2double ([]), NaN)
%!assert (str2double (char(zeros(3,0))), NaN)
 */
