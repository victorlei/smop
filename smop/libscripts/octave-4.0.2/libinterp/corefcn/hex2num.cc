/*

Copyright (C) 2008-2015 David Bateman

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

#include <algorithm>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN (hex2num, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{n} =} hex2num (@var{s})\n\
@deftypefnx {Built-in Function} {@var{n} =} hex2num (@var{s}, @var{class})\n\
Typecast the 16 character hexadecimal character string to an IEEE 754\n\
double precision number.\n\
\n\
If fewer than 16 characters are given the strings are right padded with\n\
@qcode{'0'} characters.\n\
\n\
Given a string matrix, @code{hex2num} treats each row as a separate number.\n\
\n\
@example\n\
@group\n\
hex2num ([\"4005bf0a8b145769\"; \"4024000000000000\"])\n\
   @result{} [2.7183; 10.000]\n\
@end group\n\
@end example\n\
\n\
The optional argument @var{class} can be passed as the string\n\
@qcode{\"single\"} to specify that the given string should be interpreted as\n\
a single precision number.  In this case, @var{s} should be an 8 character\n\
hexadecimal string.  For example:\n\
\n\
@example\n\
@group\n\
hex2num ([\"402df854\"; \"41200000\"], \"single\")\n\
   @result{} [2.7183; 10.000]\n\
@end group\n\
@end example\n\
@seealso{num2hex, hex2dec, dec2hex}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin < 1 || nargin > 2)
    print_usage ();
  else if (nargin == 2 && ! args(1).is_string ())
    error ("hex2num: CLASS must be a string");
  else
    {
      const charMatrix cmat = args(0).char_matrix_value ();
      std::string prec = (nargin == 2) ? args(1).string_value () : "double";
      bool is_single = (prec == "single");
      octave_idx_type nchars = (is_single) ? 8 : 16;

      if (cmat.columns () > nchars)
        error ("hex2num: S must be no more than %d characters", nchars);
      else if (prec != "double" && prec != "single")
        error ("hex2num: CLASS must be either \"double\" or \"single\"");
      else if (! error_state)
        {
          octave_idx_type nr = cmat.rows ();
          octave_idx_type nc = cmat.columns ();

          if (is_single)
            {
              FloatColumnVector m (nr);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  union
                  {
                    uint32_t ival;
                    float dval;
                  } num;

                  num.ival = 0;

                  for (octave_idx_type j = 0; j < nc; j++)
                    {
                      unsigned char ch = cmat.elem (i, j);

                      if (isxdigit (ch))
                        {
                          num.ival <<= 4;
                          if (ch >= 'a')
                            num.ival += static_cast<uint32_t> (ch - 'a' + 10);
                          else if (ch >= 'A')
                            num.ival += static_cast<uint32_t> (ch - 'A' + 10);
                          else
                            num.ival += static_cast<uint32_t> (ch - '0');
                        }
                      else
                        {
                          error ("hex2num: illegal character found in string S");
                          break;
                        }
                    }

                  if (error_state)
                    break;
                  else
                    {
                      if (nc < nchars)
                        num.ival <<= (nchars - nc) * 4;

                      m(i) = num.dval;
                    }
                }

              if (! error_state)
                retval =  m;
            }
          else
            {
              ColumnVector m (nr);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  union
                  {
                    uint64_t ival;
                    double dval;
                  } num;

                  num.ival = 0;

                  for (octave_idx_type j = 0; j < nc; j++)
                    {
                      unsigned char ch = cmat.elem (i, j);

                      if (isxdigit (ch))
                        {
                          num.ival <<= 4;
                          if (ch >= 'a')
                            num.ival += static_cast<uint64_t> (ch - 'a' + 10);
                          else if (ch >= 'A')
                            num.ival += static_cast<uint64_t> (ch - 'A' + 10);
                          else
                            num.ival += static_cast<uint64_t> (ch - '0');
                        }
                      else
                        {
                          error ("hex2num: illegal character found in string S");
                          break;
                        }
                    }

                  if (error_state)
                    break;
                  else
                    {
                      if (nc < nchars)
                        num.ival <<= (nchars - nc) * 4;

                      m(i) = num.dval;
                    }
                }

              if (! error_state)
                retval =  m;
            }
        }
    }

  return retval;
}

/*
%!assert (hex2num (["c00";"bff";"000";"3ff";"400"]), [-2:2]')
%!assert (hex2num (["c00";"bf8";"000";"3f8";"400"], "single"), single([-2:2])')
*/

DEFUN (num2hex, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{s} =} num2hex (@var{n})\n\
Typecast a double or single precision number or vector to a 8 or 16\n\
character hexadecimal string of the IEEE 754 representation of the number.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
num2hex ([-1, 1, e, Inf])\n\
@result{} \"bff0000000000000\n\
    3ff0000000000000\n\
    4005bf0a8b145769\n\
    7ff0000000000000\"\n\
@end group\n\
@end example\n\
\n\
If the argument @var{n} is a single precision number or vector, the returned\n\
string has a length of 8.  For example:\n\
\n\
@example\n\
@group\n\
num2hex (single ([-1, 1, e, Inf]))\n\
@result{} \"bf800000\n\
    3f800000\n\
    402df854\n\
    7f800000\"\n\
@end group\n\
@end example\n\
@seealso{hex2num, hex2dec, dec2hex}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin != 1)
    print_usage ();
  else if (args(0).is_single_type ())
    {
      const FloatColumnVector v (args(0).float_vector_value ());

      if (! error_state)
        {
          octave_idx_type nchars = 8;
          octave_idx_type nr = v.length ();
          charMatrix m (nr, nchars);
          const float *pv = v.fortran_vec ();

          for (octave_idx_type i = 0; i < nr; i++)
            {
              union
              {
                uint32_t ival;
                float dval;
              } num;

              num.dval = *pv++;

              for (octave_idx_type j = 0; j < nchars; j++)
                {
                  unsigned char ch =
                    static_cast<char>(num.ival >> ((nchars - 1 - j) * 4) & 0xF);
                  if (ch >= 10)
                    ch += 'a' - 10;
                  else
                    ch += '0';

                  m.elem (i, j) = ch;
                }
            }

          retval = m;
        }
    }
  else
    {
      const ColumnVector v (args(0).vector_value ());

      if (! error_state)
        {
          octave_idx_type nchars = 16;
          octave_idx_type nr = v.length ();
          charMatrix m (nr, nchars);
          const double *pv = v.fortran_vec ();

          for (octave_idx_type i = 0; i < nr; i++)
            {
              union
              {
                uint64_t ival;
                double dval;
              } num;

              num.dval = *pv++;

              for (octave_idx_type j = 0; j < nchars; j++)
                {
                  unsigned char ch =
                    static_cast<char>(num.ival >> ((nchars - 1 - j) * 4) & 0xF);
                  if (ch >= 10)
                    ch += 'a' - 10;
                  else
                    ch += '0';

                  m.elem (i, j) = ch;
                }
            }

          retval = m;
        }
    }

  return retval;
}

/*
%!assert (num2hex (-2:2), ["c000000000000000";"bff0000000000000";"0000000000000000";"3ff0000000000000";"4000000000000000"])
%!assert (num2hex (single (-2:2)), ["c0000000";"bf800000";"00000000";"3f800000";"40000000"])
*/
