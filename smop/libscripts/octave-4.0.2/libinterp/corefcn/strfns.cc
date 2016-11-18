/*

Copyright (C) 1994-2015 John W. Eaton

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

#include <cctype>

#include <queue>
#include <sstream>

#include "dMatrix.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "ov.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"

DEFUN (char, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} char (@var{x})\n\
@deftypefnx {Built-in Function} {} char (@var{x}, @dots{})\n\
@deftypefnx {Built-in Function} {} char (@var{s1}, @var{s2}, @dots{})\n\
@deftypefnx {Built-in Function} {} char (@var{cell_array})\n\
Create a string array from one or more numeric matrices, character\n\
matrices, or cell arrays.\n\
\n\
Arguments are concatenated vertically.  The returned values are padded with\n\
blanks as needed to make each row of the string array have the same length.\n\
Empty input strings are significant and will concatenated in the output.\n\
\n\
For numerical input, each element is converted to the corresponding ASCII\n\
character.  A range error results if an input is outside the ASCII range\n\
(0-255).\n\
\n\
For cell arrays, each element is concatenated separately.  Cell arrays\n\
converted through @code{char} can mostly be converted back with\n\
@code{cellstr}.  For example:\n\
\n\
@example\n\
@group\n\
char ([97, 98, 99], \"\", @{\"98\", \"99\", 100@}, \"str1\", [\"ha\", \"lf\"])\n\
   @result{} [\"abc    \"\n\
       \"       \"\n\
       \"98     \"\n\
       \"99     \"\n\
       \"d      \"\n\
       \"str1   \"\n\
       \"half   \"]\n\
@end group\n\
@end example\n\
@seealso{strvcat, cellstr}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = "";
  else if (nargin == 1)
    retval = args(0).convert_to_str (true, true,
                                     args(0).is_dq_string () ? '"' : '\'');
  else
    {
      int n_elts = 0;

      int max_len = 0;

      std::queue<string_vector> args_as_strings;

      for (int i = 0; i < nargin; i++)
        {
          string_vector s = args(i).all_strings ();

          if (error_state)
            {
              error ("char: unable to convert some args to strings");
              return retval;
            }

          if (s.length () > 0)
            n_elts += s.length ();
          else
            n_elts += 1;

          int s_max_len = s.max_length ();

          if (s_max_len > max_len)
            max_len = s_max_len;

          args_as_strings.push (s);
        }

      string_vector result (n_elts);

      int k = 0;

      for (int i = 0; i < nargin; i++)
        {
          string_vector s = args_as_strings.front ();
          args_as_strings.pop ();

          int n = s.length ();

          if (n > 0)
            {
              for (int j = 0; j < n; j++)
                {
                  std::string t = s[j];
                  int t_len = t.length ();

                  if (max_len > t_len)
                    t += std::string (max_len - t_len, ' ');

                  result[k++] = t;
                }
            }
          else
            result[k++] = std::string (max_len, ' ');
        }

      retval = octave_value (result, '\'');
    }

  return retval;
}

/*
%!assert (char (), '');
%!assert (char (100), "d");
%!assert (char (100,100), ["d";"d"])
%!assert (char ({100,100}), ["d";"d"])
%!assert (char ([100,100]), ["dd"])
%!assert (char ({100,{100}}), ["d";"d"])
%!assert (char (100, [], 100), ["d";" ";"d"])
%!assert (char ({100, [], 100}), ["d";" ";"d"])
%!assert (char ({100,{100, {""}}}), ["d";"d";" "])
%!assert (char (["a";"be"], {"c", 100}), ["a";"be";"c";"d"])
%!assert (char ("a", "bb", "ccc"), ["a  "; "bb "; "ccc"])
%!assert (char ([65, 83, 67, 73, 73]), "ASCII")

%!test
%! x = char ("foo", "bar", "foobar");
%! assert (x(1,:), "foo   ");
%! assert (x(2,:), "bar   ");
%! assert (x(3,:), "foobar");
*/

DEFUN (strvcat, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} strvcat (@var{x})\n\
@deftypefnx {Built-in Function} {} strvcat (@var{x}, @dots{})\n\
@deftypefnx {Built-in Function} {} strvcat (@var{s1}, @var{s2}, @dots{})\n\
@deftypefnx {Built-in Function} {} strvcat (@var{cell_array})\n\
Create a character array from one or more numeric matrices, character\n\
matrices, or cell arrays.\n\
\n\
Arguments are concatenated vertically.  The returned values are padded with\n\
blanks as needed to make each row of the string array have the same length.\n\
Unlike @code{char}, empty strings are removed and will not appear in the\n\
output.\n\
\n\
For numerical input, each element is converted to the corresponding ASCII\n\
character.  A range error results if an input is outside the ASCII range\n\
(0-255).\n\
\n\
For cell arrays, each element is concatenated separately.  Cell arrays\n\
converted through @code{strvcat} can mostly be converted back with\n\
@code{cellstr}.  For example:\n\
\n\
@example\n\
@group\n\
strvcat ([97, 98, 99], \"\", @{\"98\", \"99\", 100@}, \"str1\", [\"ha\", \"lf\"])\n\
      @result{} [\"abc    \"\n\
          \"98     \"\n\
          \"99     \"\n\
          \"d      \"\n\
          \"str1   \"\n\
          \"half   \"]\n\
@end group\n\
@end example\n\
@seealso{char, strcat, cstrcat}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      int n_elts = 0;

      size_t max_len = 0;

      std::queue<string_vector> args_as_strings;

      for (int i = 0; i < nargin; i++)
        {
          string_vector s = args(i).all_strings ();

          if (error_state)
            {
              error ("strvcat: unable to convert some args to strings");
              return retval;
            }

          size_t n = s.length ();

          // do not count empty strings in calculation of number of elements
          if (n > 0)
            {
              for (size_t j = 0; j < n; j++)
                {
                  if (s[j].length () > 0)
                    n_elts++;
                }
            }

          size_t s_max_len = s.max_length ();

          if (s_max_len > max_len)
            max_len = s_max_len;

          args_as_strings.push (s);
        }

      string_vector result (n_elts);

      octave_idx_type k = 0;

      for (int i = 0; i < nargin; i++)
        {
          string_vector s = args_as_strings.front ();
          args_as_strings.pop ();

          size_t n = s.length ();

          if (n > 0)
            {
              for (size_t j = 0; j < n; j++)
                {
                  std::string t = s[j];
                  if (t.length () > 0)
                    {
                      size_t t_len = t.length ();

                      if (max_len > t_len)
                        t += std::string (max_len - t_len, ' ');

                      result[k++] = t;
                    }
                }
            }
        }

      retval = octave_value (result, '\'');
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (strvcat (""), "");
%!assert (strvcat (100) == "d");
%!assert (strvcat (100,100), ["d";"d"])
%!assert (strvcat ({100,100}), ["d";"d"])
%!assert (strvcat ([100,100]), ["dd"])
%!assert (strvcat ({100,{100}}), ["d";"d"])
%!assert (strvcat (100, [], 100), ["d";"d"])
%!assert (strvcat ({100, [], 100}), ["d";"d"])
%!assert (strvcat ({100,{100, {""}}}), ["d";"d"])
%!assert (strvcat (["a";"be"], {"c", 100}), ["a";"be";"c";"d"])
%!assert (strvcat ("a", "bb", "ccc"), ["a  "; "bb "; "ccc"])

%!error strvcat ()
*/


DEFUN (ischar, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ischar (@var{x})\n\
Return true if @var{x} is a character array.\n\
@seealso{isfloat, isinteger, islogical, isnumeric, iscellstr, isa}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = args(0).is_string ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (ischar ("a"), true)
%!assert (ischar (["ab";"cd"]), true)
%!assert (ischar ({"ab"}), false)
%!assert (ischar (1), false)
%!assert (ischar ([1, 2]), false)
%!assert (ischar ([]), false)
%!assert (ischar ([1, 2; 3, 4]), false)
%!assert (ischar (""), true)
%!assert (ischar ("test"), true)
%!assert (ischar (["test"; "ing"]), true)
%!assert (ischar (struct ("foo", "bar")), false)

%!error ischar ()
%!error ischar ("test", 1)
*/

static octave_value
do_strcmp_fun (const octave_value& arg0, const octave_value& arg1,
               octave_idx_type n, const char *fcn_name,
               bool (*array_op) (const charNDArray&, const charNDArray&,
                                 octave_idx_type),
               bool (*str_op) (const std::string&, const std::string&,
                               octave_idx_type))

{
  octave_value retval;

  bool s1_string = arg0.is_string ();
  bool s1_cell = arg0.is_cell ();
  bool s2_string = arg1.is_string ();
  bool s2_cell = arg1.is_cell ();

  if (s1_string && s2_string)
    retval = array_op (arg0.char_array_value (), arg1.char_array_value (), n);
  else if ((s1_string && s2_cell) || (s1_cell && s2_string))
    {
      octave_value str_val, cell_val;

      if (s1_string)
        {
          str_val = arg0;
          cell_val = arg1;
        }
      else
        {
          str_val = arg1;
          cell_val = arg0;
        }

      const Cell cell = cell_val.cell_value ();
      const string_vector str = str_val.all_strings ();
      octave_idx_type r = str.length ();

      if (r == 0 || r == 1)
        {
          // Broadcast the string.

          boolNDArray output (cell_val.dims (), false);

          std::string s = r == 0 ? std::string () : str[0];

          if (cell_val.is_cellstr ())
            {
              const Array<std::string> cellstr = cell_val.cellstr_value ();
              for (octave_idx_type i = 0; i < cellstr.length (); i++)
                output(i) = str_op (cellstr(i), s, n);
            }
          else
            {
              // FIXME: should we warn here?
              for (octave_idx_type i = 0; i < cell.length (); i++)
                {
                  if (cell(i).is_string ())
                    output(i) = str_op (cell(i).string_value (), s, n);
                }
            }

          retval = output;
        }
      else if (r > 1)
        {
          if (cell.length () == 1)
            {
              // Broadcast the cell.

              const dim_vector dv (r, 1);
              boolNDArray output (dv, false);

              if (cell(0).is_string ())
                {
                  const std::string str2 = cell(0).string_value ();

                  for (octave_idx_type i = 0; i < r; i++)
                    output(i) = str_op (str[i], str2, n);
                }

              retval = output;
            }
          else
            {
              // Must match in all dimensions.

              boolNDArray output (cell.dims (), false);

              if (cell.length () == r)
                {
                  if (cell_val.is_cellstr ())
                    {
                      const Array<std::string> cellstr
                        = cell_val.cellstr_value ();
                      for (octave_idx_type i = 0; i < cellstr.length (); i++)
                        output(i) = str_op (str[i], cellstr(i), n);
                    }
                  else
                    {
                      // FIXME: should we warn here?
                      for (octave_idx_type i = 0; i < r; i++)
                        {
                          if (cell(i).is_string ())
                            output(i) = str_op (str[i],
                                                cell(i).string_value (), n);
                        }
                    }

                  retval = output;
                }
              else
                retval = false;
            }
        }
    }
  else if (s1_cell && s2_cell)
    {
      octave_value cell1_val, cell2_val;
      octave_idx_type r1 = arg0.numel (), r2;

      if (r1 == 1)
        {
          // Make the singleton cell2.

          cell1_val = arg1;
          cell2_val = arg0;
        }
      else
        {
          cell1_val = arg0;
          cell2_val = arg1;
        }

      const Cell cell1 = cell1_val.cell_value ();
      const Cell cell2 = cell2_val.cell_value ();
      r1 = cell1.numel ();
      r2 = cell2.numel ();

      const dim_vector size1 = cell1.dims ();
      const dim_vector size2 = cell2.dims ();

      boolNDArray output (size1, false);

      if (r2 == 1)
        {
          // Broadcast cell2.

          if (cell2(0).is_string ())
            {
              const std::string str2 = cell2(0).string_value ();

              if (cell1_val.is_cellstr ())
                {
                  const Array<std::string> cellstr = cell1_val.cellstr_value ();
                  for (octave_idx_type i = 0; i < cellstr.length (); i++)
                    output(i) = str_op (cellstr(i), str2, n);
                }
              else
                {
                  // FIXME: should we warn here?
                  for (octave_idx_type i = 0; i < r1; i++)
                    {
                      if (cell1(i).is_string ())
                        {
                          const std::string str1 = cell1(i).string_value ();
                          output(i) = str_op (str1, str2, n);
                        }
                    }
                }
            }
        }
      else
        {
          if (size1 != size2)
            {
              error ("%s: nonconformant cell arrays", fcn_name);
              return retval;
            }

          if (cell1.is_cellstr () && cell2.is_cellstr ())
            {
              const Array<std::string> cellstr1 = cell1_val.cellstr_value ();
              const Array<std::string> cellstr2 = cell2_val.cellstr_value ();
              for (octave_idx_type i = 0; i < r1; i++)
                output (i) = str_op (cellstr1(i), cellstr2(i), n);
            }
          else
            {
              // FIXME: should we warn here?
              for (octave_idx_type i = 0; i < r1; i++)
                {
                  if (cell1(i).is_string () && cell2(i).is_string ())
                    {
                      const std::string str1 = cell1(i).string_value ();
                      const std::string str2 = cell2(i).string_value ();
                      output(i) = str_op (str1, str2, n);
                    }
                }
            }
        }

      retval = output;
    }
  else
    retval = false;

  return retval;
}

// If both args are arrays, dimensions may be significant.
static bool
strcmp_array_op (const charNDArray& s1, const charNDArray& s2, octave_idx_type)
{
  return (s1.dims () == s2.dims ()
          && std::equal (s1.data (), s1.data () + s1.numel (), s2.data ()));
}

// Otherwise, just use strings.
static bool
strcmp_str_op (const std::string& s1, const std::string& s2,
               octave_idx_type)
{
  return s1 == s2;
}

DEFUN (strcmp, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} strcmp (@var{s1}, @var{s2})\n\
Return 1 if the character strings @var{s1} and @var{s2} are the same,\n\
and 0 otherwise.\n\
\n\
If either @var{s1} or @var{s2} is a cell array of strings, then an array\n\
of the same size is returned, containing the values described above for\n\
every member of the cell array.  The other argument may also be a cell\n\
array of strings (of the same size or with only one element), char matrix\n\
or character string.\n\
\n\
@strong{Caution:} For compatibility with @sc{matlab}, Octave's strcmp\n\
function returns 1 if the character strings are equal, and 0 otherwise.\n\
This is just the opposite of the corresponding C library function.\n\
@seealso{strcmpi, strncmp, strncmpi}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      retval = do_strcmp_fun (args(0), args(1), 0,
                              "strcmp", strcmp_array_op, strcmp_str_op);
    }
  else
    print_usage ();

  return retval;
}

/*
%!shared x
%! x = char (zeros (0, 2));
%!assert (strcmp ("", x), false)
%!assert (strcmp (x, ""), false)
%!assert (strcmp (x, x), true)
## %!assert (strcmp ({""}, x), true)
## %!assert (strcmp ({x}, ""), false)
## %!assert (strcmp ({x}, x), true)
## %!assert (strcmp ("", {x}), false)
## %!assert (strcmp (x, {""}), false)
## %!assert (strcmp (x, {x}), true)
## %!assert (strcmp ({x; x}, ""), [false; false])
## %!assert (strcmp ({x; x}, {""}), [false; false])
## %!assert (strcmp ("", {x; x}), [false; false])
## %!assert (strcmp ({""}, {x; x}), [false; false])
%!assert (strcmp ({"foo"}, x), false)
%!assert (strcmp ({"foo"}, "foo"), true)
%!assert (strcmp ({"foo"}, x), false)
%!assert (strcmp (x, {"foo"}), false)
%!assert (strcmp ("foo", {"foo"}), true)
%!assert (strcmp (x, {"foo"}), false)
%!shared y
%! y = char (zeros (2, 0));
%!assert (strcmp ("", y), false)
%!assert (strcmp (y, ""), false)
%!assert (strcmp (y, y), true)
%!assert (strcmp ({""}, y), [true; true])
%!assert (strcmp ({y}, ""), true)
%!assert (strcmp ({y}, y), [true; true])
%!assert (strcmp ("", {y}), true)
%!assert (strcmp (y, {""}), [true; true])
%!assert (strcmp (y, {y}), [true; true])
%!assert (strcmp ({y; y}, ""), [true; true])
%!assert (strcmp ({y; y}, {""}), [true; true])
%!assert (strcmp ("", {y; y}), [true; true])
%!assert (strcmp ({""}, {y; y}), [true; true])
%!assert (strcmp ({"foo"}, y), [false; false])
%!assert (strcmp ({"foo"}, y), [false; false])
%!assert (strcmp (y, {"foo"}), [false; false])
%!assert (strcmp (y, {"foo"}), [false; false])
%!assert (strcmp ("foobar", "foobar"), true)
%!assert (strcmp ("fooba", "foobar"), false)

%!error strcmp ()
%!error strcmp ("foo", "bar", 3)
*/

// Apparently, Matlab ignores the dims with strncmp. It also
static bool
strncmp_array_op (const charNDArray& s1, const charNDArray& s2,
                  octave_idx_type n)
{
  octave_idx_type l1 = s1.numel ();
  octave_idx_type l2 = s2.numel ();
  return (n > 0 && n <= l1 && n <= l2
          && std::equal (s1.data (), s1.data () + n, s2.data ()));
}

// Otherwise, just use strings. Note that we neither extract substrings (which
// would mean a copy, at least in GCC), nor use string::compare (which is a
// 3-way compare).
static bool
strncmp_str_op (const std::string& s1, const std::string& s2, octave_idx_type n)
{
  octave_idx_type l1 = s1.length ();
  octave_idx_type l2 = s2.length ();
  return (n > 0 && n <= l1 && n <= l2
          && std::equal (s1.data (), s1.data () + n, s2.data ()));
}

DEFUN (strncmp, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} strncmp (@var{s1}, @var{s2}, @var{n})\n\
Return 1 if the first @var{n} characters of strings @var{s1} and @var{s2} are\n\
the same, and 0 otherwise.\n\
\n\
@example\n\
@group\n\
strncmp (\"abce\", \"abcd\", 3)\n\
      @result{} 1\n\
@end group\n\
@end example\n\
\n\
If either @var{s1} or @var{s2} is a cell array of strings, then an array\n\
of the same size is returned, containing the values described above for\n\
every member of the cell array.  The other argument may also be a cell\n\
array of strings (of the same size or with only one element), char matrix\n\
or character string.\n\
\n\
@example\n\
@group\n\
strncmp (\"abce\", @{\"abcd\", \"bca\", \"abc\"@}, 3)\n\
     @result{} [1, 0, 1]\n\
@end group\n\
@end example\n\
\n\
@strong{Caution:} For compatibility with @sc{matlab}, Octave's strncmp\n\
function returns 1 if the character strings are equal, and 0 otherwise.\n\
This is just the opposite of the corresponding C library function.\n\
@seealso{strncmpi, strcmp, strcmpi}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 3)
    {
      octave_idx_type n = args(2).idx_type_value ();

      if (! error_state)
        {
          if (n > 0)
            {
              retval = do_strcmp_fun (args(0), args(1), n, "strncmp",
                                      strncmp_array_op, strncmp_str_op);
            }
          else
            error ("strncmp: N must be greater than 0");
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (strncmp ("abce", "abc", 3), true)
%!assert (strncmp (100, 100, 1), false)
%!assert (strncmp ("abce", {"abcd", "bca", "abc"}, 3), logical ([1, 0, 1]))
%!assert (strncmp ("abc",  {"abcd", "bca", "abc"}, 4), logical ([0, 0, 0]))
%!assert (strncmp ({"abcd", "bca", "abc"},"abce", 3), logical ([1, 0, 1]))
%!assert (strncmp ({"abcd", "bca", "abc"},{"abcd", "bca", "abe"}, 3), logical ([1, 1, 0]))
%!assert (strncmp ("abc", {"abcd", 10}, 2), logical ([1, 0]))

%!error strncmp ()
%!error strncmp ("abc", "def")
*/

// case-insensitive character equality functor
struct icmp_char_eq : public std::binary_function<char, char, bool>
{
  bool operator () (char x, char y) const
  { return std::toupper (x) == std::toupper (y); }
};

// strcmpi is equivalent to strcmp in that it checks all dims.
static bool
strcmpi_array_op (const charNDArray& s1, const charNDArray& s2, octave_idx_type)
{
  return (s1.dims () == s2.dims ()
          && std::equal (s1.data (), s1.data () + s1.numel (), s2.data (),
                         icmp_char_eq ()));
}

// Ditto for string.
static bool
strcmpi_str_op (const std::string& s1, const std::string& s2,
                octave_idx_type)
{
  return (s1.size () == s2.size ()
          && std::equal (s1.data (), s1.data () + s1.size (), s2.data (),
                         icmp_char_eq ()));
}

DEFUNX ("strcmpi", Fstrcmpi, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} strcmpi (@var{s1}, @var{s2})\n\
Return 1 if the character strings @var{s1} and @var{s2} are the same,\n\
disregarding case of alphabetic characters, and 0 otherwise.\n\
\n\
If either @var{s1} or @var{s2} is a cell array of strings, then an array\n\
of the same size is returned, containing the values described above for\n\
every member of the cell array.  The other argument may also be a cell\n\
array of strings (of the same size or with only one element), char matrix\n\
or character string.\n\
\n\
@strong{Caution:} For compatibility with @sc{matlab}, Octave's strcmp\n\
function returns 1 if the character strings are equal, and 0 otherwise.\n\
This is just the opposite of the corresponding C library function.\n\
\n\
@strong{Caution:} National alphabets are not supported.\n\
@seealso{strcmp, strncmp, strncmpi}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      retval = do_strcmp_fun (args(0), args(1), 0,
                              "strcmpi", strcmpi_array_op, strcmpi_str_op);
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (strcmpi ("abc123", "ABC123"), true)
*/

// Like strncmp.
static bool
strncmpi_array_op (const charNDArray& s1, const charNDArray& s2,
                   octave_idx_type n)
{
  octave_idx_type l1 = s1.numel ();
  octave_idx_type l2 = s2.numel ();
  return (n > 0 && n <= l1 && n <= l2
          && std::equal (s1.data (), s1.data () + n, s2.data (),
                         icmp_char_eq ()));
}

// Ditto.
static bool
strncmpi_str_op (const std::string& s1, const std::string& s2,
                 octave_idx_type n)
{
  octave_idx_type l1 = s1.length ();
  octave_idx_type l2 = s2.length ();
  return (n > 0 && n <= l1 && n <= l2
          && std::equal (s1.data (), s1.data () + n, s2.data (),
                         icmp_char_eq ()));
}

DEFUNX ("strncmpi", Fstrncmpi, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} strncmpi (@var{s1}, @var{s2}, @var{n})\n\
Return 1 if the first @var{n} character of @var{s1} and @var{s2} are the\n\
same, disregarding case of alphabetic characters, and 0 otherwise.\n\
\n\
If either @var{s1} or @var{s2} is a cell array of strings, then an array\n\
of the same size is returned, containing the values described above for\n\
every member of the cell array.  The other argument may also be a cell\n\
array of strings (of the same size or with only one element), char matrix\n\
or character string.\n\
\n\
@strong{Caution:} For compatibility with @sc{matlab}, Octave's strncmpi\n\
function returns 1 if the character strings are equal, and 0 otherwise.\n\
This is just the opposite of the corresponding C library function.\n\
\n\
@strong{Caution:} National alphabets are not supported.\n\
@seealso{strncmp, strcmp, strcmpi}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 3)
    {
      octave_idx_type n = args(2).idx_type_value ();

      if (! error_state)
        {
          if (n > 0)
            {
              retval = do_strcmp_fun (args(0), args(1), n, "strncmpi",
                                      strncmpi_array_op, strncmpi_str_op);
            }
          else
            error ("strncmpi: N must be greater than 0");
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (strncmpi ("abc123", "ABC456", 3), true)
*/

DEFUN (list_in_columns, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} list_in_columns (@var{arg}, @var{width}, @var{prefix})\n\
Return a string containing the elements of @var{arg} listed in columns with\n\
an overall maximum width of @var{width} and optional prefix @var{prefix}.\n\
\n\
The argument @var{arg} must be a cell array of character strings or a\n\
character array.\n\
\n\
If @var{width} is not specified or is an empty matrix, or less than or equal\n\
to zero, the width of the terminal screen is used.  Newline characters are\n\
used to break the lines in the output string.  For example:\n\
@c Set example in small font to prevent overfull line\n\
\n\
@smallexample\n\
@group\n\
list_in_columns (@{\"abc\", \"def\", \"ghijkl\", \"mnop\", \"qrs\", \"tuv\"@}, 20)\n\
     @result{} abc     mnop\n\
        def     qrs\n\
        ghijkl  tuv\n\
\n\
whos ans\n\
     @result{}\n\
     Variables in the current scope:\n\
\n\
       Attr Name        Size                     Bytes  Class\n\
       ==== ====        ====                     =====  =====\n\
            ans         1x37                        37  char\n\
\n\
     Total is 37 elements using 37 bytes\n\
@end group\n\
@end smallexample\n\
\n\
@seealso{terminal_size}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  string_vector s = args(0).all_strings ();

  if (error_state)
    {
      error ("list_in_columns: ARG must be a cellstr or char array");
      return retval;
    }

  int width = -1;

  if (nargin > 1 && ! args(1).is_empty ())
    {
      width = args(1).int_value ();

      if (error_state)
        {
          error ("list_in_columns: WIDTH must be an integer");
          return retval;
        }
    }

  std::string prefix;

  if (nargin > 2)
    {
      if (args(2).is_string ())
        prefix = args(2).string_value ();
      else
        {
          error ("list_in_columns: PREFIX must be a string");
          return retval;
        }
    }

  std::ostringstream buf;

  s.list_in_columns (buf, width, prefix);

  retval = buf.str ();

  return retval;
}

/*
%!test
%! input  = {"abc", "def", "ghijkl", "mnop", "qrs", "tuv"};
%! result = "abc     mnop\ndef     qrs\nghijkl  tuv\n";
%! assert (list_in_columns (input, 20), result);
%!test
%! input  = ["abc"; "def"; "ghijkl"; "mnop"; "qrs"; "tuv"];
%! result = "abc     mnop  \ndef     qrs   \nghijkl  tuv   \n";
%! assert (list_in_columns (input, 20), result);
%!test
%! input  = ["abc"; "def"; "ghijkl"; "mnop"; "qrs"; "tuv"];
%! result = "  abc     mnop  \n  def     qrs   \n  ghijkl  tuv   \n";
%! assert (list_in_columns (input, 20, "  "), result);

%!error list_in_columns ()
%!error list_in_columns (["abc", "def"], 20, 2)
%!error list_in_columns (["abc", "def"], 20, "  ", 3)
%!error <invalid conversion from string to real scalar> list_in_columns (["abc", "def"], "a")
*/
