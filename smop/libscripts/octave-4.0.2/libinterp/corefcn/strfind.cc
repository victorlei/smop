/*

Copyright (C) 2009-2015 Jaroslav Hajek
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

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <deque>
#include <limits>
#include <string>

#include "oct-locbuf.h"

#include "Cell.h"
#include "ov.h"
#include "defun.h"
#include "unwind-prot.h"
#include "gripes.h"
#include "utils.h"

// This allows safe indexing with char.
// In C++, char may be (and often is) signed!
#define ORD(ch) static_cast<unsigned char>(ch)
#define TABSIZE (std::numeric_limits<unsigned char>::max () + 1)

// This is the quick search algorithm, as described at
// http://www-igm.univ-mlv.fr/~lecroq/string/node19.html
static void
qs_preprocess (const Array<char>& needle,
               octave_idx_type *table)
{
  const char *x = needle.data ();
  octave_idx_type m = needle.numel ();

  for (octave_idx_type i = 0; i < TABSIZE; i++)
    table[i] = m + 1;
  for (octave_idx_type i = 0; i < m; i++)
    table[ORD(x[i])] = m - i;
}


static Array<octave_idx_type>
qs_search (const Array<char>& needle,
           const Array<char>& haystack,
           const octave_idx_type *table,
           bool overlaps = true)
{
  const char *x = needle.data ();
  octave_idx_type m = needle.numel ();
  const char *y = haystack.data ();
  octave_idx_type n = haystack.numel ();

  // We'll use deque because it typically has the most favorable properties for
  // the operation we need.
  std::deque<octave_idx_type> accum;
  if (m == 1)
    {
      // Looking for a single character.
      for (octave_idx_type i = 0; i < n; i++)
        {
          if (y[i] == x[0])
            accum.push_back (i);
        }
    }
  else if (m == 2)
    {
      // Two characters.
      if (overlaps)
        {
          for (octave_idx_type i = 0; i < n-1; i++)
            {
              if (y[i] == x[0] && y[i+1] == x[1])
                accum.push_back (i);
            }
        }
      else
        {
          for (octave_idx_type i = 0; i < n-1; i++)
            {
              if (y[i] == x[0] && y[i+1] == x[1])
                accum.push_back (i++);
            }
        }
    }
  else if (n >= m)
    {
      // General case.
      octave_idx_type j = 0;

      if (overlaps)
        {
          while (j < n - m)
            {
              if (std::equal (x, x + m, y + j))
                accum.push_back (j);
              j += table[ORD(y[j + m])];
            }
        }
      else
        {
          while (j < n - m)
            {
              if (std::equal (x, x + m, y + j))
                {
                  accum.push_back (j);
                  j += m;
                }
              else
                j += table[ORD(y[j + m])];
            }
        }

      if (j == n - m && std::equal (x, x + m, y + j))
        accum.push_back (j);
    }

  octave_idx_type nmatch = accum.size ();
  octave_idx_type one = 1;
  Array<octave_idx_type> result (dim_vector (std::min (one, nmatch), nmatch));
  octave_idx_type k = 0;
  for (std::deque<octave_idx_type>::const_iterator iter = accum.begin ();
       iter != accum.end (); iter++)
    {
      result.xelem (k++) = *iter;
    }

  return result;
}

DEFUN (strfind, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{idx} =} strfind (@var{str}, @var{pattern})\n\
@deftypefnx {Built-in Function} {@var{idx} =} strfind (@var{cellstr}, @var{pattern})\n\
@deftypefnx {Built-in Function} {@var{idx} =} strfind (@dots{}, \"overlaps\", @var{val})\n\
Search for @var{pattern} in the string @var{str} and return the starting\n\
index of every such occurrence in the vector @var{idx}.\n\
\n\
If there is no such occurrence, or if @var{pattern} is longer than\n\
@var{str}, or if @var{pattern} itself is empty, then @var{idx} is the empty\n\
array @code{[]}.\n\
\n\
The optional argument @qcode{\"overlaps\"} determines whether the pattern\n\
can match at every position in @var{str} (true), or only for unique\n\
occurrences of the complete pattern (false).  The default is true.\n\
\n\
If a cell array of strings @var{cellstr} is specified then @var{idx} is a\n\
cell array of vectors, as specified above.\n\
\n\
Examples:\n\
\n\
@example\n\
@group\n\
strfind (\"abababa\", \"aba\")\n\
     @result{} [1, 3, 5]\n\
\n\
strfind (\"abababa\", \"aba\", \"overlaps\", false)\n\
     @result{} [1, 5]\n\
\n\
strfind (@{\"abababa\", \"bebebe\", \"ab\"@}, \"aba\")\n\
     @result{}\n\
        @{\n\
          [1,1] =\n\
\n\
             1   3   5\n\
\n\
          [1,2] = [](1x0)\n\
          [1,3] = [](1x0)\n\
        @}\n\
@end group\n\
@end example\n\
@seealso{findstr, strmatch, regexp, regexpi, find}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();
  bool overlaps = true;

  if (nargin == 4 && args(2).is_string () && args(3).is_scalar_type ())
    {
      std::string opt = args(2).string_value ();
      if (opt == "overlaps")
        {
          overlaps = args(3).bool_value ();
          nargin = 2;
        }
      else
        {
          error ("strfind: unknown option: %s", opt.c_str ());
          return retval;
        }
    }

  if (nargin == 2)
    {
      octave_value argstr = args(0);
      octave_value argpat = args(1);
      if (argpat.is_string ())
        {
          Array<char> needle = argpat.char_array_value ();
          OCTAVE_LOCAL_BUFFER (octave_idx_type, table, TABSIZE);
          qs_preprocess (needle, table);

          if (argstr.is_string ())
            if (argpat.is_empty ())
              // Return a null matrix for null pattern for MW compatibility
              retval = Matrix ();
            else
              retval = octave_value (qs_search (needle,
                                                argstr.char_array_value (),
                                                table, overlaps),
                                     true, true);
          else if (argstr.is_cell ())
            {
              const Cell argsc = argstr.cell_value ();
              Cell retc (argsc.dims ());
              octave_idx_type ns = argsc.numel ();

              for (octave_idx_type i = 0; i < ns; i++)
                {
                  octave_value argse = argsc(i);
                  if (argse.is_string ())
                    {
                      if (argpat.is_empty ())
                        retc(i) = Matrix ();
                      else
                        retc(i) = octave_value (qs_search (needle,
                                                     argse.char_array_value (),
                                                     table, overlaps),
                                                true, true);
                    }
                  else
                    {
                      error ("strfind: each element of CELLSTR must be a string");
                      break;
                    }
                }

              retval = retc;
            }
          else
            error ("strfind: first argument must be a string or cell array of strings");
        }
      else if (argpat.is_cell ())
        retval = do_simple_cellfun (Fstrfind, "strfind", args);
      else
        error ("strfind: PATTERN must be a string or cell array of strings");
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (strfind ("abababa", "aba"), [1, 3, 5])
%!assert (strfind ("abababa", "aba", "overlaps", false), [1, 5])
%!assert (strfind ({"abababa", "bla", "bla"}, "a"), {[1, 3, 5, 7], 3, 3})
%!assert (strfind ("Linux _is_ user-friendly. It just isn't ignorant-friendly or idiot-friendly.", "friendly"), [17, 50, 68])
%!assert (strfind ("abc", ""), [])
%!assert (strfind ("abc", {"", "b", ""}), {[], 2, []})
%!assert (strfind ({"abc", "def"}, ""), {[], []})

%!error strfind ()
%!error strfind ("foo", "bar", 1)
%!error <unknown option: foobar> strfind ("foo", 100, "foobar", 1)
%!error <each element of CELLSTR must be a string> strfind ({"A", 1}, "foo")
%!error <first argument must be a string> strfind (100, "foo")
%!error <PATTERN must be a string> strfind ("foo", 100)
*/

static Array<char>
qs_replace (const Array<char>& str, const Array<char>& pat,
            const Array<char>& rep,
            const octave_idx_type *table,
            bool overlaps = true)
{
  Array<char> ret = str;

  octave_idx_type siz = str.numel ();
  octave_idx_type psiz = pat.numel ();
  octave_idx_type rsiz = rep.numel ();

  if (psiz != 0)
    {
      // Look up matches, without overlaps.
      const Array<octave_idx_type> idx = qs_search (pat, str, table, overlaps);
      octave_idx_type nidx = idx.numel ();

      if (nidx)
        {
          // Compute result size.
          octave_idx_type retsiz;
          if (overlaps)
            {
              retsiz = 0;
              // OMG. Is this the "right answer" MW always looks for, or
              // someone was just lazy?
              octave_idx_type k = 0;
              for (octave_idx_type i = 0; i < nidx; i++)
                {
                  octave_idx_type j = idx(i);
                  if (j >= k)
                    retsiz += j - k;
                  retsiz += rsiz;
                  k = j + psiz;
                }

              retsiz += siz - k;
            }
          else
            retsiz = siz + nidx * (rsiz - psiz);

          if (retsiz == 0)
            ret.clear (dim_vector (0, 0));
          else
            {
              ret.clear (dim_vector (1, retsiz));
              const char *src = str.data ();
              const char *reps = rep.data ();
              char *dest = ret.fortran_vec ();

              octave_idx_type k = 0;
              for (octave_idx_type i = 0; i < nidx; i++)
                {
                  octave_idx_type j = idx(i);
                  if (j >= k)
                    dest = std::copy (src + k, src + j, dest);
                  dest = std::copy (reps, reps + rsiz, dest);
                  k = j + psiz;
                }

              std::copy (src + k, src + siz, dest);
            }
        }
    }

  return ret;
}

DEFUN (strrep, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{newstr} =} strrep (@var{str}, @var{ptn}, @var{rep})\n\
@deftypefnx {Built-in Function} {@var{newstr} =} strrep (@var{cellstr}, @var{ptn}, @var{rep})\n\
@deftypefnx {Built-in Function} {@var{newstr} =} strrep (@dots{}, \"overlaps\", @var{val})\n\
Replace all occurrences of the pattern @var{ptn} in the string @var{str}\n\
with the string @var{rep} and return the result.\n\
\n\
The optional argument @qcode{\"overlaps\"} determines whether the pattern\n\
can match at every position in @var{str} (true), or only for unique\n\
occurrences of the complete pattern (false).  The default is true.\n\
\n\
@var{s} may also be a cell array of strings, in which case the replacement is\n\
done for each element and a cell array is returned.\n\
\n\
Example:\n\
\n\
@example\n\
@group\n\
strrep (\"This is a test string\", \"is\", \"&%$\")\n\
    @result{}  \"Th&%$ &%$ a test string\"\n\
@end group\n\
@end example\n\
\n\
@seealso{regexprep, strfind, findstr}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();
  bool overlaps = true;

  if (nargin == 5 && args(3).is_string () && args(4).is_scalar_type ())
    {
      std::string opt = args(3).string_value ();
      if (opt == "overlaps")
        {
          overlaps = args(4).bool_value ();
          nargin = 3;
        }
      else
        {
          error ("strrep: unknown option: %s", opt.c_str ());
          return retval;
        }
    }

  if (nargin == 3)
    {
      octave_value argstr = args(0);
      octave_value argpat = args(1);
      octave_value argrep = args(2);
      if (argpat.is_string () && argrep.is_string ())
        {
          const Array<char> pat = argpat.char_array_value ();
          const Array<char> rep = argrep.char_array_value ();

          OCTAVE_LOCAL_BUFFER (octave_idx_type, table, TABSIZE);
          qs_preprocess (pat, table);

          if (argstr.is_string ())
            retval = qs_replace (argstr.char_array_value (), pat, rep,
                                 table, overlaps);
          else if (argstr.is_cell ())
            {
              const Cell argsc = argstr.cell_value ();
              Cell retc (argsc.dims ());
              octave_idx_type ns = argsc.numel ();

              for (octave_idx_type i = 0; i < ns; i++)
                {
                  octave_value argse = argsc(i);
                  if (argse.is_string ())
                    retc(i) = qs_replace (argse.char_array_value (), pat, rep,
                                          table, overlaps);
                  else
                    {
                      error ("strrep: each element of S must be a string");
                      break;
                    }
                }

              retval = retc;
            }
          else
            error ("strrep: S must be a string or cell array of strings");
        }
      else if (argpat.is_cell () || argrep.is_cell ())
        retval = do_simple_cellfun (Fstrrep, "strrep", args);
      else
        error ("strrep: PTN and REP arguments must be strings or cell arrays of strings");
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (strrep ("This is a test string", "is", "&%$"),
%!                "Th&%$ &%$ a test string")
%!assert (strrep ("abababc", "abab", "xyz"), "xyzxyzc")
%!assert (strrep ("abababc", "abab", "xyz", "overlaps", false), "xyzabc")

%!assert (size (strrep ("a", "a", "")), [0 0])

%!error strrep ()
%!error strrep ("foo", "bar", 3, 4)
*/
