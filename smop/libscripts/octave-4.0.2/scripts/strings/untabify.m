## Copyright (C) 2010-2015 Ben Abbott
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} untabify (@var{t})
## @deftypefnx {Function File} {} untabify (@var{t}, @var{tw})
## @deftypefnx {Function File} {} untabify (@var{t}, @var{tw}, @var{deblank})
## Replace TAB characters in @var{t} with spaces.
##
## The input, @var{t}, may be either a 2-D character array, or a cell array of
## character strings.  The output is the same class as the input.
##
## The tab width is specified by @var{tw}, and defaults to eight.
##
## If the optional argument @var{deblank} is true, then the spaces will be
## removed from the end of the character data.
##
## The following example reads a file and writes an untabified version of the
## same file with trailing spaces stripped.
##
## @example
## @group
## fid = fopen ("tabbed_script.m");
## text = char (fread (fid, "uchar")');
## fclose (fid);
## fid = fopen ("untabified_script.m", "w");
## text = untabify (strsplit (text, "\n"), 8, true);
## fprintf (fid, "%s\n", text@{:@});
## fclose (fid);
## @end group
## @end example
##
## @seealso{strjust, strsplit, deblank}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-10-15

function s = untabify (t, tw = 8, dblank = false)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  elseif (! (ischar (t) || iscellstr (t)))
    error ("untabify: T must be a string or cellstring");
  endif

  if (ischar (t))
    s = replace_tabs (t, tw);
  else
    s = cellfun (@replace_tabs, t, {tw}, "uniformoutput", false);
  endif

  if (dblank)
    s = deblank (s);
  endif

endfunction

function s = replace_tabs (t, tw)

  if (ndims (t) != 2)
    error ("untabify: character strings to untabify must have 2 dimensions");
  endif

  if (isempty (t))
    s = t;
  else
    nr = rows (t);
    sc = cell (nr, 1);
    for j = 1:nr
      n = 1:numel (t(j,:));
      m = find (t(j,:) == "\t");
      t(j,m) = " ";
      for i = 1:numel (m)
        k = tw * ceil (n(m(i)) / tw);
        dn = k - n(m(i));
        n(m(i):end) += dn;
      endfor
      sc{j} = blanks (n(end));
      sc{j}(n) = t(j,:);
    endfor
    s = char (sc);
  endif

endfunction


%!test
%! s = untabify ("\thello\t");
%! assert (s, [blanks(8) "hello" blanks(3)]);

%!test
%! s = untabify ("\thello\t", 2);
%! assert (s, [blanks(2) "hello" blanks(1)]);

%!test
%! s = untabify ("\thello\t", 4, true);
%! assert (s, [blanks(4) "hello"]);

%!assert (isempty (untabify ("")))

%!test
%! s = char (randi ([97 97+25], 3, 3));
%! assert (untabify (s), char (untabify (cellstr (s))));

%!error untabify ()
%!error untabify (1,2,3,4)
%!error <must be a string> untabify (1)

