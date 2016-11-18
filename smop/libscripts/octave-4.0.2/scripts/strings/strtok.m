## Copyright (C) 2000-2015 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {[@var{tok}, @var{rem}] =} strtok (@var{str})
## @deftypefnx {Function File} {[@var{tok}, @var{rem}] =} strtok (@var{str}, @var{delim})
##
## Find all characters in the string @var{str} up to, but not including, the
## first character which is in the string @var{delim}.
##
## @var{str} may also be a cell array of strings in which case the function
## executes on every individual string and returns a cell array of tokens and
## remainders.
##
## Leading delimiters are ignored.  If @var{delim} is not specified,
## whitespace is assumed.
##
## If @var{rem} is requested, it contains the remainder of the string, starting
## at the first delimiter.
##
## Examples:
##
## @example
## @group
## strtok ("this is the life")
##      @result{} "this"
##
## [tok, rem] = strtok ("14*27+31", "+-*/")
##      @result{}
##         tok = 14
##         rem = *27+31
## @end group
## @end example
## @seealso{index, strsplit, strchr, isspace}
## @end deftypefn

function [tok, rem] = strtok (str, delim)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! (ischar (str) || iscellstr (str)))
    error ("strtok: STR must be a string or cell array of strings.");
  elseif (ischar (str) && ! isvector (str) &&! isempty (str))
    error ("strtok: STR cannot be a 2-D character array.");
  endif

  if (nargin < 2 || isempty (delim))
    ws_delim = true;
  else
    ws_delim = false;
  endif

  if (isempty (str))
    tok = rem = "";
  elseif (ischar (str))
    if (ws_delim)
      idx = isspace (str);
    elseif (length (delim) <= 7)
      ## Build index of delimiters incrementally for low N.
      idx = str == delim(1);
      for i = 2:length (delim)
        idx |= str == delim(i);
      endfor
    else
      ## Index the str into a mask of valid values.  Faster for large N.
      f = false (256, 1);
      ## This is slower than it could be because of the +1 issue.
      f(uint8 (delim)+1) = true;
      ## Default goes via double -- unnecessarily long.
      si = uint32 (str);
      ## in-place is faster than str+1
      ++si;
      idx = f(si);
    endif

    idx_dlim = find (idx, 1);
    idx_nodlim = find (! idx, 1);
    if (isempty (idx_dlim))
      ## No delimiter.  Return whole string.
      tok = str;
      rem = "";
    elseif (idx_dlim > idx_nodlim)
      ## Normal case.  No leading delimiters and at least 1 delimiter in STR.
      tok = str(1:idx_dlim-1);
      rem = str(idx_dlim:end);
    else
      ## Leading delimiter found.
      idx_dlim = find (idx(idx_nodlim+1:end), 1);
      if (isempty (idx_dlim))
        ## No further delimiters.  Return STR stripped of delimiter prefix.
        tok = str(idx_nodlim:end);
        rem = "";
      else
        ## Strip delimiter prefix.  Return STR up to 1st delimiter
        tok = str(idx_nodlim:(idx_dlim + idx_nodlim -1));
        rem = str((idx_dlim + idx_nodlim):end);
      endif
    endif
  else    # Cell array of strings
    if (ws_delim)
      delim = '\s';
    endif
    ptn = [ '^[' delim ']*','([^' delim ']+)','([' delim '].*)$' ];
    matches = regexp (str, ptn, "tokens");
    eidx = cellfun ("isempty", matches);
    midx = ! eidx;
    tok = cell (size (str));
    tok(eidx) = regexprep (str(eidx), [ '^[' delim ']+' ], '');
    ## Unwrap doubly nested cell array from regexp
    tmp = [matches{midx}];
    if (! isempty (tmp))
      tmp = [tmp{:}];
    endif
    tok(midx) = tmp(1:2:end);
    if (isargout (2))
      rem = cell (size (str));
      rem(eidx) = {""};
      rem(midx) = tmp(2:2:end);
    endif
  endif

endfunction


%!demo
%! strtok ("this is the life")
%! % split at the first space, returning "this"

%!demo
%! s = "14*27+31"
%! while (1)
%!   [t, s] = strtok (s, "+-*/");
%!   printf ("<%s>", t);
%!   if (isempty (s))
%!     break;
%!   endif
%!   printf ("<%s>", s(1));
%! endwhile
%! printf ("\n");
%! % ----------------------------------------------------
%! % Demonstrates processing of an entire string split on
%! % a variety of delimiters.  Tokens and delimiters are
%! % printed one after another in angle brackets.

## Test the tokens for all cases
%!assert (strtok (""), "");             # no string
%!assert (strtok ("this"), "this");     # no delimiter in string
%!assert (strtok ("this "), "this");    # delimiter at end
%!assert (strtok ("this is"), "this");  # delimiter in middle
%!assert (strtok (" this"), "this");    # delimiter at start
%!assert (strtok (" this "), "this");   # delimiter at start and end
%!assert (strtok (" "), ""(1:0));       # delimiter only

## Test the remainder for all cases
%!test [t,r] = strtok (""); assert (r, "");
%!test [t,r] = strtok ("this"); assert (r, "");
%!test [t,r] = strtok ("this "); assert (r, " ");
%!test [t,r] = strtok ("this is"); assert (r, " is");
%!test [t,r] = strtok (" this"); assert (r, "");
%!test [t,r] = strtok (" this "); assert (r, " ");
%!test [t,r] = strtok (" "); assert (r, "");

## Test all tokens and remainders with cell array input
%!test
%! str = {"", "this", "this ", "this is", " this", " this ", " "};
%! [t, r] = strtok (str);
%! assert (t{1}, "");
%! assert (r{1}, "");
%! assert (t{2}, "this");
%! assert (r{2}, "");
%! assert (t{3}, "this");
%! assert (r{3}, " ");
%! assert (t{4}, "this");
%! assert (r{4}, " is");
%! assert (t{5}, "this");
%! assert (r{5}, "");
%! assert (t{6}, "this");
%! assert (r{6}, " ");
%! assert (t{7}, "");
%! assert (r{7}, "");

## Simple check for 2, 3, and 4 delimeters
%!assert (strtok ("this is", "i "), "th")
%!assert (strtok ("this is", "ij "), "th")
%!assert (strtok ("this is", "ijk "), "th")

## Test all cases for 8 delimiters since a different
%!# algorithm is used when more than 7 delimiters
%!assert (strtok ("","jklmnop "), "")
%!assert (strtok ("this","jklmnop "), "this")
%!assert (strtok ("this ","jklmnop "), "this")
%!assert (strtok ("this is","jklmnop "), "this")
%!assert (strtok (" this","jklmnop "), "this")
%!assert (strtok (" this ","jklmnop "), "this")
%!assert (strtok (" ","jklmnop "), ""(1:0))

## Test 'bad' string orientations
%!assert (strtok (" this ".'), "this".');   # delimiter at start and end
%!assert (strtok (" this ".',"jkl "), "this".');

## Test with TAB, LF, VT, FF, and CR
%!test
%! for ch = "\t\n\v\f\r"
%!   [t, r] = strtok (["beg", ch, "end"]);
%!   assert (t, "beg");
%!   assert (r, [ch, "end"]);
%! endfor

## Test input validation
%!error strtok ()
%!error strtok ("a", "b", "c")
%!error <STR must be a string> strtok (1, "b")
%!error <STR cannot be a 2-D> strtok (char ("hello", "world"), "l")

