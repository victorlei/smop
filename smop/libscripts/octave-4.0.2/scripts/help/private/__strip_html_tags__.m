## Copyright (C) 2009-2015 Søren Hauberg
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
## @deftypefn {Function File} {[@var{text}, @var{status}] =} __strip_html_tags__ (@var{html_text})
## Undocumented internal function.
## @end deftypefn

## Remove HTML tags from text.  This is used as a simple HTML-to-text
## function.

function [text, status] = __strip_html_tags__ (html_text)
  start = find (html_text == "<");
  stop  = find (html_text == ">");
  if (length (start) == length (stop))
    text = html_text;
    for n = length(start):-1:1
      text (start (n):stop (n)) = [];
    endfor
    text = strip_superfluous_endlines (text);
    status = 0;
  else
    warning ("help: invalid HTML data -- raw HTML source follows...");
    disp (html_text);
    text = "";
    status = 1;
  endif
endfunction

## This function removes end-lines (\n) that makes printing look bad
function text = strip_superfluous_endlines (text)
  ## Find groups of end-lines
  els = find (text == "\n");
  dels = diff (els);
  groups = [els(1), 1]; # list containing [start, length] of each group
  for k = 1:length (dels)
    if (dels (k) == 1)
      groups(end, 2) ++;
    else
      groups(end+1, 1:2) = [els(k+1), 1];
    endif
  endfor

  keep = true (size (text));

  ## Remove end-lines in the beginning
  if (groups (1, 1) == 1)
    keep(1:groups (1, 2)) = false;
  endif

  ## Remove end-lines from the end
  if (sum (groups(end, :)) - 1 == length (text))
    keep(groups(end, 1):end) = false;
  endif

  ## Remove groups of end-lines with more than 3 end-lines next to each other
  idx = find (groups (:, 2) >= 3);
  for k = 1:length (idx)
    start = groups(idx(k), 1);
    stop = start + groups(idx(k), 2) - 1;
    keep(start+2:stop) = false;
  endfor

  ## Actually remove the elements
  text = text (keep);
endfunction

