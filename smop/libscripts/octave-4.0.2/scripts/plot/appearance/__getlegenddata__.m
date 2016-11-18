## Copyright (C) 2010-2015 David Bateman
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
## @deftypefn {Function File} {[@var{hplots}, @var{strings}] =} __getlegenddata__ (@var{h})
## Undocumented internal function.
## @end deftypefn

function [hplots, text_strings] = __getlegenddata__ (hlegend)

  hplots = [];
  text_strings = {};
  ca = getfield (get (hlegend, "userdata"), "handle");
  if (numel (ca) == 1)
    kids = get (ca, "children");
  else
    kids = cell2mat (get (ca, "children"));
  endif

  for i = numel (kids):-1:1
    typ = get (kids(i), "type");
    if (any (strcmp (typ, {"line", "patch", "surface", "hggroup"})))
      if (strcmp (typ, "hggroup"))
        hgkids = get (kids(i), "children");
        for j = 1 : length (hgkids)
          try
            dname = get (hgkids(j), "DisplayName");
            if (! isempty (dname))
              hplots(end+1) = hgkids(j);
              text_strings(end+1) = dname;
              break;  # break from j-loop over hgkids
            endif
          end_try_catch
        endfor
      else
        dname = get (kids(i), "DisplayName");
        if (! isempty (dname))
          hplots(end+1) = kids(i);
          text_strings(end+1) = dname;
        endif
      endif
    endif
  endfor

endfunction


## No test needed for internal helper function.
%!assert (1)

