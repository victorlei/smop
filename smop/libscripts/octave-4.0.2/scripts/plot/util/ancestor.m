## Copyright (C) 2007-2015 Michael Goffioul
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
## @deftypefn  {Function File} {@var{parent} =} ancestor (@var{h}, @var{type})
## @deftypefnx {Function File} {@var{parent} =} ancestor (@var{h}, @var{type}, "toplevel")
## Return the first ancestor of handle object @var{h} whose type matches
## @var{type}, where @var{type} is a character string.
##
## If @var{type} is a cell array of strings, return the first parent whose
## type matches any of the given type strings.
##
## If the handle object @var{h} itself is of type @var{type}, return @var{h}.
##
## If @qcode{"toplevel"} is given as a third argument, return the highest
## parent in the object hierarchy that matches the condition, instead
## of the first (nearest) one.
## @seealso{findobj, findall, allchild}
## @end deftypefn

function p = ancestor (h, type, toplevel)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (ischar (type))
    type = { type };
  elseif (! iscellstr (type))
    error ("ancestor: TYPE must be a string or cell array of strings");
  endif

  find_first = true;
  if (nargin == 3)
    if (ischar (toplevel) && strcmpi (toplevel, "toplevel"))
      find_first = false;
    else
      error ('ancestor: third argument must be "toplevel"');
    endif
  endif

  if (isempty (h))
    p = [];
  else
    p = cell (numel (h), 1);
    h = num2cell (h);
    for nh = 1:numel (h)
      while (true)
        if (isempty (h{nh}) || ! ishandle (h{nh}))
          break;
        endif
        if (any (strcmpi (get (h{nh}, "type"), type)))
          p{nh} = h{nh};
          if (find_first)
            break;
          endif
        endif
        h{nh} = get (h{nh}, "parent");
      endwhile
    endfor
    if (nh == 1)
      p = p{1};
    endif
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hl = line;
%!   assert (ancestor (hl, "axes"), gca);
%!   assert (ancestor (hl, "figure"), hf);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!    hg1 = hggroup ("parent", gca);
%!    hg2 = hggroup ("parent", hg1);
%!    hl = line ("parent", hg2);
%!    assert (ancestor (hl, "line"), hl);
%!    assert (ancestor (hl, "axes"), gca);
%!    assert (ancestor (hl, "figure"), hf);
%!    assert (ancestor (hl, "hggroup"), hg2);
%!    assert (ancestor (hl, "hggroup", "toplevel"), hg1);
%!    assert (ancestor (hl, {"hggroup", "axes"}), hg2);
%!    assert (ancestor (hl, {"hggroup", "axes"}, "toplevel"), gca);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!assert (ancestor ([], "axes"), [])

%!error ancestor ()
%!error ancestor (1,2,3)
%!error <TYPE must be a string> ancestor (1,2)
%!error <third argument must be "toplevel"> ancestor (1, "axes", "foo")

