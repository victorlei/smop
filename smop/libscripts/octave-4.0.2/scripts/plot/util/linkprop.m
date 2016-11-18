## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {@var{hlink} =} linkprop (@var{h}, "@var{prop}")
## @deftypefnx {Function File} {@var{hlink} =} linkprop (@var{h}, @{"@var{prop1}", "@var{prop2}", @dots{}@})
## Link graphic object properties, such that a change in one is propagated to
## the others.
##
## The input @var{h} is a vector of graphic handles to link.
##
## @var{prop} may be a string when linking a single property, or a cell array
## of strings for multiple properties.  During the linking process all
## properties in @var{prop} will initially be set to the values that exist on
## the first object in the list @var{h}.
##
## The function returns @var{hlink} which is a special object describing the
## link.  As long as the reference @var{hlink} exists the link between graphic
## objects will be active.  This means that @var{hlink} must be preserved in
## a workspace variable, a global variable, or otherwise stored using a
## function such as @code{setappdata}, @code{guidata}.  To unlink properties,
## execute @code{clear @var{hlink}}.
##
## An example of the use of @code{linkprop} is
##
## @example
## @group
## x = 0:0.1:10;
## subplot (1,2,1);
## h1 = plot (x, sin (x));
## subplot (1,2,2);
## h2 = plot (x, cos (x));
## hlink = linkprop ([h1, h2], @{"color","linestyle"@});
## set (h1, "color", "green");
## set (h2, "linestyle", "--");
## @end group
## @end example
##
## @seealso{linkaxes}
## @end deftypefn

function hlink = linkprop (h, prop)

  if (nargin != 2)
    print_usage ();
  endif

  if (numel (h) < 2)
    error ("linkprop: H must contain at least 2 handles");
  elseif (! all (ishandle (h(:))))
    error ("linkprop: invalid graphic handle in input H");
  endif

  if (ischar (prop))
    prop = {prop};
  elseif (! iscellstr (prop))
    error ("linkprop: PROP must be a string or cell string array");
  endif

  h = h(:)';  # set() prefers column vectors
  ## Match all objects to the first one in the list before linking
  for j = 1 : numel (prop)
    set (h(2:end), prop{j}, get (h(1), prop{j}));
  endfor

  ## Add listeners to all objects
  for i = 1 : numel (h)
    for j = 1 : numel (prop)
      addlistener (h(i), prop{j},
                   {@update_prop, [h(1:i-1),h(i+1:end)], prop{j}});
    endfor
  endfor

  hlink = onCleanup (@() delete_linkprop (h, prop));

endfunction

function update_prop (h, ~, hlist, prop)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      set (hlist(ishandle (hlist)), prop, get (h, prop));
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function delete_linkprop (hlist, prop)

  for i = 1 : numel (hlist)
    if (ishandle (hlist(i)))
      for j = 1 : numel (prop)
        dellistener (hlist(i), prop{j}),
      endfor
    endif
  endfor

endfunction


%!demo
%! clf;
%! x = 0:0.1:10;
%! subplot (1,2,1);
%! h1 = plot (x, sin (x), 'r');
%! subplot (1,2,2);
%! h2 = plot (x, cos (x), 'b');
%! input ('Type <RETURN> to link plots');
%! hlink = linkprop ([h1, h2], {'color', 'linestyle'});
%! input ('Type <RETURN> to change color');
%! set (h1, 'color', 'green');
%! input ('Type <RETURN> to change linestyle');
%! set (h2, 'linestyle', '--');

%!test
%! hf1 = figure ("visible", "off");
%! hl1 = plot (1:10, "or");
%! hf2 = figure ("visible", "off");
%! hl2 = plot (1:10, "-*g");
%! hf3 = figure ("visible", "off");
%! hl3 = plot (1:10, "-xb");
%! unwind_protect
%!   hlink = linkprop ([hl1, hl2, hl3], {"color", "linestyle"});
%!   ## Test initial values taken from first object in list
%!   assert (get (hl2, "color"), [1 0 0]);
%!   assert (get (hl3, "linestyle"), "none");
%!   ## Test linking
%!   set (hl2, "color", "b");
%!   assert (get (hl1, "color"), [0 0 1]);
%!   assert (get (hl3, "color"), [0 0 1]);
%!   set (hl3, "linestyle", "--");
%!   assert (get (hl1, "linestyle"), "--");
%!   assert (get (hl2, "linestyle"), "--");
%!   ## Test linking of remaining objects after deletion of one object
%!   delete (hl2);
%!   set (hl1, "linestyle", ":");
%!   assert (get (hl3, "linestyle"), ":");
%!   ## Test deletion of link
%!   clear hlink;
%!   set (hl1, "color", "g");
%!   assert (get (hl3, "color"), [0 0 1]);
%! unwind_protect_cleanup
%!   close ([hf1 hf2 hf3]);
%! end_unwind_protect

## Test input validation
%!error linkprop ()
%!error linkprop (1)
%!error linkprop (1,2,3)
%!error <H must contain at least 2 handles> linkprop (1, "color")
%!error <invalid graphic handle in input H> linkprop ([pi, e], "color")
%!error <PROP must be a string or cell string array> linkprop ([0, 0], 1)

