## Copyright (C) 2006-2015 Kai Habel
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
## @deftypefn  {Function File} {} shading (@var{type})
## @deftypefnx {Function File} {} shading (@var{hax}, @var{type})
## Set the shading of patch or surface graphic objects.
##
## Valid arguments for @var{type} are
##
## @table @asis
## @item @qcode{"flat"}
## Single colored patches with invisible edges.
##
## @item @qcode{"faceted"}
## Single colored patches with visible edges.
##
## @item @qcode{"interp"}
## Color between patch vertices are interpolated and the patch edges are
## invisible.
## @end table
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
## @seealso{fill, mesh, patch, pcolor, surf, surface, hidden}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function shading (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("shading", varargin{:});

  if (nargin != 1)
    print_usage ();
  endif

  mode = varargin{1};

  if (isempty (hax))
    hax = gca ();
  endif

  ## Find all patch and surface objects that are descendants of hax
  ## and  which are not part of a contour plot hggroup.
  hlist = [];
  kids = get (hax, "children");
  while (! isempty (kids))
    types = get (kids, "type");
    hlist = [hlist; kids(strcmp(types, "patch"))];
    hlist = [hlist; kids(strcmp(types, "surface"))];
    parents = kids(strcmp (types, "axes"));
    hglist = kids(strcmp (types, "hggroup"));
    for i = 1 : numel (hglist)
      props = get (hglist(i));
      if (! isfield (props, "levelstep"))
        parents(end+1) = hglist(i);
      endif
    endfor
    kids = get (parents, "children");
  endwhile

  ## FIXME: This is the old, simple code.
  ##        Unfortunately, it also shades contour plots which is not desirable.
  ##hp = findobj (hax, "type", "patch");
  ##hs = findobj (hax, "type", "surface");
  ##hlist = [hp(:); hs(:)];

  switch (lower (mode))
    case "flat"
      set (hlist, "facecolor", "flat");
      set (hlist, "edgecolor", "none");
    case "interp"
      set (hlist, "facecolor", "interp");
      set (hlist, "edgecolor", "none");
    case "faceted"
      set (hlist, "facecolor", "flat");
      set (hlist, "edgecolor", [0 0 0]);
    otherwise
      error ('shading: Invalid MODE "%s"', mode);
  endswitch

endfunction


%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! shading faceted;
%! title ('shading ''faceted''');

%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! shading flat;
%! title ('shading ''flat''');

%!demo
%! clf;
%! colormap ('default');
%! sombrero ();
%! shading interp;
%! title ('shading ''interp''');

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks ());
%! shading faceted;
%! title ('shading ''faceted''');

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks ());
%! shading flat;
%! title ('shading ''flat''');

%!demo
%! clf;
%! colormap ('default');
%! pcolor (peaks ());
%! shading interp;
%! title ('shading ''interp''');

