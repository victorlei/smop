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
## @deftypefn  {Function File} {} refreshdata ()
## @deftypefnx {Function File} {} refreshdata (@var{h})
## @deftypefnx {Function File} {} refreshdata (@var{h}, @var{workspace})
## Evaluate any @samp{datasource} properties of the current figure and update
## the plot if the corresponding data has changed.
##
## If the first argument @var{h} is a list of graphic handles, then operate
## on these objects rather than the current figure returned by @code{gcf}.
##
## The optional second argument @var{workspace} can take the following values:
##
## @table @asis
## @item @qcode{"base"}
## Evaluate the datasource properties in the base workspace.  (default).
##
## @item @qcode{"caller"}
## Evaluate the datasource properties in the workspace of the function
## that called @code{refreshdata}.
## @end table
##
## An example of the use of @code{refreshdata} is:
##
## @example
## @group
## x = 0:0.1:10;
## y = sin (x);
## plot (x, y, "ydatasource", "y");
## for i = 1 : 100
##   pause (0.1);
##   y = sin (x + 0.1*i);
##   refreshdata ();
## endfor
## @end group
## @end example
## @end deftypefn

function refreshdata (h, workspace)

  if (nargin == 0)
    h = gcf ();
    workspace = "base";
  else
    if (iscell (h))
      h = [h{:}];
    endif
    if (! all (isfigure (h)))
      error ("refreshdata: H must be a list of figure handles");
    endif
    if (nargin == 1)
      workspace = "base";
    elseif (nargin == 2)
      if (! ischar (workspace)
          || ! any (strcmpi (workspace, {"base", "caller"})))
        error ('refreshdata: WORKSPACE must be "base" or "caller"');
      endif
      workspace = tolower (workspace);
    else
      print_usage ();
    endif
  endif

  h = findall (h);
  objs = [];
  props = {};

  for i = 1 : numel (h)
    obj = get (h(i));
    flds = fieldnames (obj);
    ## regexp() is proper way to do searching, but is 3X slower.
    ## Pretty unlikely that people are going to be adding datasource
    ## properties that are not, in fact, datasources.
    ## m = regexp (flds, '^.+datasource$');
    m = strfind (flds, "datasource");
    m = flds(! cellfun (@isempty, m));
    for j = 1 : numel (m)
      if (isempty (obj.(m{j})))
        continue;  # datasource field doesn't point to anything
      endif
      expr = obj.(m{j});       # datasource field
      val = evalin (workspace, expr);
      pdname = m{j}(1:end-6);  # property data name without "source"
      set (h(i), pdname, val);
    endfor
  endfor
endfunction


%!demo
%! clf;
%! x = 0:0.1:10;
%! y = sin (x);
%! plot (x, y, 'ydatasource', 'y');
%! title ('refreshdata() showing moving sine curve');
%! axis manual;
%! for i = 1 : 100
%!   pause (0);
%!   y = sin (x + 0.1 * i);
%!   refreshdata (gcf, 'caller');
%! end

