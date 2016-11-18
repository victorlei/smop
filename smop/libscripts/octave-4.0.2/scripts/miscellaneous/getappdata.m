## Copyright (C) 2010-2015 Ben Abbott
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
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
## @deftypefn  {Function File} {@var{value} =} getappdata (@var{h}, @var{name})
## @deftypefnx {Function File} {@var{appdata} =} getappdata (@var{h})
## Return the @var{value} of the application data @var{name} for the graphics
## object with handle @var{h}.
##
## @var{h} may also be a vector of graphics handles.  If no second argument
## @var{name} is given then @code{getappdata} returns a structure,
## @var{appdata}, whose fields correspond to the appdata properties.
##
## @seealso{setappdata, isappdata, rmappdata, guidata, get, set, getpref, setpref}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function value = getappdata (h, name)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! all (ishandle (h(:))))
    error ("getappdata: H must be a scalar or vector of graphic handles");
  endif

  if (nargin == 2)
    if (! ischar (name))
      error ("getappdata: NAME must be a string");
    endif

    value = cell (numel (h), 1);
    appdata = struct ();
    for i = 1:numel (h)
      value{i} = [];
      pval = get (h(i));
      if (isfield (pval, "__appdata__") && isfield (pval.__appdata__, name))
        value{i} = pval.__appdata__.(name);
      endif
    endfor

    if (i == 1)
      value = value{1};
    endif

  else  # nargin == 1
    if (numel (h) != 1)
      error ("getappdata: Only one handle H may be used when fetching appdata");
    endif
    try
      value = get (h, "__appdata__");
    catch
      value = struct ();
    end_try_catch
  endif

endfunction


%!test
%! unwind_protect
%!   setappdata (0, "%data1%", ones (3), "%data2%", "hello world");
%!   assert (getappdata (0, "%data1%"), ones (3));
%!   assert (getappdata (0, "%data2%"), "hello world");
%!   appdata = getappdata (0);
%!   name1 = "%data1%";  name2 = "%data2%";
%!   assert (appdata.(name1), ones (3));
%!   assert (appdata.(name2), "hello world");
%! unwind_protect_cleanup
%!   rmappdata (0, "%data1%", "%data2%");
%! end_unwind_protect

## Test input validation
%!error getappdata ()
%!error getappdata (1,2,3)
%!error <H must be a scalar .* graphic handle> getappdata (-1, "hello")
%!error <NAME must be a string> getappdata (0, 1)
%!error <Only one handle H may be used when fetching appdata> getappdata ([0 0])

