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
## @deftypefn  {Function File} {} rmappdata (@var{h}, @var{name})
## @deftypefnx {Function File} {} rmappdata (@var{h}, @var{name1}, @var{name2}, @dots{})
## Delete the application data @var{name} from the graphics object with handle
## @var{h}.
##
## @var{h} may also be a vector of graphics handles.  Multiple application data
## names may be supplied to delete several properties at once.
##
## @seealso{setappdata, getappdata, isappdata}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function rmappdata (h, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  h = h(:).';
  if (! all (ishandle (h)))
    error ("rmappdata: H must be a scalar or vector of graphic handles");
  elseif (! iscellstr (varargin))
    error ("rmappdata: NAME must be a string");
  endif

  for hg = h
    if (isprop (hg, "__appdata__"))
      appdata = get (hg, "__appdata__");
      vld = isfield (appdata, varargin);
      if (! all (vld))
        ## FIXME: Should we bother to error out?  Or just silently continue?
        missing = varargin{find (! vld, 1)};
        error ("rmappdata: appdata '%s' is not present", missing);
      endif
      appdata = rmfield (appdata, varargin);
      set (hg, "__appdata__", appdata);
    endif
  endfor

endfunction


%!test
%! setappdata (0, "%hello%", "world");
%! rmappdata (0, "%hello%");
%! assert (isappdata (0, "%hello%"), false);

%!test
%! setappdata (0, "%data1%", ones (3));
%! setappdata (0, "%data2%", {"hello", "world"});
%! rmappdata (0, "%data1%", "%data2%");
%! assert (isappdata (0, "%data1%"), false);
%! assert (isappdata (0, "%data2%"), false);

## Test input validation
%!error rmappdata ()
%!error rmappdata (1)
%!error <H must be a scalar .* graphic handle> rmappdata (-1, "hello")
%!error <NAME must be a string> rmappdata (0, 1)
%!error <appdata 'foobar' is not present> rmappdata (0, "foobar")

