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
## @deftypefn  {Function File} {} setappdata (@var{h}, @var{name}, @var{value})
## @deftypefnx {Function File} {} setappdata (@var{h}, @var{name1}, @var{value1}, @var{name2}, @var{value3}, @dots{})
## Set the application data @var{name} to @var{value} for the graphics object
## with handle @var{h}.
##
## @var{h} may also be a vector of graphics handles.  If the application data
## with the specified @var{name} does not exist, it is created.  Multiple
## @var{name}/@var{value} pairs can be specified at a time.
##
## @seealso{getappdata, isappdata, rmappdata, guidata, get, set, getpref, setpref}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function setappdata (h, varargin)

  h = h(:).';
  if (! all (ishandle (h)))
    error ("setappdata: H must be a scalar or vector of graphic handles");
  elseif (mod (numel (varargin), 2) != 0)
    error ("setappdata: NAME/VALUE arguments must occur in pairs");
  endif

  for hg = h
    try
      appdata = get (hg, "__appdata__");
    catch
      appdata = struct ();
      addproperty ("__appdata__", hg, "any", appdata);
    end_try_catch

    for narg = 1:2:numel (varargin)
      if (ischar (varargin{narg}))
        appdata.(varargin{narg}) = varargin{narg+1};
      elseif (iscellstr (varargin{narg}))
        ## Handle cell arrays like set() does.
        set (hg, "__appdata__", appdata);
        setappdata (hg, vertcat (varargin{narg}', varargin{narg+1}'){:});
        appdata = get (hg, "__appdata__");
      else
        error ("setappdata: NAME must be a string or cellstr");
      endif
    endfor

    set (hg, "__appdata__", appdata);
  endfor

endfunction


%!test
%! unwind_protect
%!   setappdata (0, "%hello%", "world");
%!   assert (isappdata (0, "%hello%"), true);
%!   assert (getappdata (0, "%hello%"), "world");
%! unwind_protect_cleanup
%!   rmappdata (0, "%hello%");
%! end_unwind_protect

%!test
%! unwind_protect
%!   setappdata (0, "%data1%", ones (3), "%data2%", "hello world");
%!   assert (getappdata (0, "%data1%"), ones (3));
%!   assert (getappdata (0, "%data2%"), "hello world");
%!   setappdata (0, "%data1%", zeros (3));
%!   assert (getappdata (0, "%data1%"), zeros (3));
%! unwind_protect_cleanup
%!   rmappdata (0, "%data1%", "%data2%");
%! end_unwind_protect

## Test input validation
%!error <H must be a scalar .* graphic handle> rmappdata (-1, "hello")
%!error <NAME must be a string> setappdata (0, 1, 2)
%!error <NAME/VALUE arguments must occur in pairs> setappdata (0, "1")

