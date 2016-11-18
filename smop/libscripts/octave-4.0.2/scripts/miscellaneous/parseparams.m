## Copyright (C) 2006-2015 Alexander Barth
## Copyright (C) 2010 VZLU Prague
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
## @deftypefn  {Function File} {[@var{reg}, @var{prop}] =} parseparams (@var{params})
## @deftypefnx {Function File} {[@var{reg}, @var{var1}, @dots{}] =} parseparams (@var{params}, @var{name1}, @var{default1}, @dots{})
## Return in @var{reg} the cell elements of @var{param} up to the first
## string element and in @var{prop} all remaining elements beginning with the
## first string element.
##
## For example:
##
## @example
## @group
## [reg, prop] = parseparams (@{1, 2, "linewidth", 10@})
## reg =
## @{
##   [1,1] = 1
##   [1,2] = 2
## @}
## prop =
## @{
##   [1,1] = linewidth
##   [1,2] = 10
## @}
## @end group
## @end example
##
## The parseparams function may be used to separate regular numeric arguments
## from additional arguments given as property/value pairs of the
## @var{varargin} cell array.
##
## In the second form of the call, available options are specified directly
## with their default values given as name-value pairs.  If @var{params} do
## not form name-value pairs, or if an option occurs that does not match any
## of the available options, an error occurs.
##
## When called from an m-file function, the error is prefixed with the name
## of the caller function.
##
## The matching of options is case-insensitive.
##
## @seealso{varargin, inputParser}
## @end deftypefn

## Author: Alexander Barth <abarth93@users.sourceforge.net>
## Author: Aida Alvera Azcarate <aida@netecho.info>

function [reg, varargout] = parseparams (params, varargin)

  strs = cellfun ("isclass", params, "char");
  i = find (strs, 1);
  if (i)
    reg = params(1:i-1);
    prop = params(i:end);
  else
    reg = params;
    prop = {};
  endif

  if (nargin == 1)
    varargout = {prop};
  else
    names = varargin(1:2:end);
    defaults = varargin(2:2:end);
    if (! size_equal (names, defaults))
      error ("parseparams: needs odd number of arguments");
    endif
    [names, sidx] = sort (names);

    varargout = defaults;
    if (i)
      ## Let's parse the properties.
      pnames = prop(1:2:end);
      values = prop(2:2:end);
      if (! size_equal (pnames, values) || ! all (strs(i:2:end)))
        error_as_caller ("options must be given as name-value pairs");
      endif
      idx = lookup (toupper (names), toupper (pnames), "m");
      if (! all (idx))
        error_as_caller ("unrecognized option: %s", pnames{find (idx == 0, 1)});
      else
        varargout(sidx(idx)) = values;
      endif
    endif
  endif

endfunction

function error_as_caller (msg, varargin)
  stack = dbstack (1); # omit me
  fname = stack(min (2, end)).name;
  error ([fname, ": ", msg], varargin{:});
endfunction


%!test
%! [reg, prop] = parseparams ({1, 2, "linewidth", 10});
%! assert (reg, {[1], [2]});
%! assert (prop, {"linewidth", 10});
%!test
%! [reg, prop] = parseparams ({1, 2, 3});
%! assert (reg, {[1], [2], [3]});
%! assert (isempty (prop));
%!test
%! [reg, prop] = parseparams ({"prop1", "val1"});
%! assert (isempty (reg));
%! assert (prop, {"prop1", "val1"});
%!test
%! [reg, prop1] = parseparams ({"linewidth", 5}, "linewidth", 10);
%! assert (isempty (reg));
%! assert (prop1, 5);

%!error <needs odd number of arguments> parseparams ({1}, "linewidth")
%!error <must be given as name-value pairs> parseparams ({1, "color"}, "linewidth", 5)
%!error <unrecognized option: color> parseparams ({1, "color", 5}, "linewidth", 5)

