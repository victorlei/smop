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
## @deftypefn {Function File} {} ishghandle (@var{h})
## Return true if @var{h} is a graphics handle and false otherwise.
##
## This function is equivalent to @code{ishandle} and is provided for
## compatibility with @sc{matlab}.
## @seealso{ishandle}
## @end deftypefn

function retval = ishghandle (h)
  ## This function is just included for compatibility as Octave has
  ## no simulink equivalent.
  retval = ishandle (h);
endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (ishghandle (hf));
%!   assert (! ishghandle (-hf));
%!   l = line;
%!   ax = gca;
%!   assert (ishghandle (ax));
%!   assert (! ishghandle (-ax));
%!   assert (ishghandle (l));
%!   assert (! ishghandle (-l));
%!   p = patch;
%!   assert (ishghandle (p));
%!   assert (! ishghandle (-p));
%!   s = surface;
%!   assert (ishghandle (s));
%!   assert (! ishghandle (-s));
%!   t = text;
%!   assert (ishghandle (t));
%!   assert (! ishghandle (-t));
%!   i = image ([1]);
%!   assert (ishghandle (i));
%!   assert (! ishghandle (-i));
%!   hg = hggroup;
%!   assert (ishghandle (hg));
%!   assert (! ishghandle (-hg));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

