## Copyright (C) 2015 Carnë Draug
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

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   all_colormaps = colormap ("list");
%!
%!   assert (numel (all_colormaps) > 0)
%!
%!   for i = 1:numel (all_colormaps)
%!     f = str2func (all_colormaps{i});
%!
%!     assert (iscolormap (f (1)))
%!     assert (iscolormap (f (12)))
%!     assert (iscolormap (f (200)))
%!
%!     ## bug #44070
%!     assert (class (f (uint8 (12))), "double")
%!     assert (iscolormap (f (uint8 (12))))
%!
%!     assert (f (0), zeros (0, 3))
%!   endfor
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

