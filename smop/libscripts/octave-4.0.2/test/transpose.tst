## Copyright (C) 2006-2015 John W. Eaton
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

%% Basic tests
%!test
%! scalar = 2;
%! assert (scalar', 2);

%!test
%! range = 1:4;
%! assert (range', [1;2;3;4]);

%!test
%! vector = [1;2;3;4];
%! assert (vector', [1,2,3,4]);

%!test
%! matrix = [1,2;3,4];
%! assert (matrix', [1,3;2,4]);

%% Basic tests on complex numbers
%!test
%! scalar = 2i;
%! assert (scalar', -2i);

%!test
%! range = (1:4)*i;
%! assert (range', [-1i;-2i;-3i;-4i]);

%!test
%! vector = [1;2;3;4]*i;
%! assert (vector', [-1i,-2i,-3i,-4i]);

%!test
%! matrix = [1,2;3,4]*i;
%! assert (matrix', [-1i,-3i;-2i,-4i]);

%% Test non-Hermitian transpose
%!test
%! scalar = 2i;
%! assert (scalar.', 2i);

%!test
%! range = (1:4)*i;
%! assert (range.', [1i;2i;3i;4i]);

%!test
%! vector = [1;2;3;4]*i;
%! assert (vector.', [1i,2i,3i,4i]);

%!test
%! matrix = [1,2;3,4]*i;
%! assert (matrix.', [1i,3i;2i,4i]);

%% Basic tests on float complex numbers
%!test
%! scalar = single (2i);
%! assert (scalar', single (-2i));

%!test
%! range = single ((1:4)*i);
%! assert (range', single ([-1i;-2i;-3i;-4i]));

%!test
%! vector = single ([1;2;3;4]*i);
%! assert (vector', single ([-1i,-2i,-3i,-4i]));

%!test
%! matrix = single ([1,2;3,4]*i);
%! assert (matrix', single ([-1i,-3i;-2i,-4i]));

