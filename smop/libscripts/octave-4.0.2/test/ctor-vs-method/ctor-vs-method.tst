## Copyright (C) 2012-2015 John W. Eaton
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

%%  Test script for legacy OOP.
%%  Requires the path to contain the directory ctor-vs-method.
%%
%%  Note: This script and all classes are also intended to run
%%        in Matlab to test compatibility.  Don't break that!

%!shared d, o
%! d = derived ();
%! o = other ();
%!
%!error method (o);

%!test
%! ctrace = {'begin parent/method';
%!           'begin derived/parent';
%!           'begin parent/parent';
%!           'end parent/parent';
%!           'end derived/parent';
%!           'end parent/method'};
%! __trace__ (); %% clear call trace info
%! method (d);
%! assert (__trace__ (), ctrace);

%!test
%! ctrace = {'begin other/parent';
%!           'end other/parent'};
%! __trace__ (); %% clear call trace info
%! parent (o);
%! assert (__trace__ (), ctrace);

%!test
%! ctrace = {'begin derived/parent';
%!           'begin parent/parent';
%!           'end parent/parent';
%!           'end derived/parent'};
%! __trace__ (); %% clear call trace info
%! parent (d);
%! assert (__trace__ (), ctrace);
