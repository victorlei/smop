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

%% FIXME: Can't use 'clear -classes' because it also clears all functions in the
%% namespace of test.m (bug #35881).  This is a problem only if Octave would
%% re-use a class definition that was defined somewhere else.  Unfortunately,
%% that is exactly the case when running 'make check' since the ctor-vs-method
%% test also uses an @parent, @derived, and @other class.
%% Until the bug is fixed, it suffices to make the class names unique so that
%% there is no re-use.  Using the prefix fhdr (fcn-handle-derived-resolution)
%% for this directory.
%%!shared
%%! #clear -classes

%!test
%! p = fhdr_parent (7);
%! assert (numel (p), 7)

%!test
%! d = fhdr_derived (13);
%! assert (numel (d), 13)

%!test
%! p = fhdr_parent (11);
%! f = @numel;
%! assert (f (p), 11)

%!test
%! d = fhdr_parent (21);
%! f = @numel;
%! assert (f (d), 21)

%!test
%! o(1) = fhdr_other (13);
%! o(2) = fhdr_other (42);
%! assert (getsize_loop (o), [13, 42])

%!test
%! o(1) = fhdr_other (13);
%! o(2) = fhdr_other (42);
%! assert (getsize_cellfun (o), [13, 42])

%!test
%! o(1) = fhdr_other (13);
%! o(2) = fhdr_other (42);
%! assert (getsize_arrayfun (o), [13, 42])
