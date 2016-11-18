## Copyright (C) 2013-2015 Ben Abbott
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

%%  Test script for classdef OOP.
%%  Requires the path to contain the test classes.
%%
%%  Note: This script and all classes are also intended to run
%%        in MATLAB to test compatibility.  Don't break that!
%%
%%  To Do:  This script tests to ensure that things done correctly work
%%          corrrectly.  It should also check that things done incorrectly
%%          error properly.
%%
%%  The classes used for the tests reside in the test/classdef with others
%%  in the test directory.
%%
%%  The classes provide the ability to test most of the major features
%%  of the classdef OOP facilities.  There are a number of classes, mostly
%%  kind of the same, that create a hierarchy.

%%  Basic classdef tests for value class
%!shared p, q, i, amt
%! q = foo_value_class ();
%! p = foo_value_class (4, 4*12, 50e3);
%! i = p.rate / (12 * 100);
%! amt = (p.principle * i) / (1 - (1 + i)^(-p.term));
%!assert (isempty (q.rate));
%!assert (isempty (q.principle));
%!assert (isempty (q.term));
%!assert (class (p), "foo_value_class");
%!assert (p.term, 48);
%!assert (p.rate, 4.0);
%!assert (p.principle, 50e3);
%!assert (p.amount, amt, eps ())
%!assert (amount (p), amt, eps ())
%!xtest
%! assert (properties (p), {'rate'; 'term'; 'principle'})
%!xtest
%! assert (methods (p), {'amount'; 'foo_value_class'})
%!assert (isempty (foo_value_class().rate))
%!error <property `rate' is not constant> foo_value_class.rate

%%  Static method and Constant Property
%!assert (foo_static_method_constant_property.radians_per_cycle, 2*pi);
%!assert (foo_static_method_constant_property().radians_per_cycle, 2*pi);
%!assert (foo_static_method_constant_property().pie, pi);
%!error <property `frequency' is not constant> foo_static_method_constant_property.frequency
%!error <method `cosine' is not static> foo_static_method_constant_property.cosine
%!test
%! obj = foo_static_method_constant_property;
%! obj.frequency = 10;
%! assert (obj.cosine (0.1), cos (2 * pi * 10 * 0.1), eps ())
%! assert (obj.sine (0.1), sin (2 * pi * 10 * 0.1), eps ())

%!test
%! obj = foo_method_changes_property_size (3);
%! obj = obj.move_element_to_end (2);
%! assert (obj.element, [1 3 2])
