## Copyright (C) 2013-2015 Julien Bect
## Copyright (C) 2009-2015 Robert T. Short
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

%% Test script for legacy OOP.
%% Requires the path to contain the test classes.
%%
%% Note: This script and all classes are also intended to run
%%       in MATLAB to test compatibility.  Don't break that!
%%
%%       Note(JBect, 2013/01/27) : in order to ease to process of testing
%%       Matlab compatibility, the syntax assert(observed, expected) should
%%       be avoided. I use assert(isequal(observed, expected) instead.
%%
%% To Do:  This script tests to ensure that things done correctly work
%%         corrrectly.  It should also check that things done incorrectly
%%         error properly.
%%
%% The classes used for the tests reside in the test directory.
%%
%% The classes provide the ability to test most of the major features
%% of the legacy OOP facilities.  There are a number of classes, mostly
%% kind of the same, that create a hierarchy.

%% Test the Snork class.  The Snork class has a number of the standard
%% methods that the others don't so we can test indexing and other
%% features.
%!shared snk, snk1, snk2
%!test snk = Snork ();
%! assert (isequal (gick (snk), 1));
%! assert (isequal (snk.gick, 1));
%! assert (isequal (snk(2), 1));
%! assert (isequal (snk{end}, 3));
%!test snk = gick (snk, 2);
%! assert (isequal (gick (snk), 2));
%!test snk = set (snk, 'gick', 7);
%! assert (isequal (get (snk, 'gick'), 7));
%!test snk.gick = 4;
%! assert (isequal (gick (snk),4));
%!  snk(1) = 3;
%!test snk{end} = 9;
%! assert (isequal (cack (snk), [3 1 2 9]));
%! assert (isequal (getStash (snk), 1));             % Check private functions.
%! assert (isobject (snk));
%! assert (isequal (class (snk), 'Snork'));
%! assert (isa (snk, 'Snork'));
%! assert (!isa (snk, 'Sneetch'));
%! assert (ismethod (snk, 'gick'));
%! assert (!ismethod (snk, 'bletch'));
%! assert (exist ('snk') == 1);
%! assert (exist ('blink') == 0);
%!test snk1 = Snork (snk);
%! assert (isequal (class (snk1), 'Snork'));
%! assert (isequal (gick (snk1), 4));
%!test snk2 = Snork (-3);
%! assert (isequal (class (snk2), 'Snork'));
%! assert (isequal (gick (snk2), -3));
%!test x = [1 2 3 4];
%! assert (isequal (x(snk), 1));

%% x = methods ('Snork');                % Need to test the methods function.
%% save temp snk;
%% load temp                             % This load causes a segfault.
%% assert (isequal (cack(snk), [-1 -2 -3 -4]));      % This is a major bug!

%% The Spork class is a near clone of Snork but without as many standard
%% methods.  We are testing no new octave features, but this is makes
%% sure that we haven't bollixed up the Spork class if we should make
%% changes.  We use Spork in the class hierarchy.
%!shared sprk
%!test sprk = Spork ();
%! assert (isequal (geek (sprk), 1));
%!test sprk = geek (sprk, 3);
%! assert (isequal (geek (sprk), 3));
%!test sprk = set (sprk,'geek',7);
%! assert (isequal (get (sprk, 'geek'), 7));
%! assert (isequal (class (sprk), 'Spork'));
%! assert (isa (sprk, 'Spork'));

%%  The Blork class is a near clone of Snork but without as many standard
%%  methods.  We are testing no new octave features, but this is makes
%%  sure that we haven't bollixed up the Blork class if we should make
%%  changes.  We use Blork in the class hierarchy.
%!shared blrk
%!test blrk = Blork ();
%! assert (isequal (bleek(blrk), 1));
%!test blrk = bleek (blrk, 3);
%! assert (isequal (bleek (blrk), 3));
%!test blrk = set (blrk, 'bleek', 13);
%! assert (isequal (get (blrk, 'bleek'), 13));
%! assert (isequal (class (blrk), 'Blork'));
%! assert (isa (blrk, 'Blork'));

%%  The Cork class is a near clone of Snork but without as many standard
%%  methods.  We are testing no new octave features, but this is makes
%%  sure that we haven't bollixed up the Cork class if we should make
%%  changes.  We use Cork in the class hierarchy.
%!shared crk
%!test crk = Cork (23);
%! assert (isequal (click(crk), 23));
%!test crk = click(crk,3);
%! assert (isequal (click(crk), 3));
%!test crk = set (crk, 'click', 13);
%! assert (isequal (get (crk, 'click'), 13));
%! assert (isequal (class (crk), 'Cork'));
%! assert (isa (crk, 'Cork'));

%%  The Dork class tests single inheritance.
%!shared drk
%!test drk = Dork ();
%! assert (isequal (gack (drk),0));
%!test drk = gack (drk,-2);
%! assert (isequal (gack (drk),-2));
%!test drk = gick (drk,2);
%! assert (isequal (gick (drk),2));
%!test drk = set (drk, 'gick',3, 'gack',-3);
%! assert (isequal (get (drk, 'gick'), 3));
%! assert (isequal (get (drk, 'gack'), -3));
%! assert (isequal (class (drk), 'Dork'));
%! assert (isa (drk, 'Dork'));
%! assert (isa (drk, 'Snork'));
%! assert (isequal (getStash (drk), 2));
%!test drk1 = Dork (drk);
%! assert (isequal (class (drk1), 'Dork'));
%! assert (isa (drk1, 'Snork'));
%! assert (isequal (gick (drk1), 3));
%! assert (isequal (gack (drk1), -3));
%!test drk2 = Dork (-4, 4);
%! assert (isequal (class (drk2), 'Dork'));
%! assert (isa (drk2, 'Snork'));
%! assert (isequal (gick (drk2), -4));
%! assert (isequal (gack (drk2), 4));

%%  The Pork class is essentially a clone of Dork.  It is used as part
%%  of the multiple inheritance test.
%!shared prk, drk
%!test prk = Pork ();
%! assert (isequal (geek (prk), 1));
%! assert (isequal (gurk (prk), 0));
%!test prk = gurk (prk,-3);
%! assert (isequal (gurk (prk), -3));
%!test prk = geek (prk,9);
%! assert (isequal (geek (prk), 9));
%! assert (isequal (class (prk), 'Pork'));
%! assert (isa (prk, 'Pork'));
%! assert (isa (prk, 'Spork'));
%!test drk = Dork ();                   % Precedence.
%! assert (isequal (bling (drk, prk), 2));
%! assert (isequal (bling (prk, drk), 2));

%%  The Gork class tests aggregation and multiple inheritance.
%!shared grk
%!test grk = Gork ();
%! assert (isequal (gick (grk), 1));
%! assert (isequal (geek (grk), 1));
%! assert (isequal (gack (grk), 0));
%! assert (isequal (gurk (grk), 0));
%! assert (isequal (bleek (grk), 1));
%! assert (isequal (gark(grk), -2));
%! assert (isequal (click (cork (grk)), 17));
%! assert (isequal (class (cork (grk)), 'Cork'));
%!test grk = gick (grk, 3);
%!test grk = geek (grk, 4);
%!test grk = gack (grk, -9);
%!test grk = gurk (grk, -8);
%!test grk = bleek (grk, -7);
%!test grk = gark (grk, -6);
%!test grk = cork (grk, click (cork (grk), 23));
%! assert (isequal (gick (grk), 3));
%! assert (isequal (geek (grk), 4));
%! assert (isequal (gack (grk), -9));
%! assert (isequal (gurk (grk), -8));
%! assert (isequal (bleek (grk), -7));
%! assert (isequal (gark (grk), -6));
%! assert (isequal (click (cork (grk)), 23));
%!test
%! cork1 = Cork (13);
%! grk = set (grk, 'gick', -5, 'gack', -6, 'gark', -7, 'cork', cork1);
%! assert (isequal (get (grk, 'gick'), -5));
%! assert (isequal (get (grk, 'gack'), -6));
%! assert (isequal (get (grk, 'gark'), -7));
%! assert (isequal (click(get (grk, 'cork')), 13));
%!test grk = set (grk, 'cork', 12);
%! assert (isequal (click(get (grk, 'cork')),12));
%! assert (isequal (class (cork(grk)), 'Cork'));
%! assert (isequal (class (grk), 'Gork'));
%! assert (isa (grk, 'Gork'));
%! assert (isa (grk, 'Dork'));
%! assert (isa (grk, 'Pork'));
%! assert (isa (grk, 'Blork'));
%! assert (isa (grk, 'Snork'));
%! assert (isa (grk, 'Spork'));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing (some) overloaded operators %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Common variables for testing overloaded operators
%!shared x1, x2, x3, s1, s2, s3
%!  x1 = 1 + rand (3);    s1 = Snork (x1);
%!  x2 = 1 + rand (3);    s2 = Snork (x2);
%!  x3 = diag ([1 2 3]);  s3 = Snork (x3);

%% Test overloaded plus (+) and uplus (unitary +) for the Snork class
%!test  s = s1 + s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 + x2));
%!test  s = s1 + x2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 + x2));
%!test  s = x1 + s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 + x2));
%!test  s = +s1;      assert (isequal (s, s1));

%% Test overloaded minus (-) for the Snork class
%!test  s = s1 - s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 - x2));
%!test  s = s1 - x2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 - x2));
%!test  s = x1 - s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 - x2));
%!test  s = -s1;      assert (isequal (s, Snork (-x1)));

%% Test overloaded mtimes (*) for the Snork class
%!test  s = s1 * s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 * x2));
%!test  s = s1 * x2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 * x2));
%!test  s = x1 * s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 * x2));

%% Test overloaded times (.*) for the Snork class
%!test  s = s1 .* s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 .* x2));
%!test  s = s1 .* x2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 .* x2));
%!test  s = x1 .* s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 .* x2));

%% Test overloaded mpower (^) for the Snork class
%!test  s = s1 ^ 3;   assert (isa (s, 'Snork') && isequal (s.gick, x1 ^ 3));
%!error <mpower Snork!!!>  s = s1 ^ s1;
%!error <mpower Snork!!!>  s = 20 ^ s1;

%% Test overloaded power (.^) for the Snork class
%!test  s = s1 .^ 2;   assert (isa (s, 'Snork') && isequal (s.gick, x1 .^ 2));
%!error <power Snork!!!>  s = s1 .^ s1;
%!error <power Snork!!!>  s = 20 .^ s1;

%% Test overloaded rdivide (./) for the Snork class
%!test  s = s1 ./ s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 ./ x2));
%!test  s = s1 ./ x2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 ./ x2));
%!test  s = x1 ./ s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 ./ x2));

%% Test overloaded ldivide (.\) for the Snork class
%!test  s = s1 .\ s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 .\ x2));
%!test  s = s1 .\ x2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 .\ x2));
%!test  s = x1 .\ s2;  assert (isa (s, 'Snork') && isequal (s.gick, x1 .\ x2));

%% Test overloaded mrdivide (/) for the Snork class
%!test  s = s1 / s3;  assert (isa (s, 'Snork') && isequal (s.gick, x1 / x3));
%!test  s = s1 / x3;  assert (isa (s, 'Snork') && isequal (s.gick, x1 / x3));
%!test  s = x1 / s3;  assert (isa (s, 'Snork') && isequal (s.gick, x1 / x3));

%% Test overloaded mldivide (\) for the Snork class
%!test  s = s3 \ s2;  assert (isa (s, 'Snork') && isequal (s.gick, x3 \ x2));
%!test  s = s3 \ x2;  assert (isa (s, 'Snork') && isequal (s.gick, x3 \ x2));
%!test  s = x3 \ s2;  assert (isa (s, 'Snork') && isequal (s.gick, x3 \ x2));

%% Test overloaded eq (==) for the Snork class
%!assert (s1 == s1)
%!assert (s1 == x1)
%!assert (x1 == s1)
%!assert (!(s1 == (s1 + 1)))
%!assert (!(s1 == (x1 + 1)))
%!assert (!(x1 == (s1 + 1)))

%% Test overloaded ne (!=) for the Snork class
%!assert (!(s1 != s1))
%!assert (!(s1 != x1))
%!assert (!(x1 != s1))
%!assert (s1 != (s1 + 1))
%!assert (x1 != (s1 + 1))
%!assert (s1 != (x1 + 1))

%% Test overloaded lt (<) for the Snork class
%!assert (s1 < (s1 + 1))
%!assert (s1 < (x1 + 1))
%!assert (x1 < (s1 + 1))

%% Test overloaded gt (>) for the Snork class
%!assert (s1 > (s1 - 1))
%!assert (s1 > (x1 - 1))
%!assert (x1 > (s1 - 1))

%% Test overloaded lt (<=) for the Snork class
%!assert (s1 <= (s1 + 1))
%!assert (s1 <= (x1 + 1))
%!assert (x1 <= (s1 + 1))

%% Test overloaded gt (>=) for the Snork class
%!assert (s1 >= (s1 - 1))
%!assert (s1 >= (x1 - 1))
%!assert (x1 >= (s1 - 1))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing horizontal & vertical concatenation %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test overloaded vertcat() for the Snork class
%% See bug #38128 (http://savannah.gnu.org/bugs/?38128)
%!test   s = [s1; s2];  assert (isa (s, 'Snork') && isequal (s.gick, [x1; x2]));
%!xtest  s = [s1; x2];  assert (isa (s, 'Snork') && isequal (s.gick, [x1; x2]));
%!xtest  s = [x1; s2];  assert (isa (s, 'Snork') && isequal (s.gick, [x1; x2]));

%% Test overloaded horzcat() for the Snork class
%% See bug #38128 (http://savannah.gnu.org/bugs/?38128)
%!test   s = [s1 s2];  assert (isa (s, 'Snork') && isequal (s.gick, [x1 x2]));
%!xtest  s = [s1 x2];  assert (isa (s, 'Snork') && isequal (s.gick, [x1 x2]));
%!xtest  s = [x1 s2];  assert (isa (s, 'Snork') && isequal (s.gick, [x1 x2]));

%% Test with the Blork class, where neither vertcat() nor horzcat() is overloaded
%!shared x1, x2, x3
%!test x1 = Blork ();
%!test x2 = [x1 x1];
%!assert (isa (x2, 'Blork') && isequal (size (x2), [1 2]));
%!test x2 = [x1 51];
%!assert (isa (x2, 'Blork') && isequal (size (x2), [1 2]));
%!test x3 = [x2; x2];
%!assert (isa (x3, 'Blork') && isequal (size (x3), [2 2]));
%!test x3 = [x2; [51 x1]];
%!assert (isa (x3, 'Blork') && isequal (size (x3), [2 2]));
%!error <dimension mismatch> x4 = [x1  x3];
%!error <dimension mismatch> x4 = [x1; x3];

%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing precedence %%
%%%%%%%%%%%%%%%%%%%%%%%%

%% default: leftmost object wins
%!shared A, B
%!test A = Snork (rand (2));
%!test B = CPrecedenceTester1 ();  % no call to inferiorto/superiorto
%!assert (isequal (tattack (A, B), 'Snork'))
%!assert (isequal (tattack (B, A), 'CPrecedenceTester1'))  % idem

%!shared A, B
%!test A = Snork (rand (2));
%!test B = CPrecedenceTester2 (1);  % CPrecedenceTester2 > Snork
%!assert (isequal (tattack (A, B), 'CPrecedenceTester2'))
%!assert (isequal (tattack (B, A), 'CPrecedenceTester2'))
%% Trying to change to CPrecendenceTester < Snork
%!error D = CPrecedenceTester2 (2);

%!shared A, B
%!test A = Snork (rand (2));
%!test B = CPrecedenceTester3 (2);  % CPrecedenceTester3 < Snork
%!assert (isequal (tattack (A, B), 'Snork'))
%!assert (isequal (tattack (B, A), 'Snork'))
%% Trying to change to CPrecendenceTester3 > Snork
%!error D = CPrecedenceTester3 (1);

##############################################
## Testing overridden size+numel method     ##
## (builtin size method and similar methods ##
## use the size of the struct container)    ##
##############################################

%!shared st
%!test st = SizeTester ([1 1]);
%! assert (isequal (size (st), [1 1]))
%! assert (isequal (numel (st), 1))
%!assert (isequal (ndims (st), 2))
%!assert (isequal (rows (st), 1))
%!assert (isequal (columns (st), 1))
%!assert (isequal (st, st))
%!assert (isscalar (st))
%!assert (isvector (st))

%!test st = SizeTester ([1 2]);
%! assert (isequal (size (st), [1 2]))
%! assert (isequal (numel (st), 2))
%!assert (isequal (ndims (st), 2))
%!assert (isequal (rows (st), 1))
%!xtest assert (isequal (columns (st), 2))
%!assert (isequal (st, st))                # bug #44334
%!xtest assert (not (isscalar (st)))       # bug #44498
%!assert (isvector (st))

%!test st = SizeTester ([2 3]);
%! assert (isequal (size (st), [2 3]))
%! assert (isequal (numel (st), 6))
%!assert (isequal (ndims (st), 2))
%!xtest assert (isequal (rows (st), 2))
%!xtest assert (isequal (columns (st), 3))
%!assert (isequal (st, st))                # bug #44334
%!xtest assert (not (isscalar (st)))       # bug #44498
%!xtest assert (not (isvector (st)))       # bug #44498

%!test st = SizeTester ([2 3 4]);
%! assert (isequal (size (st), [2 3 4]))
%! assert (isequal (numel (st), 24))
%!xtest assert (isequal (ndims (st), 3))
%!xtest assert (isequal (rows (st), 2))
%!xtest assert (isequal (columns (st), 3))
%!assert (isequal (st, st))                # bug #44334
%!xtest assert (not (isscalar (st)))       # bug #44498
%!xtest assert (not (isvector (st)))       # bug #44498
