## Copyright (C) 2008-2015 Jaroslav Hajek
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
%! a = 1:3; a(:) = []; assert (size (a), [0, 0])
%!test
%! a = 1:3; a(1:3) = []; assert (size (a), [1, 0])
%!test
%! a = (1:3).'; a(1:3) = []; assert (size (a), [0, 1])
%!test
%! a = ones (3); a(:,:) = []; assert (size (a), [0, 3])
%!test
%! a = ones (3); a(1:3,:) = []; assert (size (a), [0, 3])
%!test
%! a = ones (3); a(:,1:3) = []; assert (size (a), [3, 0])
%!test
%! a = ones (3); fail ("a(1:2,1:2) = []", ".*");
%!test
%! a = ones (3); fail ("a(1:3,1:3) = []", ".*");

## null strings should delete. [,] and [;] should delete.
%!test
%! a = ones (3); a(1:2,:) = [,]; assert (size (a), [1,3])
%!test
%! a = ones (3); a(1:2,:) = [;]; assert (size (a), [1,3])
%!test
%! a = ones (3); a(1:2,:) = ''; assert (size (a), [1,3])
%!test
%! a = ones (3); a(1:2,:) = ""; assert (size (a), [1,3])

## null matrix stored anywhere should lose its special status
%!test
%! a = ones (3); b = []; fail ("a(:,1:3) = b", ".")
%!test
%! a = ones (3); b{1} = []; fail ("a(:,1:3) = b{1}", ".")
%!test
%! a = ones (3); b.x = []; fail ("a(:,1:3) = b.x", ".")

## filtering a null matrix through a function should not delete
%!test
%! a = ones (3); fail ("a(:,1:3) = double ([])")

## subsasgn should work the same way
%!test
%! a = ones (3); a = subsasgn (a, substruct ('()', {':',1:2}), []); assert (size (a), [3,1])
%!test
%! a = ones (3); b = []; fail ("subsasgn (a, substruct ('()', {':',1:2}), b)", ".")

%!test
%! classes = {@int8, @int16, @int32, @int64, ...
%!   @uint8, @uint16, @uint32, @uint64, ...
%!   @single, @double, @logical};
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   x = cls ([1, 2, 3]);
%!   cls_nm = class (x);
%!   x(2) = [];
%!   assert (x, cls ([1, 3]));
%!   assert (class (x), cls_nm);
%!   x(2) = [];
%!   assert (x, cls (1));
%!   assert (class (x), cls_nm);
%!   x(1) = [];
%!   assert (x, cls (zeros (1, 0)));
%!   assert (class (x), cls_nm);
%! endfor
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   x = cls ([1, 2, 3]);
%!   cls_nm = class (x);
%!   x(2) = '';
%!   assert (x, cls ([1, 3]));
%!   assert (class (x), cls_nm);
%!   x(2) = '';
%!   assert (x, cls (1));
%!   assert (class (x), cls_nm);
%!   x(1) = '';
%!   assert (x, cls (zeros (1, 0)));
%!   assert (class (x), cls_nm);
%! endfor
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   x = cls ([1, 2, 3]);
%!   cls_nm = class (x);
%!   x(2) = "";
%!   assert (x, cls ([1, 3]));
%!   assert (class (x), cls_nm);
%!   x(2) = "";
%!   assert (x, cls (1));
%!   assert (class (x), cls_nm);
%!   x(1) = "";
%!   assert (x, cls (zeros (1, 0)));
%!   assert (class (x), cls_nm);
%! endfor
