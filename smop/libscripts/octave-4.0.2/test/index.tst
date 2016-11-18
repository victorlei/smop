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

%!test
%! a = [];
%! assert (isempty (a));

%!shared a
%! a = 1;
%!assert (a(1), 1)
%!assert (a(:), 1)
%!assert (a(:,:), 1)
%!assert (a(1,:), 1)
%!assert (a(:,1), 1)
%!assert (isempty (a(logical (0))))
%!error a(-1)
%!error a(2)
%!error a(2,:)
%!error a(:,2)
%!error a(-1,:)
%!error a(:,-1)
%!error a([1,2,3])
%!error a([1;2;3])
%!error a([1,2;3,4])
%!error a([0,1])
%!error a([0;1])
%!error a([-1,0])
%!error a([-1;0])

%!shared a, a_prime, mid_a
%! a = [4,3,2,1];
%! a_prime = [4;3;2;1];
%! mid_a = [3,2];

%!assert (a(1),4)
%!assert (a(2),3)
%!assert (all (a(:) == a_prime))
%!assert (all (a(1,:) == a))
%!assert (a(:,3),2)
%!assert (all (a(:,:) == a))
%!assert (all (a(logical ([0,1,1,0])) == mid_a))
%!error a(0)
%!error a(5)
%!error a(0,1)
%!assert (isempty (a(logical (0),:)))
%!error a(:,0)
%!assert (isempty (a([])))
%!assert (isempty (a([],:)))
%!assert (isempty (a(:,[])))

%!shared a, a_fvec, a_col_1, a_col_2, a_row_1, a_row_2
%! a = [1,2;3,4];
%! a_fvec = [1;3;2;4];
%! a_col_1 = [1;3];
%! a_col_2 = [2;4];
%! a_row_1 = [1,2];
%! a_row_2 = [3,4];

%!assert (all (all (a(:,:) == a)))
%!assert (all (a(:) == a_fvec))
%!error a(0)
%!assert (a(2), 3)

%% Additional tests

%!shared a, b
%! a = [1,2;3,4];
%! b = a;
%! b(:,:,2) = [5,6;7,8];

%!assert (a(:), [1;3;2;4])
%!assert (a(1:2), [1,3])
%!assert (a(:,:), [1,2;3,4])
%!assert (a(:,1), [1;3])
%!assert (a(1,1), 1)
%!assert (a(1:2,1), [1;3])
%!assert (a(:,:,1), [1,2;3,4])

%!test
%! c(:,:,1) = [1,2;3,4];
%! c(:,:,2) = [1,2;3,4];
%! assert (a(:,:,[1,1]), c)

%!test
%! c(:,:,1,1) = [1,2;3,4];
%! c(:,:,1,2) = [1,2;3,4];
%! assert (a(:,:,1,[1,1]), c)

%!test
%! c(:,:,1,1) = [1,2;3,4];
%! c(:,:,2,1) = [1,2;3,4];
%! c(:,:,1,2) = [1,2;3,4];
%! c(:,:,2,2) = [1,2;3,4];
%! assert (a(:,:,[1,1],[1,1]), c)

%!assert (a(1,[]), zeros (1,0))
%!assert (a(1,[],[1,1]), zeros (1,0,2))
%!assert (a(1,1,[]), zeros (1,1,0))

%!test
%! c (1:10,1) = 1:10;
%! assert (c, [1:10]');

%!assert (b(:), [1; 3; 2; 4; 5; 7; 6; 8])
%!assert (b(:,:), [1, 2, 5, 6; 3, 4, 7, 8])
%!assert (b(:,1), [1;3])
%!assert (b(:,:,:), reshape ([1,3,2,4,5,7,6,8], [2,2,2]))
%!assert (b(:,1,1), [1;3])
%!assert (b(:,1,1,[1,1]),reshape ([1,3,1,3], [2,1,1,2]))
%!assert (b(1,3), 5)
%!assert (b(1,[3,4]), [5,6])
%!assert (b(1,1:4), [1,2,5,6])
%!assert (b(1,[],:), zeros (1,0,2))
%!assert (b(1,[]), zeros (1,0))
%!assert (b(:,3), [5;7])
%!assert (b([1,2],3), [5;7])
%!assert (b(true (2,1), 3), [5;7])
%!assert (b(false (2,1), 3), zeros (0,1))
%!assert (b([],3), zeros (0,1))

%!shared x
%! ## Dummy shared block to clear any previous definitions
%! x = 1;

%!test
%! a(1,:) = [1,3];
%! assert (a, [1,3]);

%!test
%! a(1,:) = [1;3];
%! assert (a, [1,3]);

%!test
%! a(:,1) = [1;3];
%! assert (a, [1;3]);

%!test
%! a = [1,2;3,4];
%! b (1,:,:) = a;
%! assert (b, reshape (a, [1,2,2]));

%!test
%! a(1,1:4,2) = reshape (1:4, [1,1,4]);
%! b(:,:,2) = 1:4;
%! assert (a, b);

%!test
%! a(:,:,:) = 1:4;
%! assert (a, [1:4]);

%!test
%! a(:,:,1) = 1:4;;
%! assert (a, [1:4]);

%!test
%! a(:,:,1) = [1:4]';
%! assert (a, [1:4]');

%!test
%! a(:,:,1) = reshape (1:4,[1,1,4]);
%! assert (a, [1:4]');

%!test
%! a(:,1,:) = 1:4;
%! assert (a, reshape (1:4,[1,1,4]));

%!test
%! a(:,1,:) = [1:4]';
%! assert (a, [1:4]');

%!test
%! a(:,1,:) = reshape (1:4,[1,1,4]);;
%! assert (a, [1:4]');

%!test
%! a(1,:,:) = 1:4;
%! assert (a, reshape (1:4,[1,1,4]));

%!test
%! a(1,:,:) = [1:4]';
%! assert (a, [1:4]);

%!test
%! a(1,:,:) = reshape (1:4,[1,1,4]);
%! assert (a, [1:4]);

%!test
%! a(1,:,:,:) = reshape (1:4,[1,1,4]);
%! assert (a, reshape (1:4,[1,1,1,4]));

%!error (a(1:2,1:2) = 1:4)

%!shared x
%! x = 1:5;
%!error <attempted to use a complex scalar as an index> x(i)
%!error <attempted to use a complex scalar as an index> x(j)
%!error <attempted to use a complex scalar as an index> x(1+i)

## bug #38357
%!shared d, dd
%! d = diag ([1, 2, 3]);
%! dd = diag ([1, 2, 3], 6, 3);
%!assert (d(1), 1);
%!assert (dd(1), 1);
%!assert (d(3, 3), 3);
%!assert (dd(3, 3), 3);
%!assert (d(2), 0);
%!assert (dd(2), 0);
%!assert (dd(6,1), 0);
%!error d(6,6);
%!error dd(6,6);
%!error d(3,6);
%!error dd(3,6);

## bug 31287
%!test
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false,[]) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false,[],false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false, 1) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2, 2);
%! x = ones (2, 2, 2);
%! x(false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], []) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x([], []) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, []) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], 1, []) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, [], 1, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], 1, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x([], 1, ea2) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2, ea2) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! ea2 = ones (3, 2, 0, 2);
%! x(1, ea2, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, 1) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, [], false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, [], false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], false, false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, [], false, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(:, false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(:, false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, :) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false, :) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, :, [], 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(:, [], false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%!error x(1, 1, []) = [];

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false, 1) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false, []) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false, false, [], false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(1, false, [], false) = [];
%! assert (x, y);

%!shared x, y
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(:, false, 1) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([]) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x([]) = [];
%! assert (x, y);

%!test
%! y = [];
%! x = ones (2, 2);
%! x(:) = [];
%! assert (x, y);

%!test
%! y = sparse ([]);
%! x = sparse (ones (2, 2));
%! x(:) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x(false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x(false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], false) = [];
%! assert (x, y);

%!test
%! y = sparse (ones (2, 2));
%! x = sparse (ones (2, 2));
%! x([], false) = [];
%! assert (x, y);

%!test
%! y = ones (2, 2);
%! x = ones (2, 2);
%! x([], false, :) = [];
%! assert (x, y);
