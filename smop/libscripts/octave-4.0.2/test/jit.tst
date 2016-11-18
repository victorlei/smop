## Copyright (C) 2012-2015 Max Brister
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3 of the License, or (at your
## option) any later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Author: Max Brister <max@2bass.com>

## Turn on JIT and set defaults before running tests
%!testif HAVE_LLVM
%! global __old_jit_enable__;
%! global __old_jit_startcnt__;
%! __old_jit_enable__ = jit_enable (true);
%! __old_jit_startcnt__ = jit_startcnt (1000);

## Test some simple cases that compile.
%!testif HAVE_LLVM
%! jit_failcnt (0)
%! for i=1:1e6
%!   if (i < 5)
%!     break;
%!   else
%!     break;
%!   endif
%! endfor
%! assert (i, 1);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! while (1)
%!   if (1)
%!     break;
%!   else
%!     break;
%!   endif
%! endwhile
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! do
%!   break;
%! until (0)
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! do
%!   if (1)
%!     break;
%!   end;
%! until (0)
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! i=1;
%! do
%!   continue;
%!   i=i+1;
%! until (1)
%! assert (i, 1);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! for i=1:1e6
%!   if (i == 100)
%!     break;
%!   endif
%! endfor
%! assert (i, 100);
%! assert (jit_failcnt, 0);

## Also test parfor keyword
%!testif HAVE_LLVM
%! jit_failcnt (0)
%! parfor i=1:1e6
%!   if (i == 100)
%!     break;
%!   endif
%! endparfor
%! assert (i, 100);
%! assert (jit_failcnt, 0);
## Test some switch statements
%!testif HAVE_LLVM
%! jit_failcnt (0)
%! do
%!   switch (1)
%!   end;
%! until(1)
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! do
%!   switch (1)
%!   case 1
%!     break;
%!   end;
%! until(1)
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! do
%!   switch (1)
%!   otherwise
%!     break;
%!   end;
%! until(1)
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! do
%!   switch (1)
%!   case 1
%!     break;
%!   otherwise
%!     break;
%!   end;
%! until(1)
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! i=0;
%! a=0;
%! b=0;
%! do
%!   i=i+1;
%!   switch (i)
%!   case 1
%!     continue;
%!   case 2
%!     b=1;
%!     continue;
%!   case 4
%!     break;
%!   otherwise
%!     a=a+5;
%!   end;
%!   a=a+1;
%! until(0);
%! assert (i, 4);
%! assert (a, 6);
%! assert (b, 1);
%! assert (jit_failcnt, 0);

## Some more complex calculations
%!testif HAVE_LLVM
%! jit_failcnt (0)
%! inc = 1e-5;
%! result = 0;
%! for ii = 0:inc:1
%!   result = result + inc * (1/3 * ii * ii);
%! endfor
%! assert (abs (result - 1/9) < 1e-5);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! inc = 1e-5;
%! result = 0;
%! for ii = 0:inc:1
%!   ## the ^ operator's result is complex
%!   result = result + inc * (1/3 * ii ^ 2);
%! endfor
%! assert (abs (result - 1/9) < 1e-5);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! temp = 1+1i;
%! nan = NaN;
%! while (1)
%!   temp = temp - 1i;
%!   temp = temp * nan;
%!   break;
%! endwhile
%! assert (imag (temp), 0);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! temp = 1+1i;
%! nan = NaN+1i;
%! while (1)
%!   nan = nan - 1i;
%!   temp = temp - 1i;
%!   temp = temp * nan;
%!   break;
%! endwhile
%! assert (imag (temp), 0);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! temp = 1+1i;
%! while (1)
%!   temp = temp * 5;
%!   break;
%! endwhile
%! assert (temp, 5+5i);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! nr = 1001;
%! mat = zeros (1, nr);
%! for i = 1:nr
%!   mat(i) = i;
%! endfor
%! assert (mat == 1:nr);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! nr = 1001;
%! mat = 1:nr;
%! mat(end) = 0; # force mat to a matrix
%! total = 0;
%! for i = 1:nr
%!   total = mat(i) + total;
%! endfor
%! assert (sum (mat) == total);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! nr = 1001;
%! mat = [3 1 5];
%! try
%!   for i = 1:nr
%!     if (i > 500)
%!       result = mat(100);
%!     else
%!       result = i;
%!     endif
%!   endfor
%! catch
%! end_try_catch
%! assert (result == 500);
%! assert (jit_failcnt, 0);

%!function result = gen_test (n)
%!  result = double (rand (1, n) > .01);
%!endfunction

%!function z = vectorized (A, K)
%!  temp = ones (1, K);
%!  z = conv (A, temp);
%!  z = z > K-1;
%!  z = conv (z, temp);
%!  z = z(K:end-K+1);
%!  z = z >= 1;
%!endfunction

%!function z = loopy (A, K)
%!  z = A;
%!  n = numel (A);
%!  counter = 0;
%!  for ii=1:n
%!    if (z(ii))
%!      counter = counter + 1;
%!    else
%!      if (counter > 0 && counter < K)
%!        z(ii-counter:ii-1) = 0;
%!      endif
%!      counter = 0;
%!    endif
%!  endfor
%!
%!  if (counter > 0 && counter < K)
%!    z(end-counter+1:end) = 0;
%!  endif
%!endfunction

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! test_set = gen_test (10000);
%! assert (all (vectorized (test_set, 3) == loopy (test_set, 3)));
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! niter = 1001;
%! i = 0;
%! while (i < niter)
%!   i = i + 1;
%! endwhile
%! assert (i == niter);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! niter = 1001;
%! result = 0;
%! m = [5 10];
%! for i=1:niter
%!   result = result + m(end);
%! endfor
%! assert (result == m(end) * niter);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! ndim = 100;
%! result = 0;
%! m = zeros (ndim);
%! m(:) = 1:ndim^2;
%! i = 1;
%! while (i <= ndim)
%!   for j = 1:ndim
%!     result = result + m(i, j);
%!    endfor
%!   i = i + 1;
%! endwhile
%! assert (result == sum (sum (m)));
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! ndim = 100;
%! m = zeros (ndim);
%! i = 1;
%! while (i <= ndim)
%!   for j = 1:ndim
%!     m(i, j) = (j - 1) * ndim + i;
%!   endfor
%!   i = i + 1;
%! endwhile
%! m2 = zeros (ndim);
%! m2(:) = 1:(ndim^2);
%! assert (all (m == m2));
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! ndim = 2;
%! m = zeros (ndim, ndim, ndim, ndim);
%! result = 0;
%! i0 = 1;
%! while (i0 <= ndim)
%!   for i1 = 1:ndim
%!     for i2 = 1:ndim
%!       for i3 = 1:ndim
%!         m(i0, i1, i2, i3) = 1;
%!         m(i0, i1, i2, i3, 1, 1, 1, 1, 1, 1) = 1;
%!         result = result + m(i0, i1, i2, i3);
%!       endfor
%!     endfor
%!   endfor
%!   i0 = i0 + 1;
%! endwhile
%! expected = ones (ndim, ndim, ndim, ndim);
%! assert (all (m == expected));
%! assert (result == sum (expected (:)));
%! assert (jit_failcnt, 0);

%!function test_divide ()
%! state = warning ("query", "Octave:divide-by-zero").state;
%! unwind_protect
%!   warning ("error", "Octave:divide-by-zero");
%!   for i=1:1e5
%!     a = 1;
%!     a / 0;
%!   endfor
%! unwind_protect_cleanup
%!   warning (state, "Octave:divide-by-zero");
%! end_unwind_protect
%!endfunction

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! lasterr ("");
%! try
%!   test_divide ();
%! end_try_catch
%! assert (strcmp (lasterr (), "division by zero"));
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! while (1)
%!   a = 0;
%!   result = a / 1;
%!   break;
%! endwhile
%! assert (result, 0);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! m = zeros (2, 1001);
%! for i=1:1001
%!   m(end, i) = i;
%!   m(end - 1, end - i + 1) = i;
%! endfor
%! m2 = zeros (2, 1001);
%! m2(1, :) = fliplr (1:1001);
%! m2(2, :) = 1:1001;
%! assert (m, m2);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! m = [1 2 3];
%! for i=1:1001
%!   m = sin (m);
%!   break;
%! endfor
%! assert (m == sin ([1  2 3]));
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! i = 0;
%! while i < 10
%!   i += 1;
%! endwhile
%! assert (i == 10);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! i = 0;
%! while i < 10
%!   a = ++i;
%! endwhile
%! assert (i == 10);
%! assert (a == 10);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! i = 0;
%! while i < 10
%!   a = i++;
%! endwhile
%! assert (i == 10);
%! assert (a == 9);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! num = 2;
%! a = zeros (1, num);
%! i = 1;
%! while i <= num
%!   a(i) = norm (eye (i));
%!   ++i;
%! endwhile
%! assert (a, ones (1, num));
%! assert (jit_failcnt, 0);

%!function test_compute_idom ()
%! while (li <= length (l1) && si <= length (s1))
%!   if (l1 (li) < s1 (si))
%!     if (li == si)
%!       break;
%!     endif;
%!     li++;
%!   else
%!     si++;
%!   endif;
%! endwhile

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! lasterr ("");
%! try
%!   test_compute_idom ();
%! end_try_catch
%! assert (! isempty (lasterr ()));
%! assert (jit_failcnt, 1);

%!function x = test_overload (a)
%!  while (1)
%!    x = a;
%!    break;
%!  endwhile
%!endfunction

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! assert (test_overload (1), 1);
%! assert (test_overload ([1 2]), [1 2]);
%! assert (jit_failcnt, 0);

%!function a = bubble (a = [3 2 1])
%!  swapped = 1;
%!  n = length (a);
%!  while (swapped)
%!    swapped = 0;
%!    for i = 1:n-1
%!      if (a(i) > a(i + 1))
%!        swapped = 1;
%!        temp = a(i);
%!        a(i) = a(i + 1);
%!        a(i + 1) = temp;
%!      endif
%!    endfor
%!  endwhile
%!endfunction

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! assert (bubble (), [1 2 3]);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! a = 0;
%! b = 1;
%! for i=1:1e3
%!   for j=1:2
%!     a = a + b;
%!   endfor
%! endfor
%! assert (a, 2000);
%! assert (b, 1);
%! assert (jit_failcnt, 0);

%!xtest
%! ## FIXME: No support for functions with complex input prototypes
%! ## testif HAVE_LLVM
%! jit_failcnt (0)
%! a = [1+1i 1+2i];
%! b = 0;
%! while (1)
%!   b = a(1);
%!   break;
%! endwhile
%! assert (b, a(1));
%! assert (jit_failcnt, 0);

%!function test_undef ()
%!  for i=1:1e7
%!    XXX;
%!  endfor
%!endfunction

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! lasterr ("");
%! try
%!   test_undef ();
%! end_try_catch
%! assert (strncmp (lasterr (), "'XXX' undefined near", 20));
%! assert (jit_failcnt, 1);

%!shared id
%! id = @(x) x;

%!xtest
%! ## FIXME: No support for functions with complex input prototypes
%! ## testif HAVE_LLVM
%! jit_failcnt (0)
%! assert (id (1), 1);
%! assert (id (1+1i), 1+1i);
%! assert (id (1, 2), 1);
%! assert (jit_failcnt, 0);

%!testif HAVE_LLVM
%! jit_failcnt (0)
%! lasterr ("");
%! try
%!   id ();
%! end_try_catch
%! assert (strncmp (lasterr (), "'x' undefined near", 18));
%! assert (jit_failcnt, 0);

## Restore JIT settings
%!testif HAVE_LLVM
%! global __old_jit_enable__;
%! global __old_jit_startcnt__;
%! jit_enable (__old_jit_enable__);
%! jit_startcnt (__old_jit_startcnt__);
%! clear -g __old_jit_enable__ __old_jit_startcnt__;

