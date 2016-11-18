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
%! try
%! catch
%!   error ("Shoudn't get here");
%! end  # "end" is part of test, check not using "end_try_catch"

%!test
%! try
%!   clear a;
%!   a;
%! catch
%! end_try_catch
%! a = 1;
%! assert (a,1);

%!test
%! clear x;
%! try
%!   clear a;
%!   a;
%!   x = 1;
%! catch
%! end_try_catch
%! a = 2;
%! assert (!exist ('x'));
%! assert (a,2);

%!test
%! try
%!   clear a;
%!   a;
%! catch
%!   x = 1;
%! end_try_catch
%! assert (exist ('x'));

%!test
%! try
%!   clear a;
%!   a;
%!   error ("Shoudn't get here");
%! catch
%!   assert (lasterr()(1:13), "'a' undefined");
%! end_try_catch
%! assert (lasterr()(1:13), "'a' undefined");

%!test
%! try
%!   error ("user-defined error");
%! catch
%!   assert (lasterr, "user-defined error");
%! end_try_catch

%!function ms = mangle (s)
%!  ## Wrap angle brackets around S.
%!  ms = ["<" s ">"];
%!endfunction
%!test
%! try
%!   clear a;
%!   a;
%!   error ("Shoudn't get here");
%! catch
%!   assert (mangle (lasterr)(1:14), "<'a' undefined");
%! end_try_catch

%!test
%! try
%!   try
%!     clear a;
%!     a;
%!     error ("Shoudn't get here");
%!   catch
%!     assert (lasterr()(1:13), "'a' undefined");
%!   end_try_catch
%!   clear b;
%!   b;
%!   error ("Shoudn't get here");
%! catch
%!   assert (lasterr()(1:13), "'b' undefined");
%! end_try_catch

%!test
%! try
%!   clear a;
%!   a;
%!   error ("Shoudn't get here");
%! catch
%!   try
%!     assert (lasterr()(1:13), "'a' undefined");
%!     clear b;
%!     b;
%!     error ("Shoudn't get here");
%!   catch
%!     assert (lasterr()(1:13), "'b' undefined");
%!   end_try_catch
%! end_try_catch

%!test
%! try
%!   try
%!     clear a;
%!     a;
%!     error ("Shoudn't get here");
%!   catch
%!     error (["rethrow: " lasterr]);
%!   end_try_catch
%! catch
%!   assert (lasterr()(1:22), "rethrow: 'a' undefined");
%! end_try_catch

%!test
%! clear myerr;
%! try
%!   error ("user-defined error");
%! catch myerr
%!   assert (myerr.message, "user-defined error");
%! end_try_catch

%!test
%! try
%!   clear a;
%!   error ("user-defined error");
%! catch a=1;
%!   assert (lasterr, "user-defined error");
%!   assert (a, 1);
%! end_try_catch

%!test
%! clear myerr1
%! clear myerr2
%! try
%!   try
%!     clear a;
%!     a;
%!   catch myerr1
%!     error (myerr1);
%!   end_try_catch
%! catch myerr2
%!   assert (myerr1.message, myerr2.message);
%!   assert (myerr1.identifier, myerr2.identifier);
%! end_try_catch

%!test
%! x = 1;
%! try error ("foo"); catch x; assert (x.message, "foo"); end_try_catch

%!test
%! x = 1;
%! try error ("foo"); catch x end_try_catch
%! assert (x.message, "foo");

%!test
%! x = 1;
%! try error ("foo"); catch, x; assert (x, 1); end_try_catch

%!test
%! x = 1;
%! try error ("foo"); catch; x; assert (x, 1); end_try_catch

%!test
%! x = 1;
%! try error ("foo"); catch
%!   x; assert (x, 1); end_try_catch
