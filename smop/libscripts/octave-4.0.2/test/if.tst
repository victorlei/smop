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
%! i = 0;
%! if (i == 0)
%!   i++;
%!   __printf_assert__ ("%d\n", i);
%! end  # "end" is part of test, check not using "endif"
%! assert (__prog_output_assert__ ("1"));

%!test
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! else
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 2;
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 0;
%! y = -2;
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! elseif (y)
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 0;
%! y = -2;
%! if (eye (2))
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! else
%!   __printf_assert__ ("pass\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

%!test
%! x = 0;
%! y = -2;
%! if (y)
%!   __printf_assert__ ("pass\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! elseif (x)
%!   __printf_assert__ ("fail\n");
%! endif
%! assert (__prog_output_assert__ ("pass"));

## test parsing of single-quoted character string appearing at the
## beginning of an if condition
%!test
%! if (1)
%!   'foo';
%!   x = 13;
%! endif
%! assert (x, 13);

## test parsing of single-quoted character string appearing at the
## beginning of an if condition
%!test
%! if (0)
%!   x = 42;
%! elseif (1)
%!   'foo';
%!   x = 13;
%! endif
%! assert (x, 13);

