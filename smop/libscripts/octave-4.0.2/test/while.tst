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
%! while (eye (2))
%!   i++;
%!   __printf_assert__ ("%d\n", i);
%! end  # "end" is part of test, check not using "endwhile"
%! assert (__prog_output_assert__ (""));

%!test
%! i = 5;
%! while (--i)
%!   __printf_assert__ ("%d", i);
%! endwhile
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("4321"));

%!test
%! i = 5;
%! while (i)
%!   i--;
%!   __printf_assert__ ("%d", i);
%! endwhile
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("43210"));

%!test
%! i = 0;
%! while (i++ < 20)
%!   if (i > 2)
%!     break;
%!   endif
%!   __printf_assert__ ("%d", i);
%! endwhile
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("12"));

%!test
%! i = 0;
%! while (++i < 5)
%!   if (i < 3)
%!     continue;
%!   endif
%!   __printf_assert__ ("%d", i);
%! endwhile
%! __printf_assert__ ("\n");
%! assert (__prog_output_assert__ ("34"));

%% test parsing of single-quoted character string appearing immediately
%% after a while condition.
%!test
%! i = 0;
%! while (++i < 5)
%!   'foo';
%! endwhile
%! assert (i, 5);
