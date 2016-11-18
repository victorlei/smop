## Copyright (C) 2004-2015 John W. Eaton
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

## -*- texinfo -*-
## @deftypefn  {Function File} {} computer ()
## @deftypefnx {Function File} {@var{c} =} computer ()
## @deftypefnx {Function File} {[@var{c}, @var{maxsize}] =} computer ()
## @deftypefnx {Function File} {[@var{c}, @var{maxsize}, @var{endian}] =} computer ()
## @deftypefnx {Function File} {@var{arch} =} computer ("arch")
## Print or return a string of the form @var{cpu}-@var{vendor}-@var{os} that
## identifies the type of computer that Octave is running on.
##
## If invoked with an output argument, the value is returned instead of
## printed.  For example:
##
## @example
## @group
## computer ()
##    @print{} i586-pc-linux-gnu
##
## mycomp = computer ()
##    @result{} mycomp = "i586-pc-linux-gnu"
## @end group
## @end example
##
## If two output arguments are requested, also return the maximum number of
## elements for an array.  This will depend on whether Octave has been
## compiled with 32-bit or 64-bit index vectors.
##
## If three output arguments are requested, also return the byte order of the
## current system as a character (@qcode{"B"} for big-endian or @qcode{"L"}
## for little-endian).
##
## If the argument @qcode{"arch"} is specified, return a string indicating the
## architecture of the computer on which Octave is running.
## @seealso{isunix, ismac, ispc}
## @end deftypefn

function [c, maxsize, endian] = computer (a)

  if (nargin > 1)
    print_usage ();
  elseif (nargin == 1 && ! strcmpi (a, "arch"))
    error ('computer: "arch" is only valid argument');
  endif

  if (nargin == 0)
    msg = octave_config_info ("canonical_host_type");

    if (strcmp (msg, "unknown"))
      msg = "Hi Dave, I'm a HAL-9000";
    endif

    if (nargout == 0)
      disp (msg);
    else
      c = msg;
      if (isargout (2))
        if (strcmp (octave_config_info ("USE_64_BIT_IDX_T"), "true"))
          maxsize = 2^63-1;
        else
          maxsize = 2^31-1;
        endif
      endif
      if (isargout (3))
        if (octave_config_info ("words_big_endian"))
          endian = "B";
        elseif (octave_config_info ("words_little_endian"))
          endian = "L";
        else
          endian = "?";
        endif
      endif
    endif
  else
    ## "arch" argument asked for
    tmp = ostrsplit (octave_config_info ("canonical_host_type"), "-");
    if (numel (tmp) == 4)
      c = sprintf ("%s-%s-%s", tmp{4}, tmp{3}, tmp{1});
    else
      c = sprintf ("%s-%s", tmp{3}, tmp{1});
    endif

  endif

endfunction


%!assert (ischar (computer ()))
%!assert (computer (), octave_config_info ("canonical_host_type"))
%!assert (ischar (computer ("arch")))

%!error computer (1,2)
%!error <"arch" is only valid argument> computer ("xyz")

