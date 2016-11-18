## Copyright (C) 2009-2015 David Bateman
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

function b = loadobj (a)
  b = a;
  if (isfield (b, "jobject"))
    b = rmfield (b, "jobject");
  endif
  b.curlhandle = __ftp__ (b.host, b.username, b.password);
  if (isfield (b, "dir"))
    if (! isempty (b.dir))
      __ftp_cwd__ (b.curlhandle, b.dir);
    endif
    b = rmfield (b, "dir");
  elseif (isfield (b, "remotePwd"))
    ## FIXME: Can we read matlab java stringBuffer objects?
    warning ("can not change remote directory in loaded FTP object");
    b = rmfield (b, "remotePwd");
  endif
endfunction


## No test possible for interactive function.
%!assert (1)

