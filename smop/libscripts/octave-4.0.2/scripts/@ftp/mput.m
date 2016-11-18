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

## -*- texinfo -*-
## @deftypefn {Function File} {} mput (@var{f}, @var{file})
## Upload the local file @var{file} into the current remote directory on the
## FTP connection @var{f}.
##
## @var{f} is an FTP object returned by the ftp function.
##
## The argument @var{file} is passed through the @code{glob} function and any
## files that match the wildcards in @var{file} will be uploaded.
## @end deftypefn

function retval = mput (f, file)
  if (nargout == 0)
    __ftp_mput__ (f.curlhandle, file);
  else
    retval = __ftp_mput__ (f.curlhandle, file);
  endif
endfunction


## No test possible for interactive function.
%!assert (1)

