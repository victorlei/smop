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
## @deftypefn {Function File} {} delete (@var{f}, @var{file})
## Delete the remote file @var{file} over the FTP connection @var{f}.
##
## @var{f} is an FTP object returned by the @code{ftp} function.
## @end deftypefn

function delete (f, file)
  __ftp_delete__ (f.curlhandle, file);
endfunction


## No test possible for interactive function.
%!assert (1)

