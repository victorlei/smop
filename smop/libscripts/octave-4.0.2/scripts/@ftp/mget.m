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
## @deftypefn  {Function File} {} mget (@var{f}, @var{file})
## @deftypefnx {Function File} {} mget (@var{f}, @var{dir})
## @deftypefnx {Function File} {} mget (@var{f}, @var{remote_name}, @var{target})
## Download a remote file @var{file} or directory @var{dir} to the local
## directory on the FTP connection @var{f}.
##
## @var{f} is an FTP object returned by the @code{ftp} function.
##
## The arguments @var{file} and @var{dir} can include wildcards and any
## files or directories on the remote server that match will be downloaded.
##
## If a third argument @var{target} is given, then a single file or
## directory will be downloaded to the local directory and the local name
## will be changed to @var{target}.
## @end deftypefn

function mget (f, file)
  __ftp_mget__ (f.curlhandle, file);
endfunction


## No test possible for interactive function.
%!assert (1)

