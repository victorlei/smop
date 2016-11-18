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
## @deftypefn  {Function File} {} cd (@var{f})
## @deftypefnx {Function File} {} cd (@var{f}, @var{path})
## Get or set the remote directory on the FTP connection @var{f}.
##
## @var{f} is an FTP object returned by the @code{ftp} function.
##
## If @var{path} is not specified, return the remote current working
## directory.  Otherwise, set the remote directory to @var{path} and return
## the new remote working directory.
##
## If the directory does not exist, an error message is printed and the
## working directory is not changed.
## @end deftypefn

function path = cd (f, path)
  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargin == 2)
    __ftp_cwd__ (f.curlhandle, path);
  endif
  path = __ftp_pwd__ (f.curlhandle);
endfunction


## No test possible for interactive function.
%!assert (1)

