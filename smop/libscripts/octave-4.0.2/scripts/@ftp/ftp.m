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
## @deftypefn  {Function File} {@var{f} =} ftp (@var{host})
## @deftypefnx {Function File} {@var{f} =} ftp (@var{host}, @var{username}, @var{password})
## Connect to the FTP server @var{host} with @var{username} and @var{password}.
##
## If @var{username} and @var{password} are not specified, user
## @qcode{"anonymous"} with no password is used.  The returned FTP object
## @var{f} represents the established FTP connection.
##
## The list of actions for an FTP object are shown below.  All functions
## require an FTP object as the first argument.
##
## @multitable @columnfractions 0.15 0.8
## @headitem Method @tab Description
## @item ascii @tab Set transfer type to ascii
## @item binary @tab Set transfer type to binary
## @item cd @tab Change remote working directory
## @item close @tab Close FTP connection
## @item delete @tab Delete remote file
## @item dir @tab List remote directory contents
## @item mget @tab Download remote files
## @item mkdir @tab Create remote directory
## @item mput @tab Upload local files
## @item rename @tab Rename remote file or directory
## @item rmdir @tab Remove remote directory
## @end multitable
##
## @end deftypefn

function obj = ftp (host = "", username = "anonymous", password = "")
  if (nargin == 1 && isa (host, "ftp"))
    obj = host;   # Copy constructor
  else
    p.host = host;
    p.username = username;
    p.password = password;
    p.curlhandle = tempname ("ftp-");
    if (nargin > 0)
      p.curlhandle = __ftp__ (host, username, password);
    endif
    obj = class (p, "ftp");
  endif
endfunction


## No test possible for interactive function.
%!assert (1)

