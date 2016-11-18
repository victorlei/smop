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
## @deftypefn {Function File} {} binary (@var{f})
## Set the FTP connection @var{f} to use binary mode for transfers.
##
## In binary mode there is no conversion of newlines from the remote
## representation to the local representation.
##
## @var{f} is an FTP object returned by the @code{ftp} function.
## @end deftypefn

function binary (f)
  __ftp_binary__ (f.curlhandle);
endfunction


## No test possible for interactive function.
%!assert (1)

