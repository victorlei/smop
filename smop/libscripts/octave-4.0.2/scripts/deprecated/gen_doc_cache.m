## Copyright (C) 2013-2015 Rik Wehbring
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
## @deftypefn {Function File} {} gen_doc_cache (@var{out_file}, @var{directory})
## This function has been deprecated.  Use @code{doc_cache_create} instead.
## @seealso{doc_cache_create}
## @end deftypefn

## Deprecated in 3.8

function gen_doc_cache (varargin)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "gen_doc_cache is obsolete and will be removed from a future version of Octave, please use doc_cache_create instead");
  endif

  doc_cache_create (varargin{:});

endfunction

