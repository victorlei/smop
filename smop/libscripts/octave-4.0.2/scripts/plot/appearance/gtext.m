## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} gtext (@var{s})
## @deftypefnx {Function File} {} gtext (@{@var{s1}, @var{s2}, @dots{}@})
## @deftypefnx {Function File} {} gtext (@{@var{s1}; @var{s2}; @dots{}@})
## @deftypefnx {Function File} {} gtext (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{h} =} gtext (@dots{})
## Place text on the current figure using the mouse.
##
## The text is defined by the string @var{s}.  If @var{s} is a cell string
## organized as a row vector then each string of the cell array is written to a
## separate line.  If @var{s} is organized as a column vector then one string
## element of the cell array is placed for every mouse click.
##
## Optional property/value pairs are passed directly to the underlying text
## objects.
##
## The optional return value @var{h} is a graphics handle to the created
## text object(s).
## @seealso{ginput, text}
## @end deftypefn

function h = gtext (s, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (ischar (s) || iscellstr (s)))
    error ("gtext: S must be a string or cell array of strings");
  endif

  htmp = -1;
  if (! isempty (s))
    if (ischar (s) || isrow (s))
      [x, y] = ginput (1);
      htmp = text (x, y, s, varargin{:});
    else
      for i = 1:numel (s)
        [x, y] = ginput (1);
        htmp = text (x, y, s{i}, varargin{:});
      endfor
    endif
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


## Remove from test statistics.  No real tests possible.
%!assert (1)

