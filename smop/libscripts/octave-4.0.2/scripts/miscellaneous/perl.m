## Copyright (C) 2008-2015 Julian Schnidder
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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{output} =} perl (@var{scriptfile})
## @deftypefnx {Function File} {@var{output} =} perl (@var{scriptfile}, @var{argument1}, @var{argument2}, @dots{})
## @deftypefnx {Function File} {[@var{output}, @var{status}] =} perl (@dots{})
## Invoke Perl script @var{scriptfile}, possibly with a list of command line
## arguments.
##
## Return output in @var{output} and optional status in @var{status}.  If
## @var{scriptfile} is not an absolute file name it is searched for in the
## current directory and then in the Octave loadpath.
## @seealso{system, python}
## @end deftypefn

function [output, status] = perl (scriptfile = "-e ''", varargin)

  ## VARARGIN is intialized to {}(1x0) if no additional arguments are
  ## supplied, so there is no need to check for it, or provide an
  ## initial value in the argument list of the function definition.

  if (ischar (scriptfile)
      && (   (nargin == 1 && ! isempty (scriptfile))
          || (nargin != 1 && iscellstr (varargin))))
    if (! strcmp (scriptfile(1:2), "-e"))
      ## Attempt to find file in loadpath.  No effect for absolute filenames.
      scriptfile = file_in_loadpath (scriptfile);
    endif

    [status, output] = system (["perl " scriptfile ...
                                sprintf(" %s", varargin{:})]);
  else
    error ("perl: invalid arguments");
  endif

endfunction


%!error <invalid arguments> perl (123)

