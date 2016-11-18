## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn {Function File} {@var{h} =} __line__ (@var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## __line__ (p, x, y, z)
## Create line object from x, y, and z with parent p.
## Return handle to line object.

## Author: jwe

function h = __line__ (p, varargin)

  nvargs = numel (varargin);

  if (nvargs > 1 && ! ischar (varargin{1}) && ! ischar (varargin{2}))
    if (nvargs > 2 && ! ischar (varargin{3}))
      num_data_args = 3;
    else
      num_data_args = 2;
    endif
  else
    num_data_args = 0;
  endif

  if (num_data_args > 0 && ! size_equal (varargin{1:num_data_args}))
    n = 1:num_data_args;
    m = cellfun (@numel, varargin(1:num_data_args));
    [~, m] = max (m);
    b = ones (size (varargin{m(1)}));
    try
      varargin(n) = cellfun (@(x) bsxfun (@times, b, x), varargin(n),
                             "uniformoutput", false);
    catch
      error ("line: number of X, Y, and Z points must be equal");
    end_try_catch
  endif

  if (rem (nvargs - num_data_args, 2) != 0)
    error ("line: invalid number of PROPERTY / VALUE pairs");
  endif

  other_args = {};
  if (nvargs > num_data_args)
    other_args = varargin(num_data_args+1:end);
  endif

  nlines = 0;
  nvecpts = 0;
  ismat = false (1, 3);
  for i = 1:num_data_args
    tmp = varargin{i}(:,:);
    if (isvector (tmp))
      nlines = max (1, nlines);
      if (! isscalar (tmp))
        if (nvecpts == 0)
          nvecpts = numel (tmp);
        elseif (nvecpts != numel (tmp))
          error ("line: data size mismatch");
        endif
      endif
    else
      ismat(i) = true;
      nlines = max (columns (tmp), nlines);
    endif
    varargin{i} = tmp;
  endfor

  if (num_data_args == 0)
    varargin = {[0, 1], [0, 1]};
    num_data_args = 2;
    nlines = 1;
  endif

  handles = zeros (nlines, 1);

  data = cell (1, 3);

  if (num_data_args > 1)
    data(1:num_data_args) = varargin(1:num_data_args);
    for i = 1:num_data_args
      if (islogical (data{i}))
        data(i) = double (data{i});
      elseif (iscomplex (data{i}))
        data(i) = real (data{i});
      endif
    endfor
  endif

  data_args = reshape ({"xdata", "ydata", "zdata"; data{:}}, [1, 6]);
  mask = reshape ([false(1,3); ismat], [1, 6]);

  for i = 1:nlines
    tmp = data(ismat);
    if (! size_equal (tmp)
        || (nvecpts != 0 && any (nvecpts != cellfun ("size", tmp, 1))))
      error ("line: data size_mismatch");
    endif
    data_args(mask) = cellfun (@(x) x(:,i), data(ismat),
                               "uniformoutput", false);

    handles(i) = __go_line__ (p, data_args{:}, other_args{:});

  endfor

  if (nargout > 0)
    h = handles;
  endif

endfunction

