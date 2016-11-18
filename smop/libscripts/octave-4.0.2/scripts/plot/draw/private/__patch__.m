## Copyright (C) 2007-2015 John W. Eaton, Shai Ayal, Kai Habel
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
## @deftypefn {Function File} {[@var{h}, @var{fail}] =} __patch__ (@var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## __patch__ (p, x, y, c)
## Create patch object from x and y with color c and parent p.
## Return handle to patch object.

## Author: Kai Habel

function [h, failed] = __patch__ (p, varargin)

  h = NaN;
  failed = false;

  is_numeric_arg = cellfun (@isnumeric, varargin);

  if (isempty (varargin))
    args = varargin;
  elseif (isstruct (varargin{1}))
    if (isfield (varargin{1}, "vertices") && isfield (varargin{1}, "faces"))
      fvs = varargin{1};
      fvc = cell (1, 2*numfields (fvs));
      fvc(1:2:end) = fieldnames (fvs);
      fvc(2:2:end) = struct2cell (fvs);
      args = [fvc{:}, varargin(2:end)];
    else
      failed = true;
    endif
  elseif (is_numeric_arg(1))
    if (nargin < 3 || ! is_numeric_arg(2))
      failed = true;
    else
      if (nargin > 4 && all (is_numeric_arg(1:4)))
        x = varargin{1};
        y = varargin{2};
        z = varargin{3};
        c = varargin{4};
        iarg = 5;
      elseif (nargin > 3 && all (is_numeric_arg(1:3)))
        x = varargin{1};
        y = varargin{2};
        iarg = 4;
        if (rem (nargin - iarg, 2) == 1)
          c = varargin{iarg};
          z = varargin{3};
          iarg = 5;
        else
          z = [];
          c = varargin{3};
        endif
      elseif (nargin > 2 && all (is_numeric_arg(1:2)))
        x = varargin{1};
        y = varargin{2};
        z = [];
        iarg = 3;
        if (rem (nargin - iarg, 2) == 1)
          c = varargin{iarg};
          iarg++;
        else
          c = [];
        endif
      endif

      if (isvector (x))
        x = x(:);
        y = y(:);
        z = z(:);
        if (isnumeric (c))
          if (isvector (c) && numel (c) == numel (x))
            c = c(:);
          elseif (rows (c) != numel (x) && columns (c) == numel (x))
            c = c.';
          endif
        endif
      endif
      args{1} = "xdata";
      args{2} = x;
      args{3} = "ydata";
      args{4} = y;
      args{5} = "zdata";
      args{6} = z;

      if (isnumeric (c))

        if (ndims (c) == 3 && columns (c) == 1)
          c = permute (c, [1, 3, 2]);
        endif

        if (isvector (c) && numel (c) == columns (x))
          if (isnan (c))
            args{7} = "facecolor";
            args{8} = [1, 1, 1];
            args{9} = "cdata";
            args{10} = c;
          elseif (isnumeric (c))
            args{7} = "facecolor";
            args{8} = "flat";
            args{9} = "cdata";
            args{10} = c;
          else
            error ("patch: color data C must be numeric");
          endif
        elseif (isvector (c) && numel (c) == 3)
          args{7} = "facecolor";
          args{8} = c;
          args{9} = "cdata";
          args{10} = [];
        elseif (ndims (c) == 3 && size (c, 3) == 3)
          ## CDATA is specified as RGB data
          if ((rows (c) == 1 && columns (c) == 1) ...
              || (rows (c) == 1 && columns (c) == columns (x)))
            ## Single patch color or per-face color
            args{7} = "facecolor";
            args{8} = "flat";
            args{9} = "cdata";
            args{10} = c;
          elseif (rows (c) == rows (x) && columns (c) == columns (x))
            ## Per-vertex color
            args{7} = "facecolor";
            args{8} = "interp";
            args{9} = "cdata";
            args{10} = c;
          else
            error ("patch: Invalid TrueColor data C");
          endif
        else
          ## Color Vectors
          if (isempty (c))
            args{7} = "facecolor";
            args{8} = "interp";
            args{9} = "cdata";
            args{10} = [];
          elseif (size_equal (c, x) && size_equal (c, y))
            args{7} = "facecolor";
            args{8} = "interp";
            args{9} = "cdata";
            args{10} = c;
          else
            error ("patch: size of X, Y, and C must be equal");
          endif
        endif
      elseif (ischar (c) && rem (nargin - iarg, 2) == 0)
        ## Assume any additional argument over an even number is a color string.
        args{7} = "facecolor";
        args{8} = tolower (c);
        args{9} = "cdata";
        args{10} = [];
      else
        args{7} = "facecolor";
        args{8} = [0, 0, 0];
        args{9} = "cdata";
        args{10} = [];
      endif

      args = [args, varargin(iarg:end)];
    endif
  else
    args = varargin;
  endif

  if (! failed)
    h = __go_patch__ (p, args{:});
  endif
endfunction
