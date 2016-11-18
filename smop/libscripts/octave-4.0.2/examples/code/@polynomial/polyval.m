function [y, dy] = polyval (p, varargin)
  if (nargout == 2)
    [y, dy] = polyval (fliplr (p.poly), varargin{:});
  else
    y = polyval (fliplr (p.poly), varargin{:});
  endif
endfunction
