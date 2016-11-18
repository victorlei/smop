function h = plot (p, varargin)
  n = 128;
  rmax = max (abs (roots (p.poly)));
  x = [0 : (n - 1)] / (n - 1) * 2.2 * rmax - 1.1 * rmax;
  if (nargout > 0)
    h = plot (x, p(x), varargin{:});
  else
    plot (x, p(x), varargin{:});
  endif
endfunction
