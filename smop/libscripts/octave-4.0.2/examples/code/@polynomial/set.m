function s = set (p, varargin)
  s = p;
  if (length (varargin) < 2 || rem (length (varargin), 2) != 0)
    error ("set: expecting property/value pairs");
  endif
  while (length (varargin) > 1)
    prop = varargin{1};
    val = varargin{2};
    varargin(1:2) = [];
    if (ischar (prop) && strcmp (prop, "poly"))
      if (isvector (val) && isreal (val))
        s.poly = val(:).';
      else
        error ("set: expecting the value to be a real vector");
      endif
    else
      error ("set: invalid property of polynomial class");
    endif
  endwhile
endfunction
