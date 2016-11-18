function out = gack (in, val)

  if (nargin == 1)
    out = in.gack;
  else
    in.gack = val;
    out = in;
  end

end
