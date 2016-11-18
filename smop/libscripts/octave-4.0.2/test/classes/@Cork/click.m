function out = click (in, val)

  if (nargin == 1)
    out = in.click;
  else
    in.click = val;
    out = in;
  end

end
