function out = gurk (in, val)

  if (nargin == 1)
    out = in.gurk;
  else
    in.gurk = val;
    out = in;
  end

end
