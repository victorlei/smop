function out = geek (in, val)

  if (nargin == 1)
    out = in.geek;
  else
    in.geek = val;
    out = in;
  end

end
