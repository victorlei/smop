function out = gark (in, val)

  if (nargin == 1)
    out = in.gark;
  else
    in.gark = val;
    out = in;
  end

end
