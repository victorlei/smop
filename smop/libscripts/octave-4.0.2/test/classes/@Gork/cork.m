function out = cork (in, val)

  if (nargin == 1)
    out = in.Cork;
  else
    in.Cork = val;
    out = in;
  end

end
