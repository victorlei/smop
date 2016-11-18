function out = bleek (in, val)

  if (nargin == 1)
    out = in.bleek;
  else
    in.bleek = val;
    out = in;
  end

end
