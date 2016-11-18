function out = gick (in, val)

  if (nargin == 1)
    out = in.gick;
  else
    in.gick = val;
    out = in;
  end

end
