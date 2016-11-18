function s = Snork (gick)
% Test class.

  if (nargin == 1 && isa (gick, 'Snork'))
    s = gick;
  else
    s.cack = [0 1 2 3];
    if (nargin == 0)
      s.gick = 1;
    else
      s.gick = gick;
    end
    s = class (s, 'Snork');
  end

end
