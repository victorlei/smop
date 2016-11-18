function s = Dork (gick, gack)

  if (nargin == 1 && isa (gick, 'Dork'))
    s = gick;
  else
    s.gack = 0;
    if (nargin == 0)
      s0 = Snork ();
    elseif (nargin == 1)
      s0 = Snork (gick);
    else
      s0 = Snork (gick);
      s.gack = gack;
    end
    s = class (s, 'Dork', s0);
   end

end
