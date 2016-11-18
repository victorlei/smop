function s = Pork (geek, gurk)

  if (nargin == 1 && isa (geek, 'Pork'))
    s = geek;
  else
    s.gurk = 0;
    if (nargin == 0)
      s0 = Spork ();
    elseif (nargin == 1)
      s0 = Spork (geek);
    else
      s0 = Spork (geek);
      s.gurk = gurk;
    end
    s = class (s, 'Pork', s0);
   end
   superiorto ('Dork');

end
