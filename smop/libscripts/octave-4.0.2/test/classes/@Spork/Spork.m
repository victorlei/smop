function s = Spork (geek)
% Test class.

    if (nargin == 1 && isa (geek, 'Spork'))
      s = geek;
    else
      s.cack = [-1 -2 -3 -4];
      if (nargin == 0)
        s.geek = 1;
      else
        s.geek = geek;
      end
      s = class (s, 'Spork');
    end

end
