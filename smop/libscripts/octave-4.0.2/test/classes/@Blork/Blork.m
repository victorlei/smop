function s = Blork (bleek)
% Test class.

  if (nargin == 1 && isa (bleek, 'Blork'))
    s = bleek;
  else
    if (nargin == 0)
      s.bleek = 1;
    else
      s.bleek = bleek;
    end
    s = class (s, 'Blork');
  end

end
